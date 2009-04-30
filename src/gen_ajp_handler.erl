%%%-------------------------------------------------------------------
%%% File:      gen_ajp_handler.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2009 Jebu Ittiachen
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-18 by Jebu Ittiachen
%%%-------------------------------------------------------------------
% Time to move the worker to an FSM
% gen_server spawns off the fsm for each connected socket
% connection handler is spawned and attached to this server
% Possible states
% initialized switch the connection to {active, once}
% idle - 
% assigned - can get data

-module(gen_ajp_handler).
-author('jebu@jebu.net').
-export([behaviour_info/1]).
-behaviour(gen_fsm).
-record(handler_state, {socket, buffer, hpid}).

%% API
-export([start_link/2, request_data/2, send_data/2, send_headers/2, end_request/1, 
        get_header/2, set_header/2, send_error_response/3, dispatch_request/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([idle/2, idle/3, assigned/2, assigned/3, connect_inited/2]).

-include("ajp_records.hrl").

%%====================================================================
%% API
%%====================================================================
behaviour_info(callbacks) ->
    [{handle_request, 2}, % handle a request for given action and the request params, state
     {handle_get_request, 2},
     {handle_post_request, 2},
     {handle_put_request, 2},
     {handle_delete_request, 2}
    ];
behaviour_info(_Other) ->
    undefined.

%
% exposed startup code
start_link(Name, Socket) ->
  gen_fsm:start_link({local, Name}, ?MODULE, [Socket], []).
  
%
init([Socket]) ->
  process_flag(trap_exit, true),
  {ok, connect_inited, #handler_state{socket = Socket, buffer = <<>>}, 0}.
  
% we are getting a new request, parse packet and handle switch to assigned
idle(Message, _, State) ->
  error_logger:info_report([{"Unknown message received in state idle"}, {message, Message}]),
  {next_state, idle, State}.

%
connect_inited(timeout, State = #handler_state{socket = Socket}) ->
  inet:setopts(Socket,[{active, once}]),
  {next_state, idle, State}.
  
% async version
idle(Message, State) ->
  error_logger:info_report([{"Unknown message received in state idle"}, {message, Message}, {state, State}]),
  {next_state, idle, State}.
  
% assigned - 
% we serve data to the handler get_data
% we send headers back send_headers
% we send data back send_data
% we end the request - switches to idle
% 
assigned({get_data, Length}, _, State = #handler_state{socket = Socket, buffer = <<>>}) ->
  gen_tcp:send(Socket, ajp:encode_get_body_response(Length)),
  {ok, _L, Data} = ajp:read_ajp_packet(Socket, 500),
  D1 = read_requested_data(Socket, Data),
  case ajp:read_buffered_ajp_packet(D1) of
    {ok, _L, Req, Rest} ->
      << DataLength:16, Binary:DataLength/binary >> = Req,
      {reply, {requested_data, Binary, DataLength}, assigned, State#handler_state{buffer = Rest}, ?SCRIPT_TIMEOUT};
    incomplete ->
      error_logger:info_report([{"Did not get enought data"},{data, D1}]),
      {reply, {requested_data, <<>>, 0}, assigned, State#handler_state{buffer = D1}, ?SCRIPT_TIMEOUT}
  end;
  
%
assigned({get_data, _}, _, State = #handler_state{socket = Socket, buffer = Buffer}) ->
  D1 = read_requested_data(Socket, Buffer),
  case ajp:read_buffered_ajp_packet(D1) of
    {ok, _L, Req, Rest} ->
      << DataLength:16, Binary:DataLength/binary >> = Req,
      {reply, {requested_data, Binary, DataLength}, assigned, State#handler_state{buffer = Rest}, ?SCRIPT_TIMEOUT};
    incomplete ->
      error_logger:info_report([{"Did not get enought data"},{data, D1}]),
      {reply, {requested_data, <<>>, 0}, assigned, State#handler_state{buffer = D1}, ?SCRIPT_TIMEOUT}
  end;
  
assigned({send_header, Headers}, _, State = #handler_state{socket = Socket}) 
  when is_record(Headers, ajp_response_envelope) ->
  gen_tcp:send(Socket,
               ajp:encode_header_response(
                 add_standard_headers(Headers))),
  {reply, headers_sent, assigned, State, ?SCRIPT_TIMEOUT};
%
% req to send data
%
assigned({send_data, Binary}, _, State = #handler_state{socket = Socket}) ->
  split_send_data(Socket, Binary),
  {reply, data_sent, assigned, State, ?SCRIPT_TIMEOUT};

%
% Catch all for sync messages in assigned state
assigned(Message, _, State) ->
  error_logger:info_report([{"Unknown message received in state assigned"}, {message, Message}]),
  {next_state, assigned, State}.

%
% async end response 
%
assigned(end_response, State = #handler_state{socket = Socket}) ->
  local_send_end_response(Socket, 1),
  drain_socket(Socket, 1),
  inet:setopts(Socket,[{active, once}]),
  {next_state, idle, State#handler_state{hpid = undefined, buffer = <<>>}};
%
% handle script timeout
%
assigned(timeout, State = #handler_state{socket = Socket}) ->
  local_send_error_response(503, "Timeout - Handler timedout", Socket),
  local_send_end_response(Socket, 1),
  drain_socket(Socket, 1),
  inet:setopts(Socket,[{active, once}]),
  {next_state, idle, State#handler_state{hpid = undefined, buffer = <<>>}};

%
% Catch all for async messages in assigned state
%
assigned(Message, State) ->
  error_logger:info_report([{"Unknown message received in state assigned"}, {message, Message}]),
  {next_state, assigned, State}.
%
%%--------------------------------------------------------------------
%% @spec 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%% @end 
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
  error_logger:info_report([{"Received async event"}, {event, Event}, {state, StateName}, {statedata, State}]),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%% @end 
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
  error_logger:info_report([{"Received sync event from somebody"}, {event, Event}, {from, From}, {state, StateName}, {statedata, State}]),
  {reply, ok, StateName, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% @doc This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%% @end 
%%--------------------------------------------------------------------
handle_info({'EXIT', HPid, _Reason}, assigned, State = #handler_state{socket = Socket, hpid = HPid}) ->
  local_send_error_response(500, "Server Error - Handler died unexpectedly.", 
      term_to_binary(_Reason), Socket),
  local_send_end_response(Socket, 1),
  drain_socket(Socket, 1),
  inet:setopts(Socket,[{active, once}]),
  {next_state, idle, State#handler_state{hpid = undefined, buffer = <<>>}};
%
handle_info({'EXIT', HPid, _Reason}, idle, State) ->
  error_logger:info_report([{"Unknown process died in state idle "}, {pid, HPid}, {state_data, State}]),
  {next_state, idle, State};
%
handle_info({tcp, Socket, Data}, idle, State = #handler_state{socket = Socket, buffer = Buffer}) ->
  {ok, L, AJPBody, Rest} = ajp:read_buffered_ajp_packet(Data),
  try ajp:receive_buffered_message(L, AJPBody) of
    {ok, shutdown_request, <<>>} ->
      error_logger:info_report(["Service Handler received shutdown message"]),
      gen_tcp:close(Socket),
      {stop, normal, State};
    {ok, cping_request, <<>>} ->
      error_logger:info_report(["Service Handler received ping message"]),
      gen_tcp:send(Socket, << $A, $B,1:16,9:8>>),
      {next_state, idle, State#handler_state{buffer = <<Buffer/binary, Rest/binary>>}};
    {ok, Msg, <<>>} when is_record(Msg, ajp_request_envelope) -> 
      {ok, HPid} = init_request(Msg),
      {next_state, assigned, State#handler_state{buffer = <<Buffer/binary, Rest/binary>>, hpid = HPid}, ?SCRIPT_TIMEOUT};
    ReturnVal -> 
      error_logger:error_report(["Service Handler returned with ", ReturnVal]),
      gen_tcp:close(Socket),
      {stop, normal, State}
  catch
    EType:EReason ->
      error_logger:error_report(["Service Handler threw up with ", {exception, EType}, {reason, EReason}]),
      gen_tcp:close(Socket),
      {stop, normal, State}
  end;
  
handle_info(Message, StateName, State) ->
  error_logger:info_report([{"Unknown info message recieved "}, {message, Message}, {state_data, State}, {state, StateName}]),
  {next_state, StateName, State}.

%
%%--------------------------------------------------------------------
%% @spec terminate(Reason, StateName, State) -> void()
%% @doc function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% @spec dispatch_request(Module, Msg, PPid) -> ok
%% @doc routes the request to the proper handler script, does 
%% wrap sround processing of a request.
%% @end 
%%--------------------------------------------------------------------  
dispatch_request(Module, Msg, PPid) ->
  ok = dispatch_method_request(Module, Msg, PPid),
  gen_ajp_handler:end_request(PPid).
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc routes to correct handler method
%% @end 
%%--------------------------------------------------------------------  
dispatch_method_request(Module, #ajp_request_envelope{method = "GET"} = Msg, PPid) ->
  Module:handle_get_request(Msg, PPid);
dispatch_method_request(Module, #ajp_request_envelope{method = "POST"} = Msg, PPid) ->
  Module:handle_post_request(Msg, PPid);
dispatch_method_request(Module, #ajp_request_envelope{method = "PUT"} = Msg, PPid) ->
  Module:handle_put_request(Msg, PPid);
dispatch_method_request(Module, #ajp_request_envelope{method = "DELETE"} = Msg, PPid) ->
  Module:handle_delete_request(Msg, PPid);
dispatch_method_request(Module, Msg, PPid) when is_record (Msg, ajp_request_envelope)->
  Module:handle_request(Msg, PPid).

%%--------------------------------------------------------------------
%% @spec 
%% @doc encodes and sends an error response with requested code
%% and message to the caller.
%% @end 
%%--------------------------------------------------------------------  
send_error_response(ErrorCode, ErrorReason, PPid) ->
  send_headers(#ajp_response_envelope{status = ErrorCode, message = ErrorReason}, PPid),
  end_request(PPid).
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc reads data from the caller and sends back to handler.
%% @end 
%%--------------------------------------------------------------------  
request_data(Length, Pid) ->
  % request from the other end.
  % blocks 
  {requested_data, Data, ActualLength} = gen_fsm:sync_send_event(Pid, {get_data, Length}),
  {ok, Data, ActualLength}.
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the data response to the caller.
%% @end 
%%--------------------------------------------------------------------  
send_data(Data, Pid) ->
  data_sent = gen_fsm:sync_send_event(Pid, {send_data, Data}),
  ok.
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the response headers to the caller.
%% @end 
%%--------------------------------------------------------------------  
send_headers(ResponseHeaders, Pid) ->
  %io:format("send_header from [~p] to [~p] ~n", [self(), Pid]),
  headers_sent = gen_fsm:sync_send_event(Pid, {send_header, ResponseHeaders}),
  ok.
    
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the ajp request end headers.
%% @end 
%%--------------------------------------------------------------------  
end_request(Pid) ->
  ok = gen_fsm:send_event(Pid, end_response),
  ok.
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the ajp request end headers.
%% @end 
%%--------------------------------------------------------------------  
get_header(Request, Header) ->
  case lists:keysearch(Header, 1, Request#ajp_request_envelope.headers) of
    {value,{Header, Val}} -> Val;
    _ -> false
  end.

%
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the ajp request end headers.
%% @end 
%%--------------------------------------------------------------------  
set_header(Response, Header) when is_record(Response, ajp_response_envelope)->
  Headers = Response#ajp_response_envelope.headers,
  NH = merge_headers(Headers, Header),
  Response#ajp_response_envelope{headers = NH};

set_header(_Response, Header) ->
  NH = merge_headers([], Header),
  #ajp_response_envelope{headers = NH}.
    
%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% map a request uri to the handler module. The expectation is that the
%% script name matches a known module in the bin path. If not able to 
%% find returns unknown_module
%%--------------------------------------------------------------------  
lookup_uri_handler(URI) ->
  try list_to_existing_atom(lists:last(split_uri_path(URI))) of
    Module -> Module
  catch
    _:_ -> {unknown_module, URI}
  end.

%%--------------------------------------------------------------------
%% tokenizes a uri on the path seperator char
%%--------------------------------------------------------------------  
split_uri_path(URI) ->
  string:tokens(URI, "/").

%%--------------------------------------------------------------------
%% does necessary stuff to send an error response to the server.
%%--------------------------------------------------------------------  
local_send_error_response(ErrorCode, ErrorResponse, Socket) ->
  ResponseEnvelope = add_standard_headers(#ajp_response_envelope{status = ErrorCode, message = ErrorResponse}),
  gen_tcp:send(Socket, ajp:encode_header_response(ResponseEnvelope)).

local_send_error_response(ErrorCode, ErrorResponse, Binary, Socket) ->
  ResponseEnvelope = add_standard_headers(#ajp_response_envelope{status = ErrorCode, message = ErrorResponse}),
  gen_tcp:send(Socket, ajp:encode_header_response(ResponseEnvelope)),
  gen_tcp:send(Socket, ajp:encode_body_response(Binary, byte_size(Binary))).
  
local_send_end_response(Socket, Open) ->
  gen_tcp:send(Socket, << $A, $B,2:16,5:8,Open:8>>).
  
%%--------------------------------------------------------------------
%% @spec init_request(Socket, Message) -> ok
%% @doc generic skelton for handling a service request
%% @end 
%%--------------------------------------------------------------------  
init_request(Msg) ->
  HPid = 
  case lookup_uri_handler(Msg#ajp_request_envelope.request_uri) of
    {unknown_module, Module} -> 
      spawn_link(gen_ajp_handler, send_error_response, [500, lists:concat(["No handler for path ", Module]), self()]);
    Module ->
      spawn_link(gen_ajp_handler, dispatch_request, [Module, Msg, self()])
  end,
  {ok, HPid}.

%
add_standard_headers(#ajp_response_envelope{headers=ResponseHeaders} = Envelope) ->
  Envelope#ajp_response_envelope{headers=
    [{"x-erlang-pid",pid_to_list(self())}, 
    {"servlet-engine","AJPERL"} | ResponseHeaders]}.

%
merge_headers(Headers, Header) when is_list(Header) ->
  lists:umerge(Header, Headers);
merge_headers(Headers, Header) ->
  [Header | Headers].

% this reads in packets from the socket and adds them to the buffer
read_requested_data(Socket, Buffer) ->
  case ajp:read_buffered_ajp_packet(Buffer) of
    {ok, _, _, _} ->
      Buffer;
    incomplete_packet ->
      case gen_tcp:recv(Socket, 0, 0) of
        {ok, D1} ->
          read_requested_data(Socket, <<Buffer/binary, D1/binary>>);
        _ ->
          Buffer
        end
  end.
%
%
split_send_data(_, <<>>) ->
  ok;
split_send_data(Socket, Binary) when size(Binary) < 8000 ->
  gen_tcp:send(Socket, 
               ajp:encode_body_response(Binary, byte_size(Binary))),
  ok;
split_send_data(Socket, <<Binary:8000/binary, Rest/binary>>) ->
  gen_tcp:send(Socket, 
               ajp:encode_body_response(Binary, byte_size(Binary))),  
  split_send_data(Socket, Rest).

% this clears up the socket, for cleaning up after a dead or incomplete script
drain_socket(_, 0) ->
  ok;
drain_socket(Socket, _) ->
  case gen_tcp:recv(Socket, 0, 0) of
    {ok, D1} ->
      drain_socket(Socket, size(D1));
    _ ->
      ok
    end.