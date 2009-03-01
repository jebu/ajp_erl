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
-module(gen_ajp_handler).
-author('jebu@jebu.net').
-export([behaviour_info/1]).

%% API
-export([init_request/2, request_data/2, send_data/2, send_headers/2, end_request/1, send_error_response/3, dispatch_request/3]).

-include_lib("../include/ajp_records.hrl").

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

%%--------------------------------------------------------------------
%% @spec init_request(Socket, Message) -> ok
%% @doc generic skelton for handling a service request
%% @end 
%%--------------------------------------------------------------------  
init_request(Socket, Msg) ->
  HPid = 
  case lookup_uri_handler(Msg#ajp_request_envelope.request_uri) of
    {unknown_module, Module} -> 
      spawn_link(gen_ajp_handler, send_error_response, [500, lists:concat(["No handler for path ", Module]), self()]);
    Module ->
      spawn_link(gen_ajp_handler, dispatch_request, [Module, Msg, self()])
  end,
  service_child_pid(Socket, HPid).

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
  Pid!{self(), get_data, Length},
  {Binary, AL} = 
    receive
      {Pid, requested_data, Data, ActualLength} ->
        {Data, ActualLength}
    end,  
  {ok, Binary, AL}.
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the data response to the caller.
%% @end 
%%--------------------------------------------------------------------  
send_data(Data, Pid) ->
  Pid!{self(), send_data, Data},
  receive
    {Pid, data_sent} ->
      ok
  end.
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the response headers to the caller.
%% @end 
%%--------------------------------------------------------------------  
send_headers(ResponseHeaders, Pid) ->
  Pid!{self(), send_header, ResponseHeaders},
  receive
    {Pid, headers_sent} ->
      ok
  end.
  
%%--------------------------------------------------------------------
%% @spec 
%% @doc sends the ajp request end headers.
%% @end 
%%--------------------------------------------------------------------  
end_request(Pid) ->
    Pid!{self(), end_response}.
  
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
  gen_tcp:send(Socket, ajp:encode_header_response(ResponseEnvelope)),
  gen_tcp:send(Socket, << $A, $B,2:16,5:8,0:8>>).
  
%%--------------------------------------------------------------------
%% after dispatching the request to the proper module the process waits
%% here for servicing the handler requests.
%%--------------------------------------------------------------------  
service_child_pid(Socket, HPid) ->
  receive
    {HPid, get_data, Length} ->
      gen_tcp:send(Socket, ajp:encode_get_body_response(Length)),
      {ok, L} = ajp:read_ajp_packet(Socket),
      {ok, Data, L1} = 
        case gen_tcp:recv(Socket, L) of
          {ok, << DataLength:16, Binary/binary >>} ->
            {ok, Binary, DataLength};
          _ ->
            {error, read_error}
        end,
      HPid ! {self(), requested_data, Data, L1},
      service_child_pid(Socket, HPid);
    {HPid, send_header, ResponseHeaders} when is_record(ResponseHeaders, ajp_response_envelope) ->
      gen_tcp:send(Socket, ajp:encode_header_response(add_standard_headers(ResponseHeaders))),
      HPid ! {self(), headers_sent},
      service_child_pid(Socket, HPid);
    {HPid, send_data, Binary} ->
      gen_tcp:send(Socket, ajp:encode_body_response(Binary, byte_size(Binary))),
      HPid ! {self(), data_sent},
      service_child_pid(Socket, HPid);
    {HPid, end_response} ->
      gen_tcp:send(Socket, << $A, $B,2:16,5:8,0:8>>),
      ok;
    {'EXIT', HPid, _Reason} ->
      local_send_error_response(500, "Server Error - Handler died unexpectedly", Socket),
      ok      
  after ?SCRIPT_TIMEOUT ->
    local_send_error_response(503, "Timeout - Handler timedout", Socket),
    ok
  end.

%
add_standard_headers(#ajp_response_envelope{headers=ResponseHeaders} = Envelope) ->
  Envelope#ajp_response_envelope{headers=
    [{"x-erlang-pid",pid_to_list(self())}, 
    {"servlet-engine","AJPERL"} | ResponseHeaders]}.

