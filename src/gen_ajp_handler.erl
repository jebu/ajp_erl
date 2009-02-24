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
-author('jebui@yahoo-inc.com').
-export([behaviour_info/1]).

%% API
-export([init_request/2, request_data/2, send_data/2, send_headers/2, end_request/1]).

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
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------  
init_request(Socket, Msg) ->
  Module = lookup_uri_handler(Msg#ajp_request_envelope.request_uri),
  HPid = dispatch_request(Module, Msg),
  service_child_pid(Socket, HPid).

dispatch_request(Module, #ajp_request_envelope{method = "GET"} = Msg) ->
  spawn_link(Module, handle_get_request, [Msg, self()]);
dispatch_request(Module, #ajp_request_envelope{method = "POST"} = Msg) ->
  spawn_link(Module, handle_post_request, [Msg, self()]);
dispatch_request(Module, #ajp_request_envelope{method = "PUT"} = Msg) ->
  spawn_link(Module, handle_put_request, [Msg, self()]);
dispatch_request(Module, #ajp_request_envelope{method = "DELETE"} = Msg) ->
  spawn_link(Module, handle_delete_request, [Msg, self()]);
dispatch_request(Module, Msg) when is_record (Msg, ajp_request_envelope)->
  spawn_link(Module, handle_request, [Msg, self()]).
  
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
  
send_data(Data, Pid) ->
  Pid!{self(), send_data, Data},
  receive
    {Pid, data_sent} ->
      ok
  end.
  
send_headers(ResponseHeaders, Pid) ->
  Pid!{self(), send_header, ResponseHeaders},
  receive
    {Pid, headers_sent} ->
      ok
  end.
  
end_request(Pid) ->
    Pid!{self(), end_response}.
  
%%====================================================================
%% Internal functions
%%====================================================================

lookup_uri_handler(_) ->
  % do lookup based on env params
  test_ajp_mount.

%
service_child_pid(Socket, HPid) ->
  receive
    {HPid, get_data, Length} ->
      gen_tcp:send(Socket, ajp:encode_get_body_response(Length)),
      {ok, Data, L} = 
        case gen_tcp:recv(Socket, 4) of
          {ok, <<18,52, L1:16>>} -> 
            case gen_tcp:recv(Socket, L1) of
              {ok, << DataLength:16, Binary/binary >>} ->
                {ok, Binary, DataLength};
              _ ->
                {error, read_error}
            end;
          _Other -> 
            HPid ! {self(), error_getting_data},
            {error, read_error}
      end,
      HPid ! {self(), requested_data, Data, L},
      service_child_pid(Socket, HPid);
    {HPid, send_header, ResponseHeaders} when is_record(ResponseHeaders, ajp_response_envelope) ->
      gen_tcp:send(Socket, ajp:encode_header_response(ResponseHeaders)),
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
      gen_tcp:send(Socket, ajp:encode_header_response(#ajp_response_envelope{status = 503})),
      gen_tcp:send(Socket, << $A, $B,2:16,5:8,0:8>>),
      ok      
  after ?SCRIPT_TIMEOUT ->
    gen_tcp:send(Socket, ajp:encode_header_response(#ajp_response_envelope{status = 503})),
    gen_tcp:send(Socket, << $A, $B,2:16,5:8,0:8>>),
    ok
  end.
