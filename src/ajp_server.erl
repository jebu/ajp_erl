%%%-------------------------------------------------------------------
%%% File:      ajp_server.erl
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
%%% @since 2009-02-13 by Jebu Ittiachen
%%%-------------------------------------------------------------------
-module(ajp_server).
-author('jebui@yahoo-inc.com').
-behaviour(gen_server).

-compile([verbose, report_errors, report_warnings, trace, debug_info]).
-define(TCP_OPTIONS, [binary, {active, false}, {reuseaddr, true}, {packet, raw}]).

-export([start_link/1, stop/0, handle_request/2, ajp_worker/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).

-include_lib("../include/ajp_records.hrl").

% ajp_server external interface

start_link(Port) ->
  error_logger:info_report([{'AJP13 SERVER STARTS'}]),
  case gen_server:start_link({local, ajp_server}, ajp_server, [], []) of
  {ok, Server} -> gen_server:call(ajp_server, {listen, Port}),
                    {ok, Server};
  {error, {already_started, Server}} -> {ok, Server}
  end.

stop() ->
  gen_server:call(ajp_server, stop).

% gen_server callbacks

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, {}}.

handle_call({listen, Port}, _From, State) ->
  case catch gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSocket} -> spawn_link(fun() -> ajp_worker(LSocket) end),
                      {reply, ok, LSocket};
    Error -> {stop, {listen_failed, Error}, State}
  end;

handle_call({new_worker, Worker}, _From, LSocket) -> 
  spawn_link(fun() -> ajp_server:ajp_worker(LSocket) end),
  erlang:unlink(Worker),
  {reply, ok, LSocket};

handle_call(stop, _From, LSocket) -> 
  gen_tcp:close(LSocket),
  {stop, stop_requested, []}.

handle_info({'EXIT', _Pid, _Reason}, LSocket) -> 
  gen_tcp:close(LSocket),
  {noreply, []}.

% ajp stuff

ajp_worker(LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, Socket} -> 
      gen_server:call(ajp_server, {new_worker, self()}),
      {ok, <<18,52, Length:16>>} = gen_tcp:recv(Socket, 4),
      {ok, Msg} = ajp:receive_message(Socket, Length),
      ajp_server:handle_request(Socket, Msg),
      gen_tcp:close(Socket);
    {error, closed} -> 
      gen_tcp:close(LSocket)
  end.

handle_request(Socket, Msg) ->
  case catch dispatch_request(Socket, Msg) of
    ok -> null;
    {'EXIT', {timeout, Error}} ->
      error_logger:info_report(["Timeout",
        trunc_io:fprint(Error, 500)]);
      %send error response
    Error ->
      error_logger:info_report(["Error",
        trunc_io:fprint(Error, 500)])
      %send error response
  end.

dispatch_request(Socket, Msg) when is_record(Msg, ajp_request_envelope) ->
  % here we should invoke the right module based on request uri
  % wait for messages back from the modules for getting more data
  % also messages from module to send responses back to the server.
  % timeout errors, process dying should be handled here.
  gen_ajp_handler:init_request(Socket, Msg).
      
% callback stubs
terminate(_Reason, []) -> 
  ok;
terminate(_Reason, LSocket) -> 
  gen_tcp:close(LSocket),
  ok.

handle_cast(_Cast, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
