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
-author('jebu@jebu.net').
-behaviour(gen_server).

-compile([verbose, report_errors, report_warnings, trace, debug_info]).
-define(TCP_OPTIONS, [binary, {active, false}, {reuseaddr, true}, {packet, raw}, {sndbuf, 16384}, {recbuf, 16384}, {ip, {127,0,0,1}}]).

-export([start_link/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).

-include("ajp_records.hrl").

% ajp_server external interface

start_link(Port) ->
  error_logger:info_report([{"AJP13 SERVER STARTS"}]),
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
    {ok, LSocket} -> 
      %io:format("Spawning listener for [~p] ~n", [Port]),
      spawn_link(fun() -> ajp_worker(LSocket) end),
                      {reply, ok, LSocket};
    Error -> {stop, {listen_failed, Error}, State}
  end;

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
    	{Ms,S,Us} = erlang:now(),
    	HandlerName = lists:flatten(["ajp_worker_",
                                    integer_to_list(Ms),
                                    integer_to_list(S),
                                    integer_to_list(Us)]),
      HandlerSpec = {HandlerName, {gen_ajp_handler, start_link, [Socket]}, temporary, 2000, worker, dynamic},
      {ok, CPid} = supervisor:start_child(ajp_sup, HandlerSpec),
      gen_tcp:controlling_process(Socket, CPid),
      ajp_worker(LSocket);
    {error, closed} -> 
      gen_tcp:close(LSocket)
  end.
        
% callback stubs
terminate(_Reason, []) -> 
  ok;
terminate(_Reason, LSocket) -> 
  gen_tcp:close(LSocket),
  ok.

handle_cast(_Cast, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
