%%%-------------------------------------------------------------------
%%% File:      ajp_server.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2008 Jebu Ittiachen
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

-define(ERROR_REDIRECT, "Location: /\r\n\r\n").
-define(ERROR_500_TEXT, "500 - Internal Server Error").
-define(ERROR_503_TEXT, "503 - Service Unavailable").

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

handle_call(stop, _From, LSocket) -> {stop, stop_requested, LSocket}.

handle_info({'EXIT', _Pid, _Reason}, LSocket) -> 
  {noreply, LSocket}.

% ajp stuff

ajp_worker(LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, Socket} -> 
      gen_server:call(ajp_server, {new_worker, self()}),
      {ok, Msg} = ajp:receive_ajp13_message(Socket),
      ajp_server:handle_request(Socket, Msg),
      gen_tcp:close(Socket)
  end.

handle_request(Socket, Msg) ->
  case catch dispatch_request(Socket, Msg) of
    ok -> null;
    {'EXIT', {timeout, Error}} ->
      error_logger:info_report(["Timeout",
        trunc_io:fprint(Error, 500)]),
      gen_tcp:send(Socket, ?ERROR_500_TEXT);
    Error ->
      error_logger:info_report(["Error",
        trunc_io:fprint(Error, 500)]),
      gen_tcp:send(Socket, ?ERROR_500_TEXT)
  end,
  gen_tcp:send(Socket, ajp:encode_ajp_header_response(#ajp_response_envelope{headers=[{"content-length", "12"}]})),
  gen_tcp:send(Socket, ajp:encode_ajp_body_response(<<"testing data">>, 12)),
  gen_tcp:send(Socket, << 16#4142:16,2:16,5:8,0:8>>).

dispatch_request(Socket, Msg) ->
  % here we should invoke the right module based on request uri
  % wait for messages back from the modules for getting more data
  % also messages from module to send responses back to the server.
  % timeout errors, process dying should be handled here.
  ok.

% callback stubs

terminate(_Reason, _State) -> {}.

handle_cast(_Cast, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
