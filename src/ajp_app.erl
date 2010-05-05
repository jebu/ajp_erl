%%%-------------------------------------------------------------------
%%% File:      ajp_app.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2009 Jebu Ittiachen
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-26 by Jebu Ittiachen
%%%-------------------------------------------------------------------
-module(ajp_app).
-author('jebu@jebu.net').

-behaviour(application).
-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).
-define(DEFAULT_AJP_PORT, 8009).

%%====================================================================
%% Application callbacks
%%====================================================================
start() ->
  application:start(ajp_app).
  
%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%% @doc This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end 
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
  ListenPort = get_configuration_parameter(ajp_port, ?DEFAULT_AJP_PORT),
  case ajp_sup:start_link(ListenPort) of
    {ok, Pid} -> 
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% @end 
%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
get_configuration_parameter(P, Default) ->
  case application:get_env(P) of
    {ok, Val} -> Val;
    _ -> Default
  end.
