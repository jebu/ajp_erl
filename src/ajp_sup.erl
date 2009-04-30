%%%-------------------------------------------------------------------
%%% File:      ajp_sup.erl
%%% @author    Jebu Ittiachen <jebui@yahoo-inc.com> [http://blog.jebu.net/]
%%% @copyright 2009 Jebu Ittiachen
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-26 by Jebu Ittiachen
%%%-------------------------------------------------------------------
-module(ajp_sup).
-author('jebu@jebu.net').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% @end 
%%--------------------------------------------------------------------
start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end 
%%--------------------------------------------------------------------
init([Port]) ->
    AJPServer = {ajp_server, {ajp_server, start_link, [Port]},
              permanent, 2000, worker, dynamic},
    {ok, {{one_for_all, 10, 10}, [AJPServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
