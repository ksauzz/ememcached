
-module(ememcached_sup).

-behaviour(supervisor).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%%  
%%   child_spec() = {Id,StartFunc,Restart,Shutdown,Type,Modules}
%%   Id = term()
%%   StartFunc = {M,F,A}
%%   M = F = atom()
%%   A = [term()]
%%   Restart = permanent | transient | temporary
%%   Shutdown = brutal_kill | int()>=0 | infinity
%%   Type = worker | supervisor
%%   Modules = [Module] | dynamic
%%   Module = atom()

%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, LSock), {I, {I, start_link, [LSock]}, temporary, brutal_kill, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    ?debugVal("start_link is running..."),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]).

start_child() ->
    ?debugVal("start_child is running..."),
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%  Module:init(Args) -> Result
%% 
%%         Types  Args = term()
%%                Result = {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%%                RestartStrategy = simple_one_for_one | one_for_one | one_for_all | rest_for_one
%%                MaxR = MaxT = integer()>=0
%%                ChildSpec = child_spec()

init([LSock]) ->
    ?debugVal("init is running..."),
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(ememcached_server, worker, LSock)]} }.

