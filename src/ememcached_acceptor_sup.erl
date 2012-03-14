-module(ememcached_acceptor_sup).

-behaviour(supervisor).

-define(PORT, 11211).
-define(POOL_SIZE, 16).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Module, No, Type, Args),
  {list_to_atom(
      atom_to_list(Module) ++ "_" ++ integer_to_list(No)),
    {Module, start_link, Args}, permanent, 5000, Type, [Module]
  }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    {ok, LSock} = gen_tcp:listen(?PORT, [{reuseaddr, true},
                                         {active, true}]),
    WorkerSpec = lists:map(fun(X) ->
          ?CHILD(ememcached_acceptor, X, worker, [LSock])
      end,
      lists:seq(1, ?POOL_SIZE)),
    {ok, {{one_for_one, 5, 10}, WorkerSpec}}.
