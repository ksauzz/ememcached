-module(ememcached_acceptor_sup).

-behaviour(supervisor).

-define(PORT, 9999).
-define(POOL_SIZE, 16).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Module, No, Type, Args),
  PName = list_to_atom(atom_to_list(Module) ++ "_" ++ integer_to_list(No)),
  {PName, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

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
                                         {mode, binary},
                                         {packet, line},
                                         {active, once}]),
    WorkerSpec = lists:map(fun(X) ->
          ModuleName = ememcached_acceptor,
          PName = list_to_atom(atom_to_list(ModuleName) ++ "_" ++ integer_to_list(X)),
          {PName, {ModuleName, start_link, [LSock, PName]}, permanent, 5000, worker, [ModuleName]}
      end,
      lists:seq(1, ?POOL_SIZE)),
    {ok, {{one_for_one, 5, 10}, WorkerSpec}}.
