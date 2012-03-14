-module(ememcached_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _} = ememcached_store:start_link(),
  {ok, _} = ememcached_server:start_link(),
  {ok, _} = ememcached_acceptor_sup:start_link(),
  {ok, self()}.

stop(_State) ->
  ememcached_store:stop(),
  ememcached_server:stop(),
  ok.
