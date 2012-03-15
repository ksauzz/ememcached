-module(ememcached_acceptor).
-export([init/2, start_link/1]).
-include_lib("eunit/include/eunit.hrl").

start_link(LSock) ->
  proc_lib:start_link(?MODULE, init, [LSock, self()]).

init(LSock, Parent) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  {ok, Sock} = gen_tcp:accept(LSock),
  loop(Sock).

loop(Sock) ->
  receive
    Msg -> gen_server:call(ememcached_server, Msg)
  end,
  loop(Sock).
  
