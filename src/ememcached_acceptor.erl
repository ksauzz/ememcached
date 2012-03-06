-module(ememcached_acceptor).
-export([init/2, start_link/2]).

start_link(Server, LSock) ->
  proc_lib:spawn_link(?MODULE, init, [Server, LSock]).

init(Server, LSock) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
  receive
    Msg -> gen_server:call(Server, Msg)
  end.
