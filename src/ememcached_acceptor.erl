-module(ememcached_acceptor).
-export([init/3, start_link/2]).
-include_lib("eunit/include/eunit.hrl").

start_link(LSock, PName) ->
  proc_lib:start_link(?MODULE, init, [LSock, PName, self()]).

init(LSock, PName, Parent) ->
  register(PName, self()),
  proc_lib:init_ack(Parent, {ok, self()}),
  {ok, Sock} = gen_tcp:accept(LSock),
  loop(Sock).

loop(Sock) ->
  receive
    Msg -> gen_server:call(ememcached_server, Msg)
  end,
  inet:setopts(Sock, [active, once]), 
  loop(Sock).
  
