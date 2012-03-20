-module(ememcached_acceptor).
-export([init/3, start_link/2]).

start_link(LSock, PName) ->
  proc_lib:start_link(?MODULE, init, [LSock, PName, self()]).

init(LSock, PName, Parent) ->
  register(PName, self()),
  proc_lib:init_ack(Parent, {ok, self()}),
  {ok, Sock} = gen_tcp:accept(LSock),
  loop(Sock).

loop(Sock) ->
  receive
    {tcp, Sock, RawData} ->
      ememcached_request:process(Sock, RawData)
  end,
  inet:setopts(Sock, [{active, once}]), 
  loop(Sock).
