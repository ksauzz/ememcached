-module(ememcached_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% TODO: replace to fixture's setup
setup() ->
  application:start(ememcached).

cleanup() ->
  application:stop(ememcached).

start_stop_test() ->
  ?assertEqual(ok, application:start(ememcached)),
  ?assertEqual(ok, application:stop(ememcached)),
  ok.

std_test() ->
  setup(),
  {ok, Sock} = gen_tcp:connect("localhost", 11211, [binary, {packet, 0}]),

  ?assertEqual("ERROR\r\n", send(Sock, ["invaid command"])),
  ?assertEqual("END\r\n", send(Sock, ["get empty"])),
  ?assertEqual("STORED\r\n", send(Sock, ["set key1 0 6", "value1"])),
  ?assertEqual("VALUE key1 0 6\r\nvalue1\r\nEND\r\n", send(Sock, ["get key1"])),
  ?assertEqual("DELETED\r\n", send(Sock, ["delete key1"])),
  ?assertEqual("NOT_FOUND\r\n", send(Sock, ["delete key1"])),
  ?assertEqual(quit, send(Sock, ["quit"])),
  cleanup().

send(Sock, Msgs) ->
  lists:foreach(fun(Msg) -> 
        ok = gen_tcp:send(Sock, Msg ++ "\r\n")
    end,
    Msgs),
  receive_data(Sock).

receive_data(Sock) ->
  receive
    {tcp, Sock, Data} ->
      binary_to_list(Data);
    {tcp_closed, Sock} ->
      quit
  end.
