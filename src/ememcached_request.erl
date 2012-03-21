-module(ememcached_request).
-export([process/2]).

-include("ememcached.hrl").

process(Sock, RawData) ->
  ParsedData = binary:split(RawData, [<<" ">>,<<"\r\n">>],[global]),
  [Cmd|Data] = lists:filter(fun(X) -> X=/=<<>> end, ParsedData),
  process(Sock, Cmd, Data).

process(Socket, <<"get">>, [<<Key/binary>>]) ->
  case ememcached_store:get(Key) of
    #ememcached_record{key=_Key,flags=Flags,bytes=Bytes,data_block=DataBlock} ->
      %% VALUE <key> <flags> <bytes> [<cas unique>]\r\n
      %% <data block>\r\n
      response(Socket,
        list_to_binary([<<"VALUE ">>,  Key, <<" ">>,  Flags, <<" ">>, Bytes, <<"\r\n">>,
            DataBlock, <<"\r\nEND\r\n">>]));
    [] ->
      response(Socket, <<"END\r\n">>)
  end;
process(Socket, <<"set">>, [<<Key/binary>>, <<Flags/binary>>, <<Bytes/binary>>]) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, RawData} ->
      [DataBlock, <<>>] = binary:split(RawData, [<<"\r\n">>]),
      Record = #ememcached_record{key=Key,flags=Flags,bytes=Bytes,data_block=DataBlock},
      ememcached_store:set(Key, Record),
      response(Socket, <<"STORED\r\n">>);
    Other ->
      error_logger:error_report(io_lib:format("receive unexpected data ~p", [Other])),
      response(Socket, <<"ERROR\r\n">>)
  end;
process(Socket, <<"delete">>, [<<Key/binary>>]) ->
  case ememcached_store:delete(Key) of
    ok -> response(Socket, <<"DELETED\r\n">>);
    not_found -> response(Socket, <<"NOT_FOUND\r\n">>)
  end;
process(Socket, <<"quit">>, []) ->
  gen_tcp:close(Socket);
process(Socket, _Cmd, _Data) ->
  response(Socket, <<"ERROR\r\n">>).

response(Socket, Response) ->
  gen_tcp:send(Socket, Response).
