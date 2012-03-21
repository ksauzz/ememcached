-module(ememcached_request).
-export([process/2]).

-include("ememcached.hrl").

process(Sock, RawData) ->
  ParsedData = binary:split(RawData, [<<" ">>,<<"\r\n">>],[global]),
  [Cmd|Data] = lists:filter(fun(X) -> X=/=<<>> end, ParsedData),
  process(Sock, Cmd, Data).

process(Sock, <<"get">>, [<<Key/binary>>]) ->
  case ememcached_store:get(Key) of
    #ememcached_record{key=_Key,flags=Flags,bytes=Bytes,data_block=DataBlock} ->
      %% VALUE <key> <flags> <bytes> [<cas unique>]\r\n
      %% <data block>\r\n
      response(Sock,
        list_to_binary([<<"VALUE ">>,  Key, <<" ">>,  Flags, <<" ">>, Bytes, <<"\r\n">>,
            DataBlock, <<"\r\nEND\r\n">>]));
    [] ->
      response(Sock, <<"END\r\n">>)
  end;
process(Sock, <<"set">>, [<<Key/binary>>, <<Flags/binary>>, <<Bytes/binary>>]) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, RawData} ->
      [DataBlock, <<>>] = binary:split(RawData, [<<"\r\n">>]),
      Record = #ememcached_record{key=Key,flags=Flags,bytes=Bytes,data_block=DataBlock},
      ememcached_store:set(Key, Record),
      response(Sock, <<"STORED\r\n">>);
    Other ->
      error_logger:error_report(io_lib:format("receive unexpected data ~p", [Other])),
      response(Sock, <<"ERROR\r\n">>)
  end;
process(Sock, <<"delete">>, [<<Key/binary>>]) ->
  case ememcached_store:delete(Key) of
    ok -> response(Sock, <<"DELETED\r\n">>);
    not_found -> response(Sock, <<"NOT_FOUND\r\n">>)
  end;
process(Sock, <<"quit">>, []) ->
  gen_tcp:close(Sock);
process(Sock, _Cmd, _Data) ->
  response(Sock, <<"ERROR\r\n">>).

response(Sock, Response) ->
  gen_tcp:send(Sock, Response).
