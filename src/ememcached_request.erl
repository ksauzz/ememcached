-module(ememcached_request).
-export([process/2]).

-include("ememcached.hrl").

process(Sock, RawData) ->
  ParsedData = binary:split(RawData, [<<" ">>,<<"\r\n">>],[global]),
  [Cmd|Data] = lists:filter(fun(X) -> X=/=<<>> end, ParsedData),
  process(Sock, Cmd, Data).

process(Sock, <<"get">>, [<<Key/binary>>]) ->
  case ememcached_store:get(Key) of
    #ememcached_record{key=_Key,flags=Flags,bytes=Bytes,data_block=DataBlock} = Record ->
      %% VALUE <key> <flags> <bytes> [<cas unique>]\r\n
      %% <data block>\r\n
      case is_expired(Record) of
        true ->
          ememcached_store:delete(Key),
          response(Sock, <<"END\r\n">>);
        false -> 
          response(Sock, list_to_binary([<<"VALUE ">>,  Key, <<" ">>,  integer_to_list(Flags), <<" ">>, integer_to_list(Bytes), <<"\r\n">>,
                DataBlock, <<"\r\nEND\r\n">>]))
      end;
    [] ->
      response(Sock, <<"END\r\n">>)
  end;
process(Sock, <<"set">>, [<<Key/binary>>, <<Flags/binary>>, <<ExpTime/binary>>, <<Bytes/binary>>]) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, <<RawData/bitstring>>} ->
      [DataBlock, <<>>] = binary:split(RawData, [<<"\r\n">>]),
      Record = #ememcached_record{
        key=Key,
        flags=bin_to_integer(Flags),
        exptime=bin_to_integer(ExpTime),
        bytes=bin_to_integer(Bytes),
        created_datetime=current_datetime(),
        data_block=DataBlock},
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

current_datetime() ->
  Now = calendar:local_time(),
  calendar:datetime_to_gregorian_seconds(Now).

bin_to_integer(Bin) ->
  erlang:list_to_integer(binary:bin_to_list(Bin)).

is_expired(#ememcached_record{exptime=ExpTime, created_datetime=CreatedDatetime}) ->
  Now = current_datetime(),
  if
      ExpTime == 0 -> false;
      (CreatedDatetime + ExpTime) > Now -> false;
      true -> true
  end.
