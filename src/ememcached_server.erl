-module(ememcached_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("ememcached.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% TODO: remove debug option.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{debug,[trace]}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, []}.

handle_call({tcp, Socket, RawData}, _From,  State) ->
  ParsedData = binary:split(RawData, [<<" ">>,<<"\r\n">>],[global]),
  execute(Socket, lists:filter(fun(X) -> X=/=<<>> end, ParsedData)),
  {reply, ok, State};
handle_call({stop}, _From, State) ->
  {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
  {reply, error, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

execute(Socket, [<<"get">>, <<Key/binary>>]) ->
  case ememcached_store:get(Key) of
    #ememcached_record{key=_Key,flags=Flags,bytes=Bytes,data_block=DataBlock} ->
      %% VALUE <key> <flags> <bytes> [<cas unique>]\r\n
      %% <data block>\r\n
      response(Socket,
        list_to_binary([<<"VALUE ">>,  Key, <<" ">>,  Flags, <<" ">>, Bytes, <<"\r\n">>,
            DataBlock, <<"\r\nEND\r\n">>]));
    [] ->
      response(Socket, <<"\r\nEND\r\n">>)
  end;
execute(Socket, [<<"set">>, <<Key/binary>>, <<Flags/binary>>, <<Bytes/binary>>, <<DataBlock/binary>>]) ->
  Record = #ememcached_record{key=Key,flags=Flags,bytes=Bytes,data_block=DataBlock},
  ememcached_store:set(Key, Record),
  response(Socket, <<"STORED\r\n">>);
execute(Socket, [<<"delete">>, <<Key/binary>>]) ->
  case ememcached_store:delete(Key) of
    ok -> response(Socket, <<"DELETED\r\n">>);
    not_found -> response(Socket, <<"NOT_FOUND\r\n">>)
  end;
execute(Socket, [<<"quit">>]) ->
  gen_tcp:close(Socket);
execute(Socket, _) ->
  response(Socket, <<"ERROR\r\n">>).

response(Socket, Response) ->
  gen_tcp:send(Socket, Response).

stop() ->
  gen_server:call(?SERVER, {stop}).
