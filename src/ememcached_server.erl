-module(ememcached_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(PORT, 11211).
-define(POOL_SIZE, 16).
-include("ememcached.hrl").

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
  [CmdLine | DataBlock] = string:tokens(RawData, "\r\n"),
  execute(Socket, string:tokens(CmdLine, " "), DataBlock),
  {noreply, State};
handle_call({stop}, _From, State) ->
  {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

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

execute(Socket, ["get", Key], _) ->
  #ememcached_record{key=_Key,flags=Flags,bytes=Bytes,data_block=DataBlock} = ememcached_store:get(Key),
  %% VALUE <key> <flags> <bytes> [<cas unique>]\r\n
  %% <data block>\r\n
  response(Socket, "VALUE " ++ Key ++ " " ++ Flags ++ " " ++ Bytes ++ "\r\n" ++ DataBlock ++ "\r\n");
execute(Socket, ["set", Key, Flags, Bytes], DataBlock) ->
  Record = #ememcached_record{key=Key,flags=Flags,bytes=Bytes,data_block=DataBlock},
  ememcached_store:set(Key, Record),
  response(Socket, "STORED\r\n");
execute(Socket, ["delete", Key], _) ->
  case ememcached_store:delete(Key) of
    ok -> response(Socket, "DELETED\r\n");
    not_found -> response(Socket, "NOT_FOUND\r\n")
  end;
execute(Socket, ["quit"], _) ->
  gen_tcp:close(Socket);
execute(Socket, _, _) ->
  response(Socket, "ERROR\r\n").

response(Socket, Response) ->
  gen_tcp:send(Socket, Response).

stop() ->
  gen_server:call(?SERVER, {stop}).
