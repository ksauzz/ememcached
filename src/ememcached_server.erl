-module(ememcached_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("ememcached.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% TODO: remove debug option.
start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], [{debug,[trace]}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([LSock]) ->
  {ok, #state{lsock=LSock}, 0}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Socket, RawData}, State) ->
  [CmdLine | DataBlock] = string:tokens(RawData, "\r\n"),
  execute(Socket, string:tokens(CmdLine, " "), DataBlock),
  {noreply, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
  ememcached_sup:start_child(),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info(_, State) ->
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
