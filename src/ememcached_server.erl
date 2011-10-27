-module(ememcached_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

% TODO: only sock?
-record(state, {port::integer(), lsock::gen_tcp:socket()}).

-include_lib("eunit/include/eunit.hrl").

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
  ?debugMsg("start_link runnging..."),
  gen_server:start_link(?MODULE, [LSock], [{debug,[trace]}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([LSock]) ->
  ?debugMsg("init runnging..."),
  {ok, #state{lsock=LSock}, 0}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Socket, RawData}, State) ->
  ?debugMsg("handle_info runnging..."),
  execute(Socket, string:tokens(RawData, " \r\n")),
  {noreply, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
  ?debugMsg("handle_info:timeout runnging..."),
  {ok, _Sock} = gen_tcp:accept(LSock),
  ememcached_sup:start_child(),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
  ?debugMsg("session is closed..."),
  {noreply, State};
handle_info(_, State) ->
  ?debugMsg("handle_info recv Ilegal msg.."),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

execute(Socket, ["get", Data]) ->
  Value = ememcached:get(Data),
  %% VALUE <key> <flags> <bytes> [<cas unique>]\r\n
  %% <data block>\r\n
  response(Socket, "VALUE " ++ Data ++ " " ++ Value ++ " \r\n" ++ "" ++ "\r\n");
execute(Socket, ["set", Key, Value]) ->
  ememcached:set(Key, Value),
  response(Socket, "STORED\r\n");
execute(Socket, ["quit"]) ->
  gen_tcp:close(Socket);
execute(Socket, _) ->
  response(Socket, "ERROR\r\n").

response(Socket, Response) ->
  gen_tcp:send(Socket, Response).
