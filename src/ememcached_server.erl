-module(ememcached_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(PORT, 11211).

% TODO: only sock?
-record(state, {port, lsock}).

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

init(_) ->
  ?debugVal("init runnging..."),
  ememcached:init(),
  {ok, LSock} = gen_tcp:listen(?PORT, [{active, true}]),
  ?debugVal("listening port"),
  {ok, #state{port=?PORT,lsock=LSock}, 0}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Socket, RawData}, State) ->
  execute(Socket, string:tokens(RawData, " \r\n")),
  {noreply, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
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
execute(Socket, _) ->
  response(Socket, "ERROR\r\n").

response(Socket, Response) ->
  gen_tcp:send(Socket, Response).
