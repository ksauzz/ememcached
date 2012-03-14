-module(ememcached_store).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

-export([stop/0]).
-export([set/2,add/2]).
-export([get/1,contains/1]).
-export([delete/1]).

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
  init_table(),
  {ok, []}.

handle_call({get, [Key]}, _From, State) ->
  {reply, do_get(Key), State};
handle_call({set, [Key, Value]}, _From, State) ->
  {reply, do_set(Key, Value), State};
handle_call({add, [Key, Value]}, _From, State) ->
  {reply, do_add(Key, Value), State};
handle_call({contains, [Key]}, _From, State) ->
  {reply, do_contains(Key), State};
handle_call({delete, [Key]}, _From, State) ->
  {reply, do_delete(Key), State};
handle_call({stop}, _From, State) ->
  {stop, normal, stopped, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  do_stop().

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get(Key) ->
  gen_server:call(?SERVER, {get, [Key]}).

set(Key, Value) ->
  gen_server:call(?SERVER, {set, [Key, Value]}).

add(Key, Value) ->
  gen_server:call(?SERVER, {add, [Key, Value]}).

delete(Key) ->
  gen_server:call(?SERVER, {delete, [Key]}).

contains(Key) ->
  gen_server:call(?SERVER, {contains, [Key]}).

stop() ->
  gen_server:call(?SERVER, {stop}),
  ok.


%% https://raw.github.com/memcached/memcached/master/doc/protocol.txt

%% Storage commands (there are six: "set", "add", "replace", "append" "prepend" and "cas") 
%% Retrieval commands (there are two: "get" and "gets")
%% The command "delete" allows for explicit deletion of items
%% Commands "incr" and "decr"

-spec(init_table/0 :: () -> 'ok').
init_table() ->
  ets:new(ememcached,[public, bag, named_table]),
  ok.

-spec(do_stop/0 :: () -> 'ok').
do_stop() ->
  ets:delete(ememcached),
  ok.

-spec(do_set/2 :: (nonempty_string(), any()) -> 'ok').
%% "set" means "store this data".
do_set(Key,Value) ->
  do_delete(Key),
  ets:insert(ememcached, {Key,Value}),
  ok.

-spec(do_add/2 :: (nonempty_string(), any()) -> 'ok').
%% "add" means "store this data, but only if the server
%% *doesn't* already hold data for this key".
do_add(Key,Value) ->
  case do_get(Key) of
     [] -> do_set(Key,Value);
     _  -> ok
   end.

-spec(do_get/1 :: (nonempty_string()) -> any()).
do_get(Key) ->
  case ets:lookup(ememcached,Key) of
    []  -> [];
    [{Key,Value}|_] -> Value
  end.


-spec(do_contains/1 :: (nonempty_string()) -> boolean()).
do_contains(Key) ->
  case do_get(Key) of
     [] -> false;
     _  -> true
   end.

-spec(do_delete/1 :: (nonempty_string()) -> ok|not_found).
do_delete(Key) ->
  case do_contains(Key) of
    true -> 
      ets:delete(ememcached, Key),
      ok;
    false -> not_found
  end.

