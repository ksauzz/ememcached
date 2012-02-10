-module(ememcached).

-export([init/0,destroy/0]).
-export([set/2,add/2]).
-export([get/1,contains/1]).
-export([delete/1]).

%% https://raw.github.com/memcached/memcached/master/doc/protocol.txt

%% Storage commands (there are six: "set", "add", "replace", "append" "prepend" and "cas") 
%% Retrieval commands (there are two: "get" and "gets")
%% The command "delete" allows for explicit deletion of items
%% Commands "incr" and "decr"

-spec(init/0 :: () -> 'ok').
init() ->
  ets:new(ememcached,[public, bag, named_table]),
  ok.

-spec(destroy/0 :: () -> 'ok').
destroy() ->
  ets:delete(ememcached),
  ok.

-spec(set/2 :: (nonempty_string(), any()) -> 'ok').
%% "set" means "store this data".
set(Key,Value) ->
  delete(Key),
  ets:insert(ememcached, {Key,Value}),
  ok.

-spec(add/2 :: (nonempty_string(), any()) -> 'ok').
%% "add" means "store this data, but only if the server
%% *doesn't* already hold data for this key".
add(Key,Value) ->
  case ememcached:get(Key) of
     [] -> ememcached:set(Key,Value);
     _  -> ok
   end.

-spec(get/1 :: (nonempty_string()) -> any()).
get(Key) ->
  case ets:lookup(ememcached,Key) of
    []  -> [];
    [{Key,Value}|_] -> Value
  end.


-spec(contains/1 :: (nonempty_string()) -> boolean()).
contains(Key) ->
  case ememcached:get(Key) of
     [] -> false;
     _  -> true
   end.

-spec(delete/1 :: (nonempty_string()) -> ok|not_found).
delete(Key) ->
  case contains(Key) of
    true -> 
      ets:delete(ememcached, Key),
      ok;
    false -> not_found
  end.

