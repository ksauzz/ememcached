-module(ememcached).

-export([init/0]).
-export([set/2,add/2]).
-export([get/1,contains/1]).
-export([delete/1]).

%% https://raw.github.com/memcached/memcached/master/doc/protocol.txt

%% Storage commands (there are six: "set", "add", "replace", "append" "prepend" and "cas") 
%% Retrieval commands (there are two: "get" and "gets")
%% The command "delete" allows for explicit deletion of items
%% Commands "incr" and "decr"

init() ->
  ets:new(ememcached,[public, bag, named_table]),
  ok.

%% "set" means "store this data".
set(Key,Value) ->
  delete(Key),
  ets:insert(ememcached, {Key,Value}),
  ok.

%% "add" means "store this data, but only if the server
%% *doesn't* already hold data for this key".
add(Key,Value) ->
  case ememcached:get(Key) of
     [] -> ememcached:set(Key,Value);
     _  -> ok
   end.

get(Key) ->
  lists:concat(lists:map(fun({_,V}) -> V end, ets:lookup(ememcached,Key))).

contains(Key) ->
  case ememcached:get(Key) of
     [] -> false;
     _  -> true
   end.

delete(Key) ->
  case contains(Key) of
    true -> 
      ets:delete(ememcached, Key),
      ok;
    false -> not_found
  end.

