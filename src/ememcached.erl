-module(ememcached).

-export([init/0]).
-export([set/2,add/2]).
-export([get/1]).

%% https://github.com/memcached/memcached/blob/master/doc/protocol.txt
%% Storage commands (there are six: "set", "add", "replace", "append" "prepend" and "cas") 
%% Retrieval commands (there are two: "get" and "gets")
%% The command "delete" allows for explicit deletion of items
%% Commands "incr" and "decr"

init() ->
  ets:new(ememcached,[bag, named_table]).

%% "set" means "store this data".
set(Key,Value) ->
  ets:delete(ememcached, Key),
  ets:insert(ememcached, {Key,Value}).

%% "add" means "store this data, but only if the server
%% *doesn't* already hold data for this key".
add(Key,Value) ->
  case ememcached:get(Key) of
     [] -> ememcached:set(Key,Value);
     _  -> ok
   end.

get(Key) ->
  lists:concat(lists:map(fun({_,V}) -> V end, ets:lookup(ememcached,Key))).

-ifdef(TEST).
-endif.
