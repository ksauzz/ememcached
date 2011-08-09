-module(ememcached).

-export([my_func/0]).

my_func() ->
  ok.

-ifdef(TEST).
-endif.
