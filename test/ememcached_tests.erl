-module(ememcached_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assertEqual(ok, ememcached:init()),

  ?assertEqual(ok,    ememcached:set("github","hg")),
  ?assertEqual(ok,    ememcached:set("github","git")),
  ?assertEqual("git", ememcached:get("github")),

  ?assertEqual(ok,   ememcached:add("bitbucket","hg")),
  ?assertEqual(ok,   ememcached:add("bitbucket","git")),
  ?assertEqual("hg", ememcached:get("bitbucket")),
  
  ok.
