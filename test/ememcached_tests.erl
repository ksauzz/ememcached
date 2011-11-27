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

  ?assertEqual(ok, ememcached:destroy()),
  ok.

delete_test() ->
  ?assertEqual(ok, ememcached:init()),

  ?assertEqual(not_found, ememcached:delete("not_found")),
  ?assertEqual(ok,        ememcached:add("found", 1)),
  ?assertEqual(ok,        ememcached:delete("found")),

  ?assertEqual(ok, ememcached:destroy()),
  ok.

contains_test() ->
  ?assertEqual(ok, ememcached:init()),

  ?assertNot(ememcached:contains("not_found")),
  ?assertEqual(ok, ememcached:add("found", 1)),
  ?assert(ememcached:contains("found")),

  ?assertEqual(ok, ememcached:destroy()),
  ok.
