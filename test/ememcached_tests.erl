-module(ememcached_tests).
-include("include/ememcached.hrl").
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assertEqual(ok, ememcached:init()),

  Hg1=#ememcached_record{key="github",data_block="hg"},
  Git1=#ememcached_record{key="github",data_block="git"},
  ?assertEqual(ok,   ememcached:set("github",Hg1)),
  ?assertEqual(ok,   ememcached:set("github",Git1)),
  ?assertEqual(Git1, ememcached:get("github")),

  Hg2=#ememcached_record{key="bitbucket",data_block="hg"},
  Git2=#ememcached_record{key="bitbucket",data_block="git"},
  ?assertEqual(ok,  ememcached:add("bitbucket",Hg2)),
  ?assertEqual(ok,  ememcached:add("bitbucket",Git2)),
  ?assertEqual(Hg2, ememcached:get("bitbucket")),

  ?assertEqual(ok, ememcached:destroy()),
  ok.

delete_test() ->
  ?assertEqual(ok, ememcached:init()),

  ?assertEqual(not_found, ememcached:delete("not_found")),
  ?assertEqual(ok,        ememcached:add("found", #ememcached_record{key="found", data_block="1"})),
  ?assertEqual(ok,        ememcached:delete("found")),

  ?assertEqual(ok, ememcached:destroy()),
  ok.

contains_test() ->
  ?assertEqual(ok, ememcached:init()),

  ?assertNot(ememcached:contains("not_found")),
  ?assertEqual(ok, ememcached:add("found", #ememcached_record{key="found", data_block="1"})),
  ?assert(ememcached:contains("found")),

  ?assertEqual(ok, ememcached:destroy()),
  ok.
