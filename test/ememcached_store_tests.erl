-module(ememcached_store_tests).
-include("include/ememcached.hrl").
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok, _Pid} = ememcached_store:start_link(),

  Hg1=#ememcached_record{key="github",data_block="hg"},
  Git1=#ememcached_record{key="github",data_block="git"},
  ?assertEqual(ok,   ememcached_store:set("github",Hg1)),
  ?assertEqual(ok,   ememcached_store:set("github",Git1)),
  ?assertEqual(Git1, ememcached_store:get("github")),

  Hg2=#ememcached_record{key="bitbucket",data_block="hg"},
  Git2=#ememcached_record{key="bitbucket",data_block="git"},
  ?assertEqual(ok,  ememcached_store:add("bitbucket",Hg2)),
  ?assertEqual(ok,  ememcached_store:add("bitbucket",Git2)),
  ?assertEqual(Hg2, ememcached_store:get("bitbucket")),

  ?assertEqual(ok, ememcached_store:stop()),
  ok.

delete_test() ->
  {ok, _Pid} = ememcached_store:start_link(),

  ?assertEqual(not_found, ememcached_store:delete("not_found")),
  ?assertEqual(ok,        ememcached_store:add("found", #ememcached_record{key="found", data_block="1"})),
  ?assertEqual(ok,        ememcached_store:delete("found")),

  ?assertEqual(ok, ememcached_store:stop()),
  ok.

contains_test() ->
  {ok, _Pid} = ememcached_store:start_link(),

  ?assertNot(ememcached_store:contains("not_found")),
  ?assertEqual(ok, ememcached_store:add("found", #ememcached_record{key="found", data_block="1"})),
  ?assert(ememcached_store:contains("found")),

  ?assertEqual(ok, ememcached_store:stop()),
  ok.
