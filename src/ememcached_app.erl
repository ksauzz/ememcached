-module(ememcached_app).

-behaviour(application).

-define(PORT, 11211).

-include_lib("eunit/include/eunit.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ememcached:init(),
    {ok, LSock} = gen_tcp:listen(?PORT, [{active, true}]),
    ?debugMsg("listening port"),
    case ememcached_sup:start_link(LSock) of
      {ok, Pid} -> ememcached_sup:start_child(),
        {ok, Pid};
      Other ->
        {error, Other}
    end.

stop(_State) ->
    ok.
