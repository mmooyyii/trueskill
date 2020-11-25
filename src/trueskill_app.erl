%%%-------------------------------------------------------------------
%% @doc trueskill public API
%% @end
%%%-------------------------------------------------------------------

-module(trueskill_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    trueskill_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
