-module(trueskill).
-author("yimo").

-include("ts.hrl").



-export([new_player/0, new_player/1, new_player/2]).
-export([vs/1]).

new_player() ->
    ts_player:new(?Mu, ?Sigma).
new_player(Mu) ->
    ts_player:new(Mu, ?Sigma).
new_player(Mu, Sigma) ->
    ts_player:new(Mu, Sigma).


vs(Groups) when length(Groups) >= 2 ->
    ts_player:vs(Groups).

rate(Groups) ->
    ok.
