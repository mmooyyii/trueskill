-module(trueskill).
-author("yimo").

-include("ts.hrl").



-export([new_player/0, new_player/1, new_player/2]).


new_player() ->
    ts_player:new(?Mu, ?Sigma).
new_player(Mu) ->
    ts_player:new(Mu, ?Sigma).
new_player(Mu, Sigma) ->
    ts_player:new(Mu, Sigma).




