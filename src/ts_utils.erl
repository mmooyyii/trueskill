-module(ts_utils).
-author("yimo").


-include("ts.hrl").

-export([enum/1, all_has_rank/1]).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).

all_has_rank(Groups) ->
    lists:foldl(fun(#ts_player{rank = Rank}, Ret) -> Ret and Rank =/= 0 end, true, lists:flatten(Groups)).
