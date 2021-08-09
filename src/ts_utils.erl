-module(ts_utils).
-author("yimo").


-include("ts.hrl").

-export([enum/1, memset/2, prefix_sum/1]).

prefix_sum(Ls) ->
    F = fun(X, []) -> [X];(X, Acc) -> [X + hd(Acc) | Acc] end,
    lists:reverse(lists:foldl(F, [], Ls)).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).

memset(Ls, N) when is_list(Ls) ->
    [memset(I, N) || I <- Ls];
memset(_, N) ->
    N.


