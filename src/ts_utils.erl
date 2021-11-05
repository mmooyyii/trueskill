-module(ts_utils).
-author("yimo").


-include("ts.hrl").

-export([enum/1, memset/2, prefix_sum/1]).
-export([for_loop/1]).

prefix_sum(Ls) ->
    F = fun(X, []) -> [X];(X, Acc) -> [X + hd(Acc) | Acc] end,
    lists:reverse(lists:foldl(F, [], Ls)).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).

memset(Ls, N) when is_list(Ls) ->
    [memset(I, N) || I <- Ls];
memset(_, N) ->
    N.

for_loop(Ls) ->
    Lists = lists:reverse(Ls),
    Tuples = lists:map(fun list_to_tuple/1, Lists),
    Indexes = [1 || _ <- Lists],
    p_for_loop(Tuples, Indexes, []).

p_for_loop(_, stop, Ret) -> lists:reverse(Ret);
p_for_loop(Tuples, Indexes, Ret) ->
    R = lists:map(fun({Tuple, Index}) -> element(Index, Tuple) end, lists:zip(Tuples, Indexes)),
    NewRet = [list_to_tuple(lists:reverse(R)) | Ret],
    p_for_loop(Tuples, next_index(Tuples, Indexes), NewRet).

next_index(Tuples, Index) -> p_next_index([size(Tuple) || Tuple <- Tuples], Index, 1, []).

p_next_index(_, [], 1, _) -> stop;
p_next_index(_, [], 0, Return) -> lists:reverse(Return);
p_next_index([MaxN | RestTuples], [N | RestIndexes], Carry, Return) when MaxN < N + Carry ->
    p_next_index(RestTuples, RestIndexes, 1, [1 | Return]);
p_next_index([_ | RestTuples], [N | RestIndexes], Carry, Return) ->
    p_next_index(RestTuples, RestIndexes, 0, [N + Carry | Return]).