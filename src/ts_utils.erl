-module(ts_utils).
-author("yimo").


-include("ts.hrl").

-export([enum/1, memset/2, prefix_sum/1, record_type/1]).
-export([stack/0]).

prefix_sum(Ls) ->
    F = fun(X, []) -> [X];(X, Acc) -> [X + hd(Acc) | Acc] end,
    lists:reverse(lists:foldl(F, [], Ls)).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).

memset(Ls, N) when is_list(Ls) ->
    [memset(I, N) || I <- Ls];
memset(_, N) ->
    N.


record_type(Record) ->
    element(1, Record).


stack() ->
    try
        1 / 0
    catch
        _:_:S -> io:format("~p~n", [S])
    end.
