%%%-------------------------------------------------------------------
%%% @author yimo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 8月 2021 9:23 下午
%%%-------------------------------------------------------------------
-module(ts_assert).
-author("yimo").

-include("ts.hrl").

-export([assert_matrix_eq/2, assert_player_equal/2]).

assert_matrix_eq(M1, M2) ->
    true = (ts_matrix:shape(M1) =:= ts_matrix:shape(M2)),
    R = ts_matrix:foldt(fun({X, Y, E}, Acc) -> Acc andalso abs(E - ts_matrix:at(X, Y, M2)) < 0.001 end, true, M1),
    result(R, M1, M2).


assert_player_equal(
    A = #ts_player{mu = A1, sigma = S1, pi = P1, tau = T1},
    B = #ts_player{mu = A2, sigma = S2, pi = P2, tau = T2}) ->
    Threhold = 0.001,
    R = (abs(A1 - A2) < Threhold andalso abs(S1 - S2) < Threhold andalso
        abs(P1 - P2) < Threhold andalso abs(T1 - T2) < Threhold),
    result(R, A, B).

result(false, A, B) -> throw({assert_error, A, B});
result(true, _, _) -> ok.
