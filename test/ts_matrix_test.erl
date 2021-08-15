-module(ts_matrix_test).
-author("yimo").

-include_lib("eunit/include/eunit.hrl").

-import(ts_assert, [assert_matrix_eq/2]).

dot_1_test() ->
    A = ts_matrix:new([[1, 2], [3, 4]]),
    assert_matrix_eq(ts_matrix:new([[7, 10], [15, 22]]), ts_matrix:dot(A, A)).

dot_2_test() ->
    A = ts_matrix:new([[1, 2]]),
    B = ts_matrix:new([[1], [2]]),
    assert_matrix_eq(ts_matrix:new([[5]]), ts_matrix:dot(A, B)).

mul_test() ->
    A = ts_matrix:new([[1, 2]]),
    assert_matrix_eq(ts_matrix:new([[2, 4]]), ts_matrix:mul(2, A)).

inverse_test() ->
    A = ts_matrix:new([[1, 2], [3, 4]]),
    Expect = ts_matrix:new([
        [1, 0],
        [0, 1]
    ]),
    assert_matrix_eq(Expect, ts_matrix:dot(ts_matrix:inverse(A), A)).

minor_test() ->
    A = ts_matrix:new([
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ]),
    Expect = ts_matrix:new([
        [5, 6],
        [8, 9]
    ]),
    assert_matrix_eq(Expect, ts_matrix:minor(0, 0, A)).


adjugate_test() ->
    A = ts_matrix:new([
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ]),
    Expect = ts_matrix:new([
        [-3, 6, -3],
        [6, -12, 6],
        [-3, 6, -3]
    ]),
    assert_matrix_eq(Expect, ts_matrix:adjugate(A)).

