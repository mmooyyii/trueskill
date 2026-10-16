-module(ts_linalg_tests).
-author("yimo").

-include_lib("eunit/include/eunit.hrl").
-include("test_util.hrl").

assert_matrix(Expect, Actual) ->
    ?assertEqual(length(Expect), length(Actual)),
    lists:foreach(fun({E, A}) -> ?assertNear(E, A, 1.0e-9) end,
        lists:zip(lists:append(Expect), lists:append(Actual))).

dot_test() ->
    ?assertEqual(11, ts_linalg:dot([1, 2], [3, 4])),
    ?assertEqual(0, ts_linalg:dot([], [])).

mat_vec_test() ->
    ?assertEqual([5, 11], ts_linalg:mat_vec([[1, 2], [3, 4]], [1, 2])).

mul_test() ->
    ?assertEqual([[7, 10], [15, 22]], ts_linalg:mul([[1, 2], [3, 4]], [[1, 2], [3, 4]])),
    ?assertEqual([[5]], ts_linalg:mul([[1, 2]], [[1], [2]])).

add_test() ->
    ?assertEqual([[2, 4], [6, 8]], ts_linalg:add([[1, 2], [3, 4]], [[1, 2], [3, 4]])).

scale_test() ->
    ?assertEqual([[2, 4]], ts_linalg:scale(2, [[1, 2]])).

transpose_test() ->
    ?assertEqual([[1, 3], [2, 4]], ts_linalg:transpose([[1, 2], [3, 4]])),
    ?assertEqual([[1], [2], [3]], ts_linalg:transpose([[1, 2, 3]])).

diag_test() ->
    ?assertEqual([[1, 0.0], [0.0, 2]], ts_linalg:diag([1, 2])),
    ?assertEqual([1, 4], ts_linalg:diagonal([[1, 2], [3, 4]])).

determinant_test() ->
    ?assertEqual(1.0, ts_linalg:determinant([])),
    ?assertNear(5.0, ts_linalg:determinant([[5]]), 1.0e-12),
    ?assertNear(-2.0, ts_linalg:determinant([[1, 2], [3, 4]]), 1.0e-12),
    %% 需要换行, 主元为负
    ?assertNear(-1.0, ts_linalg:determinant([[0, 1], [1, 0]]), 1.0e-12),
    ?assertNear(49.0, ts_linalg:determinant([[2, -3, 1], [2, 0, -1], [1, 4, 5]]), 1.0e-9),
    ?assertNear(-10.0, ts_linalg:determinant([[0, 0, 2], [0, 1, 0], [5, 0, 0]]), 1.0e-12),
    ?assert(ts_linalg:determinant([[1, 2], [2, 4]]) == 0),
    ?assert(ts_linalg:determinant([[0, 1], [0, 2]]) == 0).

inverse_test() ->
    A = [[1, 2], [3, 4]],
    assert_matrix([[-2, 1], [1.5, -0.5]], ts_linalg:inverse(A)),
    assert_matrix([[1, 0], [0, 1]], ts_linalg:mul(ts_linalg:inverse(A), A)),
    B = [[0, 2, 1], [1, 0, 0], [3, 1, 4]],
    assert_matrix([[1, 0, 0], [0, 1, 0], [0, 0, 1]], ts_linalg:mul(B, ts_linalg:inverse(B))),
    assert_matrix([[0.25]], ts_linalg:inverse([[4]])).

singular_test() ->
    ?assertError(singular_matrix, ts_linalg:inverse([[1, 2], [2, 4]])).
