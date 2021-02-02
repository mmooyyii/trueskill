-module(ts_matrix_test).
-author("yimo").

%% API
-include_lib("eunit/include/eunit.hrl").



dot_1_test() ->
    A = ts_matrix:new([[1, 2], [3, 4]]),
    Result = ts_matrix:new([[7, 10], [15, 22]]),
    Result = ts_matrix:dot(A, A).

dot_2_test() ->
    A = ts_matrix:new([[1, 2]]),
    B = ts_matrix:new([[1], [2]]),
    Result = ts_matrix:new([[5]]),
    Result = ts_matrix:dot(A, B).

mul_test() ->
    A = ts_matrix:new([[1, 2]]),
    Result = ts_matrix:new([[2, 4]]),
    Result = ts_matrix:mul(2, A).

