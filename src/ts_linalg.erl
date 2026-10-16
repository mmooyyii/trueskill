-module(ts_linalg).
-author("yimo").

%% 小规模稠密矩阵, 向量是 [number()], 矩阵是按行存的 [[number()]].

-export([dot/2, mat_vec/2, mul/2, add/2, scale/2, transpose/1]).
-export([diag/1, diagonal/1, determinant/1, inverse/1]).

-export_type([vector/0, matrix/0]).

-type vector() :: [number()].
-type matrix() :: [vector()].

-spec dot(vector(), vector()) -> number().
dot(A, B) ->
    lists:sum(lists:zipwith(fun erlang:'*'/2, A, B)).

-spec mat_vec(matrix(), vector()) -> vector().
mat_vec(M, V) ->
    [dot(Row, V) || Row <- M].

-spec mul(matrix(), matrix()) -> matrix().
mul(A, B) ->
    Cols = transpose(B),
    [mat_vec(Cols, Row) || Row <- A].

-spec add(matrix(), matrix()) -> matrix().
add(A, B) ->
    lists:zipwith(fun(RA, RB) -> lists:zipwith(fun erlang:'+'/2, RA, RB) end, A, B).

-spec scale(number(), matrix()) -> matrix().
scale(K, M) ->
    [[K * X || X <- Row] || Row <- M].

-spec transpose(matrix()) -> matrix().
transpose([[] | _]) -> [];
transpose(M) -> [[H || [H | _] <- M] | transpose([T || [_ | T] <- M])].

-spec diag(vector()) -> matrix().
diag(V) ->
    N = length(V),
    [[case J of I -> X; _ -> 0.0 end || J <- lists:seq(1, N)] || {I, X} <- lists:zip(lists:seq(1, N), V)].

-spec diagonal(matrix()) -> vector().
diagonal(M) ->
    [lists:nth(I, Row) || {I, Row} <- lists:zip(lists:seq(1, length(M)), M)].

%% 列主元高斯消元
-spec determinant(matrix()) -> float().
determinant([]) ->
    1.0;
determinant(M) ->
    case pivot(M, 1) of
        {_, [P | _], _} when P == 0 ->
            0.0;
        {I, [P | Ps], Rest} ->
            Sub = [[X - R / P * Y || {X, Y} <- lists:zip(Rs, Ps)] || [R | Rs] <- Rest],
            %% 第 I 行换到最前面要做 I - 1 次相邻交换
            sign(I - 1) * P * determinant(Sub)
    end.

sign(N) when N rem 2 =:= 0 -> 1;
sign(_) -> -1.

%% Gauss-Jordan
-spec inverse(matrix()) -> matrix().
inverse(M) ->
    N = length(M),
    Identity = diag(lists:duplicate(N, 1.0)),
    Augmented = lists:zipwith(fun erlang:'++'/2, M, Identity),
    [lists:nthtail(N, Row) || Row <- jordan(Augmented, [])].

jordan([], Done) ->
    lists:reverse(Done);
jordan(Rows, Done) ->
    C = length(Done) + 1,
    {_, Pivot, Rest} = pivot(Rows, C),
    P = lists:nth(C, Pivot),
    P == 0 andalso erlang:error(singular_matrix),
    Norm = [X / P || X <- Pivot],
    Eliminate = fun(Row) ->
        F = lists:nth(C, Row),
        lists:zipwith(fun(X, Y) -> X - F * Y end, Row, Norm)
                end,
    jordan(lists:map(Eliminate, Rest), [Norm | lists:map(Eliminate, Done)]).

%% 第 C 列绝对值最大的行, 返回 {行号, 该行, 其余行}
pivot(Rows, C) ->
    Indexed = lists:zip(lists:seq(1, length(Rows)), Rows),
    %% 绝对值相同时取靠前的行
    {_, NegI} = lists:max([{abs(lists:nth(C, Row)), -I} || {I, Row} <- Indexed]),
    {Before, [Row | After]} = lists:split(-NegI - 1, Rows),
    {-NegI, Row, Before ++ After}.
