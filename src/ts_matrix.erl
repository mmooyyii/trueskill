-module(ts_matrix).
-author("yimo").

%% API
-export([new/1, new/3, set/4, transpose/1]).
-export([shape/1, reshape/3]).

-export([at/3, col/2, row/2]).
-export([add/2, mul/2, dot/2]).
-export([fold/3, foldt/3, map/2]).
-export([determinant/1, inverse/1, adjugate/1, minor/3]).

-export([print/1]).

-record(matrix, {
    row :: integer(),
    col :: integer(),
    m :: tuple()
}).

new([FirstRow | _] = Matrix) ->
    Width = length(FirstRow),
    case lists:all(fun(Row) -> length(Row) =:= Width end, Matrix) of
        true ->
            M = list_to_tuple(lists:flatten(Matrix)),
            #matrix{row = length(Matrix), col = length(FirstRow), m = M};
        false ->
            throw({error, bad_matrix})
    end.

new(List, Row, Col) when Row * Col =:= length(List) ->
    reshape(new([List]), Row, Col).

shape(#matrix{col = C, row = R}) -> {R, C}.

at(Row, Col, #matrix{col = C, m = M}) ->
    element(Row * C + Col + 1, M).

set(Row, Col, V, #matrix{col = C, m = M} = Matrix) ->
    Matrix#matrix{m = setelement(Row * C + Col + 1, M, V)}.

set_row(RowIndex, RowData, #matrix{col = C} = M) ->
    F = fun(Col, {Mat, [Ele | Rest]}) -> {set(RowIndex, Col, Ele, Mat), Rest} end,
    {Ret, []} = lists:foldl(F, {M, RowData}, lists:seq(0, C - 1)),
    Ret.

row(Row, M = #matrix{col = C}) ->
    lists:map(fun({RR, CC}) -> at(RR, CC, M) end, [{Row, Col} || Col <- lists:seq(0, C - 1)]).

col(Col, M = #matrix{row = R}) ->
    lists:map(fun({RR, CC}) -> at(RR, CC, M) end, [{Row, Col} || Row <- lists:seq(0, R - 1)]).

reshape(#matrix{row = R, col = C, m = M}, Row, Col) when R * C =:= Row * Col ->
    #matrix{row = Row, col = Col, m = M}.

transpose(Matrix = #matrix{row = R, col = C}) ->
    D = lists:reverse(foldt(fun({_, _, Ele}, Acc) -> [Ele | Acc] end, [], Matrix)),
    #matrix{row = C, col = R, m = list_to_tuple(D)}.

determinant(M = #matrix{row = Row, col = Col}) when Row =:= Col ->
    case lists:foldl(fun p_determinant/2, {1, M, false}, lists:seq(Col - 1, 1, -1)) of
        {Rv, _, true} ->
            Rv;
        {Rv, Matrix, false} ->
            Rv * at(0, 0, Matrix)
    end.

p_determinant(_, {Rv, M, true}) -> {Rv, M, true};
p_determinant(C, {Rv, Mat, false}) ->
    {Pivot, R} = lists:max([{abs(at(R, C, Mat)), R} || R <- lists:seq(0, C)]),
    CR = row(C, Mat),
    RR = row(R, Mat),
    Matrix = set_row(R, CR, set_row(C, RR, Mat)),
    case {Pivot =:= 0, R =:= C} of
        {true, _} -> {0.0, Matrix, true};
        {_, true} -> {Rv * Pivot, adjust_matrix(Matrix, -1.0 / Pivot, C), false};
        {_, false} -> {-Rv * Pivot, adjust_matrix(Matrix, -1.0 / Pivot, C), false}
    end.

adjust_matrix(Matrix, Fact, C) ->
    lists:foldl(fun(R, M) ->
        p_adjust_matrix(M, at(R, C, M) * Fact, C, R) end, Matrix, lists:seq(0, C - 1)).

p_adjust_matrix(Matrix, F, C, R) ->
    lists:foldl(fun(X, M) -> set(R, X, F * at(C, X, M) + at(R, X, M), M) end, Matrix, lists:seq(0, C - 1)).

inverse(M = #matrix{row = 1, col = 1}) -> new([[1 / at(0, 0, M)]]);
inverse(M = #matrix{}) ->
    mul(1 / determinant(M), adjugate(M)).


%% 伴随矩阵
adjugate(Matrix = #matrix{col = C, row = R}) when C =:= R ->
    Loop = fun({Row, Col}, M) -> V = determinant(minor(Row, Col, Matrix)) * p_sign(Row, Col), set(Row, Col, V, M) end,
    MatrixOfCofactors = lists:foldl(Loop, Matrix, ts_utils:for_loop([lists:seq(0, R - 1), lists:seq(0, C - 1)])),
    transpose(MatrixOfCofactors).

%% 余子式
minor(R, C, Matrix = #matrix{row = Row, col = Col}) ->
    Loop = fun
               ({R1, _}, M) when R =:= R1 -> M;
               ({_, C1}, M) when C =:= C1 -> M;
               ({R1, C1}, M) -> [at(R1, C1, Matrix) | M]
           end,
    M = lists:foldr(Loop, [], ts_utils:for_loop([lists:seq(0, Row - 1), lists:seq(0, Col - 1)])),
    new(M, Row - 1, Col - 1).

p_sign(Row, Col) when (Row + Col) rem 2 =:= 1 -> -1;
p_sign(_, _) -> 1.


add(Matrix1 = #matrix{row = R1, col = C1, m = M1}, #matrix{row = R2, col = C2, m = M2})
    when R1 =:= R2 andalso C1 =:= C2 ->
    Add = fun({A, B}) -> A + B end,
    Matrix1#matrix{m = list_to_tuple(lists:map(Add, lists:zip(tuple_to_list(M1), tuple_to_list(M2))))}.


mul(Number, M) when is_number(Number) ->
    map(fun(X) -> X * Number end, M).

dot(M1 = #matrix{row = R1, col = C1}, M2 = #matrix{row = R2, col = C2}) when C1 =:= R2 ->
    new([[p_list_mul(row(X, M1), col(Y, M2)) || Y <- lists:seq(0, C2 - 1)] || X <- lists:seq(0, R1 - 1)]).

p_list_mul(L1, L2) ->
    lists:foldl(fun({A, B}, Acc) -> Acc + A * B end, 0, lists:zip(L1, L2)).

map(Fun, Matrix = #matrix{m = M}) ->
    Matrix#matrix{m = list_to_tuple(lists:map(Fun, tuple_to_list(M)))}.

fold(Fun, Acc, #matrix{m = M}) ->
    lists:foldl(Fun, Acc, tuple_to_list(M)).

foldt(Fun, Acc, Matrix) ->
    p_foldt(Fun, Acc, Matrix, 0, 0).

p_foldt(Fun, Acc, Matrix = #matrix{row = H, col = W}, X, Y) when X =:= H - 1 andalso Y =:= W - 1 ->
    Fun({X, Y, at(X, Y, Matrix)}, Acc);
p_foldt(Fun, Acc, Matrix = #matrix{row = H}, X, Y) when X =:= H ->
    p_foldt(Fun, Acc, Matrix, 0, Y + 1);
p_foldt(Fun, Acc, Matrix, X, Y) ->
    p_foldt(Fun, Fun({X, Y, at(X, Y, Matrix)}, Acc), Matrix, X + 1, Y).


print(#matrix{row = R, col = C, m = M}) ->
    Format = lists:concat(lists:duplicate(R, "  ~p~n")),
    LM = tuple_to_list(M),
    Data = [lists:sublist(LM, X, C) || X <- lists:seq(1, R * C, C)],
    io:format("[~n" ++ Format ++ "]~n", Data).



