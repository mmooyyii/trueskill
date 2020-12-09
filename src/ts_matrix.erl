-module(ts_matrix).
-author("yimo").

%% API
-export([new/1, at/3, set/4, transpose/1]).
-export([reshape/3]).
-export([fold/3, foldt/3]).


-export([print/1]).

-record(matrix, {
    height :: integer(),
    width :: integer(),
    m :: tuple()
}).

new([]) ->
    #matrix{height = 0, width = 0, m = {}};

new([FirstRow | _] = Matrix) ->
    Width = length(FirstRow),
    case lists:all(fun(Row) -> length(Row) =:= Width end, Matrix) of
        true ->
            M = list_to_tuple(lists:flatten(Matrix)),
            #matrix{height = length(Matrix), width = length(FirstRow), m = M};
        false ->
            throw({error, bad_matrix})
    end.

at(X, Y, #matrix{width = W, m = M}) ->
    element(X * W + Y + 1, M).

set(X, Y, V, #matrix{width = W, m = M}) ->
    setelement(X * W + Y + 1, M, V).


reshape(#matrix{height = H, width = W, m = M}, Height, Weight) when H * W =:= Height * Weight ->
    #matrix{height = Height, width = Weight, m = M}.

transpose(Matrix = #matrix{height = H, width = W}) ->
    D = lists:reverse(foldt(fun(Ele, Acc) -> [Ele | Acc] end, [], Matrix)),
    #matrix{height = W, width = H, m = list_to_tuple(D)}.


fold(Fun, Acc, #matrix{m = M}) ->
    lists:foldl(Fun, Acc, tuple_to_list(M)).

foldt(Fun, Acc, Matrix) ->
    p_foldt(Fun, Acc, Matrix, 0, 0).

p_foldt(Fun, Acc, Matrix = #matrix{height = H, width = W}, X, Y) when X =:= H - 1 andalso Y =:= W - 1 ->
    Fun(at(X, Y, Matrix), Acc);
p_foldt(Fun, Acc, Matrix = #matrix{height = H}, X, Y) when X =:= H ->
    p_foldt(Fun, Acc, Matrix, 0, Y + 1);
p_foldt(Fun, Acc, Matrix, X, Y) ->
    p_foldt(Fun, Fun(at(X, Y, Matrix), Acc), Matrix, X + 1, Y).


print(#matrix{height = H, width = W, m = M}) ->
    Format = lists:concat(lists:duplicate(H, "  ~p~n")),
    LM = tuple_to_list(M),
    Data = [lists:sublist(LM, X, W) || X <- lists:seq(1, H * W, W)],
    io:format("[~n" ++ Format ++ "]~n", Data).