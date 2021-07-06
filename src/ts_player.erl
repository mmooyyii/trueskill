-module(ts_player).
-author("yimo").

%% API

-include("ts.hrl").

-export([new/2]).
-export([vs/1]).
%%-export([rate/1, rate/4]).

%% on Xbox Live，default μ = 25, σ = 25 / 3, k = 3

%%-type ts_player() :: ts_player().
%%-type player() :: ts_player().
%%-type group() :: [player()].
%%-type groups() :: [group()].

new(Mu, Sigma) ->
    Pi = math:pow(Sigma, -2),
    #ts_player{mu = Mu, sigma = Sigma, pi = Pi, tau = Pi * Mu, exposure = 0}.

vs(Groups) when length(Groups) >= 2 ->
    Players = lists:flatten(Groups),
    Weights = [lists:map(fun(_) -> 1 end, Group) || Group <- Groups],
    MeanMatrix = ts_matrix:new([[Player#ts_player.mu] || Player <- Players]),
    VarianceMatrix = p_make_variance_matrix(Players),
    RotatedAMatrix = p_make_rotated_matrix(Groups, lists:flatten(Weights)),
    AMatrix = ts_matrix:transpose(RotatedAMatrix),
    Ata = ts_matrix:dot(ts_matrix:mul(?Beta * ?Beta, RotatedAMatrix), AMatrix),
    Atasa = ts_matrix:dot(RotatedAMatrix, ts_matrix:dot(VarianceMatrix, AMatrix)),
    Start = ts_matrix:dot(ts_matrix:transpose(MeanMatrix), AMatrix),
    Middle = ts_matrix:add(Ata, Atasa),
    End = ts_matrix:dot(RotatedAMatrix, MeanMatrix),
    EArg = ts_matrix:determinant(
        ts_matrix:mul(-0.5,
            ts_matrix:dot(Start,
                ts_matrix:dot(ts_matrix:inverse(Middle), End)
            ))),
    SArg = ts_matrix:determinant(Ata) / ts_matrix:determinant(Middle),
    math:exp(EArg) * math:sqrt(SArg).


p_make_rotated_matrix(Groups, Weights) ->
    PlayerNum = length(lists:flatten(Groups)),
    Gs = lists:zip(lists:sublist(Groups, length(Groups) - 1), lists:sublist(Groups, 2, length(Groups))),
    {Ret, _} = lists:foldl(fun({G1, G2}, {Acc, Start}) ->
        {
            [pp_make_rotated_matrix(PlayerNum, Start, G1, G2, Weights) | Acc],
            Start + length(G1)
        } end, {[], 1}, Gs),
    ts_matrix:new(lists:reverse(Ret)).

pp_make_rotated_matrix(PlayerNum, Start, G1, G2, Weights) ->
    LG1 = length(G1),
    LG2 = length(G2),
    F = fun
            (N) when N < Start -> 0;
            (N) when Start =< N andalso N < Start + LG1 -> lists:nth(N, Weights);
            (N) when Start + LG1 =< N andalso N < Start + LG1 + LG2 -> -lists:nth(N, Weights);
            (_) -> 0
        end,
    lists:map(F, lists:seq(1, PlayerNum)).


p_make_variance_matrix(Players) ->
    Length = length(Players),
    F = fun(A, B) when A =:= B -> P = lists:nth(A, Players), math:pow(P#ts_player.sigma, 2);
        (_, _) -> 0
        end,
    ts_matrix:new([[F(A, B) || A <- lists:seq(1, Length)] || B <- lists:seq(1, Length)]).


adjust(Groups) ->
    adjust(Groups, [[1], [1]], ?Delta).

adjust(Groups, Weights, MinDelta) ->
%%    true = ts_utils:all_has_rank(Groups),
%%    GroupSize = length(Groups),
%%    {SortedRatingGroups, SortedWeights} = lists:unzip(lists:sort(lists:zip(Groups, Weights))),
%%    {A, B} = factor_graph_builders(SortedRatingGroups, SortedRanks, SortedWeights),
%%    [RatingLayer | _] = run_schedule(A, B),
%%    TeamSize = _team_sizes(SortedRatingGroups),
%%    Group = [],
%%    TransformedGroups = [],
    [].




