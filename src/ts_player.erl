-module(ts_player).
-author("yimo").

%% API
-export([new/0, new/1, new/2]).
-include("ts.hrl").

-export(['1vs1'/2]).

%% on Xbox Live，default μ = 25, σ = 25 / 3, k = 3
-define(Mu, 25).
-define(Sigma, ?Mu / 3).
-define(Beta, ?Sigma / 2).

new() -> new(?Mu, ?Sigma).
new(Mu) -> new(Mu, ?Sigma).
new(Mu, Sigma) ->
    Pi = math:pow(Sigma, -2),
    #ts_player{mu = Mu, sigma = Sigma, pi = Pi, tau = Pi * Mu, exposure = 0}.



'1vs1'(P1 = #ts_player{}, P2 = #ts_player{}) ->
    MeanMatrix = ts_matrix:new([P1#ts_player.mu, P2#ts_player.mu], 2, 1),
    VarianceMatrix = ts_matrix:new(
        [[P1#ts_player.sigma * P1#ts_player.sigma, 0],
            [0, P2#ts_player.sigma * P2#ts_player.sigma]]
    ),
    RotatedAMatrix = ts_matrix:new([[1, -1]]),
    AMatrix = ts_matrix:transpose(RotatedAMatrix),
    Ata = ts_matrix:dot(ts_matrix:mul(?Beta * ?Beta, RotatedAMatrix), AMatrix),
    Atasa = ts_matrix:dot(RotatedAMatrix, ts_matrix:dot(VarianceMatrix, AMatrix)),
    Start = ts_matrix:dot(ts_matrix:transpose(MeanMatrix), AMatrix),
    Middle = ts_matrix:add(Ata, Atasa),
    End = ts_matrix:dot(RotatedAMatrix, MeanMatrix),
    io:format("~p~n", [Middle]),
    EArg = ts_matrix:determinant(ts_matrix:mul(-0.5, ts_matrix:dot(Start, ts_matrix:dot(ts_matrix:inverse(Middle), End)))),
    SArg = ts_matrix:determinant(Ata) / ts_matrix:determinant(Middle),
    math:exp(EArg) * math:sqrt(SArg).

f() ->
    ts_player:'1vs1'(ts_player:new(), ts_player:new()).