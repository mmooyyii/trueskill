-module(ts_player).
-author("yimo").

%% API
-export([new/0, new/2]).

-include("ts.hrl").

%% on Xbox Live，default μ = 25, σ = 25 / 3, k = 3
-define(Mu, 25).
-define(Sigma, ?Mu / 3).
-define(Beta, ?Sigma / 2).

new() -> new(?Mu, ?Sigma).

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
    Ata = ts_matrix:dot(ts_matrix:map(fun(N) -> N * ?Beta * ?Beta end, RotatedAMatrix), AMatrix),
    Atasa = ts_matrix:dot(RotatedAMatrix, ts_matrix:dot(VarianceMatrix, AMatrix)),
    Start = ts_matrix:dot(ts_matrix:transpose(RotatedAMatrix), AMatrix),
    Middle = ts_matrix:add(Ata, Atasa),
    End = ts_matrix:dot(RotatedAMatrix, MeanMatrix),
    EArg = ok,
    SArg = ok,
    math:exp(EArg) * math:sqrt(SArg).




