-module(ts_player).
-author("yimo").

%% API
-export([new/0, new/2]).

-record(ts_player, {
    mu :: number(),
    sigma :: number(),
    pi :: number(),
    tau :: number(),
    exposure :: number()
}).

new() ->
    %% on Xbox Live，default μ = 25, σ = 25 / 3, k = 3
    new(_mu = 25, _sigma = 25 / 3).

new(Mu, Sigma) ->
    Pi = math:pow(Sigma, -2),
    #ts_player{
        mu = Mu,
        sigma = Sigma,
        pi = Pi,
        tau = Pi * Mu,
        exposure = 0
    }.

