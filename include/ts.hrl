-author("yimo").


%% on Xbox Live，default μ = 25, σ = 25 / 3, k = 3
-define(Mu, 25).
-define(Sigma, ?Mu / 3).
-define(Beta, ?Sigma / 2).
-define(Tau, ?Sigma / 100).
-define(Delta, 0.0001).
-define(DRAW_PROBABILITY, 0.10).

-define(Inf, 999999999).

-record(ts_player, {
    mu :: number(),
    sigma :: number(),
    pi :: number(),
    tau :: number()
}).
-record(context, {
    pointer :: integer(),
    pointer_to_instance :: #{},
    ref :: integer()
}).
