-author("yimo").

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
    tau :: number(),
    exposure :: number(),
    rank :: integer()
}).


-record(prior, {
    vars :: [number()|_],
    val :: number(),
    dynamic :: number()
}).

-record(likelihood, {
    vars :: [number()|_],
    mean :: number(),
    value :: number(),
    variance :: number()
}).

-record(ts_sum, {
    vars :: [],
    sum :: number(),
    terms :: number(),
    coeffs :: number()
}).

-record(context, {
    pointer :: integer(),
    pointer_to_instance :: #{}
}).


-record(gaussian, {
    pi :: number(),
    tau :: number()
}).
-record(variable, {
    gaussian,
    value,
    factors
}).