-module(ts_math).
-author("yimo").

%% API
-export([ppf/3, cdf/3, pdf/3]).

-define(Sqrt2, math:sqrt(2)).
-compile([{hipe, o3}]).


erfcinv(Y) when Y >= 2 -> -100.0;
erfcinv(Y) when Y =< 0 -> 100.0;
erfcinv(Y) when Y < 1 -> erfcinv(2 - Y, 1);
erfcinv(Y) when Y < 1 -> erfcinv(Y, -1).
erfcinv(Y, Symbol) ->
    T = math:sqrt(-2 * math:log(Y / 2.0)),
    X = -0.70711 * ((2.30753 + T * 0.27061) / (1.0 + T * (0.99229 + T * 0.04481)) - T),
    Err1 = erfc(X) - Y,
    X1 = Err1 / (1.12837916709551257 * math:exp(-math:pow(X, 2)) - X * Err1) + X,
    Err2 = erfc(X1) - Y,
    X2 = Err2 / (1.12837916709551257 * math:exp(-math:pow(X1, 2)) - X1 * Err2) + X1,
    Symbol * X2.


ppf(X, Mu, Sigma) ->
    Mu - Sigma * math:sqrt(2) * erfcinv(2 * X).

cdf(X, Mu, Sigma) ->
    0.5 * erfc(-(X - Mu) / (Sigma * math:sqrt(2))).

erfc(X) ->
%%  误差函数的泰勒展开式
%%  公式来自 https://zh.wikipedia.org/wiki/%E8%AF%AF%E5%B7%AE%E5%87%BD%E6%95%B0
    T = 1.0 / (1.0 + abs(X) / 2.0),
    R = T * math:exp(-(X * X) -
        1.26551223 + T *
        (1.00002368 + T *
            (0.37409196 + T *
                (0.09678418 + T *
                    (-0.18628806 + T *
                        (0.27886807 + T *
                            (-1.13520398 + T *
                                (1.48851587 + T *
                                    (-0.82215223 + T * 0.17087277))))))))),
    case X < 0 of true -> 2.0 - R;false -> R end.

pdf(X, Mu, Sigma) ->
    %% 概率密度函数
    1 / math:sqrt(2 * math:pi()) * abs(Sigma) * math:exp(-(math:pow((X - Mu) / abs(Sigma), 2) / 2)).


gaussian1(Mu, Sigma) when Sigma =/= 0 -> Pi = math:pow(Sigma, -2), {Pi, Pi * Mu}.
gaussian2(Pi, Tau) -> {Pi, Tau}.