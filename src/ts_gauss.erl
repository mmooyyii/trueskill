-module(ts_gauss).
-author("yimo").

%% 标准正态分布, 以及 Herbrich 2007 里截断高斯的 v / w 函数.

-export([pdf/1, cdf/1, ppf/1, erfcinv/1]).
-export([v_win/2, w_win/2, v_draw/2, w_draw/2]).

-define(SQRT2, 1.4142135623730951).

-spec pdf(number()) -> float().
pdf(X) ->
    math:exp(-X * X / 2) / math:sqrt(2 * math:pi()).

-spec cdf(number()) -> float().
cdf(X) ->
    0.5 * math:erfc(-X / ?SQRT2).

-spec ppf(number()) -> float().
ppf(P) ->
    -?SQRT2 * erfcinv(2 * P).

%% Numerical Recipes 的初值 + 两步牛顿迭代
-spec erfcinv(number()) -> float().
erfcinv(Y) when Y >= 2 -> -100.0;
erfcinv(Y) when Y =< 0 -> 100.0;
erfcinv(Y) when Y < 1 -> erfcinv_newton(Y);
erfcinv(Y) -> -erfcinv_newton(2 - Y).

erfcinv_newton(Y) ->
    T = math:sqrt(-2 * math:log(Y / 2)),
    X0 = -0.70711 * ((2.30753 + T * 0.27061) / (1 + T * (0.99229 + T * 0.04481)) - T),
    Step = fun(_, X) ->
        Err = math:erfc(X) - Y,
        X + Err / (1.12837916709551257 * math:exp(-X * X) - X * Err)
           end,
    lists:foldl(Step, X0, [1, 2]).

%% 胜负: 表现差 T 截断到 (Eps, +inf)
-spec v_win(number(), number()) -> float().
v_win(T, Eps) ->
    X = T - Eps,
    case cdf(X) of
        D when D > 0 -> pdf(X) / D;
        _ -> -X
    end.

-spec w_win(number(), number()) -> float().
w_win(T, Eps) ->
    V = v_win(T, Eps),
    check_w(V * (V + T - Eps)).

%% 平局: 表现差 T 截断到 [-Eps, Eps]
-spec v_draw(number(), number()) -> float().
v_draw(T, Eps) ->
    {A, B, D} = draw_bounds(T, Eps),
    V = case D > 0 of
            true -> (pdf(B) - pdf(A)) / D;
            false -> A
        end,
    case T < 0 of
        true -> -V;
        false -> V
    end.

-spec w_draw(number(), number()) -> float().
w_draw(T, Eps) ->
    case draw_bounds(T, Eps) of
        {A, B, D} when D > 0 ->
            V = v_draw(abs(T), Eps),
            check_w(V * V + (A * pdf(A) - B * pdf(B)) / D);
        _ ->
            erlang:error(floating_point_error)
    end.

%% 理论上 w 在 (0, 1) 之间, 超出说明浮点精度不够
check_w(W) when W > 0, W < 1 -> W;
check_w(_) -> erlang:error(floating_point_error).

draw_bounds(T, Eps) ->
    A = Eps - abs(T),
    B = -Eps - abs(T),
    {A, B, cdf(A) - cdf(B)}.
