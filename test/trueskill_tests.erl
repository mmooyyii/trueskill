-module(trueskill_tests).
-author("yimo").

-include("trueskill.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_util.hrl").

-define(BETA, 25 / 6).
-define(TAU, 25 / 300).

p() -> trueskill:new_player().
p(Mu) -> trueskill:new_player(Mu).

mus(Teams) -> [P#ts_player.mu || P <- lists:append(Teams)].
sigmas(Teams) -> [P#ts_player.sigma || P <- lists:append(Teams)].

%% Herbrich 2007 两队对局的闭式更新:
%%   c^2 = sum(w_i^2 * (sigma_i^2 + tau^2 + beta^2)), t = 胜方加权 mu 之和 - 负方加权 mu 之和
%%   mu_i' = mu_i +- w_i * sigma_i^2 / c * v(t / c, eps / c)
%%   sigma_i'^2 = sigma_i^2 * (1 - w_i^2 * sigma_i^2 / c^2 * w(t / c, eps / c))
%% 只有一个截断因子, EP 一步就是精确解, 用来校验 adjust
closed_form(Winner, Loser, WinnerWeights, LoserWeights, Draw) ->
    Var = fun(#ts_player{sigma = S}) -> S * S + ?TAU * ?TAU end,
    Ws = WinnerWeights ++ LoserWeights,
    Players = Winner ++ Loser,
    C = math:sqrt(lists:sum([W * W * (Var(P) + ?BETA * ?BETA) || {W, P} <- lists:zip(Ws, Players)])),
    T = lists:sum([W * P#ts_player.mu || {W, P} <- lists:zip(WinnerWeights, Winner)])
        - lists:sum([W * P#ts_player.mu || {W, P} <- lists:zip(LoserWeights, Loser)]),
    Eps = ts_gauss:ppf(0.55) * math:sqrt(length(Players)) * ?BETA,
    {V, W} = case Draw of
                 true -> {ts_gauss:v_draw(T / C, Eps / C), ts_gauss:w_draw(T / C, Eps / C)};
                 false -> {ts_gauss:v_win(T / C, Eps / C), ts_gauss:w_win(T / C, Eps / C)}
             end,
    Update = fun(Sign) -> fun({Wi, P = #ts_player{mu = Mu}}) ->
        S2 = Var(P),
        {Mu + Sign * Wi * S2 / C * V, math:sqrt(S2 * (1 - Wi * Wi * S2 / (C * C) * W))}
                               end end,
    lists:map(Update(1), lists:zip(WinnerWeights, Winner)) ++ lists:map(Update(-1), lists:zip(LoserWeights, Loser)).

assert_closed_form(Winner, Loser, WinnerWeights, LoserWeights, Draw, Result) ->
    lists:foreach(fun({{Mu, Sigma}, #ts_player{mu = M, sigma = S}}) ->
        ?assertNear(Mu, M, 1.0e-9),
        ?assertNear(Sigma, S, 1.0e-9)
                  end, lists:zip(closed_form(Winner, Loser, WinnerWeights, LoserWeights, Draw), lists:append(Result))).

new_player_test() ->
    ?assertEqual(#ts_player{mu = 25.0, sigma = 25 / 3}, trueskill:new_player()),
    ?assertEqual(#ts_player{mu = 30.0, sigma = 25 / 3}, trueskill:new_player(30)),
    ?assertEqual(#ts_player{mu = 30.0, sigma = 2.0}, trueskill:new_player(30, 2)).

%% 两队: q = sqrt(Ata / middle) * exp(-t^2 / (2 * middle)), middle = sum(w^2 * (beta^2 + sigma^2))
vs_two_teams_test() ->
    Q = fun(Diff, Middle, Ata) -> math:sqrt(Ata / Middle) * math:exp(-Diff * Diff / (2 * Middle)) end,
    S2 = (25 / 3) * (25 / 3),
    B2 = ?BETA * ?BETA,
    ?assertNear(Q(0, 2 * (B2 + S2), 2 * B2), trueskill:vs([[p()], [p()]]), 1.0e-12),
    ?assertNear(Q(5, 2 * (B2 + S2), 2 * B2), trueskill:vs([[p(25)], [p(30)]]), 1.0e-12),
    ?assertNear(Q(12.5, 4 * (B2 + S2), 4 * B2), trueskill:vs([[p(), p()], [p(), p(25 / 2)]]), 1.0e-12),
    ?assertNear(Q(25 - 1.5 * 25, 2.25 * (B2 + S2), 2.25 * B2), trueskill:vs([[p()], [p(), p()]], [[1], [1, 0.5]]), 1.0e-12).

%% 三队实力相同的 sigma: middle = (beta^2 + sigma^2) * M, M = [[2, -1], [-1, 2]], M^-1 = [[2, 1], [1, 2]] / 3
%%   q = beta^2 / (beta^2 + sigma^2) * exp(-d' M^-1 d / (2 * (beta^2 + sigma^2)))
vs_three_teams_test() ->
    S2 = (25 / 3) * (25 / 3),
    K = ?BETA * ?BETA + S2,
    ?assertNear(?BETA * ?BETA / K, trueskill:vs([[p()], [p()], [p()]]), 1.0e-12),
    %% d = [-5, -5], d' M^-1 d = 50
    ?assertNear(?BETA * ?BETA / K * math:exp(-50 / (2 * K)), trueskill:vs([[p(25)], [p(30)], [p(35)]]), 1.0e-12).

one_vs_one_test() ->
    assert_closed_form([p()], [p()], [1], [1], false, trueskill:adjust([[p()], [p()]], [1, 2])).

upset_test() ->
    assert_closed_form([p(20)], [p(30)], [1], [1], false, trueskill:adjust([[p(20)], [p(30)]])).

two_vs_one_test() ->
    W = [p(10), p(20)],
    assert_closed_form(W, [p(15)], [1, 1], [1], false, trueskill:adjust([W, [p(15)]])).

draw_test() ->
    assert_closed_form([p()], [p()], [1], [1], true, trueskill:adjust([[p()], [p()]], [1, 1])),
    assert_closed_form([p(20)], [p(30)], [1], [1], true, trueskill:adjust([[p(20)], [p(30)]], [1, 1])).

unsorted_rank_test() ->
    [[A], [B]] = trueskill:adjust([[p(20)], [p(30)]], [2, 1]),
    assert_closed_form([p(30)], [p(20)], [1], [1], false, [[B], [A]]).

weights_test() ->
    assert_closed_form([p()], [p(), p()], [1], [1, 0.5], false, trueskill:adjust([[p()], [p(), p()]], [1, 2], [[1], [1, 0.5]])).

%% 权重为 0 的玩家不受影响
zero_weight_test() ->
    [_, [_, Idle]] = trueskill:adjust([[p()], [p(), p()]], [1, 2], [[1], [1, 0]]),
    ?assertNear(25.0, Idle#ts_player.mu, 1.0e-12),
    ?assertNear(math:sqrt((25 / 3) * (25 / 3) + ?TAU * ?TAU), Idle#ts_player.sigma, 1.0e-12).

%% 多队没有闭式解, 检查对称性和单调性
free_for_all_test() ->
    Teams = trueskill:adjust([[p()], [p()], [p()]]),
    [M1, M2, M3] = mus(Teams),
    [S1, S2, S3] = sigmas(Teams),
    ?assert(M1 > M2 andalso M2 > M3),
    ?assertNear(25.0, M2, 1.0e-6),
    ?assertNear(M1 - 25, 25 - M3, 1.0e-6),
    ?assertNear(S1, S3, 1.0e-6),
    %% 中间的人和两边都比过, 信息更多
    ?assert(S2 < S1 andalso S1 < 25 / 3).

three_teams_test() ->
    [M1, M2, M3, M4] = mus(trueskill:adjust([[p()], [p()], [p(), p()]])),
    ?assert(M1 > M2 andalso M2 > M3),
    ?assertEqual(M3, M4).

multi_draw_test() ->
    [M1, M2, M3] = mus(trueskill:adjust([[p()], [p()], [p()]], [1, 2, 2])),
    ?assert(M1 > 25 andalso M2 < 25 andalso M3 < 25),
    ?assert(abs(M2 - M3) < 0.01),
    [D1, D2, D3] = mus(trueskill:adjust([[p()], [p()], [p()]], [1, 1, 1])),
    ?assertNear(25.0, D1, 1.0e-6),
    ?assertNear(25.0, D2, 1.0e-6),
    ?assertNear(25.0, D3, 1.0e-6).

%% 实力过于悬殊时截断因子算不出来, 技能保持不变
extreme_test() ->
    [[#ts_player{mu = M}], _] = trueskill:adjust([[trueskill:new_player(0, 1)], [trueskill:new_player(1000, 1)]]),
    ?assertNear(0.0, M, 1.0e-9).

bad_args_test() ->
    ?assertError(function_clause, trueskill:adjust([[p()]])),
    ?assertError(function_clause, trueskill:adjust([[p()], [p()]], [1])),
    ?assertError(function_clause, trueskill:vs([[p()]])).
