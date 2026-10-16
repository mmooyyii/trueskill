-module(trueskill2_tests).
-author("yimo").

-include("trueskill.hrl").
-include("trueskill2.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_util.hrl").

p() -> trueskill2:new_player().

match(Teams) ->
    match(Teams, lists:seq(1, length(Teams))).

match(Teams, Ranks) ->
    #{mode => slayer, time => 0, length => 10, ranks => Ranks, teams => [[#{player => P} || P <- T] || T <- Teams]}.

with_teams(Teams) ->
    #{mode => slayer, time => 0, length => 10, teams => Teams}.

skills(Groups, Params) ->
    [[trueskill2:skill(P, slayer, Params) || P <- G] || G <- Groups].

mu(P, Params) ->
    element(1, trueskill2:skill(P, slayer, Params)).

%% 关掉 TrueSkill 2 的扩展, 参数换成经典 TrueSkill 的, 结果应该和 trueskill:adjust 一样
classic_params(Size) ->
    Beta = 25 / 6,
    #{beta => Beta, mean => 25.0, variance => (25 / 3) * (25 / 3) + (25 / 300) * (25 / 300), gamma => 0.0, tau => 0.0,
        draw_margin => ts_gauss:ppf(0.55) * math:sqrt(Size) * Beta}.

assert_classic(Teams, Ranks) ->
    Expect = trueskill:adjust([[trueskill:new_player() || _ <- T] || T <- Teams], Ranks),
    Actual = skills(trueskill2:rate(match(Teams, Ranks), classic_params(2)), classic_params(2)),
    lists:foreach(fun({#ts_player{mu = M, sigma = S}, {Mu, Sigma}}) ->
        ?assertNear(M, Mu, 1.0e-4),
        ?assertNear(S, Sigma, 1.0e-4)
                  end, lists:zip(lists:append(Expect), lists:append(Actual))).

classic_test() ->
    assert_classic([[p()], [p()]], [1, 2]),
    assert_classic([[p()], [p()], [p()]], [1, 2, 3]),
    assert_classic([[p()], [p()], [p()]], [1, 2, 2]),
    assert_classic([[p()], [p()]], [1, 1]),
    assert_classic([[p()], [p()]], [2, 1]).

default_test() ->
    ?assertEqual({3.0, math:sqrt(1.6)}, trueskill2:skill(p(), slayer, #{})),
    [[A], [B]] = trueskill2:rate(match([[p()], [p()]]), #{}),
    ?assert(mu(A, #{}) > 3.0),
    ?assert(mu(B, #{}) < 3.0),
    #ts2_player{modes = #{slayer := #ts2_mode{experience = 1, time = 10}}, base_time = 10} = A.

%% 默认 ranks 按 teams 顺序
default_ranks_test() ->
    M = maps:remove(ranks, match([[p()], [p()]])),
    ?assertEqual(trueskill2:rate(match([[p()], [p()]]), #{}), trueskill2:rate(M, #{})).

bad_ranks_test() ->
    ?assertError(badarg, trueskill2:rate(match([[p()], [p()]], [1]), #{})),
    ?assertError(function_clause, trueskill2:rate(match([[p()]]), #{})).

%% 式 (20): base 把一个模式里的胜负带到另一个模式
mode_correlation_test() ->
    Params = #{mode_weight => 1.0},
    [[A], _] = trueskill2:rate(match([[p()], [p()]]), Params),
    {Mu, _} = trueskill2:skill(A, swat, Params),
    ?assert(Mu > 3.0),
    [[B], _] = trueskill2:rate(match([[p()], [p()]]), #{}),
    ?assertEqual({3.0, math:sqrt(1.6)}, trueskill2:skill(B, swat, #{})).

%% 式 (7): squad 被认为更强, 赢了涨得更少
squad_test() ->
    Teams = fun(Size) -> [[#{player => p(), squad_size => Size}, #{player => p(), squad_size => Size}],
        [#{player => p()}, #{player => p()}]] end,
    Rate = fun(Size, Offsets) ->
        Params = #{squad_offset => Offsets},
        [[A, _], _] = trueskill2:rate(with_teams(Teams(Size)), Params),
        mu(A, Params)
           end,
    Solo = Rate(1, #{2 => 1.0}),
    ?assert(Rate(2, #{2 => 1.0}) < Solo),
    ?assertEqual(Rate(2, #{3 => 1.0}), Solo),
    ?assertEqual(Rate(3, #{2 => 1.0}), Rate(2, #{2 => 1.0})),
    ?assertEqual(Rate(5, #{2 => 0.5, 4 => 1.0}), Rate(4, #{4 => 1.0})).

%% 式 (8): 每局之后按经验加偏移, 超过表长度取最后一个
experience_test() ->
    Params = #{experience_offset => [0.5, 0.2]},
    Play = fun(P, Ps) -> [[A], _] = trueskill2:rate(match([[P], [p()]], [1, 1]), Ps), A end,
    A1 = Play(p(), Params),
    ?assertNear(mu(Play(p(), #{}), #{}) + 0.5, mu(A1, Params), 1.0e-9),
    A2 = Play(A1, Params),
    ?assertNear(mu(Play(A1, #{}), #{}) + 0.2, mu(A2, Params), 1.0e-9),
    A3 = Play(A2, Params),
    ?assertNear(mu(Play(A2, #{}), #{}) + 0.2, mu(A3, Params), 1.0e-9),
    #ts2_player{modes = #{slayer := #ts2_mode{experience = 3}}} = A3.

%% 式 (3): 方差随不玩的时间增长
time_test() ->
    Params = #{tau => 0.01, base_tau => 0.01, mode_weight => 1.0},
    [[A], _] = trueskill2:rate(match([[p()], [p()]]), Params),
    {Mu, Sigma} = trueskill2:skill(A, slayer, Params),
    ?assertEqual({Mu, Sigma}, trueskill2:skill(A, slayer, Params, 5)),
    {Mu, Later} = trueskill2:skill(A, slayer, Params, 1000),
    ?assertNear(math:sqrt(Sigma * Sigma + 2 * 0.01 * 0.01 * 990), Later, 1.0e-9),
    ?assertEqual({3.0, math:sqrt(1.6)}, trueskill2:skill(p(), slayer, #{}, 1000)).

%% 式 (9): 同一队里击杀多的分到更多的胜利, 0 击杀走截断分支
counts_test() ->
    Params = #{counts => #{kill => {1.0, -1.0, 1.0}, death => {-1.0, 1.0, 1.0}}},
    Teams = [[#{player => p(), counts => #{kill => 10, death => 2}}, #{player => p(), counts => #{kill => 0, death => 6}}],
        [#{player => p(), counts => #{kill => 4, death => 5}}, #{player => p(), counts => #{kill => 4, death => 5}}]],
    [[A, B], [C, D]] = trueskill2:rate(with_teams(Teams), Params),
    ?assert(mu(A, Params) > mu(B, Params)),
    ?assertNear(mu(C, Params), mu(D, Params), 1.0e-9).

%% 式 (12): 同队的退出者分数更低
quit_test() ->
    Teams = [[#{player => p(), quit => true, time_played => 3}, #{player => p()}], [#{player => p()}, #{player => p()}]],
    [[Quitter, Mate], _] = trueskill2:rate(with_teams(Teams), #{}),
    ?assert(mu(Quitter, #{}) < mu(Mate, #{})),
    ?assert(mu(Quitter, #{}) < 3.0).

%% 上场时间都为 0 时名次和击杀数都没有信息, 只有对局后的 gamma;
%% 但退出仍然会被惩罚, 论文第 9 节: 0 秒就退出的也要扣分
no_time_test() ->
    Params = #{counts => #{kill => {1.0, -1.0, 1.0}}},
    Teams = [[#{player => p(), time_played => 0, quit => true, counts => #{kill => 0}}], [#{player => p(), time_played => 0}]],
    [[A], [B]] = trueskill2:rate(with_teams(Teams), Params),
    ?assertEqual({3.0, math:sqrt(1.6 + 1.0e-6)}, trueskill2:skill(B, slayer, Params)),
    ?assert(mu(A, Params) < 3.0).

%% 第 12 节: bot 不随时间和对局变化方差, 但技能仍然会被学习
bot_test() ->
    Params = #{tau => 1.0},
    Bot = trueskill2:new_bot(),
    [[Human], [Bot1]] = trueskill2:rate(match([[p()], [Bot]]), Params),
    {Mu1, Sigma1} = trueskill2:skill(Bot1, slayer, Params),
    ?assert(Mu1 < 3.0),
    ?assertEqual({Mu1, Sigma1}, trueskill2:skill(Bot1, slayer, Params, 1.0e6)),
    [_, [Bot2]] = trueskill2:rate((match([[Human], [Bot1]]))#{time => 100}, Params),
    #ts2_player{bot = true, base_time = undefined, modes = #{slayer := #ts2_mode{experience = 0, time = undefined}}} = Bot2.

iterations_test() ->
    Teams = [[p()], [p()], [p()]],
    ?assertNotEqual(trueskill2:rate(match(Teams), #{max_iterations => 1}), trueskill2:rate(match(Teams), #{})).
