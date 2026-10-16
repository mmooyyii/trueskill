-module(trueskill_tests).
-author("yimo").

-include("trueskill.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_util.hrl").

%% 期望值来自 python trueskill 0.4.5 (backend=None)

assert_teams(Expect, Teams) ->
    lists:foreach(fun({{Mu, Sigma}, #ts_player{mu = M, sigma = S}}) ->
        ?assertNear(Mu, M, 1.0e-3),
        ?assertNear(Sigma, S, 1.0e-3)
                  end, lists:zip(lists:append(Expect), lists:append(Teams))).

p() -> trueskill:new_player().

new_player_test() ->
    ?assertEqual(#ts_player{mu = 25.0, sigma = 25 / 3}, trueskill:new_player()),
    ?assertEqual(#ts_player{mu = 30.0, sigma = 25 / 3}, trueskill:new_player(30)),
    ?assertEqual(#ts_player{mu = 30.0, sigma = 2.0}, trueskill:new_player(30, 2)).

vs_test() ->
    ?assertNear(0.4472135955, trueskill:vs([[p()], [p()]]), 1.0e-6),
    ?assertNear(0.4161460676, trueskill:vs([[trueskill:new_player(25)], [trueskill:new_player(30)]]), 1.0e-6),
    ?assertNear(0.1499523184, trueskill:vs([[trueskill:new_player(25)], [trueskill:new_player(30)], [trueskill:new_player(35)]]), 1.0e-6),
    ?assertNear(0.4619344188, trueskill:vs([[trueskill:new_player(25, 8), trueskill:new_player(25, 8)],
        [trueskill:new_player(25, 8), trueskill:new_player(25, 8)]]), 1.0e-6),
    ?assertNear(0.2997762379, trueskill:vs([[p()], [p(), p()]], [[1], [1, 0.5]]), 1.0e-6).

one_vs_one_test() ->
    assert_teams([[{29.396, 7.171}], [{20.604, 7.171}]], trueskill:adjust([[p()], [p()]], [1, 2])).

two_vs_one_test() ->
    assert_teams([[{11.450, 7.868}, {21.450, 7.868}], [{13.550, 7.868}]],
        trueskill:adjust([[trueskill:new_player(10), trueskill:new_player(20)], [trueskill:new_player(15)]])).

free_for_all_test() ->
    assert_teams([[{31.675, 6.656}], [{25.000, 6.208}], [{18.325, 6.656}]], trueskill:adjust([[p()], [p()], [p()]])).

three_teams_test() ->
    assert_teams([[{33.500, 6.592}], [{27.710, 6.327}], [{13.790, 7.098}, {13.790, 7.098}]],
        trueskill:adjust([[p()], [p()], [p(), p()]])).

draw_test() ->
    assert_teams([[{25.000, 6.458}], [{25.000, 6.458}]], trueskill:adjust([[p()], [p()]], [1, 1])),
    assert_teams([[{30.109, 6.735}], [{22.443, 5.972}], [{22.448, 5.974}]], trueskill:adjust([[p()], [p()], [p()]], [1, 2, 2])).

unsorted_rank_test() ->
    assert_teams([[{20.604, 7.171}], [{29.396, 7.171}]], trueskill:adjust([[p()], [p()]], [2, 1])).

weights_test() ->
    assert_teams([[{32.417, 7.056}], [{17.583, 7.056}, {21.291, 8.033}]],
        trueskill:adjust([[p()], [p(), p()]], [1, 2], [[1], [1, 0.5]])).

%% 实力过于悬殊时截断因子算不出来, 技能保持不变 (只多了 tau)
extreme_test() ->
    [[#ts_player{mu = M}], _] = trueskill:adjust([[trueskill:new_player(0, 1)], [trueskill:new_player(1000, 1)]]),
    ?assertNear(0.0, M, 1.0e-9).

bad_args_test() ->
    ?assertError(function_clause, trueskill:adjust([[p()]])),
    ?assertError(function_clause, trueskill:adjust([[p()], [p()]], [1])),
    ?assertError(function_clause, trueskill:vs([[p()]])).
