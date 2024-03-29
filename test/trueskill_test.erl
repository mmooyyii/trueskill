-module(trueskill_test).
-author("yimo").

-include("ts.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(ts_assert, [assert_player_equal/2]).

vs_test() ->
    true = abs(0.44721 - trueskill:vs([[trueskill:new_player()], [trueskill:new_player()]])) < 0.001,
    true = abs(0.41614 - trueskill:vs([[trueskill:new_player(25)], [trueskill:new_player(30)]])) < 0.001,
    true = abs(0.14995 - trueskill:vs([[trueskill:new_player(25)], [trueskill:new_player(30)], [trueskill:new_player(35)]])) < 0.001.


adjust1_test() ->
    [[T1], [T2]] = trueskill:adjust([[trueskill:new_player()], [trueskill:new_player()]], [1, 2]),
    assert_player_equal(T1, #ts_player{mu = 29.3958, sigma = 7.1715, pi = 0.0194, tau = 0.5717}),
    assert_player_equal(T2, #ts_player{mu = 20.6042, sigma = 7.1715, pi = 0.0194, tau = 0.4006}).

adjust2_test() ->
    [[P1, P2], [P3]] = trueskill:adjust([[trueskill:new_player(10), trueskill:new_player(20)], [trueskill:new_player(15)]]),
    assert_player_equal(P1, #ts_player{mu = 11.4498, sigma = 7.8677, pi = 0.0162, tau = 0.1850}),
    assert_player_equal(P2, #ts_player{mu = 21.4498, sigma = 7.8677, pi = 0.0162, tau = 0.3465}),
    assert_player_equal(P3, #ts_player{mu = 13.5501, sigma = 7.8677, pi = 0.0162, tau = 0.2189}).

adjust3_test() ->
    trueskill:adjust(
        [
            [trueskill:new_player()],
            [trueskill:new_player()],
            [trueskill:new_player()]
        ]).

adjust4_test() ->
    trueskill:adjust(
        [
            [trueskill:new_player()],
            [trueskill:new_player()]
        ], [1, 2]).