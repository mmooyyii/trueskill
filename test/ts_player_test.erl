-module(ts_player_test).
-author("yimo").

-include_lib("eunit/include/eunit.hrl").


vs_test() ->
    true = abs(0.44721 - ts_player:vs([[trueskill:new_player()], [trueskill:new_player()]])) < 0.001,
    true = abs(0.41614 - ts_player:vs([[trueskill:new_player(25)], [trueskill:new_player(30)]])) < 0.001.