-module(ts_player_test).
-author("yimo").

-include_lib("eunit/include/eunit.hrl").


vs_test() ->
    true = abs(0.44721 - ts_player:vs([[ts_player:new()], [ts_player:new()]])) < 0.001.