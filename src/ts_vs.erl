-module(ts_vs).
-author("yimo").

-include("ts.hrl").
-export([quality/1]).


-spec quality([[#ts_player{}|_]|_]) -> number().
quality(_Groups) ->
    ok.


