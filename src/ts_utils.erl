-module(ts_utils).
-author("yimo").

%% API
-export([enum/1]).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).