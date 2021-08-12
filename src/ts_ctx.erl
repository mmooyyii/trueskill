%%%-------------------------------------------------------------------
%%% @author yimo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 8月 2021 5:10 下午
%%%-------------------------------------------------------------------
-module(ts_ctx).
-author("yimo").

-include("ts.hrl").
%% API
-export([new_context/0, get_instance/1, put_instance/1, set_instance/2, clear/0, get_serial/0]).
-define(CTX, ts_context).

new_context() ->
    get(?CTX) == undefined andalso put(ts_context, #context{pointer = 0, pointer_to_instance = #{}, ref = 0}).

put_instance(Ele) ->
    new_context(),
    C = #context{pointer = Ptr, pointer_to_instance = P2I} = get(?CTX),
    put(?CTX, C#context{pointer = Ptr + 1, pointer_to_instance = P2I#{Ptr => Ele}}),
    Ptr.
get_instance(Ptr) ->
    new_context(),
    #context{pointer_to_instance = P2I} = get(?CTX),
    maps:get(Ptr, P2I).

set_instance(Ptr, Ele) ->
    new_context(),
    C = #context{pointer = P, pointer_to_instance = P2I} = get(?CTX),
    put(?CTX, C#context{pointer = P, pointer_to_instance = P2I#{Ptr => Ele}}).


get_serial() ->
    new_context(),
    C = #context{ref = Ref} = get(?CTX),
    put(?CTX, C#context{ref = Ref + 1}),
    Ref.

clear() ->
    erase(?CTX).