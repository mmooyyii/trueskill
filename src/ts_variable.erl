%%%-------------------------------------------------------------------
%%% @author yimo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 7月 2021 9:16 下午
%%%-------------------------------------------------------------------
-module(ts_variable).
-author("yimo").

%% API
-export([]).
-include("ts.hrl").

-record(gaussian, {
    pi :: number(),
    tau :: number()
}).
-record(variable, {
    value,
    factors
}).

new_variable() ->
    Pi = math:pow(?Sigma, -2)
    #variable{
        value = new_gaussian(0, 0),
        factors = #{}
    }.

attach_factor(V = #variable{factors = Factors}, Factor) ->
    V#variable{factors = Factors#{Factor => new_gaussian(0, 0)}}.

update_message(V = #variable{factors = Factors, value = Value}, Factor, Message) ->
    OldMessage = maps:get(Factor, Factors),
    V#variable{value = gaussian_mul(gaussian_div(Value, OldMessage), Message), factors = Factors#{Factor => Message}}.

get_message(#variable{factors = Factors}, Factor) ->
    maps:get(Factor, Factors).


new_gaussian(Pi, Tau) ->
    #gaussian{pi = Pi, tau = Tau}.

gaussian_mul(#gaussian{pi = P1, tau = T1}, #gaussian{pi = P2, tau = T2}) ->
    #gaussian{pi = P1 + P2, tau = T1 + T2}.

gaussian_div(#gaussian{pi = P1, tau = T1}, #gaussian{pi = P2, tau = T2}) ->
    #gaussian{pi = P1 - P2, tau = T1 - T2}.

gaussian_mu(#gaussian{pi = P, tau = T}) ->
    case abs(P) < 0.0001 of true -> 0;false -> T / P end.

gaussian_sigma(#gaussian{pi = P, tau = T}) ->
    case abs(P) < 0.0001 of true -> ?Inf;false -> math:pow(1 / P, 0.5) end.