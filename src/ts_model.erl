%%%-------------------------------------------------------------------
%%% @author yimo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 7月 2021 9:16 下午
%%%-------------------------------------------------------------------
-module(ts_model).
-author("yimo").


%% API
-export([new_context/0, get_instance/2, put_instance/2, set_instance/3]).

-export([new_variable/0, new_gaussian/2]).
-export([prior/3, likelihood/3, ts_sum/3, truncate/4]).
-export([down/1]).

-export([rating_layer_to_player/2]).
-include("ts.hrl").




new_context() ->
    #context{pointer = 0, pointer_to_instance = #{}}.

put_instance(#context{pointer_to_instance = P2I, pointer = Serial}, Ele) ->
    {Serial, #context{pointer = Serial + 1, pointer_to_instance = P2I#{Serial => Ele}}}.

get_instance(#context{pointer_to_instance = P2I}, Ptr) ->
    maps:get(Ptr, P2I).

set_instance(#context{pointer_to_instance = P2I}, Ptr, Ele) ->
    #context{pointer = P2I#{Ptr => Ele}}.

new_variable() ->
    Pi = math:pow(?Sigma, -2),
    #variable{
        gaussian = new_gaussian(0, 0),
        value = new_gaussian(0, 0),
        factors = #{}
    }.

attach_factor(V = #variable{factors = Factors}, Factor) ->
    V#variable{factors = Factors#{Factor => new_gaussian(0, 0)}}.

update_message(V = #variable{factors = Factors, value = Value}, Factor, Message) ->
    OldMessage = maps:get(Factor, Factors),
    V#variable{
        value = gaussian_mul(gaussian_div(Value, OldMessage), Message),
        factors = Factors#{Factor => Message}
    }.

get_message(#variable{factors = Factors}, Factor) ->
    maps:get(Factor, Factors).

update_value(Var = #variable{factors = Factors, value = V}, Factor, Value) ->
    OldMessage = maps:get(Factor, Factors),
    Message = gaussian_div(gaussian_mul(Value, OldMessage), V),
    Var#variable{value = Value, factors = Factors#{Factor => Message}}.

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

prior(Var, Val, Dynamic) ->
    #prior{
        vars = [Var],
        val = Val,
        dynamic = Dynamic
    }.

likelihood(MeanVar, ValueVar, Variance) ->
    #likelihood{
        vars = [MeanVar, ValueVar],
        mean = MeanVar,
        value = ValueVar,
        variance = Variance
    }.

ts_sum(SumVar, TermVars, Coeffs) ->
    #ts_sum{
        vars = [SumVar | TermVars],
        sum = SumVar,
        terms = TermVars,
        coeffs = Coeffs
    }.

truncate(Var, Vfunc, Wfunc, DrawMargin) ->
    #{
        type => truncate,
        parent => [],
        vars => [Var],
        v_func => Vfunc,
        w_func => Wfunc,
        draw_margin => DrawMargin
    }.

down(P = #prior{vars = [Var], val = #ts_player{sigma = S, mu = Mu}, dynamic = D}) ->
    Sigma = math:sqrt(S * S + D * D),
    Value = new_gaussian(Mu, Sigma),
    NewVar = update_value(Var, P, Value),
    P#prior{vars = [NewVar]};

down(#likelihood{}) ->
    ok;
down(#ts_sum{}) ->
    ok.


rating_layer_to_player(Ctx, #prior{vars = [Ptr]}) ->
    #variable{gaussian = Gaussian} = get_instance(Ctx, Ptr),
    trueskill:new_player(gaussian_mu(Gaussian), gaussian_sigma(Gaussian)).
