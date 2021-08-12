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



-export([new_variable/0]).
-export([prior/3, likelihood/3, ts_sum/3, truncate/4]).

-export([rating_layer_to_player/1]).
-export([up/1, up/2, down/1, delta/2]).

-include("ts.hrl").

new_variable() ->
    #{
        type => variable,
        pi => 0,
        tau => 0,
        message => #{}
    }.

new_variable(Pi, Tau) ->
    #{
        type => variable,
        pi => Pi,
        tau => Tau,
        message => #{}
    }.

attach_factor(V = #{type := variable, message := Message}, Factor) ->
    V#{message => Message#{Factor => new_variable()}}.

get_factor(#{type := variable, message := Message}, Factor) ->
    maps:get(Factor, Message).

delta(#{pi := Pi1, tau := Tau1}, #{pi := Pi2, tau := Tau2}) ->
    PiDelta = abs(Pi1 - Pi2),
    case PiDelta >= ?Inf of
        true -> 0;
        false -> max(abs(Tau1 - Tau2), math:sqrt(PiDelta))
    end.

update_value(#{type := variable, message := Message, pi := Pi1, tau := Tau1}, Factor, #{pi := Pi2, tau := Tau2}) ->
    #{pi := Pi3, tau := Tau3} = maps:get(Factor, Message),
    New = new_variable(Pi2 + Pi3 - Pi1, Tau2 + Tau3 - Tau1),
    #{type => variable, message => Message#{Factor => New}, pi => Pi2, tau => Tau2}.

update_message(#{type := variable, message := Message, pi := Pi1, tau := Tau1}, Factor, Msg = #{pi := Pi2, tau := Tau2}) ->
    #{pi := Pi3, tau := Tau3} = maps:get(Factor, Message),
    #{type => variable, message => Message#{Factor => Msg}, pi => Pi1 - Pi3 + Pi2, tau => Tau1 - Tau3 + Tau2}.



variable_mul(#{pi := Pi1, tau := Tau1}, #{pi := Pi2, tau := Tau2}) ->
    new_variable(Pi1 + Pi2, Tau1 + Tau2).

variable_div(#{pi := Pi1, tau := Tau1}, #{pi := Pi2, tau := Tau2}) ->
    new_variable(Pi1 - Pi2, Tau1 - Tau2).

variable_get_mu(#{type := variable, pi := Pi, tau := Tau}) ->
    case abs(Pi) < 0.0001 of true -> 0;false -> Tau / Pi end.

variable_get_sigma(#{type := variable, pi := Pi}) ->
    case abs(Pi) < 0.0001 of true -> ?Inf;false -> math:pow(1 / Pi, 0.5) end.


prior(Var, Player, Dynamic) ->
    Ref = ts_ctx:get_serial(),
    ts_ctx:set_instance(Var, attach_factor(ts_ctx:get_instance(Var), Ref)),
    #{
        type => prior,
        ref => Ref,
        vars => [Var],
        player => Player,
        dynamic => Dynamic
    }.

likelihood(MeanVar, ValueVar, Variance) ->
    Ref = ts_ctx:get_serial(),
    ts_ctx:set_instance(MeanVar, attach_factor(ts_ctx:get_instance(MeanVar), Ref)),
    ts_ctx:set_instance(ValueVar, attach_factor(ts_ctx:get_instance(ValueVar), Ref)),
    #{
        type => likelihood,
        ref => Ref,
        vars => [MeanVar, ValueVar],
        mean => MeanVar,
        value => ValueVar,
        variance => Variance
    }.

ts_sum(SumVar, TermVars, Coeffs) ->
    Ref = ts_ctx:get_serial(),
    ts_ctx:set_instance(SumVar, attach_factor(ts_ctx:get_instance(SumVar), Ref)),
    lists:foreach(fun(TermVar) ->
        ts_ctx:set_instance(TermVar, attach_factor(ts_ctx:get_instance(TermVar), Ref)) end,
        TermVars),
    #{
        type => ts_sum,
        ref => Ref,
        vars => [SumVar | TermVars],
        sum => SumVar,
        terms => TermVars,
        coeffs => Coeffs
    }.

truncate(Var, Vfunc, Wfunc, DrawMargin) ->
    Ref = ts_ctx:get_serial(),
    ts_ctx:set_instance(Var, attach_factor(ts_ctx:get_instance(Var), Ref)),
    #{
        type => truncate,
        ref => Ref,
        vars => [Var],
        v_func => Vfunc,
        w_func => Wfunc,
        draw_margin => DrawMargin
    }.

rating_layer_to_player(#{type := prior, vars := [Ptr]}) ->
    Variable = ts_ctx:get_instance(Ptr),
    trueskill:new_player(variable_get_mu(Variable), variable_get_sigma(Variable)).

up(#{type := truncate, vars := [Var], v_func := VFunc, w_func := WFunc, draw_margin := Dm, ref := Ref}) ->
    Val = ts_ctx:get_instance(Var),
    Msg = get_factor(Val, Ref),
    #{pi := Pi, tau := Tau} = variable_div(Val, Msg),
    SqrtPi = math:sqrt(Pi),
    Diff = Tau / SqrtPi,
    DrawMargin = Dm * SqrtPi,
    V = VFunc(Diff, DrawMargin),
    W = WFunc(Diff, DrawMargin),
    ts_ctx:set_instance(Var, update_value(Val, Ref, new_variable(Pi / (1 - W), (Tau + SqrtPi * V) / (1 - W)))),
    delta(Val, new_variable(Pi / (1 - W), (Tau + SqrtPi * V) / (1 - W)));

up(#{type := likelihood, mean := MeanVar, value := ValueVar, variance := Variance, ref := Ref}) ->
    Value = ts_ctx:get_instance(ValueVar),
    #{pi := Pi, tau := Tau} = variable_div(Value, get_factor(Value, Ref)),
    A = 1.0 / (1.0 + Variance * Pi),
    ts_ctx:set_instance(MeanVar, update_message(ts_ctx:get_instance(MeanVar), Ref, new_variable(A * Pi, A * Tau))),
    delta(ts_ctx:get_instance(MeanVar), new_variable(A * Pi, A * Tau));

up(Self = #{type := ts_sum}) ->
    up(Self, 1).

up(#{type := ts_sum, terms := Terms, coeffs := Coeffs, ref := Ref}, Idx) ->
    Coeff = lists:nth(Idx, Coeffs),
    NewCoeffs = lists:foldr(fun({I, C}, Acc) ->
        case I == Idx of
            true -> [1 | Acc];
            false -> [-C / Coeff | Acc]
        end end, [], ts_utils:enum(Coeffs)),
    Vals = [ts_ctx:get_instance(T) || T <- Terms],
    Msgs = [get_factor(Val, Ref) || Val <- Vals],
    io:format("~p~n", [{Ref, lists:nth(Idx, Terms), Vals, Msgs, NewCoeffs}]),
    ts_sum_update(Ref, lists:nth(Idx, Terms), Vals, Msgs, NewCoeffs).

down(#{type := prior, vars := [Var], player := #ts_player{mu = Mu, sigma = Sigma}, dynamic := Dynamic, ref := Ref}) ->
    DSigma = math:sqrt(Sigma * Sigma + Dynamic * Dynamic),
    Value = new_variable(math:pow(DSigma, -2), math:pow(DSigma, -2) * Mu),
    VarInstance = ts_ctx:get_instance(Var),
    ts_ctx:set_instance(Var, update_value(VarInstance, Ref, Value));

down(#{type := likelihood, mean := MeanVar, value := ValueVar, variance := Variance, ref := Ref}) ->
    MeanInstance = #{pi := Pi1, tau := Tau1} = ts_ctx:get_instance(MeanVar),
    #{pi := Pi2, tau := Tau2} = get_factor(MeanInstance, Ref),
    A = 1.0 / (1.0 + Variance * (Pi1 - Pi2)),
    Value = new_variable(A * (Pi1 - Pi2), A * Tau1 - Tau2),
    ts_ctx:set_instance(ValueVar, update_message(ts_ctx:get_instance(ValueVar), Ref, Value));

down(#{type := ts_sum, terms := Terms, sum := Sum, coeffs := Coeffs, ref := Ref}) ->
    Vals = [ts_ctx:get_instance(T) || T <- Terms],
    Msg = [get_factor(Val, Ref) || Val <- Vals],
    ts_sum_update(Ref, Sum, [ts_ctx:get_instance(T) || T <- Terms], Msg, Coeffs).

ts_sum_update(Ref, Var, Vals, Msgs, Coeffs) ->
    F = fun({Val, Msg, Coeff}, {P, M}) ->
        V = #{pi := Pi} = variable_div(Val, Msg),
        case abs(Pi) < 0.0000001 of
            true -> {?Inf, M + Coeff * variable_get_mu(V)};
            false -> {P + Coeff * Coeff / Pi, M + Coeff * variable_get_mu(V)}
        end end,
    {PiInv, Mu} = lists:foldl(F, {0, 0}, lists:zip3(Vals, Msgs, Coeffs)),
    ts_ctx:set_instance(Var, update_message(ts_ctx:get_instance(Var), Ref, new_variable(1 / PiInv, 1 / PiInv * Mu))),
    delta(ts_ctx:get_instance(Var), new_variable(1 / PiInv, 1 / PiInv * Mu)).
