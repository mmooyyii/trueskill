-module(ts_layers).
-author("yimo").

%% API
-export([factor_graph_builders/4, run_schedule/7]).
-include("ts.hrl").



factor_graph_builders(Ctx, Groups, Ranks, Weights) ->
    GroupSize = length(Groups),
    FlattenGroups = lists:flatten(Groups),
    FlattenWeights = lists:flatten(Weights),
    Size = length(FlattenGroups),
    RatingVars = lists:duplicate(Size, ts_model:new_variable()),
    PerfVars = lists:duplicate(Size, ts_model:new_variable()),
    TeamPerfVars = lists:duplicate(GroupSize, ts_model:new_variable()),
    TeamDiffVars = lists:duplicate(GroupSize - 1, ts_model:new_variable()),
    TeamSize = ts_utils:prefix_sum([length(G) || G <- Groups]),
    {NewCtx, [RatingVarsIdx, PerfVarsIdx, TeamPerfVarsIdx, TeamDiffVarsIdx]} =
        p_put_to_ctx(Ctx, [RatingVars, PerfVars, TeamPerfVars, TeamDiffVars]),
    {
        NewCtx,
        [
            build_rating_layer(RatingVarsIdx, FlattenGroups),
            build_perf_layer(RatingVarsIdx, PerfVarsIdx),
            build_team_perf_layer(PerfVarsIdx, TeamPerfVarsIdx, TeamSize, FlattenWeights),
            build_team_diff_layer(TeamPerfVarsIdx, TeamDiffVarsIdx),
            build_trunc_layer(TeamDiffVarsIdx, Groups, Ranks)
        ]
    }.

p_put_to_ctx(Ctx, Vars) ->
    p_put_to_ctx(Ctx, Vars, []).
p_put_to_ctx(Ctx, [], Acc) ->
    {Ctx, lists:reverse(Acc)};
p_put_to_ctx(Ctx, [Vars | Rest], Acc) ->
    F = fun(Var, {C, Indexes}) -> {Idx, NC} = ts_model:put_instance(C, Var), {NC, [Idx | Indexes]} end,
    {NewCtx, Idxes} = lists:foldr(F, {Ctx, []}, Vars),
    p_put_to_ctx(NewCtx, Rest, [Idxes | Acc]).

run_schedule(Ctx, RatingLayer, PerfLayer, TermPerfLayer, TeamDiffLayer, TruncLayer, MinDelta) when MinDelta > 0 ->
    RatingLayer.
%%    ts_model:down(hd(RatingLayer)).

build_rating_layer(RatingVars, FlattenRatings) ->
    [ts_model:prior(RatingVar, Rating, ?Tau) || {RatingVar, Rating} <- lists:zip(RatingVars, FlattenRatings)].

build_perf_layer(RatingVars, PerfVars) ->
    [ts_model:likelihood(RatingVar, PerfVar, ?Beta * ?Beta) || {RatingVar, PerfVar} <- lists:zip(RatingVars, PerfVars)].

build_team_perf_layer(PerfVars, TeamPerfVars, TeamSize, FlattenWeights) ->
    F = fun({Team, PerfVar}) ->
        Start = case Team > 1 of true -> lists:nth(Team - 1, TeamSize);false -> 0 end,
        End = lists:nth(Team, TeamSize),
        ChildPerfVars = lists:sublist(PerfVars, Start + 1, End + 1),
        Coeffs = lists:sublist(FlattenWeights, Start + 1, End + 1),
        ts_model:ts_sum(PerfVar, ChildPerfVars, Coeffs)
        end,
    lists:map(F, ts_utils:enum(TeamPerfVars)).

build_team_diff_layer(TeamPerfVars, TeamDiffVars) ->
    [ts_model:ts_sum(TeamDiffVar, lists:sublist(TeamPerfVars, Team, Team + 2), [1, -1]) || {Team, TeamDiffVar}
        <- ts_utils:enum(TeamDiffVars)].


build_trunc_layer(TeamDiffVars, RatingGroups, Ranks) ->
    F = fun({X, TeamDiffVar}) ->
        DrawProbability = ?DRAW_PROBABILITY,
        Size = length(lists:flatten(lists:sublist(RatingGroups, X, X + 1))),
        DrawMargin = calc_draw_margin(DrawProbability, Size),
        {Vfunc, Wfunc} = case lists:sublist(Ranks, X, X + 1) of
                             [Same, Same] ->
                                 {fun v_draw/2, fun w_draw/2};
                             _ ->
                                 {fun v_win/2, fun w_win/2}
                         end,
        ts_model:truncate(TeamDiffVar, Vfunc, Wfunc, DrawMargin)
        end,
    lists:map(F, ts_utils:enum(TeamDiffVars)).

calc_draw_margin(DrawProbability, Size) ->
    ts_distributions:ppf((DrawProbability + 1) / 2.0, 0, 1) * math:sqrt(Size) * ?Beta.

v_draw(Diff, DrawMargin) ->
    AbsDiff = abs(Diff),
    A = DrawMargin - AbsDiff,
    B = -DrawMargin - AbsDiff,
    Denom = ts_distributions:cdf(A, 0, 1) - ts_distributions:cdf(B, 0, 1),
    Numer = ts_distributions:pdf(B, 0, 1) - ts_distributions:pdf(A, 0, 1),
    Symbol = case Diff < 0 of true -> -1;false -> 1 end,
    case Denom of
        0.0 -> A * Symbol;
        _ -> (Numer / Denom) * Symbol
    end.

w_draw(Diff, DrawMargin) ->
    AbsDiff = abs(Diff),
    A = DrawMargin - AbsDiff,
    B = -DrawMargin - AbsDiff,
    Denom = ts_distributions:cdf(A, 0, 1) - ts_distributions:cdf(B, 0, 1),
    true = (Denom =/= 0.0),
    V = v_draw(AbsDiff, DrawMargin),
    (V * V) + (A + ts_distributions:pdf(A, 0, 1) - B * ts_distributions:pdf(B, 0, 1)) / Denom.

v_win(Diff, DrawMargin) ->
    X = Diff - DrawMargin,
    Demon = ts_distributions:cdf(X, 0, 1),
    case Demon of
        0.0 ->
            ts_distributions:pdf(X, 0, 1) / Demon;
        _ ->
            -X
    end.

w_win(Diff, DrawMargin) ->
    X = Diff - DrawMargin,
    V = v_win(Diff, DrawMargin),
    W = V * (V + X),
    true = (0 < W andalso W < 1),
    W.
