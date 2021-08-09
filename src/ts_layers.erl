-module(ts_layers).
-author("yimo").

%% API
-export([factor_graph_builders/3]).
-include("ts.hrl").

-record(prior, {
    vars :: [number()|_],
    val :: number(),
    dynamic :: number()
}).

-record(likelihood, {
    vars :: [number()|_],
    mean :: number(),
    value :: number(),
    variance :: number()
}).

-record(ts_sum, {
    vars :: [],
    sum :: number(),
    terms :: number(),
    coeffs :: number()
}).

-record(truncate, {
    vars :: [],
    v_func :: number(),
    w_func :: number(),
    draw_margin :: number()
}).

factor_graph_builders(Groups, Ranks, Weights) ->
    GroupSize = length(Groups),
    FlattenGroups = lists:flatten(Groups),
    FlattenWeights = lists:flatten(Weights),
    Size = length(FlattenGroups),
    RatingVars = lists:duplicate(Size, ts_variable:new_variable()),
    PerfVars = lists:duplicate(Size, ts_variable:new_variable()),
    TeamPerfVars = lists:duplicate(GroupSize, ts_variable:new_variable()),
    TeamDiffVars = lists:duplicate(GroupSize - 1, ts_variable:new_variable()),
    TeamSize = ts_utils:prefix_sum([length(G) || G <- Groups]),
    {
        build_rating_layer(RatingVars, FlattenGroups),
        build_perf_layer(RatingVars, PerfVars),
        build_team_perf_layer(TeamPerfVars, TeamSize, PerfVars, FlattenWeights),
        build_team_diff_layer(TeamDiffVars),
        build_trunc_layer(TeamDiffVars, Groups, Ranks)
    }.

build_rating_layer(RatingVars, FlattenRatings) ->
    [prior(RatingVar, Rating, ?Tau) || {RatingVar, Rating} <- lists:zip(RatingVars, FlattenRatings)].

build_perf_layer(RatingVars, PerfVars) ->
    [likelihood(RatingVar, PerfVar, ?Beta * ?Beta) || {RatingVar, PerfVar} <- lists:zip(RatingVars, PerfVars)].

build_team_perf_layer(TeamPerfVars, TeamSizes, PerfVars, FlattenWeights) ->
    F = fun({Team, PerfVar}) ->
        Start = case Team > 1 of true -> lists:nth(Team - 1, TeamSizes);false -> 0 end,
        End = lists:nth(Team, TeamSizes),
        ChildPerfVars = lists:sublist(PerfVars, Start + 1, End + 1),
        Coeffs = lists:sublist(FlattenWeights, Start + 1, End + 1),
        ts_sum(PerfVar, ChildPerfVars, Coeffs)
        end,
    lists:map(F, ts_utils:enum(TeamPerfVars)).

build_team_diff_layer(TeamDiffVars) ->
    [ts_sum(TeamDiffVar, lists:sublist(TeamDiffVars, Team, Team + 2), [1, -1]) || {Team, TeamDiffVar}
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
        truncate(TeamDiffVar, Vfunc, Wfunc, DrawMargin)
        end,
    lists:map(F, ts_utils:enum(TeamDiffVars)).

calc_draw_margin(DrawProbability, Size) ->
    ts_distributions:ppf((DrawProbability + 1) / 2.0, 0, 1) * math:sqrt(Size) * ?Beta.

prior(Var, Val, Dynamiy) ->
    #prior{
        vars = [Var],
        val = Val,
        dynamic = Dynamiy
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
    #truncate{
        vars = [Var],
        v_func = Vfunc,
        w_func = Wfunc,
        draw_margin = DrawMargin
    }.

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
