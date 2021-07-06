-module(ts_layers).
-author("yimo").

%% API
-export([]).
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

-record(sum, {
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
    FlattenGroups = lists:flatten(Groups),
    FlattenWeights = lists:flatten(Weights),
    Size = length(Groups).
%%    {BuildRatingLayer, BuildPerfLayer, BuildTeamPerfLayer, BuildTeamDiffLayer, BuildTruncLayer}.


build_rating_layer(RatingVars, FlattenRatings) ->
    [prior(RatingVar, Rating, ?Tau) || {RatingVar, Rating} <- lists:zip(RatingVars, FlattenRatings)].
build_perf_layer(RatingVars, PerfVars) ->
    [likelihood(RatingVar, PerfVar, ?Beta * ?Beta) || {RatingVar, PerfVar} <- lists:zip(RatingVars, PerfVars)].
build_team_perf_layer(TeamPerfVars, TeamSizes, PerfVars, FlattenWeights) ->
    F = fun({Team, PerfVar}) ->
        Start = case Team > 1 of true -> lists:nth(Team - 1, TeamSizes);false -> 0 end,
        End = lists:nth(Team, TeamSizes),
        ChildPerfVars = lists:sublist(PerfVars, Start, End),
        Coeffs = lists:sublist(FlattenWeights, Start, End),
        ts_sum(PerfVar, ChildPerfVars, Coeffs)
        end,
    lists:map(F, ts_utils:enum(TeamPerfVars)).

build_team_diff_layer(TeamDiffVars) ->
    [ts_sum(TeamDiffVar, lists:sublist(TeamDiffVars, Team, Team + 2), [1, -1]) || {Team, TeamDiffVar}
        <- ts_utils:enum(TeamDiffVars)].
%%
%%build_trunc_layer(TeamDiffVars, RatingGroups, Ranks) ->
%%    F = fun({X, TeamDiffVar}) ->
%%        DrawProbability = ?DRAW_PROBABILITY, % TODO: dynamic draw probability
%%        Size = length(lists:flatten(RatingGroups)),
%%        DrawMargin = calc_draw_margin(DrawProbability, Size),
%%        {Vfunc, Wfunc} = case lists:sublist(Ranks, X, X + 1) of
%%                             [A, A] ->
%%                                 ok;
%%                             _ ->
%%                                 ok
%%                         end,
%%        truncate(TeamDiffVar, Vfunc, Wfunc, DrawMargin)
%%        end,
%%    lists:map(F, ts_utils:enum(TeamDiffVars)).


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
    #sum{
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
    ok.

v_win() ->
    ok.