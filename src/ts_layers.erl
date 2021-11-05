-module(ts_layers).
-author("yimo").

%% API
-export([factor_graph_builders/3, run_schedule/6]).
-include("ts.hrl").



factor_graph_builders(Groups, Ranks, Weights) ->
    GroupSize = length(Groups),
    FlattenGroups = lists:flatten(Groups),
    FlattenWeights = lists:flatten(Weights),
    Size = length(FlattenGroups),
    RatingVars = [ts_ctx:put_instance(ts_model:new_variable()) || _ <- lists:seq(1, Size)],
    PerfVars = [ts_ctx:put_instance(ts_model:new_variable()) || _ <- lists:seq(1, Size)],
    TeamPerfVars = [ts_ctx:put_instance(ts_model:new_variable()) || _ <- lists:seq(1, GroupSize)],
    TeamDiffVars = [ts_ctx:put_instance(ts_model:new_variable()) || _ <- lists:seq(1, GroupSize - 1)],
    TeamSize = ts_utils:prefix_sum([length(G) || G <- Groups]),
    {
        build_rating_layer(RatingVars, FlattenGroups),
        build_perf_layer(RatingVars, PerfVars),
        build_team_perf_layer(PerfVars, TeamPerfVars, TeamSize, FlattenWeights),
        build_team_diff_layer(TeamPerfVars, TeamDiffVars),
        build_trunc_layer(TeamDiffVars, Groups, Ranks)
    }.

run_schedule(RatingLayer, PerfLayer, TermPerfLayer, TeamDiffLayer, TruncLayer, MinDelta) when MinDelta > 0 ->
    lists:foreach(fun(Layer) -> ts_model:down(Layer) end, RatingLayer),
    lists:foreach(fun(Layer) -> ts_model:down(Layer) end, PerfLayer),
    lists:foreach(fun(Layer) -> ts_model:down(Layer) end, TermPerfLayer),
    p_run_team_diff_layer_trunc_layer(TeamDiffLayer, TruncLayer, 10, MinDelta),
    ts_model:up(hd(TeamDiffLayer), 1),
    ts_model:up(lists:last(TeamDiffLayer), 2),
    lists:foreach(fun(F = #{vars := Vars}) ->
        [ts_model:up(F, X) || X <- lists:seq(1, length(Vars) - 1)] end, TermPerfLayer),
    lists:foreach(fun(Layer) -> ts_model:up(Layer) end, PerfLayer),
    RatingLayer.

p_run_team_diff_layer_trunc_layer(_, _, 0, _) ->
    ok;
p_run_team_diff_layer_trunc_layer([TeamDiffLayer], TruncLayer, Loop, MinDelta) ->
    ts_model:down(TeamDiffLayer),
    Delta = ts_model:up(hd(TruncLayer)),
    case Delta =< MinDelta of
        true ->
            p_run_team_diff_layer_trunc_layer([TeamDiffLayer], TruncLayer, 0, MinDelta);
        false ->
            p_run_team_diff_layer_trunc_layer([TeamDiffLayer], TruncLayer, Loop - 1, MinDelta)
    end;

p_run_team_diff_layer_trunc_layer(TeamDiffLayer, TruncLayer, Loop, MinDelta) ->
    Tmp = lists:zip(TeamDiffLayer, TruncLayer),
    Loop1 = fun({Team, Trunc}, D) ->
        ts_model:down(Team),
        MaxD = max(D, ts_model:up(Trunc)),
        ts_model:up(Team, 2),
        MaxD
            end,
    Delta1 = lists:foldl(Loop1, 0, lists:droplast(Tmp)),
    Loop2 = fun({Team, Trunc}, D) ->
        ts_model:down(Team),
        MaxD = max(D, ts_model:up(Trunc)),
        ts_model:up(Team, 1),
        MaxD
            end,
    Delta = lists:foldl(Loop2, Delta1, lists:droplast(lists:reverse(Tmp))),
    case Delta =< MinDelta of
        true ->
            p_run_team_diff_layer_trunc_layer(TeamDiffLayer, TruncLayer, 0, MinDelta);
        false ->
            p_run_team_diff_layer_trunc_layer(TeamDiffLayer, TruncLayer, Loop - 1, MinDelta)
    end.

build_rating_layer(RatingVars, FlattenRatings) ->
    [ts_model:prior(RatingVar, Rating, ?Tau) || {RatingVar, Rating} <- lists:zip(RatingVars, FlattenRatings)].

build_perf_layer(RatingVars, PerfVars) ->
    [ts_model:likelihood(RatingVar, PerfVar, ?Beta * ?Beta) || {RatingVar, PerfVar} <- lists:zip(RatingVars, PerfVars)].

build_team_perf_layer(PerfVars, TeamPerfVars, TeamSize, FlattenWeights) ->
    F = fun({Team, PerfVar}) ->
        Start = case Team > 1 of true -> lists:nth(Team - 1, TeamSize);false -> 0 end,
        Length = lists:nth(Team, TeamSize),
        ChildPerfVars = lists:sublist(PerfVars, Start + 1, Length),
        Coeffs = lists:sublist(FlattenWeights, Start + 1, Length),
        ts_model:ts_sum(PerfVar, ChildPerfVars, Coeffs)
        end,
    lists:map(F, ts_utils:enum(TeamPerfVars)).

build_team_diff_layer(TeamPerfVars, TeamDiffVars) ->
    [ts_model:ts_sum(TeamDiffVar, lists:sublist(TeamPerfVars, Team, 2), [1, -1]) || {Team, TeamDiffVar}
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
        0.0 -> -X;
        _ -> ts_distributions:pdf(X, 0, 1) / Demon
    end.

w_win(Diff, DrawMargin) ->
    X = Diff - DrawMargin,
    V = v_win(Diff, DrawMargin),
    W = V * (V + X),
    true = (0 < W andalso W < 1),
    W.
