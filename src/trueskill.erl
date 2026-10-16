-module(trueskill).
-author("yimo").

%% 经典 TrueSkill (Herbrich, Minka, Graepel 2007).
%%
%% 隐变量 z = [skill_1..n, noise_1..n], perf_i = skill_i + noise_i, noise_i ~ N(0, beta^2).
%% 名次相邻的两队, 队伍表现之差 (按权重加权) 截断为胜负或平局, 用 ts_ep 做推断.

-include("trueskill.hrl").

-export([new_player/0, new_player/1, new_player/2]).
-export([vs/1, vs/2]).
-export([adjust/1, adjust/2, adjust/3]).

-export_type([player/0, team/0]).

-type player() :: #ts_player{}.
-type team() :: [player()].

%% Xbox Live 的默认参数
-define(MU, 25.0).
-define(SIGMA, (?MU / 3)).
-define(BETA, (?SIGMA / 2)).
-define(TAU, (?SIGMA / 100)).
-define(DRAW_PROBABILITY, 0.10).
-define(MIN_DELTA, 0.0001).
-define(MAX_ITERATIONS, 100).

-spec new_player() -> player().
new_player() ->
    new_player(?MU, ?SIGMA).

-spec new_player(number()) -> player().
new_player(Mu) ->
    new_player(Mu, ?SIGMA).

-spec new_player(number(), number()) -> player().
new_player(Mu, Sigma) ->
    #ts_player{mu = float(Mu), sigma = float(Sigma)}.

%% 对局质量, 即平局概率, 越接近 1 越势均力敌
-spec vs([team()]) -> float().
vs(Teams) ->
    vs(Teams, default_weights(Teams)).

-spec vs([team()], [[number()]]) -> float().
vs(Teams, Weights) when length(Teams) >= 2, length(Teams) =:= length(Weights) ->
    Players = lists:append(Teams),
    Mu = [P#ts_player.mu || P <- Players],
    Var = [P#ts_player.sigma * P#ts_player.sigma || P <- Players],
    A = [diff_coeffs(Weights, T, T + 1) || T <- lists:seq(1, length(Teams) - 1)],
    At = ts_linalg:transpose(A),
    Ata = ts_linalg:scale(?BETA * ?BETA, ts_linalg:mul(A, At)),
    Middle = ts_linalg:add(Ata, ts_linalg:mul(A, ts_linalg:mul(ts_linalg:diag(Var), At))),
    D = ts_linalg:mat_vec(A, Mu),
    E = -0.5 * ts_linalg:dot(D, ts_linalg:mat_vec(ts_linalg:inverse(Middle), D)),
    math:exp(E) * math:sqrt(ts_linalg:determinant(Ata) / ts_linalg:determinant(Middle)).

%% 名次默认按 Teams 的顺序, 越小越好, 名次相同为平局
-spec adjust([team()]) -> [team()].
adjust(Teams) ->
    adjust(Teams, lists:seq(1, length(Teams))).

-spec adjust([team()], [number()]) -> [team()].
adjust(Teams, Ranks) ->
    adjust(Teams, Ranks, default_weights(Teams)).

%% Weights 和 Teams 结构相同, 一般是上场时间占比
-spec adjust([team()], [number()], [[number()]]) -> [team()].
adjust(Teams, Ranks, Weights)
    when length(Teams) >= 2, length(Teams) =:= length(Ranks), length(Teams) =:= length(Weights) ->
    Players = lists:append(Teams),
    N = length(Players),
    Mu = [P#ts_player.mu || P <- Players] ++ lists:duplicate(N, 0.0),
    Sigma = ts_linalg:diag([P#ts_player.sigma * P#ts_player.sigma + ?TAU * ?TAU || P <- Players]
        ++ lists:duplicate(N, ?BETA * ?BETA)),
    Ws = [[max(?MIN_DELTA, W) || W <- Ws] || Ws <- Weights],
    Factors = [rank_factor(Teams, Ws, Ranks, T1, T2) || {T1, T2} <- adjacent(sort_by_rank(Ranks))],
    {Mu1, Sigma1} = ts_ep:run(Mu, Sigma, Factors, ?MIN_DELTA, ?MAX_ITERATIONS),
    Skills = lists:zipwith(fun(M, V) -> #ts_player{mu = M, sigma = math:sqrt(V)} end,
        lists:sublist(Mu1, N), lists:sublist(ts_linalg:diagonal(Sigma1), N)),
    regroup(Teams, Skills).

default_weights(Teams) ->
    [[1.0 || _ <- Team] || Team <- Teams].

%% 队伍下标按名次稳定排序
sort_by_rank(Ranks) ->
    [T || {_, T} <- lists:sort(lists:zip(Ranks, lists:seq(1, length(Ranks))))].

adjacent(L) ->
    lists:zip(lists:droplast(L), tl(L)).

rank_factor(Teams, Weights, Ranks, T1, T2) ->
    A = diff_coeffs(Weights, T1, T2),
    Size = length(lists:nth(T1, Teams)) + length(lists:nth(T2, Teams)),
    Eps = ts_gauss:ppf((?DRAW_PROBABILITY + 1) / 2) * math:sqrt(Size) * ?BETA,
    Region = case lists:nth(T1, Ranks) == lists:nth(T2, Ranks) of
                 true -> {within, Eps};
                 false -> {gt, Eps}
             end,
    {trunc, A ++ A, 0.0, 0.0, Region}.

%% 队伍 T1 的表现减去队伍 T2 的表现, 在所有玩家上的系数
diff_coeffs(Weights, T1, T2) ->
    lists:append([[case T of T1 -> W; T2 -> -W; _ -> 0.0 end || W <- Ws]
        || {T, Ws} <- lists:zip(lists:seq(1, length(Weights)), Weights)]).

regroup(Teams, Players) ->
    {Groups, []} = lists:mapfoldl(fun(Team, Rest) -> lists:split(length(Team), Rest) end, Players, Teams),
    Groups.
