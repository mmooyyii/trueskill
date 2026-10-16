-module(trueskill2).
-author("yimo").

%% TrueSkill 2 的在线更新 (Minka, Cleven, Zaykov. TrueSkill 2: An improved Bayesian skill rating system, 2018).
%%
%% 相对经典 TrueSkill 的改动, 公式编号对应论文:
%%   (3)      两局之间技能方差增加 tau^2 * 间隔时间
%%   (5)      队伍表现 = sum(perf_i * timePlayed_i / L)
%%   (7)      squad offset: perf_i ~ N(skill_i + squadOffset(squad size), beta^2)
%%   (8)      每局之后 skill 加 experienceOffset(min(experience, 200)), 方差加 gamma^2
%%   (9-11)   击杀/死亡数 count ~ max(0, N((wp * perf_i + wo * perfo_i) * timePlayed_i, v * timePlayed_i))
%%   (12)     退出当作一次观测 N(perf_i - perfo_i - mq, vq) < 0
%%   (14-20)  模式相关: skill = mode_weight * base + offset(mode)
%%   第 12 节 bot 的技能不随时间变化
%%
%% 隐变量 z = [base_1..n, offset_1..n, noise_1..n], perf_i = w * base_i + offset_i + noise_i + squadOffset_i,
%% 所有观测都是 perf 的线性组合, 交给 ts_ep 推断.
%% 只实现了论文的在线模式; 批量推断 (TrueSkill Through Time) 和参数学习需要全部历史数据, 参数由调用方给出.
%% 时间单位由调用方决定, 和 tau 保持一致即可 (论文里是分钟).

-include("trueskill2.hrl").

-export([new_player/0, new_bot/0, default_params/0]).
-export([skill/3, skill/4, rate/2]).

-export_type([player/0, params/0, match/0]).

-type player() :: #ts2_player{}.
-type params() :: #{atom() => term()}.
-type match() :: #{mode := term(), time := number(), length := number(), teams := [[map()]], ranks => [number()]}.

-record(entry, {
    player :: player(),
    team :: pos_integer(),
    time :: number(),
    squad :: pos_integer(),
    counts :: #{atom() => number()},
    quit :: boolean()
}).

-spec new_player() -> player().
new_player() ->
    #ts2_player{}.

-spec new_bot() -> player().
new_bot() ->
    #ts2_player{bot = true}.

%% 默认值取论文第 4 节 Halo 5 的典型参数
-spec default_params() -> params().
default_params() ->
    #{
        mean => 3.0,
        variance => 1.6,
        gamma => 1.0e-3,
        tau => 1.0e-8,
        beta => 1.0,
        draw_margin => 1.0e-3,
        base_variance => 1.0,
        base_gamma => 1.0e-3,
        base_tau => 1.0e-8,
        %% 0 表示模式之间不相关, 即经典 TrueSkill 的假设
        mode_weight => 0.0,
        %% #{SquadSize => Offset}, 超过最大 key 的取最大 key 的值, 单人固定为 0
        squad_offset => #{},
        %% 第 k 个元素是已打 k-1 局时的偏移, 超出长度取最后一个
        experience_offset => [],
        %% #{kill => {Wp, Wo, V}, death => {Wp, Wo, V}}
        counts => #{},
        quit_mean => 0.0,
        quit_variance => 1.0,
        min_delta => 1.0e-6,
        max_iterations => 100
    }.

%% 玩家在某个模式下的技能 {Mu, Sigma}, 用于匹配
-spec skill(player(), term(), params()) -> {float(), float()}.
skill(Player, Mode, Params) ->
    P = maps:merge(default_params(), Params),
    combine(current(Player, Mode, P), P).

%% 同上, 计入上一局到 Now 之间的方差增长
-spec skill(player(), term(), params(), number()) -> {float(), float()}.
skill(Player, Mode, Params, Now) ->
    P = maps:merge(default_params(), Params),
    combine(prior(Player, Mode, Now, P), P).

combine({BMu, BVar, OMu, OVar}, #{mode_weight := W}) ->
    {W * BMu + OMu, math:sqrt(W * W * BVar + OVar)}.

%% teams 里每个玩家是
%%   #{player := player(), time_played => 默认 L, squad_size => 默认 1,
%%     counts => #{kill => 3, death => 1}, quit => 默认 false}
%% ranks 越小越好, 相同为平局, 默认按 teams 的顺序.
%% 返回和 teams 结构相同的新玩家.
-spec rate(match(), params()) -> [[player()]].
rate(#{mode := Mode, time := Now, length := L, teams := Teams} = Match, Params) when L > 0, length(Teams) >= 2 ->
    P = maps:merge(default_params(), Params),
    Ranks = maps:get(ranks, Match, lists:seq(1, length(Teams))),
    length(Ranks) =:= length(Teams) orelse erlang:error(badarg),
    Entries = [entry(E, T, L) || {T, Team} <- lists:zip(lists:seq(1, length(Teams)), Teams), E <- Team],
    Priors = [prior(E#entry.player, Mode, Now, P) || E <- Entries],
    N = length(Entries),
    Beta = maps:get(beta, P),
    Mu = [M || {M, _, _, _} <- Priors] ++ [M || {_, _, M, _} <- Priors] ++ lists:duplicate(N, 0.0),
    Sigma = ts_linalg:diag([V || {_, V, _, _} <- Priors] ++ [V || {_, _, _, V} <- Priors]
        ++ lists:duplicate(N, Beta * Beta)),
    Squads = [squad_offset(E#entry.squad, maps:get(squad_offset, P)) || E <- Entries],
    Project = fun(A) -> {[maps:get(mode_weight, P) * X || X <- A] ++ A ++ A, ts_linalg:dot(A, Squads)} end,
    Factors = rank_factors(Entries, Ranks, L, P, Project) ++ individual_factors(Entries, L, P, Project),
    {Mu1, Sigma1} = ts_ep:run(Mu, Sigma, Factors, maps:get(min_delta, P), maps:get(max_iterations, P)),
    {BMu, Rest} = lists:split(N, Mu1),
    {OMu, _} = lists:split(N, Rest),
    {BVar, Rest1} = lists:split(N, ts_linalg:diagonal(Sigma1)),
    {OVar, _} = lists:split(N, Rest1),
    Posts = lists:zip(Entries, lists:zip(lists:zip(BMu, BVar), lists:zip(OMu, OVar))),
    New = [finish(E#entry.player, Mode, Now + L, {BM, BV, OM, OV}, P) || {E, {{BM, BV}, {OM, OV}}} <- Posts],
    regroup(Teams, New).

entry(E = #{player := Player}, Team, L) ->
    #entry{
        player = Player,
        team = Team,
        time = maps:get(time_played, E, L),
        squad = maps:get(squad_size, E, 1),
        counts = maps:get(counts, E, #{}),
        quit = maps:get(quit, E, false)
    }.

%% 存着的 base 和 offset, 新玩家/新模式取先验
current(#ts2_player{base_mu = BMu, base_var = BVar, modes = Modes}, Mode, P) ->
    BaseVar = case BVar of
                  undefined -> maps:get(base_variance, P);
                  _ -> BVar
              end,
    case Modes of
        #{Mode := #ts2_mode{mu = OMu, var = OVar}} -> {BMu, BaseVar, OMu, OVar};
        _ -> {BMu, BaseVar, maps:get(mean, P), maps:get(variance, P)}
    end.

%% 对局开始前的先验, 式 (3)(16)(19)
prior(#ts2_player{bot = true} = Player, Mode, _, P) ->
    current(Player, Mode, P);
prior(#ts2_player{base_time = BT, modes = Modes} = Player, Mode, Now, P) ->
    {BMu, BVar, OMu, OVar} = current(Player, Mode, P),
    {_, OT} = mode_history(Mode, Modes),
    #{base_tau := BTau, tau := Tau} = P,
    {BMu, BVar + BTau * BTau * elapsed(Now, BT), OMu, OVar + Tau * Tau * elapsed(Now, OT)}.

elapsed(_, undefined) -> 0;
elapsed(Now, T) -> max(0, Now - T).

%% 对局之后, 式 (8)(15)(18)
finish(#ts2_player{bot = true, modes = Modes} = Player, Mode, _, {BMu, BVar, OMu, OVar}, _) ->
    {Exp, T} = mode_history(Mode, Modes),
    M = #ts2_mode{mu = OMu, var = OVar, experience = Exp, time = T},
    Player#ts2_player{base_mu = BMu, base_var = BVar, modes = Modes#{Mode => M}};
finish(#ts2_player{modes = Modes} = Player, Mode, End, {BMu, BVar, OMu, OVar}, P) ->
    {Exp, _} = mode_history(Mode, Modes),
    #{gamma := G, base_gamma := BG, experience_offset := Offsets} = P,
    M = #ts2_mode{mu = OMu + experience_offset(Exp, Offsets), var = OVar + G * G, experience = Exp + 1, time = End},
    Player#ts2_player{base_mu = BMu, base_var = BVar + BG * BG, base_time = End, modes = Modes#{Mode => M}}.

%% {该模式已打局数, 上一局结束时间}
mode_history(Mode, Modes) ->
    case Modes of
        #{Mode := #ts2_mode{experience = Exp, time = T}} -> {Exp, T};
        _ -> {0, undefined}
    end.

experience_offset(_, []) -> 0.0;
experience_offset(Exp, Offsets) -> lists:nth(min(Exp, length(Offsets) - 1) + 1, Offsets).

squad_offset(1, _) ->
    0.0;
squad_offset(Size, Offsets) ->
    case [K || K <- maps:keys(Offsets), K =< Size] of
        [] -> 0.0;
        Keys -> maps:get(lists:max(Keys), Offsets)
    end.

%% 名次相邻的两队表现之差, 同 Herbrich 2007 的因子图
rank_factors(Entries, Ranks, L, #{draw_margin := Eps}, Project) ->
    Sorted = lists:sort(lists:zip(Ranks, lists:seq(1, length(Ranks)))),
    [rank_factor(Entries, L, Eps, Project, A, B) || {A, B} <- lists:zip(lists:droplast(Sorted), tl(Sorted))].

rank_factor(Entries, L, Eps, Project, {R1, T1}, {R2, T2}) ->
    A = [case T of T1 -> Time / L; T2 -> -Time / L; _ -> 0.0 end || #entry{team = T, time = Time} <- Entries],
    {Proj, B} = Project(A),
    Region = case R1 == R2 of
                 true -> {within, Eps};
                 false -> {gt, Eps}
             end,
    {trunc, Proj, B, 0.0, Region}.

%% 击杀/死亡数和退出, 式 (9)-(12)
individual_factors(Entries, L, P, Project) ->
    Opposing = [lists:sum([O#entry.time / L || O <- Entries, O#entry.team =/= E#entry.team]) || E <- Entries],
    Indexed = lists:zip(lists:seq(1, length(Entries)), Entries),
    lists:flatmap(fun({I, E}) ->
        Self = [case J of I -> 1.0; _ -> 0.0 end || {J, _} <- Indexed],
        Opponent = [perfo_coeff(E, O, Opp, L) || {O, Opp} <- lists:zip(Entries, Opposing)],
        count_factors(E, Self, Opponent, P, Project) ++ quit_factors(E, Self, Opponent, P, Project)
                  end, Indexed).

%% 式 (11): 对手 O 的注意力平分给他的所有对手
perfo_coeff(#entry{team = T}, #entry{team = T}, _, _) -> 0.0;
perfo_coeff(_, _, Opp, _) when Opp == 0 -> 0.0;
perfo_coeff(_, #entry{time = Time}, Opp, L) -> Time / L / Opp.

count_factors(#entry{time = T}, _, _, _, _) when T =< 0 ->
    [];
count_factors(#entry{time = T, counts = Counts}, Self, Opponent, #{counts := Params}, Project) ->
    [begin
         {Wp, Wo, V} = maps:get(Type, Params),
         A = lists:zipwith(fun(X, Y) -> T * (Wp * X + Wo * Y) end, Self, Opponent),
         count_factor(A, V * T, Count, Project)
     end || {Type, Count} <- lists:sort(maps:to_list(Counts))].

%% count > 0 是精确的高斯观测, count = 0 是截断 N(...) =< 0
count_factor(A, S0, Count, Project) when Count > 0 ->
    {Proj, B} = Project(A),
    {gauss, Proj, B, S0, Count};
count_factor(A, S0, _, Project) ->
    {Proj, B} = Project([-X || X <- A]),
    {trunc, Proj, B, S0, {gt, 0}}.

%% 论文的在线模式把退出当成和 (缩放后的) 对手打的一场小比赛
quit_factors(#entry{quit = true}, Self, Opponent, #{quit_mean := Mq, quit_variance := Vq}, Project) ->
    {Proj, B} = Project(lists:zipwith(fun(X, Y) -> Y - X end, Self, Opponent)),
    [{trunc, Proj, B + Mq, Vq, {gt, 0}}];
quit_factors(_, _, _, _, _) ->
    [].

regroup(Teams, Players) ->
    {Groups, []} = lists:mapfoldl(fun(Team, Rest) -> lists:split(length(Team), Rest) end, Players, Teams),
    Groups.
