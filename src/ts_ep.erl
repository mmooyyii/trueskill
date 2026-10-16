-module(ts_ep).
-author("yimo").

%% 多元高斯上的 Expectation Propagation (Minka 2001).
%%
%% 每个观测都是隐变量 z ~ N(Mu, Sigma) 的线性投影 y = Proj . z + B + N(0, S0):
%%   {gauss, Proj, B, S0, Value}   观测到 y = Value, 精确更新一次
%%   {trunc, Proj, B, S0, Region}  观测到 y 落在 Region, 用一维高斯 site 近似后反复迭代
%%
%% TrueSkill 的因子图就是这种形式: 胜负/平局是队伍表现差的截断,
%% TrueSkill 2 的击杀/死亡数和退出也都是表现的线性组合.

-export([run/5]).
-export_type([factor/0, region/0]).

-type region() :: {gt, number()}         %% y > Eps
                | {within, number()}.    %% |y| =< Eps
-type factor() :: {gauss, ts_linalg:vector(), number(), number(), number()}
                | {trunc, ts_linalg:vector(), number(), number(), region()}.

-spec run(ts_linalg:vector(), ts_linalg:matrix(), [factor()], number(), non_neg_integer()) ->
    {ts_linalg:vector(), ts_linalg:matrix()}.
run(Mu, Sigma, Factors, MinDelta, MaxIter) ->
    {Gauss, Trunc} = lists:partition(fun(F) -> element(1, F) =:= gauss end, Factors),
    Prior = lists:foldl(fun observe/2, {Mu, Sigma}, Gauss),
    loop(Prior, [{F, {0.0, 0.0}} || F <- Trunc], MinDelta, MaxIter).

observe({gauss, Proj, B, S0, Value}, {Mu, Sigma}) ->
    rank1(Mu, Sigma, Proj, 1 / S0, (Value - B) / S0).

loop(Posterior, _, _, 0) ->
    Posterior;
loop(Posterior, Sites, MinDelta, Iter) ->
    case sweep(Posterior, Sites) of
        {Posterior1, _, Delta} when Delta =< MinDelta -> Posterior1;
        {Posterior1, Sites1, _} -> loop(Posterior1, Sites1, MinDelta, Iter - 1)
    end.

sweep(Posterior, Sites) ->
    {Sites1, {Posterior1, Delta}} = lists:mapfoldl(fun update_site/2, {Posterior, 0}, Sites),
    {Posterior1, Sites1, Delta}.

update_site({Factor = {trunc, Proj, B, S0, Region}, Site = {STau, SNu}}, {{Mu, Sigma}, Delta}) ->
    S = ts_linalg:dot(Proj, ts_linalg:mat_vec(Sigma, Proj)),
    M = ts_linalg:dot(Proj, Mu),
    case cavity(S, M, Site) of
        {ok, CMu, CVar} ->
            case moments(CMu, CVar, B, S0, Region) of
                {ok, NewMu, NewVar} ->
                    NTau = 1 / NewVar - 1 / CVar,
                    NNu = NewMu / NewVar - CMu / CVar,
                    D = max(abs(NTau - STau), abs(NNu - SNu)),
                    {{Factor, {NTau, NNu}}, {rank1(Mu, Sigma, Proj, NTau - STau, NNu - SNu), max(Delta, D)}};
                skip ->
                    {{Factor, Site}, {{Mu, Sigma}, Delta}}
            end;
        skip ->
            {{Factor, Site}, {{Mu, Sigma}, Delta}}
    end.

%% 投影的边缘分布去掉当前 site
cavity(S, M, {STau, SNu}) when S > 0, 1 / S > STau ->
    CTau = 1 / S - STau,
    {ok, (M / S - SNu) / CTau, 1 / CTau};
cavity(_, _, _) ->
    skip.

%% 截断后的均值和方差; 数值上算不出来时保留旧的 site. w 在 (0, 1) 内且 S0 >= 0, 新方差一定为正
moments(CMu, CVar, B, S0, Region) ->
    YVar = CVar + S0,
    YSd = math:sqrt(YVar),
    try vw((CMu + B) / YSd, Region, YSd) of
        {V, W} -> {ok, CMu + CVar / YSd * V, CVar - CVar * CVar / YVar * W}
    catch
        error:floating_point_error -> skip
    end.

vw(T, {gt, Eps}, Sd) -> {ts_gauss:v_win(T, Eps / Sd), ts_gauss:w_win(T, Eps / Sd)};
vw(T, {within, Eps}, Sd) -> {ts_gauss:v_draw(T, Eps / Sd), ts_gauss:w_draw(T, Eps / Sd)}.

%% 乘上 exp(DNu * t - DTau * t^2 / 2), t = Proj . z
rank1(Mu, Sigma, Proj, DTau, DNu) ->
    SA = ts_linalg:mat_vec(Sigma, Proj),
    Den = 1 + DTau * ts_linalg:dot(Proj, SA),
    K = (DNu - DTau * ts_linalg:dot(Proj, Mu)) / Den,
    C = DTau / Den,
    Mu1 = lists:zipwith(fun(X, Y) -> X + K * Y end, Mu, SA),
    Sigma1 = lists:zipwith(fun(Row, Ai) -> lists:zipwith(fun(X, Aj) -> X - C * Ai * Aj end, Row, SA) end, Sigma, SA),
    {Mu1, Sigma1}.
