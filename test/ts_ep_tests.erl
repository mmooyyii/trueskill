-module(ts_ep_tests).
-author("yimo").

-include_lib("eunit/include/eunit.hrl").
-include("test_util.hrl").

%% 一维高斯 N(0, 1) 截断到 (0, inf): 均值 sqrt(2/pi), 方差 1 - 2/pi
truncate_test() ->
    {[M], [[V]]} = ts_ep:run([0.0], [[1.0]], [{trunc, [1.0], 0.0, 0.0, {gt, 0}}], 1.0e-9, 10),
    ?assertNear(math:sqrt(2 / math:pi()), M, 1.0e-6),
    ?assertNear(1 - 2 / math:pi(), V, 1.0e-6).

%% 观测带噪声和偏移: y = x + 1 + N(0, 1) = 3, 后验 N(1, 0.5)
gauss_test() ->
    {[M], [[V]]} = ts_ep:run([0.0], [[1.0]], [{gauss, [1.0], 1.0, 1.0, 3.0}], 1.0e-9, 10),
    ?assertNear(1.0, M, 1.0e-12),
    ?assertNear(0.5, V, 1.0e-12).

within_test() ->
    {[M], [[V]]} = ts_ep:run([1.0], [[1.0]], [{trunc, [1.0], 0.0, 0.0, {within, 0.5}}], 1.0e-9, 10),
    ?assert(M > 0 andalso M < 0.5),
    ?assert(V < 1 / 12 + 1.0e-6).

%% 两个截断互相影响, 需要多轮迭代; 迭代次数为 0 时原样返回
iterate_test() ->
    Sigma = [[1.0, 0.0], [0.0, 1.0]],
    Factors = [{trunc, [1.0, 0.0], 0.0, 0.0, {gt, 0}}, {trunc, [1.0, 1.0], 0.0, 0.0, {gt, 1}}],
    ?assertEqual({[0.0, 0.0], Sigma}, ts_ep:run([0.0, 0.0], Sigma, Factors, 1.0e-9, 0)),
    {Once, _} = ts_ep:run([0.0, 0.0], Sigma, Factors, 1.0e-9, 1),
    {Converged, _} = ts_ep:run([0.0, 0.0], Sigma, Factors, 1.0e-9, 100),
    ?assertNotEqual(Once, Converged).

%% 投影方差为 0 或者截断算不出来时跳过该因子
skip_test() ->
    Prior = {[0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]]},
    ?assertEqual(Prior, ts_ep:run([0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], [{trunc, [0.0, 1.0], 0.0, 0.0, {gt, 0}}], 1.0e-9, 10)),
    ?assertEqual({[0.0], [[1.0]]}, ts_ep:run([0.0], [[1.0]], [{trunc, [1.0], 0.0, 0.0, {gt, 1000}}], 1.0e-9, 10)).

