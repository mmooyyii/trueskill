-module(ts_gauss_tests).
-author("yimo").

-include_lib("eunit/include/eunit.hrl").
-include("test_util.hrl").

%% 期望值: 标准正态分布表, 以及按 Herbrich 2007 的定义
%%   v_win(t, e) = pdf(t - e) / cdf(t - e), w_win = v * (v + t - e)
%%   v_draw(t, e) = (pdf(-e - t) - pdf(e - t)) / (cdf(e - t) - cdf(-e - t))
%%   w_draw(t, e) = v^2 + ((e - t) pdf(e - t) + (e + t) pdf(e + t)) / (cdf(e - t) - cdf(-e - t))

pdf_test() ->
    ?assertNear(0.3989422804, ts_gauss:pdf(0), 1.0e-9),
    ?assertNear(0.2419707245, ts_gauss:pdf(1), 1.0e-9),
    ?assertNear(0.2419707245, ts_gauss:pdf(-1), 1.0e-9).

cdf_test() ->
    ?assertNear(0.5, ts_gauss:cdf(0), 1.0e-9),
    ?assertNear(0.8413447461, ts_gauss:cdf(1), 1.0e-9),
    ?assertNear(0.1586552539, ts_gauss:cdf(-1), 1.0e-9).

ppf_test() ->
    ?assertNear(0.0, ts_gauss:ppf(0.5), 1.0e-9),
    ?assertNear(1.6448536270, ts_gauss:ppf(0.95), 1.0e-9),
    ?assertNear(-1.6448536270, ts_gauss:ppf(0.05), 1.0e-9).

erfcinv_test() ->
    ?assertEqual(-100.0, ts_gauss:erfcinv(2)),
    ?assertEqual(100.0, ts_gauss:erfcinv(0)),
    ?assertNear(0.4769362762, ts_gauss:erfcinv(0.5), 1.0e-9),
    ?assertNear(-0.4769362762, ts_gauss:erfcinv(1.5), 1.0e-9).

win_test() ->
    ?assertNear(1.5251352762, ts_gauss:v_win(0, 1), 1.0e-9),
    ?assertNear(0.8009023344, ts_gauss:w_win(0, 1), 1.0e-9),
    ?assertNear(0.0176378255, ts_gauss:v_win(3, 0.5), 1.0e-9).

draw_test() ->
    ?assertNear(-0.3562728842, ts_gauss:v_draw(0.5, 1), 1.0e-9),
    ?assertNear(0.3562728842, ts_gauss:v_draw(-0.5, 1), 1.0e-9),
    ?assertNear(0.7197518498, ts_gauss:w_draw(0.5, 1), 1.0e-9).

%% cdf 下溢为 0 时 v 退化为线性, w 无法计算
extreme_win_test() ->
    ?assertEqual(100, ts_gauss:v_win(-100, 0)),
    ?assertError(floating_point_error, ts_gauss:w_win(-100, 0)).

extreme_draw_test() ->
    ?assertEqual(-99, ts_gauss:v_draw(100, 1)),
    ?assertEqual(99, ts_gauss:v_draw(-100, 1)),
    ?assertError(floating_point_error, ts_gauss:w_draw(100, 1)).
