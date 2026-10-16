-module(ts_gauss_tests).
-author("yimo").

-include_lib("eunit/include/eunit.hrl").
-include("test_util.hrl").

%% 期望值来自 python trueskill 0.4.5 (backend=None)

pdf_test() ->
    ?assertNear(0.3989422804, ts_gauss:pdf(0), 1.0e-9),
    ?assertNear(0.2419707245, ts_gauss:pdf(1), 1.0e-9),
    ?assertNear(0.2419707245, ts_gauss:pdf(-1), 1.0e-9).

cdf_test() ->
    ?assertNear(0.5, ts_gauss:cdf(0), 1.0e-9),
    ?assertNear(0.8413447386, ts_gauss:cdf(1), 1.0e-7),
    ?assertNear(0.1586552539, ts_gauss:cdf(-1), 1.0e-7).

ppf_test() ->
    ?assertNear(0.0, ts_gauss:ppf(0.5), 1.0e-9),
    ?assertNear(1.6448536128, ts_gauss:ppf(0.95), 1.0e-6),
    ?assertNear(-1.6448536128, ts_gauss:ppf(0.05), 1.0e-6).

erfcinv_test() ->
    ?assertEqual(-100.0, ts_gauss:erfcinv(2)),
    ?assertEqual(100.0, ts_gauss:erfcinv(0)),
    ?assertNear(0.4769362762, ts_gauss:erfcinv(0.5), 1.0e-9),
    ?assertNear(-0.4769362762, ts_gauss:erfcinv(1.5), 1.0e-9).

win_test() ->
    ?assertNear(1.5251352044, ts_gauss:v_win(0, 1), 1.0e-6),
    ?assertNear(0.8009021873, ts_gauss:w_win(0, 1), 1.0e-6),
    ?assertNear(0.0176378255, ts_gauss:v_win(3, 0.5), 1.0e-6).

draw_test() ->
    ?assertNear(-0.3562728721, ts_gauss:v_draw(0.5, 1), 1.0e-6),
    ?assertNear(0.3562728721, ts_gauss:v_draw(-0.5, 1), 1.0e-6),
    ?assertNear(0.7197518212, ts_gauss:w_draw(0.5, 1), 1.0e-6).

%% cdf 下溢为 0 时 v 退化为线性, w 无法计算
extreme_win_test() ->
    ?assertEqual(100, ts_gauss:v_win(-100, 0)),
    ?assertError(floating_point_error, ts_gauss:w_win(-100, 0)).

extreme_draw_test() ->
    ?assertEqual(-99, ts_gauss:v_draw(100, 1)),
    ?assertEqual(99, ts_gauss:v_draw(-100, 1)),
    ?assertError(floating_point_error, ts_gauss:w_draw(100, 1)).
