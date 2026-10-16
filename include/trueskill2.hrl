%% TrueSkill 2 的玩家状态. skill(mode) = mode_weight * base + offset(mode), 论文式 (20).

-record(ts2_mode, {
    mu :: float(),
    var :: float(),
    %% 该模式下已经打过的局数, 用于 experienceOffset
    experience = 0 :: non_neg_integer(),
    %% 上一局结束的时间, 用于 tau^2 * (t' - t)
    time = undefined :: undefined | number()
}).

-record(ts2_player, {
    base_mu = 0.0 :: float(),
    %% undefined 表示新玩家, 取参数里的 base_variance
    base_var = undefined :: undefined | float(),
    base_time = undefined :: undefined | number(),
    modes = #{} :: #{term() => #ts2_mode{}},
    %% bot 的技能不随时间和对局变化, 论文第 12 节
    bot = false :: boolean()
}).
