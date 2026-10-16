![Erlang CI](https://github.com/mmooyyii/trueskill/workflows/Erlang%20CI/badge.svg?branch=master)  [![Coverage Status](https://coveralls.io/repos/github/mmooyyii/trueskill/badge.svg?branch=master)](https://coveralls.io/github/mmooyyii/trueskill?branch=master)
# trueskill

Erlang implementation of TrueSkill and TrueSkill 2.

## TrueSkill Algorithm

TrueSkill is a kind of Bayesian inference algorithm for game rank and match, has been used on Xbox Live. TrueSkill
support 1:1,N:M, N:M:J:K game.

- [TrueSkill: A Bayesian Skill Rating System](https://www.microsoft.com/en-us/research/publication/trueskilltm-a-bayesian-skill-rating-system/) (Herbrich, Minka, Graepel 2007)
- [TrueSkill 2: An improved Bayesian skill rating system](https://www.microsoft.com/en-us/research/publication/trueskill-2-improved-bayesian-skill-rating-system/) (Minka, Cleven, Zaykov 2018)

Both are implemented as Expectation Propagation over a multivariate Gaussian (`ts_ep`).
The classic results match [python trueskill](https://github.com/sublee/trueskill).

### Build

```shell
rebar3 compile
rebar3 do eunit --cover, cover
```

### TrueSkill

```erlang
1> rr("include/trueskill.hrl").
2> P1 = trueskill:new_player().
#ts_player{mu = 25.0,sigma = 8.333333333333334}

%% quality of the match (draw probability)
3> trueskill:vs([[P1], [trueskill:new_player()]]).
0.4472135954999579

%% Team1 ranks 1, Team2 ranks 2, equal ranks mean a draw
4> trueskill:adjust([[P1], [trueskill:new_player()]], [1, 2]).
[[#ts_player{mu = 29.39583201999916,sigma = 7.171475587326195}],
 [#ts_player{mu = 20.604167980000835,sigma = 7.171475587326195}]]

%% weights, e.g. the fraction of time each player played
5> trueskill:adjust([[P1], [P1, P1]], [1, 2], [[1], [1, 0.5]]).
```

### TrueSkill 2

Online mode of the paper: squad offset, experience offset, skill drift over time, play time weighting,
kill/death counts, quit penalty, correlated skills between game modes, and bots.
Batch inference and parameter estimation are not included, pass the fitted parameters in `Params`
(defaults are the typical Halo 5 values in section 4 of the paper, see `trueskill2:default_params/0`).

```erlang
1> P = trueskill2:new_player().
2> Params = #{counts => #{kill => {1.0, -1.0, 1.0}}, squad_offset => #{2 => 0.3}, mode_weight => 1.0}.
3> Match = #{mode => slayer, time => 0, length => 10, ranks => [1, 2],
             teams => [[#{player => P, counts => #{kill => 12}, squad_size => 2},
                        #{player => P, counts => #{kill => 3}, squad_size => 2}],
                       [#{player => P, counts => #{kill => 7}},
                        #{player => P, counts => #{kill => 0}, quit => true, time_played => 4}]]}.
4> [[A, B], [C, D]] = trueskill2:rate(Match, Params).
5> trueskill2:skill(A, slayer, Params).
6> trueskill2:skill(A, swat, Params, 1440).   % skill in another mode, one day later
```

### License

MIT
