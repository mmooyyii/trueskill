![Erlang CI](https://github.com/mmooyyii/trueskill/workflows/Erlang%20CI/badge.svg?branch=master)  [![Coverage Status](https://coveralls.io/repos/github/mmooyyii/trueskill/badge.svg?branch=master)](https://coveralls.io/github/mmooyyii/trueskill?branch=master)
# trueskill

Erlang implementation of TrueSkill

## TrueSkill Algorithm

TrueSkill is a kind of Bayesian inference algorithm for game rank and match, has been used on Xbox Live. TrueSkill
support 1:1,N:M, N:M:J:K game.

### Build

```shell
rebar3 compile
```

### Quick Start

```erlang 
# create player
1> P1 = trueskill:new_player().
{ts_player,25,8.333333333333334,0.014399999999999998,
           0.35999999999999993,0}
2> P2 = trueskill:new_player().
{ts_player,25,8.333333333333334,0.014399999999999998,
           0.35999999999999993,0}

# create team
Team1 = [P1].
Team2 = [P2].
Team3 = [trueskill:new_player(), trueskill:new_player()].

# show the quality of the game
6> trueskill:vs([Team1, Team2]).
0.44721359549995787

# adjust player's rank by game result, 
# in this case, Team1 get rank 1 and Team2 get rank 2
7> trueskill:adjust([Team1, Team2],[1,2]).
[[{ts_player,29.39583201999916,7.171475587326195,
             0.019443880858302023,0.5715690555275234,0}],
 [{ts_player,20.604167980000835,7.171475587326195,
             0.019443880858302023,0.4006249873875777,0}]]
```

### License

MIT
