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
P1 = trueskill:new_player().
P2 = trueskill:new_player().

# create team
Team1 = [P1].
Team2 = [P2].
Team3 = [trueskill:new_player(), trueskill:new_player()].

# show the quality of the game
trueskill:vs([Team1, Team2]).

# adjust player's rank by game result

```

### License

MIT
