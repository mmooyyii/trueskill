# trueskill
Erlang implementation of TrueSkill

## TrueSkill 
TrueSkill is a kind of Bayesian inference algorithm for game rank and match, 
has been used on Xbox Live.
TrueSkill support 1:1,N:M, N:M:J:K game.

### Build
```shell
rebar3 compile
```

### Quick Start
```erlang 
# create player

# create team

# show the quality of the game

# adjust player's rank by game result

# create match system

# player or team join the match system

# player or team quit the match system

# find the best game in match system
```

### TODO
High Available Match System(Raft Consensus Algorithm)
### License 
MIT
