# starter-bot

Starter bot for Halite 3 written in Clojure

## TODO

### 1/14
* Figure out what's causing the exceptions
* Modify get-collect-move to figure out the actual number of turns to get to full Halite based on 4 cell mining + staying in this quadrant vs. moving to another quadrant

### 1/8

* Figure out number of turns before I could possibly build a dropoff.
  - Amount I save is the amount I need subtracting any halite in nearby ships dropping off (closer than the number of turns before I build a dropoff + 1000 (since I'll be building a ship this turn.)
* Commit to a single dropoff sooner. Based on how good that dropoff might be choose more or fewer ships required.
* Figure out big time collisions with tons of ships.

### 12/10

* [ ] Fix end game return (leaving too many turtles behind)
* [ ] Add banned cells stuff again (at least for dropoff returns)
* [X] If my move is to stand still and I'm in ram-danger, choose a different site.
  - Prefer any open cell with the least halite available
  - Prefer cell with enemy carrying the most halite
* [ ] To fix 64 map size - look at Recurse games
  - Build dropoffs prior to getting to the really good spots.
  - Potentially look at burning way more Halite.
* [ ] At the end of the game start tracking ships halite as if it is part of the cell when choosing targets.

## 12/6 - time for a rewrite
- Still divide ships the same way as before
  - stuck, collect, or dropoff
  - For collect and dropoff score the surrounding cells differently
    - collect
      - Do later
    - dropoff
      - Calculate halite lost by any move
      - Deduct (* 100 for staying STILL if blocked by an enemy (not myself) by number of turns I've stayed still)
      - Deduct 500 for trying to go back to a cell that I was stuck in last time

### 11/18
  * [X] Implement unwind-collisions to prevent self destructs
    - [X] Potentially change sort order back to old one for dropoff and collecting after
  * [X] Performance improvement - save four-cell neighbor locations (just x and y) along with cells pre-game. Use this anytime I am trying to retrieve neighbor-locations.
  * [ ] Dodge and ram ships to my benefit (focus on 2 player first?).
  * [1/2] Prevent ships from getting stuck
  * [ ] Limit the number of ships that chase inspiration.
  * [ ] Choose dropoff locations deliberately (both when and where)
    - [ ] Send a few ships to the dropoff location.
  * [ ] Track when I have a bunch of ships that are not moving but should be dropping off
    - [ ] Figure out which enemy ship(s) are blocking me and blow them up.

### OLD

  * [ ] The Duck intentionally crashes ships in 2 player and builds ships much later than I do. Also does the end of game back to dropoffs at least 5 or so rounds early.
  * [x] Bug where I avoid a dropoff if they have a ship within one cell of my dropoff. (Add a step to remove all enemy ships from my dropoffs and base)
  * [x] Self collisions too frequent - if I'm surrounded on all sides I should move first
  * [ ] Noticed some back and forth ship movement + a lot of swapping
  * [x] I think my big issue now is that when I pick a target and there are a small number of targets the ship keeps switching every turn. I should do some kind of check to find the closest ship looking for a target for each target selected and potentially the most valuable targets targeted first.
  * [ ] Find hopeless ships (ones that should pick a top target)
    * Process these ships last - sort these ships by closest to target
  * [ ] Playing against top players - should build a dropoff, see what other strategies they use https://halite.io/play/?game_id=1481425&replay_class=1&replay_name=replay-20181105-223435%2B0000-1541456383-56-56-1481425
  * [X] Change sort order for moves - when collecting use most halite on cell, when dropping off sort by distance, than by halite carrying
  * [ ] Debug issue with turtle not leaving base when it seems like there are no good directions nearby (causing lots of crashes - see https://halite.io/play/?game_id=1404714&replay_class=1&replay_name=replay-20181104-030819%2B0000-1541300884-64-64-1404714)
  * [X] Crash all ships towards base near the end
  * [ ] Build a dropoff site that's at least N (maybe 25) distance from my main one when it makes sense
  * [ ] When there are no good nearby sites, choose a farther out site (when I have lots of ships too)
  * [ ] Start basing decisions on remaining halite per ship in play to decide when to keep building ships vs. stopping (and when to build dropoffs)
  * [ ] Choose which direction to move each ship
    - [ ] Iterate through and issue still moves for any ships without enough halite to move
    - [ ] Determine when it makes sense to move based on too little Halite at current location
    - [ ] Check for collisions
    - [ ] Determine when to move back to base
    - [ ] Choose the shortest path back to base
  * [ ] Take inspiration into account
  * [ ] Strategic crashes (probably 1v1 only)
  * [ ] Score cells each round
