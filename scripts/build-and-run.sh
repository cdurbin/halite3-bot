#!/bin/sh

rm *.log
lein uberjar
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar -log"
./run-game.sh
