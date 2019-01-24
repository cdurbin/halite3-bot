#!/bin/sh

rm *.log
lein uberjar

./halite -s 1542838807 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/V32.jar" "java -jar target/uberjar/MyBot.jar -log"
