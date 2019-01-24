#!/bin/sh

lein uberjar
rm *.log
./halite -s 1539130381 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/DoNothing.jar" "java -jar target/uberjar/MyBot.jar -log"
mv flog* replays/
rm replays/*.log
