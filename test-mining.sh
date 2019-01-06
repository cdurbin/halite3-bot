#!/bin/sh

lein uberjar
rm *.log
./halite -s 1546092743 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/DoNothing.jar" "java -jar target/uberjar/MyBot.jar -log"
mv flog* replays/
rm replays/*.log
