#!/bin/sh

rm *.log
lein uberjar
./halite -s 1548203510 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V216.jar" "java -jar jars/V220.jar" "java -jar jars/V218.jar"

mv flog* replays/
rm replays/*.log
