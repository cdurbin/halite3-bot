#!/bin/sh

rm *.log
./halite --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V190.jar" "java -jar jars/V210.jar" "java -jar jars/V206.jar"

mv flog* replays/
rm replays/*.log
