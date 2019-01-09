#!/bin/sh

rm *.log
./halite --replay-directory replays/ -vvv --width 40 --height 40 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V184.jar" "java -jar jars/V181.jar" "java -jar jars/V180.jar"

mv flog* replays/
rm replays/*.log
