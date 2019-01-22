#!/bin/sh

rm *.log
./halite --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V216.jar" "java -jar jars/V220.jar" "java -jar jars/V218.jar"

mv flog* replays/
rm replays/*.log
