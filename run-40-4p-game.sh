#!/bin/sh

rm *.log

./halite --replay-directory replays/ -vvv --width 40 --height 40 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V162.jar" "java -jar jars/V170.jar" "java -jar jars/V171.jar"

mv flog* replays/
rm replays/*.log
