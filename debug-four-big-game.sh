#!/bin/sh

rm *.log
lein uberjar
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite -s 1542488926 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/V8.jar" "java -jar jars/V9.jar" "java -jar jars/V10.jar" "java -jar target/uberjar/MyBot.jar -log"
