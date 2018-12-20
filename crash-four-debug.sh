#!/bin/sh

rm *.log
lein uberjar

#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite -s 1542842174 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V33.jar" "java -jar jars/V31.jar" "java -jar jars/V30.jar" "java -jar target/uberjar/MyBot.jar -log"
