#!/bin/sh

#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite --replay-directory replays/ -vvv --width 16 --height 16 "java -jar jars/V11.jar" "java -jar target/uberjar/MyBot.jar"
