#!/bin/sh

rm *.log
lein uberjar
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite -s 1541978915 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/V11.jar" "java -jar target/uberjar/MyBot.jar"
