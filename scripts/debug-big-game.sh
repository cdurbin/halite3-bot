#!/bin/sh

rm *.log
lein uberjar
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
#./halite -s 1543534991 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V60.jar" "java -jar target/uberjar/MyBot.jar -log"
#./halite -s 1543540918 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V60.jar" "java -jar target/uberjar/MyBot.jar -log"
./halite -s 1543547050 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V60.jar" "java -jar target/uberjar/MyBot.jar -log"
