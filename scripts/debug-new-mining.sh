#!/bin/sh

rm *.log
lein uberjar
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/V23.jar" "java -jar target/uberjar/MyBot.jar -log"
./halite -s 1542939623 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/V34.jar" "java -jar target/uberjar/MyBot.jar -log"
