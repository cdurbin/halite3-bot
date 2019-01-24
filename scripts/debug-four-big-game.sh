#!/bin/sh

lein uberjar
rm *.log
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite -s 1548005832 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V190.jar" "java -jar jars/V206.jar" "java -jar jars/V195.jar" "java -jar target/uberjar/MyBot.jar -log"
rm replays/*.log
mv flog* replays
