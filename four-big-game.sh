#!/bin/sh

rm *.log
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V216.jar" "java -jar jars/V218.jar" "java -jar jars/V220.jar" "java -jar target/uberjar/MyBot.jar -log"
rm replays/*.log
mv flog* replays
