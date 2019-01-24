#!/bin/sh

rm *.log
lein uberjar
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite -s 1546029317 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V142.jar" "java -jar target/uberjar/MyBot.jar -log"

rm replays/*.log
mv flog* replays
