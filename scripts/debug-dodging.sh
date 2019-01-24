#!/bin/sh

lein uberjar
rm *.log
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite -s 1545010962 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V125.jar" "java -jar jars/V130.jar" "java -jar jars/V124.jar" "java -jar target/uberjar/MyBot.jar -log"
rm replays/*.log
mv flog* replays
