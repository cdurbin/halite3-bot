#!/bin/sh

lein uberjar
rm *.log
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
#./halite -s 303946417 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V35.jar" "java -jar jars/V93.jar" "java -jar jars/V108.jar" "java -jar target/uberjar/MyBot.jar -log"
./halite -s 303946417 --replay-directory replays/ -vvv --width 64 --height 64 "java -jar jars/V124.jar" "java -jar jars/V114.jar" "java -jar jars/V125.jar" "java -jar target/uberjar/MyBot.jar -log"
rm replays/*.log
mv flog* replays
