#!/bin/sh

lein uberjar
rm *.log

#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
./halite -s 1546197310 --replay-directory replays/ -vvv --width 48 --height 48 "java -jar jars/V162.jar" "java -jar target/uberjar/MyBot.jar -log"
mv flog* replays/
rm replays/error*