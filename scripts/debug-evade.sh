#!/bin/sh

lein uberjar
rm *.log
#./halite --replay-directory replays/ -vvv --width 16 --height 32 "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"

./halite -s 1545851507 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V125.jar" "java -jar jars/V124.jar" "java -jar jars/V130.jar"

mv flog* replays/
rm replays/*.log
