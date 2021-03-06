#!/bin/sh

lein uberjar
rm *.log
#./halite --replay-directory replays/ -vvv --width 16 --height 32 "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"

#./halite --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V125.jar" "java -jar jars/V124.jar" "java -jar jars/V130.jar"

./halite -s 1546187421 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V125.jar" "java -jar jars/V158.jar" "java -jar jars/V159.jar"

mv flog* replays/
rm replays/*.log
