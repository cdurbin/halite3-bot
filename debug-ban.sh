#!/bin/sh

lein uberjar
rm *.log
#./halite --replay-directory replays/ -vvv --width 16 --height 32 "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"

#./halite --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar" "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py" "python3 ../Python/MyBot.py"

./halite -s 1544656531 --replay-directory replays/ -vvv --width 32 --height 32 "java -jar target/uberjar/MyBot.jar -log" "java -jar jars/V35.jar" "java -jar jars/V108.jar" "java -jar jars/V93.jar"

mv flog* replays/
rm replays/*.log
