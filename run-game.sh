#!/bin/sh

rm *.log
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "python3 ../Python/MyBot.py" "java -jar target/uberjar/MyBot.jar"
#./halite --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/V190.jar" "java -XX:+UnlockCommercialFeatures -XX:+FlightRecorder -XX:StartFlightRecording=duration=60s,filename=myrecording.jfr -jar target/uberjar/MyBot.jar -log"
./halite --replay-directory replays/ -vvv --width 32 --height 32 "java -jar jars/V190.jar" "java -XX:+UnlockCommercialFeatures -XX:+FlightRecorder -jar target/uberjar/MyBot.jar -log"
mv flog* replays/
rm replays/*.log
