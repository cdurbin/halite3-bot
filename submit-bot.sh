#!/bin/sh

(mkdir -p deploy && cp target/uberjar/MyBot.jar deploy/ && echo "Clojure" > deploy/LANGUAGE)
(cd deploy && zip submission.zip ./*)
#(cd ../Halite3Tools/hlt_client/hlt_client && ./client.py bot -b ../../../ClojureBot/deploy/submission.zip)
hlt bot -b deploy/submission.zip upload
