#!/bin/bash
dname=""
for arg in "$@"
do
    if [ -e $arg ]
    then
        dname=$(dirname "$arg")
    fi
done
docker run -e DRHASKELLHOSTPATH="$dname" -v "$dname"/:/tmp/drhaskell-src --rm -it jonasbusse/drhaskell:latest drhaskell-lint "$@"