#!/bin/bash
mkdir -p .drhaskell
chmod 777 .drhaskell/
dname=""
for arg in "$@"
do
    if [ -e $arg ]
    then
        dname=$(dirname "$arg")
    fi
done
docker run -e DRHASKELLHOSTPATH="$dname" -v "$dname"/:/tmp/drhaskell-src --rm jonasbusse/drhaskell:latest drhaskell-lint "$@"
chmod 755 .drhaskell/