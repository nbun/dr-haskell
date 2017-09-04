#!/bin/bash
dname=""
for arg in "$@"
do
    if [ -e $arg ]
    then
        dname=$(dirname "$arg")
    fi
done
eval $(docker-machine env)
docker run -e DRHASKELLHOSTPATH="$dname" -v "$dname"/:/tmp/drhaskell-src --rm jonasbusse/drhaskell:latest drhaskell-lint "$@"