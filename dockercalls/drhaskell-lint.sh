#!/bin/bash
eval $(docker-machine env)
docker run -e DRHASKELLHOSTPATH=$(pwd) -v $(pwd)/:/tmp/drhaskell-src -it jonasbusse/drhaskell:latest drhaskell-lint $1 $2 $3