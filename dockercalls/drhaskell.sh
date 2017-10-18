#!/bin/bash
mkdir -p .drhaskell
chmod 777 .drhaskell/
eval $(docker-machine env)
docker run -v $(pwd)/:/tmp/drhaskell-src --rm -it jonasbusse/drhaskell:latest drhaskell "$@"
chmod 755 .drhaskell/