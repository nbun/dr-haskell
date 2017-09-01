#!/bin/bash
eval $(docker-machine env)
docker run -v $(pwd)/:/tmp/drhaskell-src --rm -it jonasbusse/drhaskell:latest drhaskell "$@"