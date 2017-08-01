#!/bin/bash
eval $(docker-machine env)
docker run -v $(pwd)/:/tmp/drhaskell-src -it drhaskell:latest