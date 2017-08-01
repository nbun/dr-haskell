#!/bin/bash
docker run DRHASKELLHOSTPATH=$(pwd) -v $(pwd)/:/tmp/drhaskell-src -it drhaskell-lint $1 $2 $3