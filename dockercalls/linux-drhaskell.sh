#!/bin/bash
sudo docker run -v $(pwd)/:/tmp/drhaskell-src --rm -it jonasbusse/drhaskell:latest drhaskell "$@"