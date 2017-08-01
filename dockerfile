FROM ubuntu:16.04

COPY . /tmp/drhaskell-build

RUN ls -la /tmp/drhaskell-build

# Dependencies
RUN apt-get update 

RUN apt-get install -y ghc cabal-install git

RUN cabal update

RUN cd /tmp/drhaskell-build && cabal install --flags="multicall"

# RUN mkdir /tmp/drhaskell-build && cd /tmp/drhaskell-build
# RUN git clone ssh://git@git.ps.informatik.uni-kiel.de:55055/student-projects/mapro-2017-ss.git && cd mapro-2017-ss

WORKDIR /tmp/drhaskell-src

#CMD ghci

# Done
# docker run -v $(pwd)/:/tmp/drhaskell-src -it drhaskell