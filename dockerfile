FROM ubuntu:16.04

# only for testing. later git pull and build from there
# COPY . /tmp/drhaskell-build
# Dependencies
RUN apt-get update 

RUN apt-get install -y ghc cabal-install git

RUN mkdir /tmp/drhaskell-build \
    && cd /tmp/drhaskell-build \
    && git clone https://git.ps.informatik.uni-kiel.de/student-projects/mapro-2017-ss.git

RUN ls -la /tmp/drhaskell-build

RUN cabal update

RUN cabal install happy-1.19.5

RUN cabal install haskell-src-exts-1.19.1

# /tmp/haskell-build/mapro-2017-ss
RUN cd /tmp/drhaskell-build/mapro-2017-ss && cabal install --flags="multicall"

RUN rm -R /tmp/drhaskell-build

RUN ln -s /root/.cabal/bin/drhaskell-multicall /usr/bin/drhaskell

RUN ln -s /root/.cabal/bin/drhaskell-multicall /usr/bin/drhaskell-lint

WORKDIR /tmp/drhaskell-src

# Done
# docker run -v $(pwd)/:/tmp/drhaskell-src -it drhaskell
# docker run DRHASKELLHOSTPATH=$(pwd) -v $(pwd)/:/tmp/drhaskell-src -it drhaskell-lint --json --hint=l1 <file>