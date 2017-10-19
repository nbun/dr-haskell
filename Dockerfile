FROM ubuntu:latest

RUN apt-get update

RUN apt-get install -y cabal-install ghc

COPY . /tmp/drhaskell-build

RUN ls -la /tmp/drhaskell-build

RUN cabal update

RUN cabal install 'happy >=1.19.7'

RUN cd /tmp/drhaskell-build && cabal install --flags="multicall"

RUN rm -r /tmp/drhaskell-build

RUN ln -s /root/.cabal/bin/drhaskell-multicall /usr/bin/drhaskell

RUN ln -s /root/.cabal/bin/drhaskell-multicall /usr/bin/drhaskell-lint

WORKDIR /tmp/drhaskell-src
