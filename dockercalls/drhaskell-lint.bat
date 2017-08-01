@echo off
docker run DRHASKELLHOSTPATH=%cd% -v %cd%:/tmp/drhaskell-src -it drhaskell-lint %1 %2 %3