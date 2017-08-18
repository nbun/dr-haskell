@echo off
@FOR /f "tokens=*" %i IN ('docker-machine env dev') DO @%i
docker run DRHASKELLHOSTPATH=%cd% -v %cd%:/tmp/drhaskell-src -it drhaskell-lint %1 %2 %3