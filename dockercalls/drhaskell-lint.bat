@echo off
REM @FOR /f "tokens=*" %i IN ('docker-machine env dev') DO @%i
docker run -e DRHASKELLHOSTPATH=%cd% --rm -v %cd%:/tmp/drhaskell-src -it jonasbusse/drhaskell drhaskell-lint %1 %2 %3