@echo off
REM @FOR /f "tokens=*" %i IN ('docker-machine env dev') DO @%i
docker run -v %cd%:/tmp/drhaskell-src --rm -it jonasbusse/drhaskell drhaskell