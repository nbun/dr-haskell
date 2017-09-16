@echo off

REM @FOR /f "tokens=*" %i IN ('docker-machine env dev') DO @%i

setlocal EnableDelayedExpansion

FOR %%a IN (%*) DO (
if exist %%a set VDOSPATH=%%~dpa
if exist %%a set VDOSFILE=%%~nxa
) 

docker run -e DRHASKELLHOSTPATH=%VDOSPATH% --rm -v %VDOSPATH%:/tmp/drhaskell-src jonasbusse/drhaskell drhaskell-lint %*