echo off
REM
REM Generate the Gwydion Dylan Makefiles for the tree
REM

for /f "delims=" %%a in ('cd') do set CURDIR=%%a

perl mk-build-tree -p"%CURDIR%\d2c\compiler\platforms.descr"


