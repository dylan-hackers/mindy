@echo off

rem Does miscellaneous stuff to the Windows distribution to prepare it for
rem Visual C++

echo Converting LF to CR-LF in Mindy.mak
d:\gnu\usr\local\bin\perl -ne "print $_;" Mindy.mak > temp.mak
del Mindy.mak
ren temp.mak Mindy.mak

mkdir Compiler
mkdir Interpretter
mkdir Compiler\Release
mkdir Interpretter\Release

mkdir bin
mkdir lib
