@echo off

rem Does miscellaneous stuff to the NT distribution to prepare it for
rem Visual C++

echo Converting LF to CR-LF in Mindy.mak
d:\winntunix\gnu\usr\local\bin\perl -ne "print $_;" Mindy.mak > temp.mak
del Mindy.mak
ren temp.mak Mindy.mak

mkdir Released
mkdir Debug
mkdir Released/Objects
mkdir Debug/Objects
