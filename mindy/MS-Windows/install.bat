@copy Compiler\Release\mindycomp.exe c:\gwydion\bin
@copy Interpretter\Release\Interpretter.exe c:\gwydion\bin\mindy.exe
@cmd /c "cd libraries && nmake /nologo install %1 %2 %3 %4 %5 %6 %7 %8 %9"
