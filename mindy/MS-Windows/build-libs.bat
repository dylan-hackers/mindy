@copy Compiler\Release\mindycomp.exe comp
@copy Interpretter\Release\Interpretter.exe interp\mindy.exe
@cmd /c "cd libraries && nmake -nologo %1 %2 %3 %4 %5 %6 %7 %8 %9"
@cmd /c "cd demos && nmake -nologo %1 %2 %3 %4 %5 %6 %7 %8 %9"
