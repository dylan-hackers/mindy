# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Interpretter - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Interpretter - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Shitpile - Win32 Release" && "$(CFG)" !=\
 "Shitpile - Win32 Debug" && "$(CFG)" != "Compiler - Win32 Release" && "$(CFG)"\
 != "Compiler - Win32 Debug" && "$(CFG)" != "Interpretter - Win32 Release" &&\
 "$(CFG)" != "Interpretter - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "Mindy.mak" CFG="Interpretter - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Shitpile - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Shitpile - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "Compiler - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Compiler - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "Interpretter - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Interpretter - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "Interpretter - Win32 Debug"
RSC=rc.exe
CPP=cl.exe

!IF  "$(CFG)" == "Shitpile - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "Interpretter - Win32 Release" "Compiler - Win32 Release" 

CLEAN : 
	-@erase 

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /Fp"$(INTDIR)/Mindy.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Mindy.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/Mindy.pdb" /machine:I386 /out:"$(OUTDIR)/Mindy.exe" 
LINK32_OBJS=

!ELSEIF  "$(CFG)" == "Shitpile - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "Interpretter - Win32 Debug" "Compiler - Win32 Debug" 

CLEAN : 
	-@erase 

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/Mindy.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Mindy.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/Mindy.pdb" /debug /machine:I386 /out:"$(OUTDIR)/Mindy.exe" 
LINK32_OBJS=

!ELSEIF  "$(CFG)" == "Compiler - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Compiler\Release"
# PROP BASE Intermediate_Dir "Compiler\Release"
# PROP BASE Target_Dir "Compiler"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Compiler\Release"
# PROP Intermediate_Dir "Compiler\Release"
# PROP Target_Dir "Compiler"
OUTDIR=.\Compiler\Release
INTDIR=.\Compiler\Release

ALL : "$(OUTDIR)\mindycomp.exe"

CLEAN : 
	-@erase ".\Compiler\Release\mindycomp.exe"
	-@erase ".\Compiler\Release\dump.obj"
	-@erase ".\Compiler\Release\feature.obj"
	-@erase ".\Compiler\Release\free.obj"
	-@erase ".\Compiler\Release\version.obj"
	-@erase ".\Compiler\Release\info.obj"
	-@erase ".\Compiler\Release\envanal.obj"
	-@erase ".\Compiler\Release\expand.obj"
	-@erase ".\Compiler\Release\literal.obj"
	-@erase ".\Compiler\Release\dup.obj"
	-@erase ".\Compiler\Release\mindycomp.obj"
	-@erase ".\Compiler\Release\print.obj"
	-@erase ".\Compiler\Release\sym.obj"
	-@erase ".\Compiler\Release\lexer-tab.obj"
	-@erase ".\Compiler\Release\lexenv.obj"
	-@erase ".\Compiler\Release\parser-tab.obj"
	-@erase ".\Compiler\Release\lose.obj"
	-@erase ".\Compiler\Release\header.obj"
	-@erase ".\Compiler\Release\src.obj"
	-@erase ".\Compiler\Release\compile.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /w /W0 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c
CPP_PROJ=/nologo /ML /w /W0 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /Fp"$(INTDIR)/Compiler.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Compiler\Release/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Compiler.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 oldnames.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"Compiler\Release/mindycomp.exe"
LINK32_FLAGS=oldnames.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/mindycomp.pdb" /machine:I386 /out:"$(OUTDIR)/mindycomp.exe" 
LINK32_OBJS= \
	"$(INTDIR)/dump.obj" \
	"$(INTDIR)/feature.obj" \
	"$(INTDIR)/free.obj" \
	"$(INTDIR)/version.obj" \
	"$(INTDIR)/info.obj" \
	"$(INTDIR)/envanal.obj" \
	"$(INTDIR)/expand.obj" \
	"$(INTDIR)/literal.obj" \
	"$(INTDIR)/dup.obj" \
	"$(INTDIR)/mindycomp.obj" \
	"$(INTDIR)/print.obj" \
	"$(INTDIR)/sym.obj" \
	"$(INTDIR)/lexer-tab.obj" \
	"$(INTDIR)/lexenv.obj" \
	"$(INTDIR)/parser-tab.obj" \
	"$(INTDIR)/lose.obj" \
	"$(INTDIR)/header.obj" \
	"$(INTDIR)/src.obj" \
	"$(INTDIR)/compile.obj"

"$(OUTDIR)\mindycomp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Compiler - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Compiler\Debug"
# PROP BASE Intermediate_Dir "Compiler\Debug"
# PROP BASE Target_Dir "Compiler"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Compiler\Debug"
# PROP Intermediate_Dir "Compiler\Debug"
# PROP Target_Dir "Compiler"
OUTDIR=.\Compiler\Debug
INTDIR=.\Compiler\Debug

ALL : "$(OUTDIR)\Compiler.exe"

CLEAN : 
	-@erase ".\Compiler\Debug\vc40.pdb"
	-@erase ".\Compiler\Debug\vc40.idb"
	-@erase ".\Compiler\Debug\Compiler.exe"
	-@erase ".\Compiler\Debug\dup.obj"
	-@erase ".\Compiler\Debug\print.obj"
	-@erase ".\Compiler\Debug\envanal.obj"
	-@erase ".\Compiler\Debug\header.obj"
	-@erase ".\Compiler\Debug\literal.obj"
	-@erase ".\Compiler\Debug\expand.obj"
	-@erase ".\Compiler\Debug\mindycomp.obj"
	-@erase ".\Compiler\Debug\parser-tab.obj"
	-@erase ".\Compiler\Debug\version.obj"
	-@erase ".\Compiler\Debug\lexer-tab.obj"
	-@erase ".\Compiler\Debug\lexenv.obj"
	-@erase ".\Compiler\Debug\dump.obj"
	-@erase ".\Compiler\Debug\sym.obj"
	-@erase ".\Compiler\Debug\free.obj"
	-@erase ".\Compiler\Debug\compile.obj"
	-@erase ".\Compiler\Debug\lose.obj"
	-@erase ".\Compiler\Debug\src.obj"
	-@erase ".\Compiler\Debug\info.obj"
	-@erase ".\Compiler\Debug\feature.obj"
	-@erase ".\Compiler\Debug\Compiler.ilk"
	-@erase ".\Compiler\Debug\Compiler.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/Compiler.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Compiler\Debug/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Compiler.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/Compiler.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/Compiler.exe" 
LINK32_OBJS= \
	"$(INTDIR)/dup.obj" \
	"$(INTDIR)/print.obj" \
	"$(INTDIR)/envanal.obj" \
	"$(INTDIR)/header.obj" \
	"$(INTDIR)/literal.obj" \
	"$(INTDIR)/expand.obj" \
	"$(INTDIR)/mindycomp.obj" \
	"$(INTDIR)/parser-tab.obj" \
	"$(INTDIR)/version.obj" \
	"$(INTDIR)/lexer-tab.obj" \
	"$(INTDIR)/lexenv.obj" \
	"$(INTDIR)/dump.obj" \
	"$(INTDIR)/sym.obj" \
	"$(INTDIR)/free.obj" \
	"$(INTDIR)/compile.obj" \
	"$(INTDIR)/lose.obj" \
	"$(INTDIR)/src.obj" \
	"$(INTDIR)/info.obj" \
	"$(INTDIR)/feature.obj"

"$(OUTDIR)\Compiler.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Interpretter - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Interpretter\Release"
# PROP BASE Intermediate_Dir "Interpretter\Release"
# PROP BASE Target_Dir "Interpretter"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Interpretter\Release"
# PROP Intermediate_Dir "Interpretter\Release"
# PROP Target_Dir "Interpretter"
OUTDIR=.\Interpretter\Release
INTDIR=.\Interpretter\Release

ALL : "$(OUTDIR)\Interpretter.exe"

CLEAN : 
	-@erase ".\Interpretter\Release\Interpretter.exe"
	-@erase ".\Interpretter\Release\num.obj"
	-@erase ".\Interpretter\Release\table.obj"
	-@erase ".\Interpretter\Release\handler.obj"
	-@erase ".\Interpretter\Release\sym.obj"
	-@erase ".\Interpretter\Release\nlx.obj"
	-@erase ".\Interpretter\Release\thread.obj"
	-@erase ".\Interpretter\Release\instance.obj"
	-@erase ".\Interpretter\Release\rint.obj"
	-@erase ".\Interpretter\Release\coll.obj"
	-@erase ".\Interpretter\Release\char.obj"
	-@erase ".\Interpretter\Release\func.obj"
	-@erase ".\Interpretter\Release\interp.obj"
	-@erase ".\Interpretter\Release\driver.obj"
	-@erase ".\Interpretter\Release\lose.obj"
	-@erase ".\Interpretter\Release\mindy.obj"
	-@erase ".\Interpretter\Release\brkpt.obj"
	-@erase ".\Interpretter\Release\init.obj"
	-@erase ".\Interpretter\Release\parser-tab.obj"
	-@erase ".\Interpretter\Release\debug.obj"
	-@erase ".\Interpretter\Release\ext-init.obj"
	-@erase ".\Interpretter\Release\error.obj"
	-@erase ".\Interpretter\Release\str.obj"
	-@erase ".\Interpretter\Release\print.obj"
	-@erase ".\Interpretter\Release\value.obj"
	-@erase ".\Interpretter\Release\list.obj"
	-@erase ".\Interpretter\Release\load.obj"
	-@erase ".\Interpretter\Release\lexer.obj"
	-@erase ".\Interpretter\Release\def.obj"
	-@erase ".\Interpretter\Release\weak.obj"
	-@erase ".\Interpretter\Release\class.obj"
	-@erase ".\Interpretter\Release\buf.obj"
	-@erase ".\Interpretter\Release\gc.obj"
	-@erase ".\Interpretter\Release\vec.obj"
	-@erase ".\Interpretter\Release\fd.obj"
	-@erase ".\Interpretter\Release\misc.obj"
	-@erase ".\Interpretter\Release\obj.obj"
	-@erase ".\Interpretter\Release\bool.obj"
	-@erase ".\Interpretter\Release\sigaction.obj"
	-@erase ".\Interpretter\Release\type.obj"
	-@erase ".\Interpretter\Release\input.obj"
	-@erase ".\Interpretter\Release\extern.obj"
	-@erase ".\Interpretter\Release\module.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /w /W0 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c
CPP_PROJ=/nologo /ML /w /W0 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /Fp"$(INTDIR)/Interpretter.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Interpretter\Release/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Interpretter.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/Interpretter.pdb" /machine:I386\
 /out:"$(OUTDIR)/Interpretter.exe" 
LINK32_OBJS= \
	"$(INTDIR)/num.obj" \
	"$(INTDIR)/table.obj" \
	"$(INTDIR)/handler.obj" \
	"$(INTDIR)/sym.obj" \
	"$(INTDIR)/nlx.obj" \
	"$(INTDIR)/thread.obj" \
	"$(INTDIR)/instance.obj" \
	"$(INTDIR)/rint.obj" \
	"$(INTDIR)/coll.obj" \
	"$(INTDIR)/char.obj" \
	"$(INTDIR)/func.obj" \
	"$(INTDIR)/interp.obj" \
	"$(INTDIR)/driver.obj" \
	"$(INTDIR)/lose.obj" \
	"$(INTDIR)/mindy.obj" \
	"$(INTDIR)/brkpt.obj" \
	"$(INTDIR)/init.obj" \
	"$(INTDIR)/parser-tab.obj" \
	"$(INTDIR)/debug.obj" \
	"$(INTDIR)/ext-init.obj" \
	"$(INTDIR)/error.obj" \
	"$(INTDIR)/str.obj" \
	"$(INTDIR)/print.obj" \
	"$(INTDIR)/value.obj" \
	"$(INTDIR)/list.obj" \
	"$(INTDIR)/load.obj" \
	"$(INTDIR)/lexer.obj" \
	"$(INTDIR)/def.obj" \
	"$(INTDIR)/weak.obj" \
	"$(INTDIR)/class.obj" \
	"$(INTDIR)/buf.obj" \
	"$(INTDIR)/gc.obj" \
	"$(INTDIR)/vec.obj" \
	"$(INTDIR)/fd.obj" \
	"$(INTDIR)/misc.obj" \
	"$(INTDIR)/obj.obj" \
	"$(INTDIR)/bool.obj" \
	"$(INTDIR)/sigaction.obj" \
	"$(INTDIR)/type.obj" \
	"$(INTDIR)/input.obj" \
	"$(INTDIR)/extern.obj" \
	"$(INTDIR)/module.obj"

"$(OUTDIR)\Interpretter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Interpretter\Debug"
# PROP BASE Intermediate_Dir "Interpretter\Debug"
# PROP BASE Target_Dir "Interpretter"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Interpretter\Debug"
# PROP Intermediate_Dir "Interpretter\Debug"
# PROP Target_Dir "Interpretter"
OUTDIR=.\Interpretter\Debug
INTDIR=.\Interpretter\Debug

ALL : "$(OUTDIR)\Interpretter.exe"

CLEAN : 
	-@erase ".\Interpretter\Debug\vc40.pdb"
	-@erase ".\Interpretter\Debug\vc40.idb"
	-@erase ".\Interpretter\Debug\Interpretter.exe"
	-@erase ".\Interpretter\Debug\driver.obj"
	-@erase ".\Interpretter\Debug\ext-init.obj"
	-@erase ".\Interpretter\Debug\list.obj"
	-@erase ".\Interpretter\Debug\gc.obj"
	-@erase ".\Interpretter\Debug\str.obj"
	-@erase ".\Interpretter\Debug\table.obj"
	-@erase ".\Interpretter\Debug\extern.obj"
	-@erase ".\Interpretter\Debug\fd.obj"
	-@erase ".\Interpretter\Debug\weak.obj"
	-@erase ".\Interpretter\Debug\module.obj"
	-@erase ".\Interpretter\Debug\rint.obj"
	-@erase ".\Interpretter\Debug\coll.obj"
	-@erase ".\Interpretter\Debug\mindy.obj"
	-@erase ".\Interpretter\Debug\def.obj"
	-@erase ".\Interpretter\Debug\misc.obj"
	-@erase ".\Interpretter\Debug\brkpt.obj"
	-@erase ".\Interpretter\Debug\init.obj"
	-@erase ".\Interpretter\Debug\thread.obj"
	-@erase ".\Interpretter\Debug\buf.obj"
	-@erase ".\Interpretter\Debug\parser-tab.obj"
	-@erase ".\Interpretter\Debug\vec.obj"
	-@erase ".\Interpretter\Debug\debug.obj"
	-@erase ".\Interpretter\Debug\interp.obj"
	-@erase ".\Interpretter\Debug\error.obj"
	-@erase ".\Interpretter\Debug\type.obj"
	-@erase ".\Interpretter\Debug\print.obj"
	-@erase ".\Interpretter\Debug\obj.obj"
	-@erase ".\Interpretter\Debug\value.obj"
	-@erase ".\Interpretter\Debug\load.obj"
	-@erase ".\Interpretter\Debug\lexer.obj"
	-@erase ".\Interpretter\Debug\class.obj"
	-@erase ".\Interpretter\Debug\handler.obj"
	-@erase ".\Interpretter\Debug\num.obj"
	-@erase ".\Interpretter\Debug\instance.obj"
	-@erase ".\Interpretter\Debug\char.obj"
	-@erase ".\Interpretter\Debug\func.obj"
	-@erase ".\Interpretter\Debug\lose.obj"
	-@erase ".\Interpretter\Debug\bool.obj"
	-@erase ".\Interpretter\Debug\sym.obj"
	-@erase ".\Interpretter\Debug\nlx.obj"
	-@erase ".\Interpretter\Debug\sigaction.obj"
	-@erase ".\Interpretter\Debug\input.obj"
	-@erase ".\Interpretter\Debug\Interpretter.ilk"
	-@erase ".\Interpretter\Debug\Interpretter.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/Interpretter.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Interpretter\Debug/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Interpretter.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/Interpretter.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/Interpretter.exe" 
LINK32_OBJS= \
	"$(INTDIR)/driver.obj" \
	"$(INTDIR)/ext-init.obj" \
	"$(INTDIR)/list.obj" \
	"$(INTDIR)/gc.obj" \
	"$(INTDIR)/str.obj" \
	"$(INTDIR)/table.obj" \
	"$(INTDIR)/extern.obj" \
	"$(INTDIR)/fd.obj" \
	"$(INTDIR)/weak.obj" \
	"$(INTDIR)/module.obj" \
	"$(INTDIR)/rint.obj" \
	"$(INTDIR)/coll.obj" \
	"$(INTDIR)/mindy.obj" \
	"$(INTDIR)/def.obj" \
	"$(INTDIR)/misc.obj" \
	"$(INTDIR)/brkpt.obj" \
	"$(INTDIR)/init.obj" \
	"$(INTDIR)/thread.obj" \
	"$(INTDIR)/buf.obj" \
	"$(INTDIR)/parser-tab.obj" \
	"$(INTDIR)/vec.obj" \
	"$(INTDIR)/debug.obj" \
	"$(INTDIR)/interp.obj" \
	"$(INTDIR)/error.obj" \
	"$(INTDIR)/type.obj" \
	"$(INTDIR)/print.obj" \
	"$(INTDIR)/obj.obj" \
	"$(INTDIR)/value.obj" \
	"$(INTDIR)/load.obj" \
	"$(INTDIR)/lexer.obj" \
	"$(INTDIR)/class.obj" \
	"$(INTDIR)/handler.obj" \
	"$(INTDIR)/num.obj" \
	"$(INTDIR)/instance.obj" \
	"$(INTDIR)/char.obj" \
	"$(INTDIR)/func.obj" \
	"$(INTDIR)/lose.obj" \
	"$(INTDIR)/bool.obj" \
	"$(INTDIR)/sym.obj" \
	"$(INTDIR)/nlx.obj" \
	"$(INTDIR)/sigaction.obj" \
	"$(INTDIR)/input.obj"

"$(OUTDIR)\Interpretter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "Shitpile - Win32 Release"
# Name "Shitpile - Win32 Debug"

!IF  "$(CFG)" == "Shitpile - Win32 Release"

!ELSEIF  "$(CFG)" == "Shitpile - Win32 Debug"

!ENDIF 

################################################################################
# Begin Project Dependency

# Project_Dep_Name "Compiler"

!IF  "$(CFG)" == "Shitpile - Win32 Release"

"Compiler - Win32 Release" : 
   $(MAKE) /$(MAKEFLAGS) /F .\Mindy.mak CFG="Compiler - Win32 Release" 

!ELSEIF  "$(CFG)" == "Shitpile - Win32 Debug"

"Compiler - Win32 Debug" : 
   $(MAKE) /$(MAKEFLAGS) /F .\Mindy.mak CFG="Compiler - Win32 Debug" 

!ENDIF 

# End Project Dependency
################################################################################
# Begin Project Dependency

# Project_Dep_Name "Interpretter"

!IF  "$(CFG)" == "Shitpile - Win32 Release"

"Interpretter - Win32 Release" : 
   $(MAKE) /$(MAKEFLAGS) /F .\Mindy.mak CFG="Interpretter - Win32 Release" 

!ELSEIF  "$(CFG)" == "Shitpile - Win32 Debug"

"Interpretter - Win32 Debug" : 
   $(MAKE) /$(MAKEFLAGS) /F .\Mindy.mak CFG="Interpretter - Win32 Debug" 

!ENDIF 

# End Project Dependency
# End Target
################################################################################
# Begin Target

# Name "Compiler - Win32 Release"
# Name "Compiler - Win32 Debug"

!IF  "$(CFG)" == "Compiler - Win32 Release"

!ELSEIF  "$(CFG)" == "Compiler - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\comp\version.c
DEP_CPP_VERSI=\
	".\comp\mindycomp.h"\
	".\comp\version.h"\
	

"$(INTDIR)\version.obj" : $(SOURCE) $(DEP_CPP_VERSI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\sym.c
DEP_CPP_SYM_C=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\sym.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\sym.obj" : $(SOURCE) $(DEP_CPP_SYM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\src.c
DEP_CPP_SRC_C=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\sym.h"\
	".\comp\lexer.h"\
	".\comp\literal.h"\
	".\comp\src.h"\
	".\comp\info.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\src.obj" : $(SOURCE) $(DEP_CPP_SRC_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\print.c
DEP_CPP_PRINT=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\comp\literal.h"\
	".\comp\print.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=".\comp\parser-tab.c"
DEP_CPP_PARSE=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\header.h"\
	".\comp\parser.h"\
	".\comp\lexer.h"\
	".\comp\feature.h"\
	".\comp\literal.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\parser-tab.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\mindycomp.c
DEP_CPP_MINDY=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\comp\mindycomp.h"\
	".\comp\parser.h"\
	".\comp\src.h"\
	".\comp\print.h"\
	".\comp\expand.h"\
	".\comp\envanal.h"\
	".\comp\lexer.h"\
	".\comp\header.h"\
	".\comp\sym.h"\
	".\comp\info.h"\
	".\comp\compile.h"\
	".\comp\dump.h"\
	".\comp\feature.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\mindycomp.obj" : $(SOURCE) $(DEP_CPP_MINDY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\lose.c
DEP_CPP_LOSE_=\
	".\compat\std-c.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lose.obj" : $(SOURCE) $(DEP_CPP_LOSE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\literal.c
DEP_CPP_LITER=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\literal.obj" : $(SOURCE) $(DEP_CPP_LITER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=".\comp\lexer-tab.c"
DEP_CPP_LEXER=\
	".\comp\lexer.h"\
	".\comp\src.h"\
	".\comp\parser-tab.h"\
	

"$(INTDIR)\lexer-tab.obj" : $(SOURCE) $(DEP_CPP_LEXER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\lexenv.c
DEP_CPP_LEXEN=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\lexenv.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lexenv.obj" : $(SOURCE) $(DEP_CPP_LEXEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\info.c
DEP_CPP_INFO_=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\comp\info.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\info.obj" : $(SOURCE) $(DEP_CPP_INFO_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\header.c
DEP_CPP_HEADE=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\header.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\header.obj" : $(SOURCE) $(DEP_CPP_HEADE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\free.c
DEP_CPP_FREE_=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\literal.h"\
	".\comp\free.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\free.obj" : $(SOURCE) $(DEP_CPP_FREE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\feature.c
DEP_CPP_FEATU=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\feature.h"\
	".\comp\lexer.h"\
	".\comp\src.h"\
	".\comp\parser-tab.h"\
	".\comp\sym.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\feature.obj" : $(SOURCE) $(DEP_CPP_FEATU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\expand.c
DEP_CPP_EXPAN=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\literal.h"\
	".\comp\dup.h"\
	".\comp\free.h"\
	".\comp\sym.h"\
	".\comp\expand.h"\
	".\comp\info.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\expand.obj" : $(SOURCE) $(DEP_CPP_EXPAN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\envanal.c
DEP_CPP_ENVAN=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\lexenv.h"\
	".\comp\envanal.h"\
	".\comp\sym.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\envanal.obj" : $(SOURCE) $(DEP_CPP_ENVAN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\dup.c
DEP_CPP_DUP_C=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\literal.h"\
	".\comp\free.h"\
	".\comp\lose.h"\
	".\comp\dup.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\dup.obj" : $(SOURCE) $(DEP_CPP_DUP_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\dump.c
DEP_CPP_DUMP_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\literal.h"\
	".\comp\sym.h"\
	".\comp\fileops.h"\
	".\comp\compile.h"\
	".\comp\dump.h"\
	".\comp\version.h"\
	".\comp\envanal.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\dump.obj" : $(SOURCE) $(DEP_CPP_DUMP_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\compile.c
DEP_CPP_COMPI=\
	".\compat\std-c.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\dump.h"\
	".\comp\lexenv.h"\
	".\comp\envanal.h"\
	".\comp\sym.h"\
	".\comp\literal.h"\
	".\comp\compile.h"\
	".\comp\byteops.h"\
	".\comp\info.h"\
	".\comp\lose.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\compile.obj" : $(SOURCE) $(DEP_CPP_COMPI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "Interpretter - Win32 Release"
# Name "Interpretter - Win32 Debug"

!IF  "$(CFG)" == "Interpretter - Win32 Release"

!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\interp\weak.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_WEAK_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\type.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\module.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\weak.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\weak.obj" : $(SOURCE) $(DEP_CPP_WEAK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_WEAK_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\type.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\module.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\weak.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\weak.obj" : $(SOURCE) $(DEP_CPP_WEAK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\vec.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_VEC_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\print.h"\
	".\interp\def.h"\
	".\interp\vec.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\vec.obj" : $(SOURCE) $(DEP_CPP_VEC_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_VEC_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\print.h"\
	".\interp\def.h"\
	".\interp\vec.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\vec.obj" : $(SOURCE) $(DEP_CPP_VEC_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\value.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_VALUE=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\class.h"\
	".\interp\value.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\value.obj" : $(SOURCE) $(DEP_CPP_VALUE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_VALUE=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\class.h"\
	".\interp\value.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\value.obj" : $(SOURCE) $(DEP_CPP_VALUE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\type.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_TYPE_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\type.h"\
	".\interp\class.h"\
	".\interp\num.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\print.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_TYPE_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\type.h"\
	".\interp\class.h"\
	".\interp\num.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\print.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\thread.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_THREA=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\thread.h"\
	".\interp\obj.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\num.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\thread.obj" : $(SOURCE) $(DEP_CPP_THREA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_THREA=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\thread.h"\
	".\interp\obj.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\num.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\thread.obj" : $(SOURCE) $(DEP_CPP_THREA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\table.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_TABLE=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\print.h"\
	".\interp\table.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\table.obj" : $(SOURCE) $(DEP_CPP_TABLE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_TABLE=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\print.h"\
	".\interp\table.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\table.obj" : $(SOURCE) $(DEP_CPP_TABLE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\sym.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_SYM_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\coll.h"\
	".\interp\str.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\sym.obj" : $(SOURCE) $(DEP_CPP_SYM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_SYM_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\coll.h"\
	".\interp\str.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\sym.obj" : $(SOURCE) $(DEP_CPP_SYM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\str.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_STR_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\char.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\type.h"\
	".\interp\print.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\str.obj" : $(SOURCE) $(DEP_CPP_STR_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_STR_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\char.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\type.h"\
	".\interp\print.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\str.obj" : $(SOURCE) $(DEP_CPP_STR_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\print.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_PRINT=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\class.h"\
	".\interp\print.h"\
	".\interp\vec.h"\
	".\interp\char.h"\
	".\interp\str.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\type.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_PRINT=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\class.h"\
	".\interp\print.h"\
	".\interp\vec.h"\
	".\interp\char.h"\
	".\interp\str.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\type.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\interp\parser-tab.c"

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_PARSE=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\lexer.h"\
	".\interp\parser.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\interp\parser-tab.h"\
	

"$(INTDIR)\parser-tab.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_PARSE=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\lexer.h"\
	".\interp\parser.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\compat\std-limits.h"\
	".\interp\parser-tab.h"\
	

"$(INTDIR)\parser-tab.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\obj.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_OBJ_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\obj.obj" : $(SOURCE) $(DEP_CPP_OBJ_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_OBJ_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\obj.obj" : $(SOURCE) $(DEP_CPP_OBJ_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\num.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_NUM_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\print.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\num.obj" : $(SOURCE) $(DEP_CPP_NUM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_NUM_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\print.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\num.obj" : $(SOURCE) $(DEP_CPP_NUM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\nlx.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_NLX_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\nlx.obj" : $(SOURCE) $(DEP_CPP_NLX_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_NLX_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\nlx.obj" : $(SOURCE) $(DEP_CPP_NLX_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\module.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_MODUL=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\sym.h"\
	".\interp\str.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\class.h"\
	".\interp\type.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\load.h"\
	".\interp\print.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\module.obj" : $(SOURCE) $(DEP_CPP_MODUL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_MODUL=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\sym.h"\
	".\interp\str.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\class.h"\
	".\interp\type.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\load.h"\
	".\interp\print.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\module.obj" : $(SOURCE) $(DEP_CPP_MODUL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\misc.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_MISC_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\vec.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\def.h"\
	".\interp\num.h"\
	".\interp\str.h"\
	".\interp\coll.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_MISC_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\vec.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\def.h"\
	".\interp\num.h"\
	".\interp\str.h"\
	".\interp\coll.h"\
	".\compat\std-limits.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\mindy.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_MINDY_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\init.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\func.h"\
	".\interp\debug.h"\
	".\interp\load.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\mindy.obj" : $(SOURCE) $(DEP_CPP_MINDY_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_MINDY_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\init.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\func.h"\
	".\interp\debug.h"\
	".\interp\load.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\mindy.obj" : $(SOURCE) $(DEP_CPP_MINDY_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\lose.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LOSE_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lose.obj" : $(SOURCE) $(DEP_CPP_LOSE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LOSE_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\lose.obj" : $(SOURCE) $(DEP_CPP_LOSE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\load.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LOAD_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\interp.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\char.h"\
	".\interp\driver.h"\
	".\interp\debug.h"\
	".\interp\instance.h"\
	".\interp\vec.h"\
	".\interp\def.h"\
	".\comp\fileops.h"\
	".\interp\load.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\load.obj" : $(SOURCE) $(DEP_CPP_LOAD_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LOAD_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\interp.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\char.h"\
	".\interp\driver.h"\
	".\interp\debug.h"\
	".\interp\instance.h"\
	".\interp\vec.h"\
	".\interp\def.h"\
	".\comp\fileops.h"\
	".\interp\load.h"\
	".\compat\std-limits.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\load.obj" : $(SOURCE) $(DEP_CPP_LOAD_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\list.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LIST_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\print.h"\
	".\interp\type.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\list.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LIST_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\print.h"\
	".\interp\type.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\list.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\lexer.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LEXER_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\lexer.h"\
	".\interp\parser.h"\
	".\interp\char.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	".\interp\parser-tab.h"\
	

"$(INTDIR)\lexer.obj" : $(SOURCE) $(DEP_CPP_LEXER_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LEXER_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\lexer.h"\
	".\interp\parser.h"\
	".\interp\char.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\compat\std-limits.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	".\interp\parser-tab.h"\
	

"$(INTDIR)\lexer.obj" : $(SOURCE) $(DEP_CPP_LEXER_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\interp.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_INTER=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\value.h"\
	".\interp\num.h"\
	".\interp\vec.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\brkpt.h"\
	".\interp\interp.h"\
	".\comp\byteops.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\interp.obj" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_INTER=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\value.h"\
	".\interp\num.h"\
	".\interp\vec.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\brkpt.h"\
	".\interp\interp.h"\
	".\comp\byteops.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\interp.obj" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\instance.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_INSTA=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\class.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\sym.h"\
	".\interp\value.h"\
	".\interp\driver.h"\
	".\interp\def.h"\
	".\interp\print.h"\
	".\interp\instance.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\instance.obj" : $(SOURCE) $(DEP_CPP_INSTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_INSTA=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\class.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\sym.h"\
	".\interp\value.h"\
	".\interp\driver.h"\
	".\interp\def.h"\
	".\interp\print.h"\
	".\interp\instance.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\instance.obj" : $(SOURCE) $(DEP_CPP_INSTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\input.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_INPUT=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\char.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\interp\def.h"\
	".\interp\fd.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_INPUT=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\char.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\interp\def.h"\
	".\interp\fd.h"\
	".\compat\std-limits.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\init.c
DEP_CPP_INIT_=\
	".\interp\init.h"\
	

"$(INTDIR)\init.obj" : $(SOURCE) $(DEP_CPP_INIT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\handler.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_HANDL=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\handler.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\handler.obj" : $(SOURCE) $(DEP_CPP_HANDL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_HANDL=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\handler.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\handler.obj" : $(SOURCE) $(DEP_CPP_HANDL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\gc.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_GC_C50=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\gc.h"\
	".\interp\weak.h"\
	".\interp\table.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\str.h"\
	".\interp\obj.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\gc.obj" : $(SOURCE) $(DEP_CPP_GC_C50) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_GC_C50=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\gc.h"\
	".\interp\weak.h"\
	".\interp\table.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\def.h"\
	".\interp\str.h"\
	".\interp\obj.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\gc.obj" : $(SOURCE) $(DEP_CPP_GC_C50) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\func.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_FUNC_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\num.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\interp.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\module.h"\
	".\interp\print.h"\
	".\interp\driver.h"\
	".\interp\def.h"\
	".\interp\extern.h"\
	".\interp\coll.h"\
	".\interp\func.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\func.obj" : $(SOURCE) $(DEP_CPP_FUNC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_FUNC_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\num.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\interp.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\module.h"\
	".\interp\print.h"\
	".\interp\driver.h"\
	".\interp\def.h"\
	".\interp\extern.h"\
	".\interp\coll.h"\
	".\interp\func.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\func.obj" : $(SOURCE) $(DEP_CPP_FUNC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\fd.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_FD_C54=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\interp\buf.h"\
	".\interp\str.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\fd.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\fd.obj" : $(SOURCE) $(DEP_CPP_FD_C54) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_FD_C54=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\interp\buf.h"\
	".\interp\str.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\fd.h"\
	".\compat\std-limits.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\fd.obj" : $(SOURCE) $(DEP_CPP_FD_C54) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\interp\ext-init.c"
DEP_CPP_EXT_I=\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\extern.h"\
	".\interp\extern1.def"\
	".\interp\extern2.def"\
	

!IF  "$(CFG)" == "Interpretter - Win32 Release"

# ADD CPP /Od

"$(INTDIR)\ext-init.obj" : $(SOURCE) $(DEP_CPP_EXT_I) "$(INTDIR)"
   $(CPP) /nologo /ML /w /W0 /GX /Od /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /Fp"$(INTDIR)/Interpretter.pch" /YX\
 /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"


"$(INTDIR)\ext-init.obj" : $(SOURCE) $(DEP_CPP_EXT_I) "$(INTDIR)"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/Interpretter.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\extern.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_EXTER=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\char.h"\
	".\interp\type.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\module.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\extern.h"\
	".\interp\num.h"\
	".\interp\str.h"\
	".\interp\print.h"\
	".\interp\coll.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\extern.obj" : $(SOURCE) $(DEP_CPP_EXTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_EXTER=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\obj.h"\
	".\interp\char.h"\
	".\interp\type.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\module.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\extern.h"\
	".\interp\num.h"\
	".\interp\str.h"\
	".\interp\print.h"\
	".\interp\coll.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\extern.obj" : $(SOURCE) $(DEP_CPP_EXTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\error.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_ERROR=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\str.h"\
	".\interp\thread.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\def.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_ERROR=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\str.h"\
	".\interp\thread.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\def.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\driver.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_DRIVE=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\interp.h"\
	".\interp\fd.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\driver.obj" : $(SOURCE) $(DEP_CPP_DRIVE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_DRIVE=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\interp.h"\
	".\interp\fd.h"\
	".\compat\std-limits.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\driver.obj" : $(SOURCE) $(DEP_CPP_DRIVE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\def.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_DEF_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\interp\instance.h"\
	".\interp\class.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\def.obj" : $(SOURCE) $(DEP_CPP_DEF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_DEF_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\interp\instance.h"\
	".\interp\class.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\def.obj" : $(SOURCE) $(DEP_CPP_DEF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\debug.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_DEBUG=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\interp.h"\
	".\interp\value.h"\
	".\interp\gc.h"\
	".\interp\brkpt.h"\
	".\interp\instance.h"\
	".\comp\byteops.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_DEBUG=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\interp.h"\
	".\interp\value.h"\
	".\interp\gc.h"\
	".\interp\brkpt.h"\
	".\interp\instance.h"\
	".\comp\byteops.h"\
	".\compat\std-limits.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"\
	

"$(INTDIR)\debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\coll.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_COLL_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\coll.obj" : $(SOURCE) $(DEP_CPP_COLL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_COLL_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\coll.obj" : $(SOURCE) $(DEP_CPP_COLL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\class.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_CLASS=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\type.h"\
	".\interp\sym.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\print.h"\
	".\interp\class.h"\
	".\interp\extern.h"\
	".\interp\instance.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\class.obj" : $(SOURCE) $(DEP_CPP_CLASS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_CLASS=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\type.h"\
	".\interp\sym.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\print.h"\
	".\interp\class.h"\
	".\interp\extern.h"\
	".\interp\instance.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\class.obj" : $(SOURCE) $(DEP_CPP_CLASS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\char.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_CHAR_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\num.h"\
	".\interp\print.h"\
	".\interp\type.h"\
	".\interp\def.h"\
	".\interp\char.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_CHAR_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\num.h"\
	".\interp\print.h"\
	".\interp\type.h"\
	".\interp\def.h"\
	".\interp\char.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\buf.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_BUF_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	".\interp\str.h"\
	".\interp\buf.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\buf.obj" : $(SOURCE) $(DEP_CPP_BUF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_BUF_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	".\interp\str.h"\
	".\interp\buf.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\buf.obj" : $(SOURCE) $(DEP_CPP_BUF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\brkpt.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_BRKPT=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\weak.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\print.h"\
	".\comp\byteops.h"\
	".\interp\brkpt.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\brkpt.obj" : $(SOURCE) $(DEP_CPP_BRKPT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_BRKPT=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\weak.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\print.h"\
	".\comp\byteops.h"\
	".\interp\brkpt.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\brkpt.obj" : $(SOURCE) $(DEP_CPP_BRKPT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\bool.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_BOOL_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\print.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\bool.obj" : $(SOURCE) $(DEP_CPP_BOOL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_BOOL_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\print.h"\
	".\interp\gc.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	

"$(INTDIR)\bool.obj" : $(SOURCE) $(DEP_CPP_BOOL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\rint.c

"$(INTDIR)\rint.obj" : $(SOURCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\sigaction.c
DEP_CPP_SIGAC=\
	".\compat\std-signal.h"\
	

"$(INTDIR)\sigaction.obj" : $(SOURCE) $(DEP_CPP_SIGAC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
