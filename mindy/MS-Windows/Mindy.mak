# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
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
CPP=cl.exe
RSC=rc.exe

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
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Mindy.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/Mindy.pdb" /machine:I386 /out:"$(OUTDIR)/Mindy.exe" 
LINK32_OBJS= \
	

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
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Mindy.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/Mindy.pdb" /debug /machine:I386 /out:"$(OUTDIR)/Mindy.exe" 
LINK32_OBJS= \
	

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
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\dump.obj"
	-@erase "$(INTDIR)\dup.obj"
	-@erase "$(INTDIR)\envanal.obj"
	-@erase "$(INTDIR)\expand.obj"
	-@erase "$(INTDIR)\feature.obj"
	-@erase "$(INTDIR)\free.obj"
	-@erase "$(INTDIR)\header.obj"
	-@erase "$(INTDIR)\info.obj"
	-@erase "$(INTDIR)\lexenv.obj"
	-@erase "$(INTDIR)\lexer-tab.obj"
	-@erase "$(INTDIR)\literal.obj"
	-@erase "$(INTDIR)\lose.obj"
	-@erase "$(INTDIR)\mindycomp.obj"
	-@erase "$(INTDIR)\parser-tab.obj"
	-@erase "$(INTDIR)\print.obj"
	-@erase "$(INTDIR)\src.obj"
	-@erase "$(INTDIR)\sym.obj"
	-@erase "$(INTDIR)\version.obj"
	-@erase "$(OUTDIR)\mindycomp.exe"

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
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Compiler.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 oldnames.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"Compiler\Release/mindycomp.exe"
LINK32_FLAGS=oldnames.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/mindycomp.pdb" /machine:I386 /out:"$(OUTDIR)/mindycomp.exe" 
LINK32_OBJS= \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\dump.obj" \
	"$(INTDIR)\dup.obj" \
	"$(INTDIR)\envanal.obj" \
	"$(INTDIR)\expand.obj" \
	"$(INTDIR)\feature.obj" \
	"$(INTDIR)\free.obj" \
	"$(INTDIR)\header.obj" \
	"$(INTDIR)\info.obj" \
	"$(INTDIR)\lexenv.obj" \
	"$(INTDIR)\lexer-tab.obj" \
	"$(INTDIR)\literal.obj" \
	"$(INTDIR)\lose.obj" \
	"$(INTDIR)\mindycomp.obj" \
	"$(INTDIR)\parser-tab.obj" \
	"$(INTDIR)\print.obj" \
	"$(INTDIR)\src.obj" \
	"$(INTDIR)\sym.obj" \
	"$(INTDIR)\version.obj"

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
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\dump.obj"
	-@erase "$(INTDIR)\dup.obj"
	-@erase "$(INTDIR)\envanal.obj"
	-@erase "$(INTDIR)\expand.obj"
	-@erase "$(INTDIR)\feature.obj"
	-@erase "$(INTDIR)\free.obj"
	-@erase "$(INTDIR)\header.obj"
	-@erase "$(INTDIR)\info.obj"
	-@erase "$(INTDIR)\lexenv.obj"
	-@erase "$(INTDIR)\lexer-tab.obj"
	-@erase "$(INTDIR)\literal.obj"
	-@erase "$(INTDIR)\lose.obj"
	-@erase "$(INTDIR)\mindycomp.obj"
	-@erase "$(INTDIR)\parser-tab.obj"
	-@erase "$(INTDIR)\print.obj"
	-@erase "$(INTDIR)\src.obj"
	-@erase "$(INTDIR)\sym.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\version.obj"
	-@erase "$(OUTDIR)\Compiler.exe"
	-@erase "$(OUTDIR)\Compiler.ilk"
	-@erase "$(OUTDIR)\Compiler.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE"\
 /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /Fp"$(INTDIR)/Compiler.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Compiler\Debug/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Compiler.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/Compiler.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/Compiler.exe" 
LINK32_OBJS= \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\dump.obj" \
	"$(INTDIR)\dup.obj" \
	"$(INTDIR)\envanal.obj" \
	"$(INTDIR)\expand.obj" \
	"$(INTDIR)\feature.obj" \
	"$(INTDIR)\free.obj" \
	"$(INTDIR)\header.obj" \
	"$(INTDIR)\info.obj" \
	"$(INTDIR)\lexenv.obj" \
	"$(INTDIR)\lexer-tab.obj" \
	"$(INTDIR)\literal.obj" \
	"$(INTDIR)\lose.obj" \
	"$(INTDIR)\mindycomp.obj" \
	"$(INTDIR)\parser-tab.obj" \
	"$(INTDIR)\print.obj" \
	"$(INTDIR)\src.obj" \
	"$(INTDIR)\sym.obj" \
	"$(INTDIR)\version.obj"

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
	-@erase "$(INTDIR)\bool.obj"
	-@erase "$(INTDIR)\brkpt.obj"
	-@erase "$(INTDIR)\buf.obj"
	-@erase "$(INTDIR)\char.obj"
	-@erase "$(INTDIR)\class.obj"
	-@erase "$(INTDIR)\coll.obj"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\def.obj"
	-@erase "$(INTDIR)\driver.obj"
	-@erase "$(INTDIR)\error.obj"
	-@erase "$(INTDIR)\ext-init.obj"
	-@erase "$(INTDIR)\extern.obj"
	-@erase "$(INTDIR)\fd.obj"
	-@erase "$(INTDIR)\func.obj"
	-@erase "$(INTDIR)\gc.obj"
	-@erase "$(INTDIR)\handler.obj"
	-@erase "$(INTDIR)\init.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(INTDIR)\instance.obj"
	-@erase "$(INTDIR)\interp.obj"
	-@erase "$(INTDIR)\lexer.obj"
	-@erase "$(INTDIR)\list.obj"
	-@erase "$(INTDIR)\load.obj"
	-@erase "$(INTDIR)\lose.obj"
	-@erase "$(INTDIR)\mindy.obj"
	-@erase "$(INTDIR)\misc.obj"
	-@erase "$(INTDIR)\module.obj"
	-@erase "$(INTDIR)\nlx.obj"
	-@erase "$(INTDIR)\num.obj"
	-@erase "$(INTDIR)\obj.obj"
	-@erase "$(INTDIR)\parser-tab.obj"
	-@erase "$(INTDIR)\print.obj"
	-@erase "$(INTDIR)\rint.obj"
	-@erase "$(INTDIR)\sigaction.obj"
	-@erase "$(INTDIR)\str.obj"
	-@erase "$(INTDIR)\sym.obj"
	-@erase "$(INTDIR)\table.obj"
	-@erase "$(INTDIR)\thread.obj"
	-@erase "$(INTDIR)\type.obj"
	-@erase "$(INTDIR)\value.obj"
	-@erase "$(INTDIR)\vec.obj"
	-@erase "$(INTDIR)\weak.obj"
	-@erase "$(OUTDIR)\Interpretter.exe"

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
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Interpretter.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/Interpretter.pdb" /machine:I386\
 /out:"$(OUTDIR)/Interpretter.exe" 
LINK32_OBJS= \
	"$(INTDIR)\bool.obj" \
	"$(INTDIR)\brkpt.obj" \
	"$(INTDIR)\buf.obj" \
	"$(INTDIR)\char.obj" \
	"$(INTDIR)\class.obj" \
	"$(INTDIR)\coll.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\def.obj" \
	"$(INTDIR)\driver.obj" \
	"$(INTDIR)\error.obj" \
	"$(INTDIR)\ext-init.obj" \
	"$(INTDIR)\extern.obj" \
	"$(INTDIR)\fd.obj" \
	"$(INTDIR)\func.obj" \
	"$(INTDIR)\gc.obj" \
	"$(INTDIR)\handler.obj" \
	"$(INTDIR)\init.obj" \
	"$(INTDIR)\input.obj" \
	"$(INTDIR)\instance.obj" \
	"$(INTDIR)\interp.obj" \
	"$(INTDIR)\lexer.obj" \
	"$(INTDIR)\list.obj" \
	"$(INTDIR)\load.obj" \
	"$(INTDIR)\lose.obj" \
	"$(INTDIR)\mindy.obj" \
	"$(INTDIR)\misc.obj" \
	"$(INTDIR)\module.obj" \
	"$(INTDIR)\nlx.obj" \
	"$(INTDIR)\num.obj" \
	"$(INTDIR)\obj.obj" \
	"$(INTDIR)\parser-tab.obj" \
	"$(INTDIR)\print.obj" \
	"$(INTDIR)\rint.obj" \
	"$(INTDIR)\sigaction.obj" \
	"$(INTDIR)\str.obj" \
	"$(INTDIR)\sym.obj" \
	"$(INTDIR)\table.obj" \
	"$(INTDIR)\thread.obj" \
	"$(INTDIR)\type.obj" \
	"$(INTDIR)\value.obj" \
	"$(INTDIR)\vec.obj" \
	"$(INTDIR)\weak.obj"

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
	-@erase "$(INTDIR)\bool.obj"
	-@erase "$(INTDIR)\brkpt.obj"
	-@erase "$(INTDIR)\buf.obj"
	-@erase "$(INTDIR)\char.obj"
	-@erase "$(INTDIR)\class.obj"
	-@erase "$(INTDIR)\coll.obj"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\def.obj"
	-@erase "$(INTDIR)\driver.obj"
	-@erase "$(INTDIR)\error.obj"
	-@erase "$(INTDIR)\ext-init.obj"
	-@erase "$(INTDIR)\extern.obj"
	-@erase "$(INTDIR)\fd.obj"
	-@erase "$(INTDIR)\func.obj"
	-@erase "$(INTDIR)\gc.obj"
	-@erase "$(INTDIR)\handler.obj"
	-@erase "$(INTDIR)\init.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(INTDIR)\instance.obj"
	-@erase "$(INTDIR)\interp.obj"
	-@erase "$(INTDIR)\lexer.obj"
	-@erase "$(INTDIR)\list.obj"
	-@erase "$(INTDIR)\load.obj"
	-@erase "$(INTDIR)\lose.obj"
	-@erase "$(INTDIR)\mindy.obj"
	-@erase "$(INTDIR)\misc.obj"
	-@erase "$(INTDIR)\module.obj"
	-@erase "$(INTDIR)\nlx.obj"
	-@erase "$(INTDIR)\num.obj"
	-@erase "$(INTDIR)\obj.obj"
	-@erase "$(INTDIR)\parser-tab.obj"
	-@erase "$(INTDIR)\print.obj"
	-@erase "$(INTDIR)\rint.obj"
	-@erase "$(INTDIR)\sigaction.obj"
	-@erase "$(INTDIR)\str.obj"
	-@erase "$(INTDIR)\sym.obj"
	-@erase "$(INTDIR)\table.obj"
	-@erase "$(INTDIR)\thread.obj"
	-@erase "$(INTDIR)\type.obj"
	-@erase "$(INTDIR)\value.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\vec.obj"
	-@erase "$(INTDIR)\weak.obj"
	-@erase "$(OUTDIR)\Interpretter.exe"
	-@erase "$(OUTDIR)\Interpretter.ilk"
	-@erase "$(OUTDIR)\Interpretter.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /Fp"$(INTDIR)/Interpretter.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Interpretter\Debug/
CPP_SBRS=.\.
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Interpretter.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/Interpretter.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/Interpretter.exe" 
LINK32_OBJS= \
	"$(INTDIR)\bool.obj" \
	"$(INTDIR)\brkpt.obj" \
	"$(INTDIR)\buf.obj" \
	"$(INTDIR)\char.obj" \
	"$(INTDIR)\class.obj" \
	"$(INTDIR)\coll.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\def.obj" \
	"$(INTDIR)\driver.obj" \
	"$(INTDIR)\error.obj" \
	"$(INTDIR)\ext-init.obj" \
	"$(INTDIR)\extern.obj" \
	"$(INTDIR)\fd.obj" \
	"$(INTDIR)\func.obj" \
	"$(INTDIR)\gc.obj" \
	"$(INTDIR)\handler.obj" \
	"$(INTDIR)\init.obj" \
	"$(INTDIR)\input.obj" \
	"$(INTDIR)\instance.obj" \
	"$(INTDIR)\interp.obj" \
	"$(INTDIR)\lexer.obj" \
	"$(INTDIR)\list.obj" \
	"$(INTDIR)\load.obj" \
	"$(INTDIR)\lose.obj" \
	"$(INTDIR)\mindy.obj" \
	"$(INTDIR)\misc.obj" \
	"$(INTDIR)\module.obj" \
	"$(INTDIR)\nlx.obj" \
	"$(INTDIR)\num.obj" \
	"$(INTDIR)\obj.obj" \
	"$(INTDIR)\parser-tab.obj" \
	"$(INTDIR)\print.obj" \
	"$(INTDIR)\rint.obj" \
	"$(INTDIR)\sigaction.obj" \
	"$(INTDIR)\str.obj" \
	"$(INTDIR)\sym.obj" \
	"$(INTDIR)\table.obj" \
	"$(INTDIR)\thread.obj" \
	"$(INTDIR)\type.obj" \
	"$(INTDIR)\value.obj" \
	"$(INTDIR)\vec.obj" \
	"$(INTDIR)\weak.obj"

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
   $(MAKE) /$(MAKEFLAGS) /F ".\Mindy.mak" CFG="Compiler - Win32 Release" 

!ELSEIF  "$(CFG)" == "Shitpile - Win32 Debug"

"Compiler - Win32 Debug" : 
   $(MAKE) /$(MAKEFLAGS) /F ".\Mindy.mak" CFG="Compiler - Win32 Debug" 

!ENDIF 

# End Project Dependency
################################################################################
# Begin Project Dependency

# Project_Dep_Name "Interpretter"

!IF  "$(CFG)" == "Shitpile - Win32 Release"

"Interpretter - Win32 Release" : 
   $(MAKE) /$(MAKEFLAGS) /F ".\Mindy.mak" CFG="Interpretter - Win32 Release" 

!ELSEIF  "$(CFG)" == "Shitpile - Win32 Debug"

"Interpretter - Win32 Debug" : 
   $(MAKE) /$(MAKEFLAGS) /F ".\Mindy.mak" CFG="Interpretter - Win32 Debug" 

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
	".\comp\mindycomp.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\sym.obj" : $(SOURCE) $(DEP_CPP_SYM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\src.c
DEP_CPP_SRC_C=\
	".\comp\info.h"\
	".\comp\lexer.h"\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\src.obj" : $(SOURCE) $(DEP_CPP_SRC_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\print.c
DEP_CPP_PRINT=\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\print.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=".\comp\parser-tab.c"
DEP_CPP_PARSE=\
	".\comp\feature.h"\
	".\comp\header.h"\
	".\comp\lexer.h"\
	".\comp\literal.h"\
	".\comp\mindycomp.h"\
	".\comp\parser.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\parser-tab.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\mindycomp.c
DEP_CPP_MINDY=\
	".\comp\compile.h"\
	".\comp\dump.h"\
	".\comp\envanal.h"\
	".\comp\expand.h"\
	".\comp\feature.h"\
	".\comp\header.h"\
	".\comp\info.h"\
	".\comp\lexer.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\parser.h"\
	".\comp\print.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\mindycomp.obj" : $(SOURCE) $(DEP_CPP_MINDY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\lose.c
DEP_CPP_LOSE_=\
	".\comp\lose.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lose.obj" : $(SOURCE) $(DEP_CPP_LOSE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\literal.c
DEP_CPP_LITER=\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\literal.obj" : $(SOURCE) $(DEP_CPP_LITER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=".\comp\lexer-tab.c"
DEP_CPP_LEXER=\
	".\comp\lexer.h"\
	".\comp\parser-tab.h"\
	".\comp\src.h"\
	

"$(INTDIR)\lexer-tab.obj" : $(SOURCE) $(DEP_CPP_LEXER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\lexenv.c
DEP_CPP_LEXEN=\
	".\comp\lexenv.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lexenv.obj" : $(SOURCE) $(DEP_CPP_LEXEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\info.c
DEP_CPP_INFO_=\
	".\comp\info.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\info.obj" : $(SOURCE) $(DEP_CPP_INFO_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\header.c
DEP_CPP_HEADE=\
	".\comp\header.h"\
	".\comp\mindycomp.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\header.obj" : $(SOURCE) $(DEP_CPP_HEADE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\free.c
DEP_CPP_FREE_=\
	".\comp\free.h"\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\free.obj" : $(SOURCE) $(DEP_CPP_FREE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\feature.c
DEP_CPP_FEATU=\
	".\comp\feature.h"\
	".\comp\lexer.h"\
	".\comp\mindycomp.h"\
	".\comp\parser-tab.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\feature.obj" : $(SOURCE) $(DEP_CPP_FEATU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\expand.c
DEP_CPP_EXPAN=\
	".\comp\dup.h"\
	".\comp\expand.h"\
	".\comp\free.h"\
	".\comp\info.h"\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\expand.obj" : $(SOURCE) $(DEP_CPP_EXPAN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\envanal.c
DEP_CPP_ENVAN=\
	".\comp\envanal.h"\
	".\comp\lexenv.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\envanal.obj" : $(SOURCE) $(DEP_CPP_ENVAN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\dup.c
DEP_CPP_DUP_C=\
	".\comp\dup.h"\
	".\comp\free.h"\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\dup.obj" : $(SOURCE) $(DEP_CPP_DUP_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\dump.c
DEP_CPP_DUMP_=\
	".\comp\compile.h"\
	".\comp\dump.h"\
	".\comp\envanal.h"\
	".\comp\fileops.h"\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\comp\version.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\dump.obj" : $(SOURCE) $(DEP_CPP_DUMP_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\compile.c
DEP_CPP_COMPI=\
	".\comp\byteops.h"\
	".\comp\compile.h"\
	".\comp\dump.h"\
	".\comp\envanal.h"\
	".\comp\info.h"\
	".\comp\lexenv.h"\
	".\comp\literal.h"\
	".\comp\lose.h"\
	".\comp\mindycomp.h"\
	".\comp\src.h"\
	".\comp\sym.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
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
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\weak.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\weak.obj" : $(SOURCE) $(DEP_CPP_WEAK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_WEAK_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\weak.h"\
	

"$(INTDIR)\weak.obj" : $(SOURCE) $(DEP_CPP_WEAK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\vec.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_VEC_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\vec.obj" : $(SOURCE) $(DEP_CPP_VEC_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_VEC_C=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	

"$(INTDIR)\vec.obj" : $(SOURCE) $(DEP_CPP_VEC_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\value.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_VALUE=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\class.h"\
	".\interp\gc.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\value.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\value.obj" : $(SOURCE) $(DEP_CPP_VALUE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_VALUE=\
	".\compat\std-c.h"\
	".\interp\class.h"\
	".\interp\gc.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\value.h"\
	

"$(INTDIR)\value.obj" : $(SOURCE) $(DEP_CPP_VALUE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\type.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_TYPE_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_TYPE_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	

"$(INTDIR)\type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\thread.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_THREA=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\thread.obj" : $(SOURCE) $(DEP_CPP_THREA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_THREA=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	

"$(INTDIR)\thread.obj" : $(SOURCE) $(DEP_CPP_THREA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\table.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_TABLE=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\table.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\table.obj" : $(SOURCE) $(DEP_CPP_TABLE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_TABLE=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\table.h"\
	".\interp\thread.h"\
	

"$(INTDIR)\table.obj" : $(SOURCE) $(DEP_CPP_TABLE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\sym.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_SYM_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\sym.obj" : $(SOURCE) $(DEP_CPP_SYM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_SYM_C=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	

"$(INTDIR)\sym.obj" : $(SOURCE) $(DEP_CPP_SYM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\str.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_STR_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\str.obj" : $(SOURCE) $(DEP_CPP_STR_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_STR_C=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	

"$(INTDIR)\str.obj" : $(SOURCE) $(DEP_CPP_STR_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\print.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_PRINT=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_PRINT=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	

"$(INTDIR)\print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\interp\parser-tab.c"

"$(INTDIR)\parser-tab.obj" : $(SOURCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\obj.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_OBJ_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\obj.obj" : $(SOURCE) $(DEP_CPP_OBJ_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_OBJ_C=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	

"$(INTDIR)\obj.obj" : $(SOURCE) $(DEP_CPP_OBJ_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\num.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_NUM_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\num.obj" : $(SOURCE) $(DEP_CPP_NUM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_NUM_C=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	

"$(INTDIR)\num.obj" : $(SOURCE) $(DEP_CPP_NUM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\nlx.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_NLX_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\nlx.obj" : $(SOURCE) $(DEP_CPP_NLX_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_NLX_C=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	

"$(INTDIR)\nlx.obj" : $(SOURCE) $(DEP_CPP_NLX_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\module.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_MODUL=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\load.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\module.obj" : $(SOURCE) $(DEP_CPP_MODUL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_MODUL=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\load.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	

"$(INTDIR)\module.obj" : $(SOURCE) $(DEP_CPP_MODUL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\misc.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_MISC_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_MISC_=\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\mindy.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_MINDY_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\debug.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\init.h"\
	".\interp\list.h"\
	".\interp\load.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\mindy.obj" : $(SOURCE) $(DEP_CPP_MINDY_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_MINDY_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\debug.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\init.h"\
	".\interp\list.h"\
	".\interp\load.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	

"$(INTDIR)\mindy.obj" : $(SOURCE) $(DEP_CPP_MINDY_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\lose.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LOSE_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\mindy.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lose.obj" : $(SOURCE) $(DEP_CPP_LOSE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LOSE_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	

"$(INTDIR)\lose.obj" : $(SOURCE) $(DEP_CPP_LOSE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\load.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LOAD_=\
	".\comp\fileops.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\debug.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\interp.h"\
	".\interp\list.h"\
	".\interp\load.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\load.obj" : $(SOURCE) $(DEP_CPP_LOAD_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LOAD_=\
	".\comp\fileops.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\debug.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\interp.h"\
	".\interp\list.h"\
	".\interp\load.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\load.obj" : $(SOURCE) $(DEP_CPP_LOAD_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\list.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LIST_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\list.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LIST_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	

"$(INTDIR)\list.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\lexer.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_LEXER_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\lexer.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\parser-tab.h"\
	".\interp\parser.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lexer.obj" : $(SOURCE) $(DEP_CPP_LEXER_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_LEXER_=\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\lexer.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\parser-tab.h"\
	".\interp\parser.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\lexer.obj" : $(SOURCE) $(DEP_CPP_LEXER_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\interp.c
DEP_CPP_INTER=\
	".\comp\byteops.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\brkpt.h"\
	".\interp\class.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\value.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\interp.obj" : $(SOURCE) $(DEP_CPP_INTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\instance.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_INSTA=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\value.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\instance.obj" : $(SOURCE) $(DEP_CPP_INSTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_INSTA=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\value.h"\
	".\interp\vec.h"\
	

"$(INTDIR)\instance.obj" : $(SOURCE) $(DEP_CPP_INSTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\input.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_INPUT=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\fd.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_INPUT=\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\fd.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

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
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\handler.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\handler.obj" : $(SOURCE) $(DEP_CPP_HANDL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_HANDL=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\handler.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	

"$(INTDIR)\handler.obj" : $(SOURCE) $(DEP_CPP_HANDL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\gc.c
DEP_CPP_GC_C50=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\table.h"\
	".\interp\thread.h"\
	".\interp\weak.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\gc.obj" : $(SOURCE) $(DEP_CPP_GC_C50) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\func.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_FUNC_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\extern.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\func.obj" : $(SOURCE) $(DEP_CPP_FUNC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_FUNC_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\extern.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	

"$(INTDIR)\func.obj" : $(SOURCE) $(DEP_CPP_FUNC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\fd.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_FD_C54=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\buf.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\fd.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\fd.obj" : $(SOURCE) $(DEP_CPP_FD_C54) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_FD_C54=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\buf.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\fd.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\fd.obj" : $(SOURCE) $(DEP_CPP_FD_C54) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\interp\ext-init.c"
DEP_CPP_EXT_I=\
	".\interp\bool.h"\
	".\interp\extern.h"\
	".\interp\extern1.def"\
	".\interp\extern2.def"\
	".\interp\gc.h"\
	".\interp\mindy.h"\
	

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
 /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /Fp"$(INTDIR)/Interpretter.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\extern.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_EXTER=\
	".\compat\shl.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\extern.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\extern.obj" : $(SOURCE) $(DEP_CPP_EXTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_EXTER=\
	".\compat\shl.h"\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\extern.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	

"$(INTDIR)\extern.obj" : $(SOURCE) $(DEP_CPP_EXTER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\error.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_ERROR=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_ERROR=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\def.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	

"$(INTDIR)\error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\driver.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_DRIVE=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\driver.h"\
	".\interp\fd.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\driver.obj" : $(SOURCE) $(DEP_CPP_DRIVE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_DRIVE=\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\driver.h"\
	".\interp\fd.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\driver.obj" : $(SOURCE) $(DEP_CPP_DRIVE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\def.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_DEF_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\instance.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\def.obj" : $(SOURCE) $(DEP_CPP_DEF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_DEF_C=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\func.h"\
	".\interp\instance.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	

"$(INTDIR)\def.obj" : $(SOURCE) $(DEP_CPP_DEF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\debug.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_DEBUG=\
	".\comp\byteops.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-limits.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\brkpt.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\interp.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\parser-tab.h"\
	".\interp\parser.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\value.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_DEBUG=\
	".\comp\byteops.h"\
	".\compat\std-c.h"\
	".\compat\std-dirent.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-os.h"\
	".\compat\std-signal.h"\
	".\compat\std-unistd.h"\
	".\interp\bool.h"\
	".\interp\brkpt.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\interp.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\parser-tab.h"\
	".\interp\parser.h"\
	".\interp\print.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\thread.h"\
	".\interp\type.h"\
	".\interp\value.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\coll.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_COLL_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\coll.obj" : $(SOURCE) $(DEP_CPP_COLL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_COLL_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	

"$(INTDIR)\coll.obj" : $(SOURCE) $(DEP_CPP_COLL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\class.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_CLASS=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\extern.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\class.obj" : $(SOURCE) $(DEP_CPP_CLASS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_CLASS=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\extern.h"\
	".\interp\gc.h"\
	".\interp\instance.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	

"$(INTDIR)\class.obj" : $(SOURCE) $(DEP_CPP_CLASS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\char.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_CHAR_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\type.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_CHAR_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\char.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	".\interp\type.h"\
	

"$(INTDIR)\char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\buf.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_BUF_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\buf.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\buf.obj" : $(SOURCE) $(DEP_CPP_BUF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_BUF_C=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\buf.h"\
	".\interp\class.h"\
	".\interp\coll.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\str.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\buf.obj" : $(SOURCE) $(DEP_CPP_BUF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\brkpt.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_BRKPT=\
	".\comp\byteops.h"\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\brkpt.h"\
	".\interp\driver.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\mindy.h"\
	".\interp\print.h"\
	".\interp\thread.h"\
	".\interp\weak.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\brkpt.obj" : $(SOURCE) $(DEP_CPP_BRKPT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_BRKPT=\
	".\comp\byteops.h"\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\brkpt.h"\
	".\interp\driver.h"\
	".\interp\gc.h"\
	".\interp\interp.h"\
	".\interp\mindy.h"\
	".\interp\print.h"\
	".\interp\thread.h"\
	".\interp\weak.h"\
	

"$(INTDIR)\brkpt.obj" : $(SOURCE) $(DEP_CPP_BRKPT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\bool.c

!IF  "$(CFG)" == "Interpretter - Win32 Release"

DEP_CPP_BOOL_=\
	".\compat\std-bstring.h"\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\bool.obj" : $(SOURCE) $(DEP_CPP_BOOL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interpretter - Win32 Debug"

DEP_CPP_BOOL_=\
	".\compat\std-c.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\def.h"\
	".\interp\gc.h"\
	".\interp\list.h"\
	".\interp\mindy.h"\
	".\interp\obj.h"\
	".\interp\print.h"\
	

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
