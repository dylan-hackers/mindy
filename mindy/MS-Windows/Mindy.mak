# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Compiler Debug
!MESSAGE No configuration specified.  Defaulting to Compiler Debug.
!ENDIF 

!IF "$(CFG)" != "Compiler Release" && "$(CFG)" != "Compiler Debug" && "$(CFG)"\
 != "Interpreter Release" && "$(CFG)" != "Interpreter Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "Mindy.mak" CFG="Compiler Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Compiler Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Compiler Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "Interpreter Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Interpreter Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Compiler Release"
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Compiler Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Released/"
# PROP Intermediate_Dir "Released/Objects/Compiler"
OUTDIR=.\Released
INTDIR=.\Released/Objects/Compiler

ALL : $(OUTDIR)/"MindyComp.exe" ".\MindyComp.bsc"

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""
CPP_PROJ=/nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  
CPP_OBJS=.\Released/Objects/Compiler/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo /o"MindyComp.bsc"
BSC32_FLAGS=/nologo /o"MindyComp.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"compile.sbr" \
	$(INTDIR)/"dump.sbr" \
	$(INTDIR)/"dup.sbr" \
	$(INTDIR)/"envanal.sbr" \
	$(INTDIR)/"expand.sbr" \
	$(INTDIR)/"free.sbr" \
	$(INTDIR)/"header.sbr" \
	$(INTDIR)/"info.sbr" \
	$(INTDIR)/"lexenv.sbr" \
	$(INTDIR)/"literal.sbr" \
	$(INTDIR)/"lose.sbr" \
	$(INTDIR)/"mindycomp.sbr" \
	$(INTDIR)/"print.sbr" \
	$(INTDIR)/"src.sbr" \
	$(INTDIR)/"sym.sbr" \
	$(INTDIR)/"version.sbr" \
	$(INTDIR)/"parser-tab.sbr" \
	$(INTDIR)/"lexer-tab.sbr"

".\MindyComp.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib oldnames.lib /NOLOGO /SUBSYSTEM:console /PDB:"Released/MindyComp.pdb" /MACHINE:I386 /OUT:"Released/MindyComp.exe"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib oldnames.lib /NOLOGO /SUBSYSTEM:console /INCREMENTAL:no\
 /PDB:"Released/MindyComp.pdb" /MACHINE:I386 /OUT:"Released/MindyComp.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"compile.obj" \
	$(INTDIR)/"dump.obj" \
	$(INTDIR)/"dup.obj" \
	$(INTDIR)/"envanal.obj" \
	$(INTDIR)/"expand.obj" \
	$(INTDIR)/"free.obj" \
	$(INTDIR)/"header.obj" \
	$(INTDIR)/"info.obj" \
	$(INTDIR)/"lexenv.obj" \
	$(INTDIR)/"literal.obj" \
	$(INTDIR)/"lose.obj" \
	$(INTDIR)/"mindycomp.obj" \
	$(INTDIR)/"print.obj" \
	$(INTDIR)/"src.obj" \
	$(INTDIR)/"sym.obj" \
	$(INTDIR)/"version.obj" \
	$(INTDIR)/"parser-tab.obj" \
	$(INTDIR)/"lexer-tab.obj"

$(OUTDIR)/"MindyComp.exe" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug/Objects/Compiler/"
OUTDIR=.\Debug
INTDIR=.\Debug/Objects/Compiler

ALL : $(OUTDIR)/"MindyComp.exe" $(OUTDIR)/"MindyComp.bsc"

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c
CPP_PROJ=/nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c 
CPP_OBJS=.\Debug/Objects/Compiler/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo /o"Debug/MindyComp.bsc"
BSC32_FLAGS=/nologo /o"Debug/MindyComp.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"compile.sbr" \
	$(INTDIR)/"dump.sbr" \
	$(INTDIR)/"dup.sbr" \
	$(INTDIR)/"envanal.sbr" \
	$(INTDIR)/"expand.sbr" \
	$(INTDIR)/"free.sbr" \
	$(INTDIR)/"header.sbr" \
	$(INTDIR)/"info.sbr" \
	$(INTDIR)/"lexenv.sbr" \
	$(INTDIR)/"literal.sbr" \
	$(INTDIR)/"lose.sbr" \
	$(INTDIR)/"mindycomp.sbr" \
	$(INTDIR)/"print.sbr" \
	$(INTDIR)/"src.sbr" \
	$(INTDIR)/"sym.sbr" \
	$(INTDIR)/"version.sbr" \
	$(INTDIR)/"parser-tab.sbr" \
	$(INTDIR)/"lexer-tab.sbr"

$(OUTDIR)/"MindyComp.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib oldnames.lib /NOLOGO /SUBSYSTEM:console /PDB:"Debug/MindyComp.pdb" /DEBUG /MACHINE:I386 /OUT:"Debug/MindyComp.exe"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib oldnames.lib /NOLOGO /SUBSYSTEM:console /INCREMENTAL:yes\
 /PDB:"Debug/MindyComp.pdb" /DEBUG /MACHINE:I386 /OUT:"Debug/MindyComp.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"compile.obj" \
	$(INTDIR)/"dump.obj" \
	$(INTDIR)/"dup.obj" \
	$(INTDIR)/"envanal.obj" \
	$(INTDIR)/"expand.obj" \
	$(INTDIR)/"free.obj" \
	$(INTDIR)/"header.obj" \
	$(INTDIR)/"info.obj" \
	$(INTDIR)/"lexenv.obj" \
	$(INTDIR)/"literal.obj" \
	$(INTDIR)/"lose.obj" \
	$(INTDIR)/"mindycomp.obj" \
	$(INTDIR)/"print.obj" \
	$(INTDIR)/"src.obj" \
	$(INTDIR)/"sym.obj" \
	$(INTDIR)/"version.obj" \
	$(INTDIR)/"parser-tab.obj" \
	$(INTDIR)/"lexer-tab.obj"

$(OUTDIR)/"MindyComp.exe" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Interpre"
# PROP BASE Intermediate_Dir "Interpre"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Released"
# PROP Intermediate_Dir "Released/Objects/Interpreter"
OUTDIR=.\Released
INTDIR=.\Released/Objects/Interpreter

ALL : $(OUTDIR)/"Mindy.exe" $(OUTDIR)/"Mindy.bsc"

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\"" /c
CPP_PROJ=/nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c 
CPP_OBJS=.\Released/Objects/Interpreter/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"Mindy.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"rint.sbr" \
	$(INTDIR)/"error.sbr" \
	$(INTDIR)/"obj.sbr" \
	$(INTDIR)/"lose.sbr" \
	$(INTDIR)/"table.sbr" \
	$(INTDIR)/"module.sbr" \
	$(INTDIR)/"fd.sbr" \
	$(INTDIR)/"value.sbr" \
	$(INTDIR)/"func.sbr" \
	$(INTDIR)/"sym.sbr" \
	$(INTDIR)/"char.sbr" \
	$(INTDIR)/"vec.sbr" \
	$(INTDIR)/"nlx.sbr" \
	$(INTDIR)/"def.sbr" \
	$(INTDIR)/"init.sbr" \
	$(INTDIR)/"input.sbr" \
	$(INTDIR)/"type.sbr" \
	$(INTDIR)/"lexer.sbr" \
	$(INTDIR)/"buf.sbr" \
	$(INTDIR)/"bool.sbr" \
	$(INTDIR)/"list.sbr" \
	$(INTDIR)/"misc.sbr" \
	$(INTDIR)/"extern.sbr" \
	$(INTDIR)/"coll.sbr" \
	$(INTDIR)/"instance.sbr" \
	$(INTDIR)/"handler.sbr" \
	$(INTDIR)/"gc.sbr" \
	$(INTDIR)/"weak.sbr" \
	$(INTDIR)/"print.sbr" \
	$(INTDIR)/"ext-init.sbr" \
	$(INTDIR)/"brkpt.sbr" \
	$(INTDIR)/"thread.sbr" \
	$(INTDIR)/"load.sbr" \
	$(INTDIR)/"interp.sbr" \
	$(INTDIR)/"driver.sbr" \
	$(INTDIR)/"num.sbr" \
	$(INTDIR)/"mindy.sbr" \
	$(INTDIR)/"str.sbr" \
	$(INTDIR)/"debug.sbr" \
	$(INTDIR)/"class.sbr" \
	$(INTDIR)/"parser-tab.sbr"

$(OUTDIR)/"Mindy.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib PSXRTL.LIB PSXDLL.LIB /NOLOGO /SUBSYSTEM:console /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib wsock32.lib PSXRTL.LIB PSXDLL.LIB /NOLOGO /SUBSYSTEM:console\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"Mindy.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"Mindy.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"rint.obj" \
	$(INTDIR)/"error.obj" \
	$(INTDIR)/"obj.obj" \
	$(INTDIR)/"lose.obj" \
	$(INTDIR)/"table.obj" \
	$(INTDIR)/"module.obj" \
	$(INTDIR)/"fd.obj" \
	$(INTDIR)/"value.obj" \
	$(INTDIR)/"func.obj" \
	$(INTDIR)/"sym.obj" \
	$(INTDIR)/"char.obj" \
	$(INTDIR)/"vec.obj" \
	$(INTDIR)/"nlx.obj" \
	$(INTDIR)/"def.obj" \
	$(INTDIR)/"init.obj" \
	$(INTDIR)/"input.obj" \
	$(INTDIR)/"type.obj" \
	$(INTDIR)/"lexer.obj" \
	$(INTDIR)/"buf.obj" \
	$(INTDIR)/"bool.obj" \
	$(INTDIR)/"list.obj" \
	$(INTDIR)/"misc.obj" \
	$(INTDIR)/"extern.obj" \
	$(INTDIR)/"coll.obj" \
	$(INTDIR)/"instance.obj" \
	$(INTDIR)/"handler.obj" \
	$(INTDIR)/"gc.obj" \
	$(INTDIR)/"weak.obj" \
	$(INTDIR)/"print.obj" \
	$(INTDIR)/"ext-init.obj" \
	$(INTDIR)/"brkpt.obj" \
	$(INTDIR)/"thread.obj" \
	$(INTDIR)/"load.obj" \
	$(INTDIR)/"interp.obj" \
	$(INTDIR)/"driver.obj" \
	$(INTDIR)/"num.obj" \
	$(INTDIR)/"mindy.obj" \
	$(INTDIR)/"str.obj" \
	$(INTDIR)/"debug.obj" \
	$(INTDIR)/"class.obj" \
	$(INTDIR)/"parser-tab.obj"

$(OUTDIR)/"Mindy.exe" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Interpr0"
# PROP BASE Intermediate_Dir "Interpr0"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug/Objects/Interpreter"
OUTDIR=.\Debug
INTDIR=.\Debug/Objects/Interpreter

ALL : $(OUTDIR)/"Mindy.exe" $(OUTDIR)/"Mindy.bsc"

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR /c
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32" /D\
 "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c 
CPP_OBJS=.\Debug/Objects/Interpreter/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"Mindy.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"rint.sbr" \
	$(INTDIR)/"error.sbr" \
	$(INTDIR)/"obj.sbr" \
	$(INTDIR)/"lose.sbr" \
	$(INTDIR)/"table.sbr" \
	$(INTDIR)/"module.sbr" \
	$(INTDIR)/"fd.sbr" \
	$(INTDIR)/"value.sbr" \
	$(INTDIR)/"func.sbr" \
	$(INTDIR)/"sym.sbr" \
	$(INTDIR)/"char.sbr" \
	$(INTDIR)/"vec.sbr" \
	$(INTDIR)/"nlx.sbr" \
	$(INTDIR)/"def.sbr" \
	$(INTDIR)/"init.sbr" \
	$(INTDIR)/"input.sbr" \
	$(INTDIR)/"type.sbr" \
	$(INTDIR)/"lexer.sbr" \
	$(INTDIR)/"buf.sbr" \
	$(INTDIR)/"bool.sbr" \
	$(INTDIR)/"list.sbr" \
	$(INTDIR)/"misc.sbr" \
	$(INTDIR)/"extern.sbr" \
	$(INTDIR)/"coll.sbr" \
	$(INTDIR)/"instance.sbr" \
	$(INTDIR)/"handler.sbr" \
	$(INTDIR)/"gc.sbr" \
	$(INTDIR)/"weak.sbr" \
	$(INTDIR)/"print.sbr" \
	$(INTDIR)/"ext-init.sbr" \
	$(INTDIR)/"brkpt.sbr" \
	$(INTDIR)/"thread.sbr" \
	$(INTDIR)/"load.sbr" \
	$(INTDIR)/"interp.sbr" \
	$(INTDIR)/"driver.sbr" \
	$(INTDIR)/"num.sbr" \
	$(INTDIR)/"mindy.sbr" \
	$(INTDIR)/"str.sbr" \
	$(INTDIR)/"debug.sbr" \
	$(INTDIR)/"class.sbr" \
	$(INTDIR)/"parser-tab.sbr"

$(OUTDIR)/"Mindy.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib PSXRTL.LIB PSXDLL.LIB /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# SUBTRACT LINK32 /PROFILE
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib wsock32.lib PSXRTL.LIB PSXDLL.LIB /NOLOGO /SUBSYSTEM:console\
 /INCREMENTAL:yes /PDB:$(OUTDIR)/"Mindy.pdb" /DEBUG /MACHINE:I386\
 /OUT:$(OUTDIR)/"Mindy.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"rint.obj" \
	$(INTDIR)/"error.obj" \
	$(INTDIR)/"obj.obj" \
	$(INTDIR)/"lose.obj" \
	$(INTDIR)/"table.obj" \
	$(INTDIR)/"module.obj" \
	$(INTDIR)/"fd.obj" \
	$(INTDIR)/"value.obj" \
	$(INTDIR)/"func.obj" \
	$(INTDIR)/"sym.obj" \
	$(INTDIR)/"char.obj" \
	$(INTDIR)/"vec.obj" \
	$(INTDIR)/"nlx.obj" \
	$(INTDIR)/"def.obj" \
	$(INTDIR)/"init.obj" \
	$(INTDIR)/"input.obj" \
	$(INTDIR)/"type.obj" \
	$(INTDIR)/"lexer.obj" \
	$(INTDIR)/"buf.obj" \
	$(INTDIR)/"bool.obj" \
	$(INTDIR)/"list.obj" \
	$(INTDIR)/"misc.obj" \
	$(INTDIR)/"extern.obj" \
	$(INTDIR)/"coll.obj" \
	$(INTDIR)/"instance.obj" \
	$(INTDIR)/"handler.obj" \
	$(INTDIR)/"gc.obj" \
	$(INTDIR)/"weak.obj" \
	$(INTDIR)/"print.obj" \
	$(INTDIR)/"ext-init.obj" \
	$(INTDIR)/"brkpt.obj" \
	$(INTDIR)/"thread.obj" \
	$(INTDIR)/"load.obj" \
	$(INTDIR)/"interp.obj" \
	$(INTDIR)/"driver.obj" \
	$(INTDIR)/"num.obj" \
	$(INTDIR)/"mindy.obj" \
	$(INTDIR)/"str.obj" \
	$(INTDIR)/"debug.obj" \
	$(INTDIR)/"class.obj" \
	$(INTDIR)/"parser-tab.obj"

$(OUTDIR)/"Mindy.exe" : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

################################################################################
# Begin Group "Source Files"

# End Group
################################################################################
# Begin Group "Compiler"

################################################################################
# Begin Source File

SOURCE=.\comp\compile.c
DEP_COMPI=\
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
	".\compat\std-bstring.h"

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"compile.obj" :  $(SOURCE)  $(DEP_COMPI) $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"compile.obj" :  $(SOURCE)  $(DEP_COMPI) $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\dump.c
DEP_DUMP_=\
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
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"dump.obj" :  $(SOURCE)  $(DEP_DUMP_) $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"dump.obj" :  $(SOURCE)  $(DEP_DUMP_) $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\dup.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"dup.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"dup.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\envanal.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"envanal.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"envanal.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\expand.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"expand.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"expand.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\free.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"free.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"free.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\header.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"header.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"header.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\info.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"info.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"info.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\lexenv.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"lexenv.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"lexenv.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\literal.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"literal.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"literal.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\lose.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"lose.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"lose.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\mindycomp.c
DEP_MINDY=\
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
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"mindycomp.obj" :  $(SOURCE)  $(DEP_MINDY) $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"mindycomp.obj" :  $(SOURCE)  $(DEP_MINDY) $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\print.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\src.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"src.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"src.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\sym.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"sym.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"sym.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\comp\version.c

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\comp\parser-tab.c"

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"parser-tab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"parser-tab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\comp\lexer-tab.c"
DEP_LEXER=\
	".\comp\lexer.h"\
	".\comp\src.h"\
	".\comp\parser.tab.h"

!IF  "$(CFG)" == "Compiler Release"

$(INTDIR)/"lexer-tab.obj" :  $(SOURCE)  $(DEP_LEXER) $(INTDIR)
   $(CPP) /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/ /D /D /D\
  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""\
   $(SOURCE) 

!ELSEIF  "$(CFG)" == "Compiler Debug"

$(INTDIR)/"lexer-tab.obj" :  $(SOURCE)  $(DEP_LEXER) $(INTDIR)
   $(CPP) /nologo /W3 /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D\
 "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D\
 "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /Fd"Debug/MindyComp.pdb" /D /D /D  VERSION="\"1.3\""\
 BINDIR="\"d:/mindy-13/bin\"" LIBDIR="\"d:/mindy-13/lib\""  /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# End Group
################################################################################
# Begin Group "Compatibility Library"

################################################################################
# Begin Source File

SOURCE=.\compat\getcwd.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\matherr.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\memmove.c
DEP_MEMMO=\
	".\compat\std-c.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\opendir.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\protected.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\rint.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"rint.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"rint.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\strerror.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\strstr.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\strtod.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\strtol.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\strtoul.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\tmpnam.c
# PROP Exclude_From_Build 1
# End Source File
################################################################################
# Begin Source File

SOURCE=.\compat\waitpid.c
# PROP Exclude_From_Build 1
# End Source File
# End Group
################################################################################
# Begin Group "Interpreter"

################################################################################
# Begin Source File

SOURCE=.\interp\error.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\obj.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"obj.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"obj.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\lose.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"lose.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"lose.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\table.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"table.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"table.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\module.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"module.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"module.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\fd.c
DEP_FD_Ca=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\list.h"\
	".\interp\bool.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\interp\buf.h"\
	".\interp\str.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"fd.obj" :  $(SOURCE)  $(DEP_FD_Ca) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"fd.obj" :  $(SOURCE)  $(DEP_FD_Ca) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\value.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"value.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"value.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\func.c
DEP_FUNC_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\bool.h"\
	".\interp\list.h"\
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
	".\interp\error.h"\
	".\interp\def.h"\
	".\interp\extern.h"\
	".\interp\func.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"func.obj" :  $(SOURCE)  $(DEP_FUNC_) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"func.obj" :  $(SOURCE)  $(DEP_FUNC_) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\sym.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"sym.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"sym.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\char.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"char.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"char.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\vec.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"vec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"vec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\nlx.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"nlx.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"nlx.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\def.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"def.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"def.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\init.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"init.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"init.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\input.c
DEP_INPUT=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\char.h"\
	".\interp\list.h"\
	".\interp\bool.h"\
	".\interp\thread.h"\
	".\interp\func.h"\
	".\interp\driver.h"\
	".\interp\error.h"\
	".\interp\def.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"input.obj" :  $(SOURCE)  $(DEP_INPUT) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"input.obj" :  $(SOURCE)  $(DEP_INPUT) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\type.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"type.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"type.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\lexer.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"lexer.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"lexer.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\buf.c
DEP_BUF_C=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\coll.h"\
	".\interp\class.h"\
	".\interp\module.h"\
	".\interp\num.h"\
	".\interp\bool.h"\
	".\interp\obj.h"\
	".\interp\error.h"\
	".\interp\list.h"\
	".\interp\def.h"\
	".\interp\sym.h"\
	".\interp\type.h"\
	".\interp\vec.h"\
	".\interp\str.h"\
	".\interp\buf.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"buf.obj" :  $(SOURCE)  $(DEP_BUF_C) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"buf.obj" :  $(SOURCE)  $(DEP_BUF_C) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\bool.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"bool.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"bool.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\list.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"list.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"list.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\misc.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"misc.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"misc.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\extern.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"extern.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"extern.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\coll.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"coll.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"coll.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\instance.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"instance.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"instance.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\handler.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"handler.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"handler.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\gc.c
DEP_GC_C12=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\class.h"\
	".\interp\gc.h"\
	".\interp\weak.h"\
	".\interp\table.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"gc.obj" :  $(SOURCE)  $(DEP_GC_C12) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"gc.obj" :  $(SOURCE)  $(DEP_GC_C12) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\weak.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"weak.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"weak.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\print.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\interp\ext-init.c"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"ext-init.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"ext-init.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\brkpt.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"brkpt.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"brkpt.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\thread.c
DEP_THREA=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\bool.h"\
	".\interp\class.h"\
	".\interp\thread.h"\
	".\interp\obj.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\num.h"\
	".\interp\list.h"\
	".\interp\def.h"\
	".\interp\type.h"\
	".\interp\error.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"thread.obj" :  $(SOURCE)  $(DEP_THREA) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"thread.obj" :  $(SOURCE)  $(DEP_THREA) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\load.c
DEP_LOAD_=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\bool.h"\
	".\interp\list.h"\
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
	".\interp\error.h"\
	".\comp\fileops.h"\
	".\interp\load.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"load.obj" :  $(SOURCE)  $(DEP_LOAD_) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"load.obj" :  $(SOURCE)  $(DEP_LOAD_) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\interp.c
DEP_INTER=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\bool.h"\
	".\interp\list.h"\
	".\interp\class.h"\
	".\interp\obj.h"\
	".\interp\module.h"\
	".\interp\value.h"\
	".\interp\num.h"\
	".\interp\vec.h"\
	".\interp\sym.h"\
	".\interp\error.h"\
	".\interp\type.h"\
	".\interp\brkpt.h"\
	".\interp\interp.h"\
	".\comp\byteops.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"interp.obj" :  $(SOURCE)  $(DEP_INTER) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"interp.obj" :  $(SOURCE)  $(DEP_INTER) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\driver.c
DEP_DRIVE=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\gc.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\bool.h"\
	".\interp\interp.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"driver.obj" :  $(SOURCE)  $(DEP_DRIVE) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"driver.obj" :  $(SOURCE)  $(DEP_DRIVE) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\num.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"num.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"num.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\mindy.c
DEP_MINDY_=\
	".\compat\std-c.h"\
	".\interp\mindy.h"\
	".\interp\init.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\bool.h"\
	".\interp\list.h"\
	".\interp\obj.h"\
	".\interp\sym.h"\
	".\interp\func.h"\
	".\interp\debug.h"\
	".\interp\load.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"mindy.obj" :  $(SOURCE)  $(DEP_MINDY_) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"mindy.obj" :  $(SOURCE)  $(DEP_MINDY_) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\str.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"str.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"str.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\debug.c
DEP_DEBUG=\
	".\compat\std-c.h"\
	".\compat\std-os.h"\
	".\interp\mindy.h"\
	".\interp\thread.h"\
	".\interp\driver.h"\
	".\interp\func.h"\
	".\interp\module.h"\
	".\interp\str.h"\
	".\interp\list.h"\
	".\interp\vec.h"\
	".\interp\type.h"\
	".\interp\sym.h"\
	".\interp\num.h"\
	".\interp\obj.h"\
	".\interp\bool.h"\
	".\interp\print.h"\
	".\interp\interp.h"\
	".\interp\value.h"\
	".\interp\error.h"\
	".\interp\gc.h"\
	".\interp\brkpt.h"\
	".\interp\instance.h"\
	".\comp\byteops.h"\
	".\compat\std-limits.h"\
	".\compat\std-stdlib.h"\
	".\compat\std-string.h"\
	".\compat\std-bstring.h"\
	".\compat\std-dirent2.h"\
	".\compat\std-dirent.h"\
	".\compat\std-unistd.h"\
	".\compat\std-signal.h"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"debug.obj" :  $(SOURCE)  $(DEP_DEBUG) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"debug.obj" :  $(SOURCE)  $(DEP_DEBUG) $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\interp\class.c

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"class.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"class.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\interp\parser-tab.c"

!IF  "$(CFG)" == "Compiler Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Compiler Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "Interpreter Release"

$(INTDIR)/"parser-tab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Od /Ob2 /Gy /D "NDEBUG" /D "FAKE_SELECT" /D\
 "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /FR$(INTDIR)/ /Fo$(INTDIR)/\
 /D /D /D  VERSION="\"1.3\"" BINDIR="\"d:/mindy-13/bin\""\
 LIBDIR="\"d:/mindy-13/lib\"" /c  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Interpreter Debug"

$(INTDIR)/"parser-tab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) /nologo /MD /W3 /GX /Zi /Od /D "_DEBUG" /D "FAKE_SELECT" /D "WIN32"\
 /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D\
 "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D\
 "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D VERSION="\"1.3\"" /D\
 BINDIR="\"d:/mindy-13/bin\"" /D LIBDIR="\"d:/mindy-13/lib\"" /FR$(INTDIR)/\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"Mindy.pdb" /c  $(SOURCE) 

!ENDIF 

# End Source File
# End Group
# End Project
################################################################################
