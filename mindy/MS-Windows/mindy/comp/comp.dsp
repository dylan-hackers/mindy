# Microsoft Developer Studio Project File - Name="comp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=comp - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "comp.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "comp.mak" CFG="comp - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "comp - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "comp - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "comp - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "$(InputDir)" /I "$(ProjDir)" /I "$(WkspDir)" /I ".." /I "..\..\mindy" /I "..\..\mindy\comp" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_MBCS" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D "NO_BSTRING_H" /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /machine:I386
# Begin Special Build Tool
ProjDir=.
TargetPath=.\Release\comp.exe
SOURCE="$(InputPath)"
PostBuild_Cmds=copy $(TargetPath) $(ProjDir)\..\..\..\comp\mindycomp.exe
# End Special Build Tool

!ELSEIF  "$(CFG)" == "comp - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\mindy" /I "..\..\mindy\comp" /D "_CONSOLE" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D "NO_BSTRING_H" /FD /D /GZ /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# Begin Special Build Tool
ProjDir=.
TargetPath=.\Debug\comp.exe
SOURCE="$(InputPath)"
PostBuild_Cmds=copy $(TargetPath) $(ProjDir)\..\..\..\comp\mindycomp.exe
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "comp - Win32 Release"
# Name "comp - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\..\comp\compile.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\dump.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\dup.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\envanal.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\expand.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\feature.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\free.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\header.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\info.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\lexenv.c
# End Source File
# Begin Source File

SOURCE=".\lexer-tab.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\literal.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\lose.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\mindycomp.c
# End Source File
# Begin Source File

SOURCE=".\parser-tab.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\print.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\src.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\sym.c
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\version.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\..\comp\byteops.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\compile.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\dump.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\dup.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\envanal.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\expand.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\feature.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\fileops.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\free.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\header.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\info.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\lexenv.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\lexer.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\literal.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\lose.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\mindycomp.h
# End Source File
# Begin Source File

SOURCE=".\parser-tab.h"
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\parser.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\print.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\src.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\sym.h
# End Source File
# Begin Source File

SOURCE=..\..\..\comp\version.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
