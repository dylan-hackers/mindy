# Microsoft Developer Studio Project File - Name="interp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=interp - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "interp.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "interp.mak" CFG="interp - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "interp - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "interp - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "interp - Win32 Release"

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
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\mindy\interp" /I ".." /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_MBCS" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D "NO_BSTRING_H" /FD /c
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
TargetPath=.\Release\interp.exe
SOURCE="$(InputPath)"
PostBuild_Cmds=copy $(TargetPath) $(ProjDir)\..\..\..\interp\mindy.exe
# End Special Build Tool

!ELSEIF  "$(CFG)" == "interp - Win32 Debug"

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
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\mindy\interp" /I ".." /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_MBCS" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /D "NO_BSTRING_H" /FD /D /D /D /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# Begin Special Build Tool
ProjDir=.
TargetPath=.\Debug\interp.exe
SOURCE="$(InputPath)"
PostBuild_Cmds=copy $(TargetPath) $(ProjDir)\..\..\..\interp\mindy.exe
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "interp - Win32 Release"
# Name "interp - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\..\interp\bool.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\brkpt.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\buf.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\char.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\class.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\coll.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\debug.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\def.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\driver.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\error.c
# End Source File
# Begin Source File

SOURCE="..\..\..\interp\ext-init.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\extern.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\fd.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\func.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\gc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\handler.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\init.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\input.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\instance.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\interp.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\lexer.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\list.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\load.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\lose.c
# End Source File
# Begin Source File

SOURCE="..\..\..\interp\make-init.pl"

!IF  "$(CFG)" == "interp - Win32 Release"

# Begin Custom Build
InputDir=\Fulgham\gd\src\mindy\interp
ProjDir=.
InputPath="..\..\..\interp\make-init.pl"

"extern1.def, extern2.def" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	perl $(InputPath) $(InputDir)\malloc.inc $(InputDir)\..\..\common\time\time.inc 
	copy $(ProjDir)\"?extern1.def" $(InputDir)\extern1.def 
	copy $(ProjDir)\"?extern2.def" $(InputDir)\extern2.def 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "interp - Win32 Debug"

# Begin Custom Build
InputDir=\Fulgham\gd\src\mindy\interp
ProjDir=.
InputPath="..\..\..\interp\make-init.pl"

"extern1.def, extern2.def" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	c:\Perl\bin\perl.exe $(InputPath) $(InputDir)\malloc.inc $(InputDir)\..\..\common\time\time.inc 
	copy $(ProjDir)\"?extern1.def" $(InputDir)\extern1.def 
	copy $(ProjDir)\"?extern2.def" $(InputDir)\extern2.def 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\interp\mindy.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\misc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\module.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\nlx.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\num.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\obj.c
# End Source File
# Begin Source File

SOURCE=".\parser-tab.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\print.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\str.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\sym.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\table.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\thread.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\common\time\time-portability.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\type.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\value.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\vec.c
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\weak.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\..\interp\bool.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\brkpt.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\buf.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\char.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\class.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\coll.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\debug.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\def.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\driver.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\error.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\extern.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\fd.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\func.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\gc.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\handler.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\init.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\instance.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\interp.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\lexer.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\list.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\load.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\mindy.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\module.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\num.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\obj.h
# End Source File
# Begin Source File

SOURCE=".\parser-tab.h"
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\parser.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\print.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\str.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\sym.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\table.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\thread.h
# End Source File
# Begin Source File

SOURCE="..\..\..\..\common\time\time-portability.h"
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\type.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\value.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\vec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\interp\weak.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
