# Microsoft Developer Studio Project File - Name="mindylibs" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Generic Project" 0x010a

CFG=mindylibs - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mindylibs.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mindylibs.mak" CFG="mindylibs - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mindylibs - Win32 Release" (based on "Win32 (x86) Generic Project")
!MESSAGE "mindylibs - Win32 Debug" (based on "Win32 (x86) Generic Project")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
MTL=midl.exe

!IF  "$(CFG)" == "mindylibs - Win32 Release"

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

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

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
# Begin Special Build Tool
ProjDir=.
SOURCE="$(InputPath)"
PostBuild_Cmds=..\..\..\..\tools\win32-misc\dbclink dylan-lib.dbc $(ProjDir)\mindylibs.list	copy dylan-lib.dbc $(ProjDir)\..\..\..\libraries\dylan\dylan-lib.dbc
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "mindylibs - Win32 Release"
# Name "mindylibs - Win32 Debug"
# Begin Source File

SOURCE=..\..\..\libraries\dylan\array.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\array.dylan
InputName=array

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# PROP Ignore_Default_Tool 1
# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\array.dylan
InputName=array

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\char.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\char.dylan
InputName=char

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\char.dylan
InputName=char

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\cmp.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\cmp.dylan
InputName=cmp

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\cmp.dylan
InputName=cmp

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\coll.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\coll.dylan
InputName=coll

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\coll.dylan
InputName=coll

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\cond.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\cond.dylan
InputName=cond

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\cond.dylan
InputName=cond

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\debug.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\debug.dylan
InputName=debug

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\debug.dylan
InputName=debug

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\deque.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\deque.dylan
InputName=deque

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\deque.dylan
InputName=deque

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\exit.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\exit.dylan
InputName=exit

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\exit.dylan
InputName=exit

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\exports.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\exports.dylan
InputName=exports

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\exports.dylan
InputName=exports

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\ext.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\ext.dylan
InputName=ext

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\ext.dylan
InputName=ext

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\extern.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\extern.dylan
InputName=extern

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\extern.dylan
InputName=extern

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\func.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\func.dylan
InputName=func

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\func.dylan
InputName=func

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\list.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\list.dylan
InputName=list

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\list.dylan
InputName=list

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\misc.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\misc.dylan
InputName=misc

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\misc.dylan
InputName=misc

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\multilock.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\multilock.dylan
InputName=multilock

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\multilock.dylan
InputName=multilock

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\num.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\num.dylan
InputName=num

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\num.dylan
InputName=num

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\range.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\range.dylan
InputName=range

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\range.dylan
InputName=range

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\ratio.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\ratio.dylan
InputName=ratio

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\ratio.dylan
InputName=ratio

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\semaphore.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\semaphore.dylan
InputName=semaphore

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\semaphore.dylan
InputName=semaphore

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\sort.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\sort.dylan
InputName=sort

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\sort.dylan
InputName=sort

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\stretchy.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\stretchy.dylan
InputName=stretchy

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\stretchy.dylan
InputName=stretchy

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\string.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\string.dylan
InputName=string

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\string.dylan
InputName=string

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\system.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\system.dylan
InputName=system

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\system.dylan
InputName=system

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\table.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\table.dylan
InputName=table

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\table.dylan
InputName=table

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\transcendental.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\transcendental.dylan
InputName=transcendental

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\transcendental.dylan
InputName=transcendental

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\libraries\dylan\vec.dylan

!IF  "$(CFG)" == "mindylibs - Win32 Release"

# Begin Custom Build
InputPath=..\..\..\libraries\dylan\vec.dylan
InputName=vec

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "mindylibs - Win32 Debug"

# Begin Custom Build - Mindy Compiling $(InputPath)
InputPath=..\..\..\libraries\dylan\vec.dylan
InputName=vec

"$(InputName).dbc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\..\..\comp\mindycomp -ldylan -o $(InputName).dbc $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
