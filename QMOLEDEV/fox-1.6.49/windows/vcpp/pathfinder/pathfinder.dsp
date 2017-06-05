# Microsoft Developer Studio Project File - Name="pathfinder" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=pathfinder - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "pathfinder.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "pathfinder.mak" CFG="pathfinder - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "pathfinder - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "pathfinder - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "pathfinder - Win32 Release"

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
F90=df.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /MT /W3 /GR /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "UNICODE" /FD /c
# SUBTRACT CPP /nologo /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 winspool.lib comctl32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib mpr.lib /entry:"mainCRTStartup" /subsystem:windows /pdb:none /machine:I386
# SUBTRACT LINK32 /nologo

!ELSEIF  "$(CFG)" == "pathfinder - Win32 Debug"

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
F90=df.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /MTd /W3 /GR /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /FD /c
# SUBTRACT CPP /nologo /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 winspool.lib comctl32.lib wsock32.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib mpr.lib /entry:"mainCRTStartup" /subsystem:windows /pdb:none /debug /machine:I386
# SUBTRACT LINK32 /nologo

!ENDIF 

# Begin Target

# Name "pathfinder - Win32 Release"
# Name "pathfinder - Win32 Debug"
# Begin Source File

SOURCE=..\..\..\pathfinder\bigicons.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\closepanel.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\clrbook.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\copy.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\CopyDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\CopyDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\copyit.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\cut.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\deleteit.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\desktop.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\details.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\dirup.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\enter.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\file_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\foxbig.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\foxmini.gif
# End Source File
# Begin Source File

SOURCE=.\gifs.list

!IF  "$(CFG)" == "pathfinder - Win32 Release"

# Begin Custom Build
WkspDir=.
InputPath=.\gifs.list

BuildCmds= \
	cd ..\..\..\pathfinder \
	$(WkspDir)\reswrap\Release\reswrap -e -o icons.cpp bigicons.bmp copy.bmp cut.bmp deleteit.bmp desktop.bmp details.bmp dirup.bmp goback.bmp goforw.bmp gotodir.bmp hosts.bmp maphost.bmp paste.bmp properties.bmp smallicons.bmp unmaphost.bmp closepanel.gif clrbook.gif copyit.gif enter.gif file_gif.gif foxbig.gif foxmini.gif home.gif iconpath.gif linkit.gif location.gif mimetype.gif moveit.gif pattern_gif.gif quit_gif.gif renameit.gif rotateleft.gif rotateright.gif setbook.gif setdir.gif work.gif \
	$(WkspDir)\reswrap\Release\reswrap -i -o icons.h bigicons.bmp copy.bmp cut.bmp deleteit.bmp desktop.bmp details.bmp dirup.bmp goback.bmp goforw.bmp gotodir.bmp hosts.bmp maphost.bmp paste.bmp properties.bmp smallicons.bmp unmaphost.bmp closepanel.gif clrbook.gif copyit.gif enter.gif file_gif.gif foxbig.gif foxmini.gif home.gif iconpath.gif linkit.gif location.gif mimetype.gif moveit.gif pattern_gif.gif quit_gif.gif renameit.gif rotateleft.gif rotateright.gif setbook.gif setdir.gif work.gif \
	

"icons.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"icons.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "pathfinder - Win32 Debug"

# Begin Custom Build
WkspDir=.
InputPath=.\gifs.list

BuildCmds= \
	cd ..\..\..\pathfinder \
	$(WkspDir)\reswrap\Debug\reswrap -e -o icons.cpp bigicons.bmp copy.bmp cut.bmp deleteit.bmp desktop.bmp details.bmp dirup.bmp goback.bmp goforw.bmp gotodir.bmp hosts.bmp maphost.bmp paste.bmp properties.bmp smallicons.bmp unmaphost.bmp closepanel.gif clrbook.gif copyit.gif enter.gif file_gif.gif foxbig.gif foxmini.gif home.gif iconpath.gif linkit.gif location.gif mimetype.gif moveit.gif pattern_gif.gif quit_gif.gif renameit.gif rotateleft.gif rotateright.gif setbook.gif setdir.gif work.gif \
	$(WkspDir)\reswrap\Debug\reswrap -i -o icons.h bigicons.bmp copy.bmp cut.bmp deleteit.bmp desktop.bmp details.bmp dirup.bmp goback.bmp goforw.bmp gotodir.bmp hosts.bmp maphost.bmp paste.bmp properties.bmp smallicons.bmp unmaphost.bmp closepanel.gif clrbook.gif copyit.gif enter.gif file_gif.gif foxbig.gif foxmini.gif home.gif iconpath.gif linkit.gif location.gif mimetype.gif moveit.gif pattern_gif.gif quit_gif.gif renameit.gif rotateleft.gif rotateright.gif setbook.gif setdir.gif work.gif \
	

"icons.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"icons.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\goback.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\goforw.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\gotodir.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\home.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\hosts.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\iconpath.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\icons.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\icons.h
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\linkit.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\location.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\Makefile.am
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\Makefile.bc
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\maphost.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\mimetype.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\moveit.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\paste.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\PathFinder.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\PathFinder.h
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\pattern_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\Preferences.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\Preferences.h
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\properties.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\PropertyDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\PropertyDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\quit_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\renameit.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\rotateleft.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\rotateright.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\setbook.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\setdir.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\smallicons.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\unmaphost.bmp
# End Source File
# Begin Source File

SOURCE=..\..\..\pathfinder\work.gif
# End Source File
# End Target
# End Project
