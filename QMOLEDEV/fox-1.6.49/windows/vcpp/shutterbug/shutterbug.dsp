# Microsoft Developer Studio Project File - Name="shutterbug" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=shutterbug - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "shutterbug.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "shutterbug.mak" CFG="shutterbug - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "shutterbug - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "shutterbug - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "shutterbug - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /MT /W3 /GR /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "UNICODE" /FD /c
# SUBTRACT CPP /nologo /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 comctl32.lib wsock32.lib kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib mpr.lib /entry:"mainCRTStartup" /subsystem:windows /pdb:none /machine:I386
# SUBTRACT LINK32 /nologo

!ELSEIF  "$(CFG)" == "shutterbug - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /MTd /W3 /GR /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /FD /GZ /c
# SUBTRACT CPP /nologo /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 comctl32.lib wsock32.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib mpr.lib /entry:"mainCRTStartup" /subsystem:windows /pdb:none /debug /machine:I386
# SUBTRACT LINK32 /nologo

!ENDIF 

# Begin Target

# Name "shutterbug - Win32 Release"
# Name "shutterbug - Win32 Debug"
# Begin Source File

SOURCE=..\shutterbug\icons

!IF  "$(CFG)" == "shutterbug - Win32 Release"

# Begin Custom Build
WkspDir=.
InputPath=..\shutterbug\icons

BuildCmds= \
	cd ..\..\..\shutterbug \
	$(WkspDir)\reswrap\Release\reswrap -e -o icons.cpp line_0.gif line_1.gif line_2.gif line_3.gif line_4.gif line_5.gif line_6.gif line_7.gif line_8.gif shutterbug.gif tinyshutterbug.gif \
	$(WkspDir)\reswrap\Release\reswrap -i -o icons.h line_0.gif line_1.gif line_2.gif line_3.gif line_4.gif line_5.gif line_6.gif line_7.gif line_8.gif shutterbug.gif tinyshutterbug.gif \
	

"icons.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"icons.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "shutterbug - Win32 Debug"

# Begin Custom Build
WkspDir=.
InputPath=..\shutterbug\icons

BuildCmds= \
	cd ..\..\..\shutterbug \
	$(WkspDir)\reswrap\Debug\reswrap -e -o icons.cpp line_0.gif line_1.gif line_2.gif line_3.gif line_4.gif line_5.gif line_6.gif line_7.gif line_8.gif shutterbug.gif tinyshutterbug.gif \
	$(WkspDir)\reswrap\Debug\reswrap -i -o icons.h line_0.gif line_1.gif line_2.gif line_3.gif line_4.gif line_5.gif line_6.gif line_7.gif line_8.gif shutterbug.gif tinyshutterbug.gif \
	

"icons.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"icons.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\icons.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\icons.h
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_0.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_1.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_2.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_3.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_4.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_5.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_6.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_7.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\line_8.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\ShutterBug.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\shutterbug.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\ShutterBug.h
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\Snapper.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\Snapper.h
# End Source File
# Begin Source File

SOURCE=..\..\..\shutterbug\tinyshutterbug.gif
# End Source File
# End Target
# End Project
