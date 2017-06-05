# Microsoft Developer Studio Project File - Name="adie" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=adie - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "adie.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "adie.mak" CFG="adie - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "adie - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "adie - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "adie - Win32 Release"

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
# ADD LINK32 winspool.lib comctl32.lib wsock32.lib ws2_32.lib kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib mpr.lib /nologo /entry:"mainCRTStartup" /subsystem:windows /pdb:none /machine:I386

!ELSEIF  "$(CFG)" == "adie - Win32 Debug"

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
# ADD CPP /MTd /W3 /GR /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
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

# Name "adie - Win32 Release"
# Name "adie - Win32 Debug"
# Begin Source File

SOURCE=..\..\..\adie\Adie.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\Adie.h
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\adie_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\big_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\bookdel_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\booknext_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\bookprev_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\bookset_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\close_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\colors_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\Commands.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\Commands.h
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\copy_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\cut_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\delete_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\delimit_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\fonts_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\help.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\help.h
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\help_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\HelpWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\HelpWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\Hilite.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\Hilite.h
# End Source File
# Begin Source File

SOURCE=.\icons

!IF  "$(CFG)" == "adie - Win32 Release"

# Begin Custom Build
WkspDir=.
InputPath=.\icons

BuildCmds= \
	cd ..\..\..\adie \
	$(WkspDir)\reswrap\Release\reswrap -e -o icons.cpp adie_gif.gif big_gif.gif bookdel_gif.gif booknext_gif.gif bookprev_gif.gif bookset_gif.gif close_gif.gif colors_gif.gif copy_gif.gif cut_gif.gif delete_gif.gif fonts_gif.gif help_gif.gif indent_gif.gif info_gif.gif lang_gif.gif new_gif.gif open_gif.gif palette_gif.gif paste_gif.gif pattern_gif.gif print_gif.gif quit_gif.gif redo_gif.gif reload_gif.gif save_gif.gif saveas_gif.gif saveall_gif.gif search_gif.gif searchnext_gif.gif searchprev_gif.gif shiftleft_gif.gif shiftright_gif.gif small_gif.gif styles_gif.gif syntax_gif.gif undo_gif.gif \
	$(WkspDir)\reswrap\Release\reswrap -i -o icons.h adie_gif.gif big_gif.gif bookdel_gif.gif booknext_gif.gif bookprev_gif.gif bookset_gif.gif close_gif.gif colors_gif.gif copy_gif.gif cut_gif.gif delete_gif.gif fonts_gif.gif help_gif.gif indent_gif.gif info_gif.gif lang_gif.gif new_gif.gif open_gif.gif palette_gif.gif paste_gif.gif pattern_gif.gif print_gif.gif quit_gif.gif redo_gif.gif reload_gif.gif save_gif.gif saveas_gif.gif saveall_gif.gif search_gif.gif searchnext_gif.gif searchprev_gif.gif shiftleft_gif.gif shiftright_gif.gif small_gif.gif styles_gif.gif syntax_gif.gif undo_gif.gif \
	

"icons.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"icons.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "adie - Win32 Debug"

# Begin Custom Build
WkspDir=.
InputPath=.\icons

BuildCmds= \
	cd ..\..\..\adie \
	$(WkspDir)\reswrap\Debug\reswrap -e -o icons.cpp adie_gif.gif big_gif.gif bookdel_gif.gif booknext_gif.gif bookprev_gif.gif bookset_gif.gif close_gif.gif colors_gif.gif copy_gif.gif cut_gif.gif delete_gif.gif fonts_gif.gif help_gif.gif indent_gif.gif info_gif.gif lang_gif.gif new_gif.gif open_gif.gif palette_gif.gif paste_gif.gif pattern_gif.gif print_gif.gif quit_gif.gif redo_gif.gif reload_gif.gif save_gif.gif saveas_gif.gif saveall_gif.gif search_gif.gif searchnext_gif.gif searchprev_gif.gif shiftleft_gif.gif shiftright_gif.gif small_gif.gif styles_gif.gif syntax_gif.gif undo_gif.gif \
	$(WkspDir)\reswrap\Debug\reswrap -i -o icons.h adie_gif.gif big_gif.gif bookdel_gif.gif booknext_gif.gif bookprev_gif.gif bookset_gif.gif close_gif.gif colors_gif.gif copy_gif.gif cut_gif.gif delete_gif.gif fonts_gif.gif help_gif.gif indent_gif.gif info_gif.gif lang_gif.gif new_gif.gif open_gif.gif palette_gif.gif paste_gif.gif pattern_gif.gif print_gif.gif quit_gif.gif redo_gif.gif reload_gif.gif save_gif.gif saveas_gif.gif saveall_gif.gif search_gif.gif searchnext_gif.gif searchprev_gif.gif shiftleft_gif.gif shiftright_gif.gif small_gif.gif styles_gif.gif syntax_gif.gif undo_gif.gif \
	

"icons.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"icons.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\adie\icons.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\icons.h
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\indent_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\info_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\lang_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\main.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\new_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\open_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\palette_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\parsesyntax.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\paste_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\pattern_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\Preferences.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\Preferences.h
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\print_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\quit_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\redo_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\reload_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\save_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\saveall_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\saveas_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\search_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\searchnext_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\searchprev_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\shiftleft_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\shiftright_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\small_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\styles_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\syntax_gif.gif
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\TextWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\TextWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\adie\undo_gif.gif
# End Source File
# End Target
# End Project
