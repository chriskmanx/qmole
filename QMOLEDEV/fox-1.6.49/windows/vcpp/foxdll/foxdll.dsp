# Microsoft Developer Studio Project File - Name="foxdll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=FOXDLL - WIN32 RELEASE
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "foxdll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "foxdll.mak" CFG="FOXDLL - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "foxdll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "foxdll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "foxdll - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\lib"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "FOXDLL_EXPORTS" /YX /FD /c
# ADD CPP /MD /W3 /GR /GX /Ox /Og /Oi /Os /Gf /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "UNICODE" /D "_USRDLL" /D "FOXDLL" /D "FOXDLL_EXPORTS" /D "HAVE_GL_H" /D "HAVE_GLU_H" /FD /c
# SUBTRACT CPP /nologo /Gy /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib opengl32.lib glu32.lib comctl32.lib winspool.lib wsock32.lib /nologo /dll /pdb:none /machine:I386 /out:"..\..\..\lib\FOXDLL-1.6.dll"

!ELSEIF  "$(CFG)" == "foxdll - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\lib"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "FOXDLL_EXPORTS" /YX /FD /GZ /c
# ADD CPP /MDd /W3 /GR /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /D "_USRDLL" /D "FOXDLL" /D "FOXDLL_EXPORTS" /D "HAVE_GL_H" /D "HAVE_GLU_H" /FD /GZ /c
# SUBTRACT CPP /nologo /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib advapi32.lib shell32.lib opengl32.lib glu32.lib comctl32.lib winspool.lib wsock32.lib /nologo /dll /pdb:none /debug /machine:I386 /out:"..\..\..\lib\FOXDLLD-1.6.dll"

!ENDIF 

# Begin Target

# Name "foxdll - Win32 Release"
# Name "foxdll - Win32 Debug"
# Begin Source File

SOURCE=..\..\..\include\fx.h
# End Source File
# Begin Source File

SOURCE=..\..\..\include\fx3d.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX4Splitter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX4Splitter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX7Segment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX7Segment.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX885910Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX885910Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX885911Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX885911Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX885913Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX885913Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX885914Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX885914Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX885915Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX885915Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX885916Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX885916Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88591Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88591Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88592Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88592Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88593Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88593Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88594Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88594Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88595Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88595Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88596Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88596Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88597Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88597Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88598Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88598Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FX88599Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FX88599Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXAccelTable.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXAccelTable.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXApp.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXApp.h
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXArray.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXArrowButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXArrowButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxascii.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\fxascii.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXBitmap.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXBitmap.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXBitmapFrame.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXBitmapFrame.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXBitmapView.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXBitmapView.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXBMPIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXBMPIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXBMPImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXBMPImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxbmpio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXBZFileStream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXBZFileStream.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCanvas.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCanvas.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCheckButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCheckButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXChoiceBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXChoiceBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXColorBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXColorDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXColorList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorNames.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorRing.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXColorRing.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorSelector.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXColorSelector.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorWell.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXColorWell.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXColorWheel.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXColorWheel.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXComboBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXComboBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXComposeContext.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXComposeContext.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXComposite.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXComposite.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1250Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1250Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1251Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1251Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1252Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1252Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1253Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1253Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1254Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1254Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1255Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1255Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1256Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1256Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1257Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1257Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP1258Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP1258Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP437Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP437Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP850Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP850Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP852Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP852Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP855Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP855Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP856Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP856Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP857Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP857Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP860Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP860Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP861Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP861Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP862Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP862Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP863Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP863Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP864Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP864Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP865Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP865Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP866Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP866Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP869Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP869Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCP874Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCP874Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCURCursor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCURCursor.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXCursor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXCursor.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDataTarget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDataTarget.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDate.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDC.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDC.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDCPrint.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDCPrint.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDCWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDCWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDebugTarget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDebugTarget.h
# End Source File
# Begin Source File

SOURCE=..\..\..\include\fxdefs.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDelegator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDelegator.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDial.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDial.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDialogBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDialogBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDict.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDict.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDir.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDir.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDirBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDirBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDirDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDirDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDirList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDirList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDirSelector.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDirSelector.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDLL.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDLL.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDockBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDockBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDockHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDockHandler.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDockSite.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDockSite.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDockTitle.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDockTitle.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDocument.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDocument.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDragCorner.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDragCorner.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDrawable.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDrawable.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXDriveBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXDriveBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXElement.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXException.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXException.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXExpression.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXExpression.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXExtentd.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXExtentd.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXExtentf.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXExtentf.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxezquantize.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFile.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFile.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFileDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFileDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFileDict.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFileDict.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFileList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFileList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxfilematch.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFileSelector.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFileSelector.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFileStream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFileStream.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFoldingList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFoldingList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFont.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFont.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFontDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFontDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFontSelector.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFontSelector.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXFrame.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXFrame.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxfsquantize.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGIFCursor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGIFCursor.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGIFIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGIFIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGIFImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGIFImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxgifio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLCanvas.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLCanvas.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLCone.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLCone.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLContext.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLContext.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLCube.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLCube.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLCylinder.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLCylinder.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLObject.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLObject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLShape.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLShape.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLSphere.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLSphere.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLTriangleMesh.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLTriangleMesh.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLViewer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLViewer.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGLVisual.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGLVisual.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGradientBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGradientBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGroupBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGroupBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGUISignal.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGUISignal.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXGZFileStream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXGZFileStream.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXHash.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXHash.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXHeader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXHeader.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXHorizontalFrame.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXHorizontalFrame.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXICOIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXICOIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXICOImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXICOImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxicoio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXIconDict.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXIconDict.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXIconList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXIconList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXIconSource.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXIconSource.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXId.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXId.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXIFFIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXIFFIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXIFFImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXIFFImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxiffio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXImageFrame.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXImageFrame.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXImageView.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXImageView.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXInputDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXInputDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXIO.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXIO.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxjpegio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXJPGIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXJPGIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXJPGImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXJPGImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxkeyboard.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\fxkeys.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxkeysym.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXKnob.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXKnob.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXKOI8RCodec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXKOI8RCodec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXLabel.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXLabel.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXListBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXListBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMainWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMainWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMat3d.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMat3d.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMat3f.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMat3f.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMat4d.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMat4d.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMat4f.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMat4f.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMatrix.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMatrix.h
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMBCSCodec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMDIButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMDIButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMDIChild.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMDIChild.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMDIClient.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMDIClient.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMemMap.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMemMap.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMemoryStream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMemoryStream.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuCaption.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuCaption.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuCascade.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuCascade.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuCheck.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuCheck.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuCommand.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuCommand.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuPane.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuPane.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuRadio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuRadio.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuSeparator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuSeparator.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMenuTitle.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMenuTitle.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXMessageBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXMessageBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXObject.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXObject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXObjectList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXObjectList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXOptionMenu.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXOptionMenu.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPacker.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPacker.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxparsegeometry.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPath.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPath.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPCXIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPCXIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPCXImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPCXImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxpcxio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPicker.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPicker.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPipe.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPipe.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPNGIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPNGIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPNGImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPNGImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxpngio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPoint.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPoint.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPopup.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPopup.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPPMIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPPMIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPPMImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPPMImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxppmio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXPrintDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXPrintDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxpriv.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxpriv.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXProgressBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXProgressBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXProgressDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXProgressDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxpsio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXQuatd.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXQuatd.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXQuatf.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXQuatf.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRadioButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRadioButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRanged.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRanged.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRangef.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRangef.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRASIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRASIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRASImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRASImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxrasio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRealSlider.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRealSlider.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRealSpinner.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRealSpinner.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRecentFiles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRecentFiles.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRectangle.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRectangle.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRegion.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRegion.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRegistry.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRegistry.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXReplaceDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXReplaceDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRex.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRex.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRGBIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRGBIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRGBImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRGBImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxrgbio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRootWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRootWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRuler.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRuler.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXRulerView.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXRulerView.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXScrollArea.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXScrollArea.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXScrollBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXScrollBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXScrollPane.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXScrollPane.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXScrollWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXScrollWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSearchDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSearchDialog.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSeparator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSeparator.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSettings.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSettings.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXShell.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXShell.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXShutter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXShutter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSize.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSize.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSlider.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSlider.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSocket.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSocket.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSphered.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSphered.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSpheref.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSpheref.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSpinner.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSpinner.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSplashWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSplashWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSplitter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSplitter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSpring.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSpring.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXStat.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXStat.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXStatusBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXStatusBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXStatusLine.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXStatusLine.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXStream.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXStream.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXString.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXString.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXStringDict.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXStringDict.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSwitcher.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSwitcher.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXSystem.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXSystem.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTabBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTabBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTabBook.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTabBook.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTabItem.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTabItem.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTable.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTable.h
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTableItem.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxtargaio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXText.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXText.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTextCodec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTextCodec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTextField.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTextField.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTGAIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTGAIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTGAImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTGAImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXThread.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXThread.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTIFIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTIFIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTIFImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTIFImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxtifio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXToggleButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXToggleButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXToolBar.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXToolBar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXToolBarGrip.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXToolBarGrip.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXToolBarShell.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXToolBarShell.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXToolBarTab.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXToolBarTab.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXToolTip.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXToolTip.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTopWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTopWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTranslator.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTranslator.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTreeList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTreeList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTreeListBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTreeListBox.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXTriStateButton.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXTriStateButton.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXUndoList.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXUndoList.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxunicode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\fxunicode.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXURL.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXURL.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXUTF16Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXUTF16Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXUTF32Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXUTF32Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXUTF8Codec.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXUTF8Codec.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxutils.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVec2d.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVec2d.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVec2f.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVec2f.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVec3d.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVec3d.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVec3f.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVec3f.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVec4d.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVec4d.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVec4f.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVec4f.h
# End Source File
# Begin Source File

SOURCE=..\..\..\include\fxver.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVerticalFrame.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVerticalFrame.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXVisual.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXVisual.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXWizard.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXWizard.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxwuquantize.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXXBMIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXXBMIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXXBMImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXXBMImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxxbmio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXXPMIcon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXXPMIcon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\FXXPMImage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\FXXPMImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\fxxpmio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\icons.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\icons.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\jitter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\strtoll.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\src\version.rc
# End Source File
# Begin Source File

SOURCE=..\..\..\src\vsscanf.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\include\xincs.h
# End Source File
# End Target
# End Project
