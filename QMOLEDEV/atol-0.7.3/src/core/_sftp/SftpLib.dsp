# Microsoft Developer Studio Project File - Name="SftpLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=SftpLib - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "SftpLib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "SftpLib.mak" CFG="SftpLib - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "SftpLib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "SftpLib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "SftpLib - Win32 Profile" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "SftpLib - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x41a /d "NDEBUG"
# ADD RSC /l 0x41a /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\SftpLib.lib"

!ELSEIF  "$(CFG)" == "SftpLib - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x41a /d "_DEBUG"
# ADD RSC /l 0x41a /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\SftpLib.lib"

!ELSEIF  "$(CFG)" == "SftpLib - Win32 Profile"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "SftpLib___Win32_Profile"
# PROP BASE Intermediate_Dir "SftpLib___Win32_Profile"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Profile"
# PROP Intermediate_Dir "Profile"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x41a /d "NDEBUG"
# ADD RSC /l 0x41a /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo /out:"..\SftpLib.lib"
# ADD LIB32 /nologo /out:"..\SftpLib.lib"

!ENDIF 

# Begin Target

# Name "SftpLib - Win32 Release"
# Name "SftpLib - Win32 Debug"
# Name "SftpLib - Win32 Profile"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\Be_none.cpp
# End Source File
# Begin Source File

SOURCE=.\Console.cpp
# End Source File
# Begin Source File

SOURCE=.\Int64.cpp
# End Source File
# Begin Source File

SOURCE=.\Logging.cpp
# End Source File
# Begin Source File

SOURCE=.\Misc.cpp
# End Source File
# Begin Source File

SOURCE=.\Noise.cpp
# End Source File
# Begin Source File

SOURCE=.\Pageantc.cpp
# End Source File
# Begin Source File

SOURCE=.\Portfwd.cpp
# End Source File
# Begin Source File

SOURCE=.\proxy.cpp
# End Source File
# Begin Source File

SOURCE=.\Psftp.cpp
# End Source File
# Begin Source File

SOURCE=.\Settings.cpp
# End Source File
# Begin Source File

SOURCE=.\Sftp.cpp
# End Source File
# Begin Source File

SOURCE=.\Ssh.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshaes.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshblowf.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshbn.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshcrc.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshcrcda.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshdes.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshdh.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshdss.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshmd5.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshpubk.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshrand.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshrsa.cpp
# End Source File
# Begin Source File

SOURCE=.\SshSession.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshsh512.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshsha.cpp
# End Source File
# Begin Source File

SOURCE=.\Sshzlib.cpp
# End Source File
# Begin Source File

SOURCE=.\Tree234.cpp
# End Source File
# Begin Source File

SOURCE=.\Version.cpp
# End Source File
# Begin Source File

SOURCE=.\Winnet.cpp
# End Source File
# Begin Source File

SOURCE=.\Winstore.cpp
# End Source File
# Begin Source File

SOURCE=.\x11fwd.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Backend.h
# End Source File
# Begin Source File

SOURCE=.\INT64.H
# End Source File
# Begin Source File

SOURCE=.\MISC.H
# End Source File
# Begin Source File

SOURCE=.\NETWORK.H
# End Source File
# Begin Source File

SOURCE=.\PROXY.H
# End Source File
# Begin Source File

SOURCE=.\PUTTY.H
# End Source File
# Begin Source File

SOURCE=.\PUTTYMEM.H
# End Source File
# Begin Source File

SOURCE=.\SFTP.H
# End Source File
# Begin Source File

SOURCE=.\SSH.H
# End Source File
# Begin Source File

SOURCE=.\Sshaes.h
# End Source File
# Begin Source File

SOURCE=.\Sshblowf.h
# End Source File
# Begin Source File

SOURCE=.\Sshbn.h
# End Source File
# Begin Source File

SOURCE=.\Sshdh.h
# End Source File
# Begin Source File

SOURCE=.\Sshmd5.h
# End Source File
# Begin Source File

SOURCE=.\Sshrand.h
# End Source File
# Begin Source File

SOURCE=.\Sshrsa.h
# End Source File
# Begin Source File

SOURCE=.\SshSession.h
# End Source File
# Begin Source File

SOURCE=.\Sshsh512.h
# End Source File
# Begin Source File

SOURCE=.\Sshsha.h
# End Source File
# Begin Source File

SOURCE=.\Sshzlib.h
# End Source File
# Begin Source File

SOURCE=.\STORAGE.H
# End Source File
# Begin Source File

SOURCE=.\TREE234.H
# End Source File
# Begin Source File

SOURCE=.\x11fwd.h
# End Source File
# End Group
# End Target
# End Project
