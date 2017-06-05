# Microsoft Developer Studio Project File - Name="Atol" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Atol - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Atol.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Atol.mak" CFG="Atol - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Atol - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Atol - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Atol - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "bin/Release"
# PROP Intermediate_Dir "bin/Release/obj"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "C:\GTK\INCLUDE" /I "C:\GTK\INCLUDE\GLIB-2.0" /I "C:\GTK\INCLUDE\GTK-2.0" /I "C:\GTK\LIB\GLIB-2.0\INCLUDE" /I "C:\GTK\LIB\GTK-2.0\INCLUDE" /I "C:\GTK\INCLUDE\PANGO-1.0" /I "C:\GTK\INCLUDE\ATK-1.0" /I "c:\OpenSSL\include" /I "C:\GTK\include\cairo\\" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_INTL_REDIRECT_MACROS" /D "ENABLE_NLS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x41a /d "NDEBUG"
# ADD RSC /l 0x41a /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib gtk-win32-2.0.lib gdk-win32-2.0.lib glib-2.0.lib gobject-2.0.lib atk-1.0.lib gdk_pixbuf-2.0.lib pango-1.0.lib intl.lib gthread-2.0.lib /nologo /subsystem:windows /machine:I386 /libpath:"C:\gtk\lib\\" /libpath:"c:\OpenSSL\lib\VC\static\\"

!ELSEIF  "$(CFG)" == "Atol - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "bin/Debug"
# PROP Intermediate_Dir "bin/Debug/obj"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "C:\GTK\INCLUDE" /I "C:\GTK\INCLUDE\GLIB-2.0" /I "C:\GTK\INCLUDE\GTK-2.0" /I "C:\GTK\LIB\GLIB-2.0\INCLUDE" /I "C:\GTK\LIB\GTK-2.0\INCLUDE" /I "C:\GTK\INCLUDE\PANGO-1.0" /I "C:\GTK\INCLUDE\ATK-1.0" /I "c:\OpenSSL\include" /I "C:\GTK\include\cairo\\" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_INTL_REDIRECT_MACROS" /D "ENABLE_NLS" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x41a /d "_DEBUG"
# ADD RSC /l 0x41a /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib gtk-win32-2.0.lib gdk-win32-2.0.lib glib-2.0.lib gobject-2.0.lib atk-1.0.lib gdk_pixbuf-2.0.lib pango-1.0.lib intl.lib gthread-2.0.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept /libpath:"C:\gtk\lib\\" /libpath:"c:\OpenSSL\lib\VC\static\\"

!ENDIF 

# Begin Target

# Name "Atol - Win32 Release"
# Name "Atol - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\src\core\ArchiverPlugin.cpp
# End Source File
# Begin Source File

SOURCE=.\res\atol.rc
# End Source File
# Begin Source File

SOURCE=.\src\core\_util\Base64.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Be_none.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\blowfish.cpp
# End Source File
# Begin Source File

SOURCE=.\src\BookmarkEditDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\bootstart.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\BrowseBookmarkList.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\BrowseHistoryList.cpp
# End Source File
# Begin Source File

SOURCE=.\src\callbacks.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\ConnectionInfoList.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Console.cpp
# End Source File
# Begin Source File

SOURCE=.\src\CopyErrDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\Crc32.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\debug.cpp
# End Source File
# Begin Source File

SOURCE=.\src\DeleteDirDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\DeleteFileDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\DeleteStartDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\Dialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\DirWatcher.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\dll.cpp
# End Source File
# Begin Source File

SOURCE=.\src\DualPanel.cpp
# End Source File
# Begin Source File

SOURCE=.\src\EncryptionDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ExecutionThread.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\File64.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\File64Enc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FileList.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\FileListController.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FileListPanel.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FileMergeDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FileSearchDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FileSearchThread.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FileSelection.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FileSplitDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FileViewer.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FileViewerWnd.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\FilterDesc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\FindInFile.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\Format.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FormatBin.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FormatHex.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FormatTxt.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\ftpCore.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\FtpListParser.cpp
# End Source File
# Begin Source File

SOURCE=.\src\GuiBlockDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\GuiInputDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\GuiLanguage.cpp
# End Source File
# Begin Source File

SOURCE=.\src\GuiProgressDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\HashResultDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\HashTypeDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\IniFile.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Int64.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Logging.cpp
# End Source File
# Begin Source File

SOURCE=.\src\main.cpp
# End Source File
# Begin Source File

SOURCE=.\src\MainWindow.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\md5.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_util\MIMECode.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Misc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\NetBrowser.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Noise.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\Op.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpConnect.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpCopy.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpDecrypt.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpDelete.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpEncrypt.cpp
# End Source File
# Begin Source File

SOURCE=.\src\OpErrorDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpHash.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpManager.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpMerge.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpMove.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpPack.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpSplit.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpState.cpp
# End Source File
# Begin Source File

SOURCE=.\src\OpStateWithBlocking.cpp
# End Source File
# Begin Source File

SOURCE=.\src\OpStateWithProgress.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpThread.cpp
# End Source File
# Begin Source File

SOURCE=.\src\OptionsDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\OpUnpack.cpp
# End Source File
# Begin Source File

SOURCE=.\src\OverwriteDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\PackFilesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Pageantc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\PathName.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\Pidl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\PluginManager.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\PortableTrayIcon.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Portfwd.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\proxy.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxyFtp.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxyHttp.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxyLayer.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxySocks.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Psftp.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\Selection.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Settings.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sftp.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\SHA1.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\Shell.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\ShellContextMenu.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\SingleInstance.cpp
# End Source File
# Begin Source File

SOURCE=.\src\SiteManagerDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Ssh.cpp

!IF  "$(CFG)" == "Atol - Win32 Release"

!ELSEIF  "$(CFG)" == "Atol - Win32 Debug"

# ADD CPP /Zi

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshaes.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshblowf.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshbn.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshcrc.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshcrcda.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshdes.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshdh.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshdss.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshmd5.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshpubk.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshrand.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshrsa.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\SshSession.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshsh512.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshsha.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Sshzlib.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\StopWatch.cpp
# End Source File
# Begin Source File

SOURCE=.\src\viewer\Storage.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\String.cpp
# End Source File
# Begin Source File

SOURCE=.\src\support.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\System.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\SystemImpl.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\Thread.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ThreadSafeGui.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ThreadSafeMsgs.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\TrayIcon.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Tree234.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\util.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Version.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\Vfs.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsArchive.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsFtp.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsItem.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsListing.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsListingFiltered.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsLocal.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsManager.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsNet.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsRemote.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsSelection.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsSftp.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Winnet.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\Winstore.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\x11fwd.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\xSocket.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\xSSLContext.cpp
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\xSSLSocket.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\src\core\ArchiverPlugin.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\backend.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_util\Base64.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\blowfish.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\blowfish2.h
# End Source File
# Begin Source File

SOURCE=.\src\BookmarkEditDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\bootstart.h
# End Source File
# Begin Source File

SOURCE=.\src\core\BrowseBookmarkList.h
# End Source File
# Begin Source File

SOURCE=.\src\core\BrowseHistoryList.h
# End Source File
# Begin Source File

SOURCE=.\src\callbacks.h
# End Source File
# Begin Source File

SOURCE=.\src\core\ConnectionInfoList.h
# End Source File
# Begin Source File

SOURCE=.\src\CopyErrDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\Crc32.h
# End Source File
# Begin Source File

SOURCE=.\src\core\debug.h
# End Source File
# Begin Source File

SOURCE=.\src\DeleteDirDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\DeleteFileDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\DeleteStartDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\Dialog.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\DirWatcher.h
# End Source File
# Begin Source File

SOURCE=.\src\core\dll.h
# End Source File
# Begin Source File

SOURCE=.\src\DualPanel.h
# End Source File
# Begin Source File

SOURCE=.\src\EncryptionDialog.h
# End Source File
# Begin Source File

SOURCE=.\src\ExecutionThread.h
# End Source File
# Begin Source File

SOURCE=.\src\core\File64.h
# End Source File
# Begin Source File

SOURCE=.\src\core\File64Enc.h
# End Source File
# Begin Source File

SOURCE=.\src\FileList.h
# End Source File
# Begin Source File

SOURCE=.\src\core\FileListController.h
# End Source File
# Begin Source File

SOURCE=.\src\FileListPanel.h
# End Source File
# Begin Source File

SOURCE=.\src\FileMergeDialog.h
# End Source File
# Begin Source File

SOURCE=.\src\FileSearchDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\FileSearchThread.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FileSelection.h
# End Source File
# Begin Source File

SOURCE=.\src\FileSplitDialog.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FileViewer.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FileViewerWnd.h
# End Source File
# Begin Source File

SOURCE=.\src\core\FilterDesc.h
# End Source File
# Begin Source File

SOURCE=.\src\core\FindInFile.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\Format.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FormatBin.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FormatHex.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\FormatTxt.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\ftpCore.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\FtpListParser.h
# End Source File
# Begin Source File

SOURCE=.\src\GuiBlockDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\GuiInputDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\core\GuiLanguage.h
# End Source File
# Begin Source File

SOURCE=.\src\GuiProgressDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\HashResultDialog.h
# End Source File
# Begin Source File

SOURCE=.\src\HashTypeDialog.h
# End Source File
# Begin Source File

SOURCE=.\src\core\IniFile.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\int64.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\macros.h
# End Source File
# Begin Source File

SOURCE=.\src\MainWindow.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\md5.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_util\MIMECode.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\misc.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\NetBrowser.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\network.h
# End Source File
# Begin Source File

SOURCE=.\src\core\Op.h
# End Source File
# Begin Source File

SOURCE=.\src\core\opcodes.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpConnect.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpCopy.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpDecrypt.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpDelete.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpEncrypt.h
# End Source File
# Begin Source File

SOURCE=.\src\OpErrorDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpHash.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpManager.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpMerge.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpMove.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpPack.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpSplit.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpState.h
# End Source File
# Begin Source File

SOURCE=.\src\OpStateWithBlocking.h
# End Source File
# Begin Source File

SOURCE=.\src\OpStateWithProgress.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpThread.h
# End Source File
# Begin Source File

SOURCE=.\src\OptionsDialog.h
# End Source File
# Begin Source File

SOURCE=.\src\core\OpUnpack.h
# End Source File
# Begin Source File

SOURCE=.\src\OverwriteDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\PackFilesDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\core\PathName.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\Pidl.h
# End Source File
# Begin Source File

SOURCE=.\src\core\PluginManager.h
# End Source File
# Begin Source File

SOURCE=.\src\core\portablethread.h
# End Source File
# Begin Source File

SOURCE=.\src\core\PortableTrayIcon.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\proxy.h
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxyFtp.h
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxyHttp.h
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxyLayer.h
# End Source File
# Begin Source File

SOURCE=.\src\core\ProxySocks.h
# End Source File
# Begin Source File

SOURCE=.\src\core\pthread.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\putty.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\puttymem.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\Selection.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sftp.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_crypt\SHA1.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\Shell.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\ShellContextMenu.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\SingleInstance.h
# End Source File
# Begin Source File

SOURCE=.\src\SiteManagerDlg.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\ssh.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshaes.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshblowf.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshbn.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshdes.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshdh.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshmd5.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshrand.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshrsa.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshsession.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshsh512.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshsha.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\sshzlib.h
# End Source File
# Begin Source File

SOURCE=.\src\core\StopWatch.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\storage.h
# End Source File
# Begin Source File

SOURCE=.\src\viewer\Storage.h
# End Source File
# Begin Source File

SOURCE=.\src\core\String.h
# End Source File
# Begin Source File

SOURCE=.\src\support.h
# End Source File
# Begin Source File

SOURCE=.\src\core\System.h
# End Source File
# Begin Source File

SOURCE=.\src\core\Thread.h
# End Source File
# Begin Source File

SOURCE=.\src\ThreadSafeGui.h
# End Source File
# Begin Source File

SOURCE=.\src\ThreadSafeMsgs.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_win\TrayIcon.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\tree234.h
# End Source File
# Begin Source File

SOURCE=.\src\core\types.h
# End Source File
# Begin Source File

SOURCE=.\src\core\util.h
# End Source File
# Begin Source File

SOURCE=.\src\core\Vfs.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsArchive.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsFtp.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsItem.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsListing.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsListingFiltered.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsLocal.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsManager.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsNet.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsRemote.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsSelection.h
# End Source File
# Begin Source File

SOURCE=.\src\core\VfsSftp.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_sftp\x11fwd.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\xSocket.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\xSSLContext.h
# End Source File
# Begin Source File

SOURCE=.\src\core\_ftp\xSSLSocket.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\atol.ico
# End Source File
# End Group
# End Target
# End Project
