#
# Atol file manager - Linux makefile
# (http://atol.sf.net)
#
# Usage: type "make help" for description of make options
#

VERSION=0.7.3

# define install prefix directory
prefix=/usr
# define top of RPM build area
topdir=/usr/src/redhat
# define source archive name
archive=atol-$(VERSION)_src

#define plugin install path
plugin_dir=$(DESTDIR)$(prefix)/local/lib/atol
#comment this to remove gnome-vfs support
HAVE_GNOME_VFS=1

# To profile Atol:
# 1. uncomment "PROFILE=..." line below 
# 2. comment out "LOPTIMIZE=..." line
# 3. comment out "strip $(BIN)/atol" line
# 4. rebuild application
# 5. start and use application
# 6. profiling result is stored in a file "gmon.out" 
# 7. exec "gprof ./bin/atol > aaa" to write results in a readable form into file "aaa"
#PROFILE=-pg

#DEBUG=-g -D_DEBUG

CXX=g++ 
CFLAGS=-Wall 
FLAGS=$(CFLAGS)
BIN=./bin
OBJ=./bin

# define processing output
Q=@
QC=@echo '===> Compiling $<'; 
QL=@echo '===> Linking $@';

# using "--as-needed" to cut down link requirements (check by "readelf -d /usr/bin/atol | grep NEEDED")
LOPTIMIZE=-Os -Wl,--as-needed

# define compile/link command line with flags
CC=$(QC) $(CXX) $(DEBUG) $(PROFILE) -pthread -DENABLE_NLS
LD=$(QL) $(CXX) $(DEBUG) $(PROFILE) -ldl -export-dynamic -pthread $(LOPTIMIZE) 

LIBS=gtk+-2.0 gdk-2.0 glib-2.0 gthread-2.0 libssl
ifdef HAVE_GNOME_VFS
 LIBS += gnome-vfs-2.0
endif
GTKCFLAGS =`pkg-config --cflags $(LIBS)`
GTKLIBS =`pkg-config --libs $(LIBS)`

#
# define default project to build
#
all: atol plugins

#
# list of all objects to build (atol dependency)
#
OBJS= $(OBJ)/main.o $(OBJ)/support.o $(OBJ)/callbacks.o \
	$(OBJ)/BrowseHistoryList.o $(OBJ)/File64.o \
	$(OBJ)/GuiLanguage.o $(OBJ)/IniFile.o $(OBJ)/OpCopy.o $(OBJ)/Op.o $(OBJ)/OpDelete.o \
	$(OBJ)/OpMove.o $(OBJ)/OpSplit.o $(OBJ)/OpState.o $(OBJ)/OpThread.o $(OBJ)/PathName.o \
	$(OBJ)/String.o $(OBJ)/Thread.o $(OBJ)/Vfs.o $(OBJ)/VfsItem.o $(OBJ)/VfsListing.o \
	$(OBJ)/VfsListingFiltered.o $(OBJ)/VfsLocal.o $(OBJ)/VfsManager.o $(OBJ)/VfsRemote.o \
	$(OBJ)/VfsSelection.o $(OBJ)/FileList.o $(OBJ)/FileListPanel.o $(OBJ)/FileListController.o \
	$(OBJ)/Dialog.o $(OBJ)/OpStateWithProgress.o $(OBJ)/GuiProgressDlg.o $(OBJ)/OpManager.o \
	$(OBJ)/ThreadSafeGui.o $(OBJ)/ThreadSafeMsgs.o $(OBJ)/GuiInputDlg.o $(OBJ)/FilterDesc.o \
	$(OBJ)/FindInFile.o $(OBJ)/util.o $(OBJ)/DualPanel.o $(OBJ)/dll.o $(OBJ)/ArchiverPlugin.o \
	$(OBJ)/PluginManager.o $(OBJ)/VfsArchive.o $(OBJ)/eggtrayicon.o $(OBJ)/PortableTrayIcon.o \
	$(OBJ)/DeleteStartDlg.o $(OBJ)/OverwriteDlg.o $(OBJ)/DeleteFileDlg.o $(OBJ)/OptionsDialog.o \
	$(OBJ)/FileSearchDlg.o $(OBJ)/FileSearchThread.o $(OBJ)/OpHash.o $(OBJ)/OpPack.o \
	$(OBJ)/OpUnpack.o $(OBJ)/HashResultDialog.o $(OBJ)/HashTypeDialog.o $(OBJ)/PackFilesDlg.o \
	$(OBJ)/blowfish.o $(OBJ)/Crc32.o $(OBJ)/md5.o $(OBJ)/SHA1.o $(OBJ)/SingleInstance.o \
	$(OBJ)/FileMergeDialog.o $(OBJ)/FileSplitDialog.o $(OBJ)/OpMerge.o $(OBJ)/OpEncrypt.o \
	$(OBJ)/OpDecrypt.o $(OBJ)/EncryptionDialog.o $(OBJ)/BrowseBookmarkList.o $(OBJ)/CopyErrDlg.o \
	$(OBJ)/BookmarkEditDlg.o $(OBJ)/MainWindow.o $(OBJ)/FileViewer.o $(OBJ)/FileViewerWnd.o \
	$(OBJ)/Storage.o $(OBJ)/Format.o $(OBJ)/FormatTxt.o $(OBJ)/FormatBin.o $(OBJ)/FormatHex.o \
	$(OBJ)/Selection.o $(OBJ)/StopWatch.o $(OBJ)/debug.o $(OBJ)/System.o $(OBJ)/SystemImpl.o \
	$(OBJ)/SiteManagerDlg.o $(OBJ)/ftpCore.o $(OBJ)/FtpListParser.o $(OBJ)/xSocket.o \
	$(OBJ)/ProxyLayer.o $(OBJ)/ProxyFtp.o $(OBJ)/ProxyHttp.o $(OBJ)/ProxySocks.o \
	$(OBJ)/Base64.o $(OBJ)/MIMECode.o $(OBJ)/VfsFtp.o $(OBJ)/ConnectionInfoList.o $(OBJ)/VfsSftp.o \
	$(OBJ)/OpConnect.o $(OBJ)/File64Enc.o $(OBJ)/DeleteDirDlg.o $(OBJ)/OpErrorDlg.o $(OBJ)/VfsNet.o \
	$(OBJ)/GuiBlockDlg.o $(OBJ)/OpStateWithBlocking.o $(OBJ)/ExecutionThread.o \
	$(OBJ)/xSSLSocket.o $(OBJ)/xSSLContext.o

COREPATH=./src/core
GUIPATH=./src

# link program binary
atol: updatesrc $(OBJS)
	$(LD) $(OBJS) -o $(BIN)/atol $(FLAGS) $(LDFLAGS) $(GTKLIBS)

# compile individual objects
$(OBJ)/main.o: $(GUIPATH)/main.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/main.cpp -o $(OBJ)/main.o $(GTKCFLAGS)

$(OBJ)/callbacks.o: $(GUIPATH)/callbacks.cpp $(GUIPATH)/callbacks.h
	$(CC) $(FLAGS) -c $(GUIPATH)/callbacks.cpp -o $(OBJ)/callbacks.o $(GTKCFLAGS)

$(OBJ)/support.o: $(GUIPATH)/support.cpp $(GUIPATH)/support.h
	$(CC) $(FLAGS) -c $(GUIPATH)/support.cpp -o $(OBJ)/support.o $(GTKCFLAGS)

$(OBJ)/Dialog.o: $(GUIPATH)/Dialog.cpp $(GUIPATH)/Dialog.h
	$(CC) $(FLAGS) -c $(GUIPATH)/Dialog.cpp -o $(OBJ)/Dialog.o $(GTKCFLAGS)
 
$(OBJ)/OpStateWithProgress.o: $(GUIPATH)/OpStateWithProgress.cpp $(GUIPATH)/OpStateWithProgress.h
	$(CC) $(FLAGS) -c $(GUIPATH)/OpStateWithProgress.cpp -o $(OBJ)/OpStateWithProgress.o $(GTKCFLAGS)

$(OBJ)/ThreadSafeGui.o: $(GUIPATH)/ThreadSafeGui.cpp $(GUIPATH)/ThreadSafeGui.h
	$(CC) $(FLAGS) -c $(GUIPATH)/ThreadSafeGui.cpp -o $(OBJ)/ThreadSafeGui.o $(GTKCFLAGS)

$(OBJ)/ThreadSafeMsgs.o: $(GUIPATH)/ThreadSafeMsgs.cpp $(GUIPATH)/ThreadSafeMsgs.h
	$(CC) $(FLAGS) -c $(GUIPATH)/ThreadSafeMsgs.cpp -o $(OBJ)/ThreadSafeMsgs.o $(GTKCFLAGS)

$(OBJ)/GuiProgressDlg.o: $(GUIPATH)/GuiProgressDlg.cpp $(GUIPATH)/GuiProgressDlg.h
	$(CC) $(FLAGS) -c $(GUIPATH)/GuiProgressDlg.cpp -o $(OBJ)/GuiProgressDlg.o $(GTKCFLAGS)

$(OBJ)/GuiInputDlg.o: $(GUIPATH)/GuiInputDlg.cpp $(GUIPATH)/GuiInputDlg.h
	$(CC) $(FLAGS) -c $(GUIPATH)/GuiInputDlg.cpp -o $(OBJ)/GuiInputDlg.o $(GTKCFLAGS)

$(OBJ)/FileListPanel.o: $(GUIPATH)/FileListPanel.cpp $(GUIPATH)/FileListPanel.h
	$(CC) $(FLAGS) -c $(GUIPATH)/FileListPanel.cpp -o $(OBJ)/FileListPanel.o $(GTKCFLAGS)

$(OBJ)/OpManager.o: $(COREPATH)/OpManager.cpp $(COREPATH)/OpManager.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpManager.cpp -o $(OBJ)/OpManager.o $(GTKCFLAGS)

$(OBJ)/FileListController.o: $(COREPATH)/FileListController.cpp $(COREPATH)/FileListController.h
	$(CC) $(FLAGS) -c $(COREPATH)/FileListController.cpp -o $(OBJ)/FileListController.o $(GTKCFLAGS)

$(OBJ)/BrowseHistoryList.o: $(COREPATH)/BrowseHistoryList.cpp $(COREPATH)/BrowseHistoryList.h
	$(CC) $(FLAGS) -c $(COREPATH)/BrowseHistoryList.cpp -o $(OBJ)/BrowseHistoryList.o $(GTKCFLAGS)

$(OBJ)/File64.o: $(COREPATH)/File64.cpp $(COREPATH)/File64.h
	$(CC) $(FLAGS) -c $(COREPATH)/File64.cpp -o $(OBJ)/File64.o $(GTKCFLAGS)

$(OBJ)/GuiLanguage.o: $(COREPATH)/GuiLanguage.cpp $(COREPATH)/GuiLanguage.h
	$(CC) $(FLAGS) -c $(COREPATH)/GuiLanguage.cpp -o $(OBJ)/GuiLanguage.o $(GTKCFLAGS)

$(OBJ)/IniFile.o: $(COREPATH)/IniFile.cpp $(COREPATH)/IniFile.h
	$(CC) $(FLAGS) -c $(COREPATH)/IniFile.cpp -o $(OBJ)/IniFile.o $(GTKCFLAGS)

$(OBJ)/OpCopy.o: $(COREPATH)/OpCopy.cpp $(COREPATH)/OpCopy.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpCopy.cpp -o $(OBJ)/OpCopy.o $(GTKCFLAGS)

$(OBJ)/Op.o: $(COREPATH)/Op.cpp $(COREPATH)/Op.h
	$(CC) $(FLAGS) -c $(COREPATH)/Op.cpp -o $(OBJ)/Op.o $(GTKCFLAGS)

$(OBJ)/OpDelete.o: $(COREPATH)/OpDelete.cpp $(COREPATH)/OpDelete.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpDelete.cpp -o $(OBJ)/OpDelete.o $(GTKCFLAGS)

$(OBJ)/OpMove.o: $(COREPATH)/OpMove.cpp $(COREPATH)/OpMove.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpMove.cpp -o $(OBJ)/OpMove.o $(GTKCFLAGS)

$(OBJ)/OpSplit.o: $(COREPATH)/OpSplit.cpp $(COREPATH)/OpSplit.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpSplit.cpp -o $(OBJ)/OpSplit.o $(GTKCFLAGS)

$(OBJ)/OpState.o: $(COREPATH)/OpState.cpp $(COREPATH)/OpState.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpState.cpp -o $(OBJ)/OpState.o $(GTKCFLAGS)

$(OBJ)/OpThread.o: $(COREPATH)/OpThread.cpp $(COREPATH)/OpThread.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpThread.cpp -o $(OBJ)/OpThread.o $(GTKCFLAGS)

$(OBJ)/PathName.o: $(COREPATH)/PathName.cpp $(COREPATH)/PathName.h
	$(CC) $(FLAGS) -c $(COREPATH)/PathName.cpp -o $(OBJ)/PathName.o $(GTKCFLAGS)
	
$(OBJ)/String.o: $(COREPATH)/String.cpp $(COREPATH)/String.h
	$(CC) $(FLAGS) -c $(COREPATH)/String.cpp -o $(OBJ)/String.o $(GTKCFLAGS)

$(OBJ)/Thread.o: $(COREPATH)/Thread.cpp $(COREPATH)/Thread.h
	$(CC) $(FLAGS) -c $(COREPATH)/Thread.cpp -o $(OBJ)/Thread.o $(GTKCFLAGS)

$(OBJ)/Vfs.o: $(COREPATH)/Vfs.cpp $(COREPATH)/Vfs.h
	$(CC) $(FLAGS) -c $(COREPATH)/Vfs.cpp -o $(OBJ)/Vfs.o $(GTKCFLAGS)

$(OBJ)/VfsItem.o: $(COREPATH)/VfsItem.cpp $(COREPATH)/VfsItem.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsItem.cpp -o $(OBJ)/VfsItem.o $(GTKCFLAGS)

$(OBJ)/VfsListing.o: $(COREPATH)/VfsListing.cpp $(COREPATH)/VfsListing.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsListing.cpp -o $(OBJ)/VfsListing.o $(GTKCFLAGS)

$(OBJ)/VfsListingFiltered.o: $(COREPATH)/VfsListingFiltered.cpp $(COREPATH)/VfsListingFiltered.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsListingFiltered.cpp -o $(OBJ)/VfsListingFiltered.o $(GTKCFLAGS)

$(OBJ)/VfsLocal.o: $(COREPATH)/VfsLocal.cpp $(COREPATH)/VfsLocal.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsLocal.cpp -o $(OBJ)/VfsLocal.o $(GTKCFLAGS)

$(OBJ)/VfsManager.o: $(COREPATH)/VfsManager.cpp $(COREPATH)/VfsManager.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsManager.cpp -o $(OBJ)/VfsManager.o $(GTKCFLAGS)

$(OBJ)/VfsRemote.o: $(COREPATH)/VfsRemote.cpp $(COREPATH)/VfsRemote.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsRemote.cpp -o $(OBJ)/VfsRemote.o $(GTKCFLAGS)

$(OBJ)/VfsSelection.o: $(COREPATH)/VfsSelection.cpp $(COREPATH)/VfsSelection.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsSelection.cpp -o $(OBJ)/VfsSelection.o $(GTKCFLAGS)

$(OBJ)/FileList.o: $(GUIPATH)/FileList.cpp $(GUIPATH)/FileList.h
	$(CC) $(FLAGS) -c $(GUIPATH)/FileList.cpp -o $(OBJ)/FileList.o $(GTKCFLAGS)

$(OBJ)/FilterDesc.o: $(COREPATH)/FilterDesc.cpp $(COREPATH)/FilterDesc.h
	$(CC) $(FLAGS) -c $(COREPATH)/FilterDesc.cpp -o $(OBJ)/FilterDesc.o $(GTKCFLAGS)

$(OBJ)/FindInFile.o: $(COREPATH)/FindInFile.cpp $(COREPATH)/FindInFile.h
	$(CC) $(FLAGS) -c $(COREPATH)/FindInFile.cpp -o $(OBJ)/FindInFile.o $(GTKCFLAGS)

$(OBJ)/util.o: $(COREPATH)/util.cpp $(COREPATH)/util.h
	$(CC) $(FLAGS) -c $(COREPATH)/util.cpp -o $(OBJ)/util.o $(GTKCFLAGS)

$(OBJ)/DualPanel.o: $(GUIPATH)/DualPanel.cpp $(GUIPATH)/DualPanel.h
	$(CC) $(FLAGS) -c $(GUIPATH)/DualPanel.cpp -o $(OBJ)/DualPanel.o $(GTKCFLAGS)

$(OBJ)/dll.o: $(COREPATH)/dll.cpp $(COREPATH)/dll.h
	$(CC) $(FLAGS) -c $(COREPATH)/dll.cpp -o $(OBJ)/dll.o $(GTKCFLAGS)

$(OBJ)/ArchiverPlugin.o: $(COREPATH)/ArchiverPlugin.cpp $(COREPATH)/ArchiverPlugin.h
	$(CC) $(FLAGS) -c $(COREPATH)/ArchiverPlugin.cpp -o $(OBJ)/ArchiverPlugin.o $(GTKCFLAGS)

$(OBJ)/PluginManager.o: $(COREPATH)/PluginManager.cpp $(COREPATH)/PluginManager.h
	$(CC) $(FLAGS) -c $(COREPATH)/PluginManager.cpp -o $(OBJ)/PluginManager.o $(GTKCFLAGS)

$(OBJ)/VfsArchive.o: $(COREPATH)/VfsArchive.cpp $(COREPATH)/VfsArchive.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsArchive.cpp -o $(OBJ)/VfsArchive.o $(GTKCFLAGS)

$(OBJ)/eggtrayicon.o: $(COREPATH)/_unx/eggtrayicon.cpp
	$(CC) $(FLAGS) -c $(COREPATH)/_unx/eggtrayicon.cpp -o $(OBJ)/eggtrayicon.o $(GTKCFLAGS)

$(OBJ)/PortableTrayIcon.o: $(COREPATH)/PortableTrayIcon.cpp
	$(CC) $(FLAGS) -c $(COREPATH)/PortableTrayIcon.cpp -o $(OBJ)/PortableTrayIcon.o $(GTKCFLAGS)

$(OBJ)/DeleteStartDlg.o: $(GUIPATH)/DeleteStartDlg.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/DeleteStartDlg.cpp -o $(OBJ)/DeleteStartDlg.o $(GTKCFLAGS)

$(OBJ)/OverwriteDlg.o: $(GUIPATH)/OverwriteDlg.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/OverwriteDlg.cpp -o $(OBJ)/OverwriteDlg.o $(GTKCFLAGS)

$(OBJ)/DeleteFileDlg.o: $(GUIPATH)/DeleteFileDlg.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/DeleteFileDlg.cpp -o $(OBJ)/DeleteFileDlg.o $(GTKCFLAGS)

$(OBJ)/OptionsDialog.o: $(GUIPATH)/OptionsDialog.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/OptionsDialog.cpp -o $(OBJ)/OptionsDialog.o $(GTKCFLAGS)

$(OBJ)/FileSearchDlg.o: $(GUIPATH)/FileSearchDlg.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/FileSearchDlg.cpp -o $(OBJ)/FileSearchDlg.o $(GTKCFLAGS)

$(OBJ)/FileSearchThread.o: $(GUIPATH)/FileSearchThread.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/FileSearchThread.cpp -o $(OBJ)/FileSearchThread.o $(GTKCFLAGS)

$(OBJ)/OpHash.o: $(COREPATH)/OpHash.cpp $(COREPATH)/OpHash.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpHash.cpp -o $(OBJ)/OpHash.o $(GTKCFLAGS)

$(OBJ)/OpPack.o: $(COREPATH)/OpPack.cpp $(COREPATH)/OpPack.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpPack.cpp -o $(OBJ)/OpPack.o $(GTKCFLAGS)

$(OBJ)/OpUnpack.o: $(COREPATH)/OpUnpack.cpp $(COREPATH)/OpUnpack.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpUnpack.cpp -o $(OBJ)/OpUnpack.o $(GTKCFLAGS)

$(OBJ)/HashResultDialog.o: $(GUIPATH)/HashResultDialog.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/HashResultDialog.cpp -o $(OBJ)/HashResultDialog.o $(GTKCFLAGS)

$(OBJ)/HashTypeDialog.o: $(GUIPATH)/HashTypeDialog.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/HashTypeDialog.cpp -o $(OBJ)/HashTypeDialog.o $(GTKCFLAGS)

$(OBJ)/PackFilesDlg.o: $(GUIPATH)/PackFilesDlg.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/PackFilesDlg.cpp -o $(OBJ)/PackFilesDlg.o $(GTKCFLAGS)

$(OBJ)/blowfish.o: $(COREPATH)/_crypt/blowfish.cpp $(COREPATH)/_crypt/blowfish.h $(COREPATH)/_crypt/blowfish2.h
	$(CC) $(FLAGS) -c $(COREPATH)/_crypt/blowfish.cpp -o $(OBJ)/blowfish.o

$(OBJ)/Crc32.o: $(COREPATH)/_crypt/Crc32.cpp $(COREPATH)/_crypt/Crc32.h
	$(CC) $(FLAGS) -c $(COREPATH)/_crypt/Crc32.cpp -o $(OBJ)/Crc32.o

$(OBJ)/md5.o: $(COREPATH)/_crypt/md5.cpp $(COREPATH)/_crypt/md5.h
	$(CC) $(FLAGS) -c $(COREPATH)/_crypt/md5.cpp -o $(OBJ)/md5.o

$(OBJ)/SHA1.o: $(COREPATH)/_crypt/SHA1.cpp $(COREPATH)/_crypt/SHA1.h
	$(CC) $(FLAGS) -c $(COREPATH)/_crypt/SHA1.cpp -o $(OBJ)/SHA1.o

$(OBJ)/SingleInstance.o: $(COREPATH)/_unx/SingleInstance.cpp $(COREPATH)/_unx/SingleInstance.h
	$(CC) $(FLAGS) -c $(COREPATH)/_unx/SingleInstance.cpp -o $(OBJ)/SingleInstance.o  $(GTKCFLAGS)

$(OBJ)/FileMergeDialog.o: $(GUIPATH)/FileMergeDialog.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/FileMergeDialog.cpp -o $(OBJ)/FileMergeDialog.o $(GTKCFLAGS)

$(OBJ)/FileSplitDialog.o: $(GUIPATH)/FileSplitDialog.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/FileSplitDialog.cpp -o $(OBJ)/FileSplitDialog.o $(GTKCFLAGS)

$(OBJ)/OpMerge.o: $(COREPATH)/OpMerge.cpp $(COREPATH)/OpMerge.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpMerge.cpp -o $(OBJ)/OpMerge.o $(GTKCFLAGS)

$(OBJ)/OpEncrypt.o: $(COREPATH)/OpEncrypt.cpp $(COREPATH)/OpEncrypt.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpEncrypt.cpp -o $(OBJ)/OpEncrypt.o $(GTKCFLAGS)

$(OBJ)/OpDecrypt.o: $(COREPATH)/OpDecrypt.cpp $(COREPATH)/OpDecrypt.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpDecrypt.cpp -o $(OBJ)/OpDecrypt.o $(GTKCFLAGS)
	
$(OBJ)/EncryptionDialog.o: $(GUIPATH)/EncryptionDialog.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/EncryptionDialog.cpp -o $(OBJ)/EncryptionDialog.o $(GTKCFLAGS)
 
$(OBJ)/BrowseBookmarkList.o: $(COREPATH)/BrowseBookmarkList.cpp $(COREPATH)/BrowseBookmarkList.h
	$(CC) $(FLAGS) -c $(COREPATH)/BrowseBookmarkList.cpp -o $(OBJ)/BrowseBookmarkList.o  $(GTKCFLAGS) 

$(OBJ)/BookmarkEditDlg.o: $(GUIPATH)/BookmarkEditDlg.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/BookmarkEditDlg.cpp -o $(OBJ)/BookmarkEditDlg.o $(GTKCFLAGS)

$(OBJ)/MainWindow.o: $(GUIPATH)/MainWindow.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/MainWindow.cpp -o $(OBJ)/MainWindow.o $(GTKCFLAGS)

$(OBJ)/FileViewer.o: $(GUIPATH)/viewer/FileViewer.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/FileViewer.cpp -o $(OBJ)/FileViewer.o $(GTKCFLAGS) 

$(OBJ)/FileViewerWnd.o: $(GUIPATH)/viewer/FileViewerWnd.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/FileViewerWnd.cpp -o $(OBJ)/FileViewerWnd.o $(GTKCFLAGS)

$(OBJ)/Storage.o: $(GUIPATH)/viewer/Storage.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/Storage.cpp -o $(OBJ)/Storage.o $(GTKCFLAGS)

$(OBJ)/Format.o: $(GUIPATH)/viewer/Format.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/Format.cpp -o $(OBJ)/Format.o $(GTKCFLAGS)

$(OBJ)/FormatTxt.o: $(GUIPATH)/viewer/FormatTxt.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/FormatTxt.cpp -o $(OBJ)/FormatTxt.o $(GTKCFLAGS)

$(OBJ)/FormatBin.o: $(GUIPATH)/viewer/FormatBin.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/FormatBin.cpp -o $(OBJ)/FormatBin.o $(GTKCFLAGS)

$(OBJ)/FormatHex.o: $(GUIPATH)/viewer/FormatHex.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/FormatHex.cpp -o $(OBJ)/FormatHex.o $(GTKCFLAGS)

$(OBJ)/Selection.o: $(GUIPATH)/viewer/Selection.cpp
	$(CC) $(FLAGS) -c $(GUIPATH)/viewer/Selection.cpp -o $(OBJ)/Selection.o $(GTKCFLAGS)

$(OBJ)/StopWatch.o: $(COREPATH)/StopWatch.cpp $(COREPATH)/StopWatch.h
	$(CC) $(FLAGS) -c $(COREPATH)/StopWatch.cpp -o $(OBJ)/StopWatch.o $(GTKCFLAGS)

$(OBJ)/debug.o: $(COREPATH)/debug.cpp $(COREPATH)/debug.h
	$(CC) $(FLAGS) -c $(COREPATH)/debug.cpp -o $(OBJ)/debug.o $(GTKCFLAGS)

$(OBJ)/System.o: $(COREPATH)/System.cpp $(COREPATH)/System.h
	$(CC) $(FLAGS) -c $(COREPATH)/System.cpp -o $(OBJ)/System.o $(GTKCFLAGS)

$(OBJ)/SystemImpl.o: $(COREPATH)/_unx/SystemImpl.cpp $(COREPATH)/System.h
	$(CC) $(FLAGS) -c $(COREPATH)/_unx/SystemImpl.cpp -o $(OBJ)/SystemImpl.o  $(GTKCFLAGS)

$(OBJ)/CopyErrDlg.o: $(GUIPATH)/CopyErrDlg.cpp $(GUIPATH)/CopyErrDlg.h
	$(CC) $(FLAGS) -c $(GUIPATH)/CopyErrDlg.cpp -o $(OBJ)/CopyErrDlg.o $(GTKCFLAGS)

$(OBJ)/SiteManagerDlg.o: $(GUIPATH)/SiteManagerDlg.cpp $(GUIPATH)/SiteManagerDlg.h
	$(CC) $(FLAGS) -c $(GUIPATH)/SiteManagerDlg.cpp -o $(OBJ)/SiteManagerDlg.o $(GTKCFLAGS)

$(OBJ)/ftpCore.o: $(COREPATH)/_ftp/ftpCore.cpp $(COREPATH)/_ftp/ftpCore.h
	$(CC) $(FLAGS) -c $(COREPATH)/_ftp/ftpCore.cpp -o $(OBJ)/ftpCore.o $(GTKCFLAGS) 

$(OBJ)/FtpListParser.o: $(COREPATH)/_ftp/FtpListParser.cpp $(COREPATH)/_ftp/FtpListParser.h
	$(CC) $(FLAGS) -c $(COREPATH)/_ftp/FtpListParser.cpp -o $(OBJ)/FtpListParser.o $(GTKCFLAGS) 

$(OBJ)/xSocket.o: $(COREPATH)/_ftp/xSocket.cpp $(COREPATH)/_ftp/xSocket.h
	$(CC) $(FLAGS) -c $(COREPATH)/_ftp/xSocket.cpp -o $(OBJ)/xSocket.o $(GTKCFLAGS)

$(OBJ)/xSSLSocket.o: $(COREPATH)/_ftp/xSSLSocket.cpp $(COREPATH)/_ftp/xSSLSocket.h
	$(CC) $(FLAGS) -c $(COREPATH)/_ftp/xSSLSocket.cpp -o $(OBJ)/xSSLSocket.o $(GTKCFLAGS)

$(OBJ)/xSSLContext.o: $(COREPATH)/_ftp/xSSLContext.cpp $(COREPATH)/_ftp/xSSLContext.h
	$(CC) $(FLAGS) -c $(COREPATH)/_ftp/xSSLContext.cpp -o $(OBJ)/xSSLContext.o $(GTKCFLAGS)

$(OBJ)/ProxyLayer.o: $(COREPATH)/ProxyLayer.cpp $(COREPATH)/ProxyLayer.h
	$(CC) $(FLAGS) -c $(COREPATH)/ProxyLayer.cpp -o $(OBJ)/ProxyLayer.o $(GTKCFLAGS)

$(OBJ)/ProxyFtp.o: $(COREPATH)/ProxyFtp.cpp $(COREPATH)/ProxyFtp.h
	$(CC) $(FLAGS) -c $(COREPATH)/ProxyFtp.cpp -o $(OBJ)/ProxyFtp.o $(GTKCFLAGS)

$(OBJ)/ProxyHttp.o: $(COREPATH)/ProxyHttp.cpp $(COREPATH)/ProxyHttp.h
	$(CC) $(FLAGS) -c $(COREPATH)/ProxyHttp.cpp -o $(OBJ)/ProxyHttp.o $(GTKCFLAGS)

$(OBJ)/ProxySocks.o: $(COREPATH)/ProxySocks.cpp $(COREPATH)/ProxySocks.h
	$(CC) $(FLAGS) -c $(COREPATH)/ProxySocks.cpp -o $(OBJ)/ProxySocks.o $(GTKCFLAGS)

$(OBJ)/Base64.o: $(COREPATH)/_util/Base64.cpp $(COREPATH)/_util/Base64.h
	$(CC) $(FLAGS) -c $(COREPATH)/_util/Base64.cpp -o $(OBJ)/Base64.o $(GTKCFLAGS)

$(OBJ)/MIMECode.o: $(COREPATH)/_util/MIMECode.cpp $(COREPATH)/_util/MIMECode.h
	$(CC) $(FLAGS) -c $(COREPATH)/_util/MIMECode.cpp -o $(OBJ)/MIMECode.o $(GTKCFLAGS)

$(OBJ)/VfsFtp.o: $(COREPATH)/VfsFtp.cpp $(COREPATH)/VfsFtp.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsFtp.cpp -o $(OBJ)/VfsFtp.o $(GTKCFLAGS)

$(OBJ)/ConnectionInfoList.o: $(COREPATH)/ConnectionInfoList.cpp $(COREPATH)/ConnectionInfoList.h
	$(CC) $(FLAGS) -c $(COREPATH)/ConnectionInfoList.cpp -o $(OBJ)/ConnectionInfoList.o $(GTKCFLAGS)

$(OBJ)/VfsSftp.o: $(COREPATH)/VfsSftp.cpp $(COREPATH)/VfsSftp.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsSftp.cpp -o $(OBJ)/VfsSftp.o $(GTKCFLAGS)

$(OBJ)/OpConnect.o: $(COREPATH)/OpConnect.cpp $(COREPATH)/OpConnect.h
	$(CC) $(FLAGS) -c $(COREPATH)/OpConnect.cpp -o $(OBJ)/OpConnect.o $(GTKCFLAGS)

$(OBJ)/File64Enc.o: $(COREPATH)/File64Enc.cpp $(COREPATH)/File64Enc.h
	$(CC) $(FLAGS) -c $(COREPATH)/File64Enc.cpp -o $(OBJ)/File64Enc.o

$(OBJ)/DeleteDirDlg.o: $(GUIPATH)/DeleteDirDlg.cpp $(GUIPATH)/DeleteDirDlg.h
	$(CC) $(FLAGS) -c $(GUIPATH)/DeleteDirDlg.cpp -o $(OBJ)/DeleteDirDlg.o $(GTKCFLAGS)

$(OBJ)/OpErrorDlg.o: $(GUIPATH)/OpErrorDlg.cpp $(GUIPATH)/OpErrorDlg.h
	$(CC) $(FLAGS) -c $(GUIPATH)/OpErrorDlg.cpp -o $(OBJ)/OpErrorDlg.o $(GTKCFLAGS)

$(OBJ)/VfsNet.o: $(COREPATH)/VfsNet.cpp $(COREPATH)/VfsNet.h
	$(CC) $(FLAGS) -c $(COREPATH)/VfsNet.cpp -o $(OBJ)/VfsNet.o $(GTKCFLAGS)

$(OBJ)/GuiBlockDlg.o: $(GUIPATH)/GuiBlockDlg.cpp $(GUIPATH)/GuiBlockDlg.h
	$(CC) $(FLAGS) -c $(GUIPATH)/GuiBlockDlg.cpp -o $(OBJ)/GuiBlockDlg.o $(GTKCFLAGS)

$(OBJ)/OpStateWithBlocking.o: $(GUIPATH)/OpStateWithBlocking.cpp $(GUIPATH)/OpStateWithBlocking.h
	$(CC) $(FLAGS) -c $(GUIPATH)/OpStateWithBlocking.cpp -o $(OBJ)/OpStateWithBlocking.o $(GTKCFLAGS)

$(OBJ)/ExecutionThread.o: $(GUIPATH)/ExecutionThread.cpp $(GUIPATH)/ExecutionThread.h
	$(CC) $(FLAGS) -c $(GUIPATH)/ExecutionThread.cpp -o $(OBJ)/ExecutionThread.o $(GTKCFLAGS)

#
# help printout
#
help:	
	@echo "Usage:"
	@echo "make             - builds application binaries"
	@echo "make clean       - delete built application and object files"
	@echo "make install     - install binaries into the official directories"
	@echo "make uninstall   - uninstall binaries from the official directories"
	@echo "make rpm         - create source and binary rpms from this source"
	@echo "make help        - prints this help"
	@echo "make poinstall   - create and install translation catalogs (.mo)"
	@echo "make pouninstall - uninstall translation catalogs (.mo)"
	@echo "make convertsrc  - convert sources to DOS text format"
	@echo "To create rpm as non-root user use this form (override path to your rpm build directory):"
	@echo "  make rpm topdir=~/rpm"

#
# install/uninstall application translations
# (dynamicaly listing available .po files inside ./po directory)
#
poinstall:
	@for i in `ls po/*.po`; do \
		loc=`basename $$i | cut -f1 -d.`; \
		rm -f po/messages.mo; \
		msgfmt $$i -o po/messages.mo; \
		echo "Installing: $(DESTDIR)$(prefix)/share/locale/$$loc/LC_MESSAGES/atol.mo";\
		install -D -m 644 po/messages.mo "$(DESTDIR)$(prefix)/share/locale/$$loc/LC_MESSAGES/atol.mo"; \
	done; \
	rm -f po/messages.mo; 
	@echo Done installing translations

pouninstall:
	@rm -vf $(DESTDIR)$(prefix)/share/locale/*/LC_MESSAGES/atol.mo
	@echo Done uninstalling translations
	
#
# install application
#
install: poinstall installplugins installatol

#
# uninstall application
#
uninstall: pouninstall uninstallplugins
	@rm -f  "$(DESTDIR)$(prefix)/bin/atol"
	@echo Done uninstalling application

#
# installs atol without plugins (for fast tests)
#
installatol: atol
	@install -D -m 755 $(BIN)/atol "$(DESTDIR)$(prefix)/bin/atol"
	@install -D -m 644 docs/atol.desktop "$(DESTDIR)$(prefix)/share/applications/atol.desktop"
	@install -D -m 644 res/atol.xpm "$(DESTDIR)$(prefix)/share/icons/atol.xpm"
	@echo Done installing application

#
# build plugins
#
plugins:
	@cd ./plugins/src/ArjLib/; make;
	@cd ./plugins/src/Bz2Lib/bzip2/; make;
	@cd ./plugins/src/Bz2Lib/; make;
	@cd ./plugins/src/GzLib/zlib/; make;
	@cd ./plugins/src/GzLib/; make;
	@cd ./plugins/src/LstLib/; make;
	@cd ./plugins/src/TarLib/; make;
	@cd ./plugins/src/ZipLib/ZipArchive/zlib/; make;
	@cd ./plugins/src/ZipLib/ZipArchive/; make;
	@cd ./plugins/src/ZipLib/; make;
	@cd ./plugins/src/ZLib/; make;

cleanplugins:
	@cd ./plugins/src/ArjLib/; make clean;
	@cd ./plugins/src/Bz2Lib/bzip2/; make clean;
	@cd ./plugins/src/Bz2Lib/; make clean;
	@cd ./plugins/src/GzLib/zlib/; make clean;
	@cd ./plugins/src/GzLib/; make clean;
	@cd ./plugins/src/LstLib/; make clean;
	@cd ./plugins/src/TarLib/; make clean;
	@cd ./plugins/src/ZipLib/ZipArchive/zlib/; make clean;
	@cd ./plugins/src/ZipLib/ZipArchive/; make clean;
	@cd ./plugins/src/ZipLib/; make clean;
	@cd ./plugins/src/ZLib/; make clean;

installplugins: plugins
	@install -D -m 755 ./plugins/src/ArjLib/ArjLib.atp "$(plugin_dir)/ArjLib.atp"
	@install -D -m 755 ./plugins/src/Bz2Lib/Bz2Lib.atp "$(plugin_dir)/Bz2Lib.atp"
	@install -D -m 755 ./plugins/src/GzLib/GzLib.atp "$(plugin_dir)/GzLib.atp"
	@install -D -m 755 ./plugins/src/LstLib/LstLib.atp "$(plugin_dir)/LstLib.atp"
	@install -D -m 755 ./plugins/src/TarLib/TarLib.atp "$(plugin_dir)/TarLib.atp"
	@install -D -m 755 ./plugins/src/ZipLib/ZipLib.atp "$(plugin_dir)/ZipLib.atp"
	@install -D -m 755 ./plugins/src/ZLib/ZLib.atp "$(plugin_dir)/ZLib.atp"

uninstallplugins:
	@rm -f "$(plugin_dir)/ArjLib.atp"
	@rm -f "$(plugin_dir)/Bz2Lib.atp"
	@rm -f "$(plugin_dir)/GzLib.atp"
	@rm -f "$(plugin_dir)/LstLib.atp"
	@rm -f "$(plugin_dir)/TarLib.atp"
	@rm -f "$(plugin_dir)/ZipLib.atp"
	@rm -f "$(plugin_dir)/ZLib.atp"

#
# build RPM and SRPM files (must be superuser)
#
rpm: pack
	cp -f docs/atol.spec $(topdir)/SPECS/atol-$(VERSION).spec
	mv -f ../$(archive).tar.gz $(topdir)/SOURCES
	cp -f ./res/atol.xpm $(topdir)/SOURCES
	cp -f ./docs/atol.desktop $(topdir)/SOURCES
	rpmbuild --define "_topdir $(topdir)" \
	         -ba "$(topdir)/SPECS/atol-$(VERSION).spec"
	@echo Done, look for your RPMs and SRPM under $(topdir)

#
# pack sources in parent directory (need not be superuser)
#
pack: clean updatesrc
	@cd ..; rm -f $(archive).tar.gz; tar cfz $(archive).tar.gz atol-$(VERSION)
	@echo "Done, see archive ../$(archive).tar.gz"

packdos: clean updatesrc convertsrcdos
	@cd ..; rm -f $(archive).zip; zip -rq $(archive).zip atol-$(VERSION)
	@echo "Done, see archive ../$(archive).zip"


#
# - update version strings from makefile value
#   1. automaticaly update version information inside .spec file
#   2. automaticaly update "Source:" label inside .spec file
#   3. automaticaly update version information inside "config.h" header file
#   4. write $(prefix) value into the config.h
#
updatesrc:
	$(Q)sed -i 's/Version:.*/Version:        $(VERSION)/' ./docs/atol.spec
	$(Q)sed -i 's/Source:.*/Source:         $(archive).tar.gz/' ./docs/atol.spec
	$(Q)sed -i 's/#define APP_VER_STR.*/#define APP_VER_STR "$(VERSION)"\r/' ./src/config.h
	$(Q)sed -i 's:\#define INSTALL_PREFIX .*:\#define INSTALL_PREFIX "$(DESTDIR)$(prefix)"\r:' ./src/config.h
	$(Q)sed -i 's:\#define PLUGIN_DIR .*:\#define PLUGIN_DIR "$(plugin_dir)"\r:' ./src/config.h
	$(Q)if [ "$(HAVE_GNOME_VFS)" == "" ]; then \
		sed -i 's:[/]*#define HAVE_GNOME_VFS*://#define HAVE_GNOME_VFS:' ./src/config.h; \
	else \
		sed -i 's:[/]*#define HAVE_GNOME_VFS*:#define HAVE_GNOME_VFS:' ./src/config.h; \
	fi;
	@echo "Source code settings updated"

#
# convert sources to Unix text format (this is multiplatform project)
#
convertsrc:
	@dos2unix -q -k $(GUIPATH)/*.cpp $(GUIPATH)/*.h $(COREPATH)/*.cpp $(COREPATH)/*.h ./Makefile

#
# convert sources to DOS text format (this is multiplatform project)
#
convertsrcdos:
	@unix2dos -q -k $(GUIPATH)/*.cpp $(GUIPATH)/*.h $(COREPATH)/*.cpp $(COREPATH)/*.h


#
# for debugging purposes
#
check: atol
	@valgrind --leak-check=full --error-limit=no ./bin/atol

#
# clean up the mess
#
clean: cleanplugins
	@echo cleaning up
	$(Q)rm -f $(OBJ)/*.o $(OBJ)/*.d $(BIN)/core.* $(BIN)/atol ./gmon.out

.PHONY: all help install uninstall plugins cleanplugins installplugins uninstallplugins rpm pack updatesrc clean check installatol
	
