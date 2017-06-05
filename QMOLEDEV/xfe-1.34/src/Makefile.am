
bin_PROGRAMS = xfe xfp xfv xfw xfi

xfe_SOURCES = ../xvt/command.c \
              ../xvt/screen.c \
              ../xvt/ttyinit.c \
              ../xvt/xsetup.c \
              ../xvt/xvt.c \
              icons.cpp \
			  xfeutils.cpp \
			  startupnotification.cpp \
			  StringList.cpp \
              File.cpp \
			  FileDict.cpp \
			  IconList.cpp \
			  FileList.cpp \
			  FileDialog.cpp \
			  DirList.cpp \
			  DialogBox.cpp \
			  MessageBox.cpp \
			  Bookmarks.cpp \
			  HistInputDialog.cpp \
			  InputDialog.cpp \
			  OverwriteBox.cpp \
			  ExecuteBox.cpp \
			  TextWindow.cpp \
			  CommandWindow.cpp \
              Properties.cpp \
			  Preferences.cpp \
			  FilePanel.cpp \
			  DirPanel.cpp \
			  DirHistBox.cpp \
			  PathLinker.cpp \
			  BrowseInputDialog.cpp \
			  ArchInputDialog.cpp \
			  FontDialog.cpp \
			  TextLabel.cpp \
			  Keybindings.cpp \
			  KeybindingsDialog.cpp \
			  XFileExplorer.cpp \
              main.cpp

if STARTUPNOTIFY
xfe_SOURCES += ../libsn/sn-common.c  \
              ../libsn/sn-launchee.c \
              ../libsn/sn-launcher.c \
              ../libsn/sn-list.c \
              ../libsn/sn-monitor.c \
              ../libsn/sn-util.c \
              ../libsn/sn-xmessages.c \
              ../libsn/sn-xutils.c
endif

xfe_LDADD = @LIBINTL@ -lutil


xfp_SOURCES = ../xvt/command.c \
              ../xvt/screen.c \
              ../xvt/ttyinit.c \
              ../xvt/xsetup.c \
              ../xvt/xvt.c \
              icons.cpp \
			  xfeutils.cpp \
			  startupnotification.cpp \
			  StringList.cpp \
              CommandWindow.cpp \
              InputDialog.cpp \
			  DialogBox.cpp \
              OverwriteBox.cpp \
              FileDict.cpp \
			  IconList.cpp \
			  File.cpp \
              FileList.cpp \
			  DirList.cpp \
              FileDialog.cpp \
			  PathLinker.cpp \
			  TextLabel.cpp \
              MessageBox.cpp \
 			  DirHistBox.cpp \
              XFilePackage.cpp

if STARTUPNOTIFY
xfp_SOURCES += ../libsn/sn-common.c  \
              ../libsn/sn-launchee.c \
              ../libsn/sn-launcher.c \
              ../libsn/sn-list.c \
              ../libsn/sn-monitor.c \
              ../libsn/sn-util.c \
              ../libsn/sn-xmessages.c \
              ../libsn/sn-xutils.c
endif

xfp_LDADD = @LIBINTL@ -lutil


xfv_SOURCES = ../xvt/command.c \
              ../xvt/screen.c \
              ../xvt/ttyinit.c \
              ../xvt/xsetup.c \
              ../xvt/xvt.c \
              icons.cpp \
			  xfeutils.cpp \
			  startupnotification.cpp \
			  StringList.cpp \
              CommandWindow.cpp \
              InputDialog.cpp \
			  DialogBox.cpp \
              OverwriteBox.cpp \
              FileDict.cpp \
 			  IconList.cpp \
			  File.cpp \
			  FileList.cpp \
			  DirList.cpp \
              FileDialog.cpp \
			  PathLinker.cpp \
			  TextLabel.cpp \
              MessageBox.cpp \
 			  DirHistBox.cpp \
			  ViewWindow.cpp \
			  FontDialog.cpp \
              XFileView.cpp

if STARTUPNOTIFY
xfv_SOURCES += ../libsn/sn-common.c  \
              ../libsn/sn-launchee.c \
              ../libsn/sn-launcher.c \
              ../libsn/sn-list.c \
              ../libsn/sn-monitor.c \
              ../libsn/sn-util.c \
              ../libsn/sn-xmessages.c \
              ../libsn/sn-xutils.c
endif

xfv_LDADD = @LIBINTL@ -lutil
	  

xfw_SOURCES = ../xvt/command.c \
              ../xvt/screen.c \
              ../xvt/ttyinit.c \
              ../xvt/xsetup.c \
              ../xvt/xvt.c \
              icons.cpp \
			  xfeutils.cpp \
			  startupnotification.cpp \
			  StringList.cpp \
			  CommandWindow.cpp \
              OverwriteBox.cpp \
			  MessageBox.cpp \
			  IconList.cpp \
			  File.cpp \
              FileList.cpp \
			  DirList.cpp \
              InputDialog.cpp \
			  DialogBox.cpp \
			  FileDict.cpp \
              FileDialog.cpp \
			  PathLinker.cpp \
			  TextLabel.cpp \
              WriteWindow.cpp \
 			  DirHistBox.cpp \
			  FontDialog.cpp \
              XFileWrite.cpp

if STARTUPNOTIFY
xfw_SOURCES += ../libsn/sn-common.c  \
              ../libsn/sn-launchee.c \
              ../libsn/sn-launcher.c \
              ../libsn/sn-list.c \
              ../libsn/sn-monitor.c \
              ../libsn/sn-util.c \
              ../libsn/sn-xmessages.c \
              ../libsn/sn-xutils.c
endif

xfw_LDADD = @LIBINTL@ -lutil

				
xfi_SOURCES = ../xvt/command.c \
              ../xvt/screen.c \
              ../xvt/ttyinit.c \
              ../xvt/xsetup.c \
              ../xvt/xvt.c \
              icons.cpp \
			  xfeutils.cpp \
			  startupnotification.cpp \
 			  StringList.cpp \
              CommandWindow.cpp \
              InputDialog.cpp \
			  DialogBox.cpp \
              OverwriteBox.cpp \
              FileDict.cpp \
			  IconList.cpp \
			  File.cpp \
              FileList.cpp \
			  DirList.cpp \
              FileDialog.cpp \
              MessageBox.cpp \
 			  DirHistBox.cpp \
 			  TextLabel.cpp \
 			  PathLinker.cpp \
              XFileImage.cpp

if STARTUPNOTIFY
xfi_SOURCES += ../libsn/sn-common.c  \
              ../libsn/sn-launchee.c \
              ../libsn/sn-launcher.c \
              ../libsn/sn-list.c \
              ../libsn/sn-monitor.c \
              ../libsn/sn-util.c \
              ../libsn/sn-xmessages.c \
              ../libsn/sn-xutils.c
endif

xfi_LDADD = @LIBINTL@ -lutil


localedir = $(datadir)/locale

AM_CPPFLAGS = -I. -I$(top_srcdir) -I$(top_srcdir)/intl
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

EXTRA_DIST =  ../libsn/sn-common.h	\
	          ../libsn/sn-internals.h \
	          ../libsn/sn-launchee.h \
			  ../libsn/sn-launcher.h \
	          ../libsn/sn-list.h \
	          ../libsn/sn-monitor.h \
	          ../libsn/sn-util.h \
	          ../libsn/sn-xmessages.h \
			  ../libsn/sn-xutils.h \
			  ../libsn/sn.h \
			  ../xvt/command.h \
			  ../xvt/screen.h \
			  ../xvt/token.h \
			  ../xvt/xsetup.h \
			  ../xvt/ttyinit.h \
			  ../xvt/xvt.h \
			  xfedefs.h \
              icons.h \
			  xfeutils.h \
              help.h \
			  startupnotification.h \
			  StringList.h \
			  FileDialog.h \
			  FileDict.h \
			  IconList.h \
			  FileList.h \
			  DirList.h \
			  DirPanel.h \
			  Properties.h \
              File.h \
			  DialogBox.h \
			  MessageBox.h \
			  HistInputDialog.h \
			  InputDialog.h \
			  Preferences.h \
			  TextWindow.h \
			  CommandWindow.h \
			  OverwriteBox.h \
			  ExecuteBox.h \
			  FilePanel.h \
			  Bookmarks.h \
			  XFileExplorer.h \
			  XFileImage.h \
			  XFilePackage.h \
			  ViewWindow.h \
			  XFileView.h \
			  WriteWindow.h \
			  XFileWrite.h \
			  DirHistBox.h \
			  PathLinker.h \
			  BrowseInputDialog.h \
			  ArchInputDialog.h \
			  FontDialog.h \
			  TextLabel.h \
			  Keybindings.h \
			  KeybindingsDialog.h \
			  foxhacks.cpp

