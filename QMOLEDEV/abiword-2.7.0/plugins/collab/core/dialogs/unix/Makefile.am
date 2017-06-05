
noinst_LTLIBRARIES = libunix.la

AM_CPPFLAGS = \
	$(COLLAB_CFLAGS) \
	-I$(srcdir)/.. \
	-I$(srcdir)/../.. \
	-I$(srcdir)/../../.. \
	-I$(srcdir)/../../packet \
	-I$(srcdir)/../../session

libunix_la_SOURCES = \
	ap_UnixDialog_CollaborationAccounts.cpp \
	ap_UnixDialog_CollaborationAccounts.h \
	ap_UnixDialog_CollaborationAddAccount.cpp \
	ap_UnixDialog_CollaborationAddAccount.h \
	ap_UnixDialog_CollaborationAddBuddy.cpp \
	ap_UnixDialog_CollaborationAddBuddy.h \
	ap_UnixDialog_CollaborationJoin.cpp \
	ap_UnixDialog_CollaborationJoin.h

uidir=$(ABIWORD_UIDIR)
ui_DATA = \
	ap_UnixDialog_CollaborationAccounts.xml \
	ap_UnixDialog_CollaborationAddAccount.xml \
	ap_UnixDialog_CollaborationAddBuddy.xml \
	ap_UnixDialog_CollaborationJoin.xml

EXTRA_DIST = \
        $(ui_DATA)

