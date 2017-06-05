
noinst_LTLIBRARIES = libunix.la

AM_CPPFLAGS = \
	$(COLLAB_CFLAGS) \
	-I$(srcdir)/.. \
	-I$(srcdir)/../.. \
	-I$(srcdir)/../../.. \
	-I$(srcdir)/../../../core

libunix_la_SOURCES = \
	ap_UnixDialog_GenericInput.cpp \
	ap_UnixDialog_GenericInput.h \
	ap_UnixDialog_GenericProgress.cpp \
	ap_UnixDialog_GenericProgress.h \
	ServiceUnixAccountHandler.cpp \
	ServiceUnixAccountHandler.h

uidir=$(ABIWORD_UIDIR)
ui_DATA = \
	ap_UnixDialog_GenericInput.xml \
	ap_UnixDialog_GenericProgress.xml

EXTRA_DIST = \
	$(xml_DATA)
