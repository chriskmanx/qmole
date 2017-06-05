#
# $Header: /cvsroot/lesstif/lesstif/test/common/Makefile.am,v 1.15 2005/03/26 06:52:34 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in

noinst_LIBRARIES = libLtTest.a

libLtTest_a_SOURCES = DumpLayout.c Test.c XdbPrintResources.c XdbPrintTree.c

noinst_HEADERS = Test.h

# AM_CPPFLAGS = $(X_CFLAGS)
AM_CPPFLAGS= -I$(top_srcdir)/../include/Motif$(include_version_suffix)  \
	  -I$(top_builddir)/../include/Motif$(include_version_suffix)  \
	  -I$(top_srcdir)/../include/Motif-1.2  \
	  -I$(top_builddir)/../include/Motif-1.2  \
	  $(X_CFLAGS) 

#
motif-tests: libLtTest.a
