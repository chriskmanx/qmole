#
# $Header: /cvsroot/lesstif/lesstif/lib/Dt/Makefile.am,v 1.23 2005/03/26 06:52:25 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in

libDtPrint_la_LDFLAGS=	-version-info 1:0 -no-undefined
libdir = $(exec_prefix)/lib

if BuildLibDtPrint

lib_LTLIBRARIES=	libDtPrint.la

libDtPrint_la_LIBADD = ../../lib/Xm-2.1/libXm.la @X_LIBS@ -lXt $(X_PRE_LIBS) \
	$(XPLIB) -lX11 $(X_EXTRA_LIBS)

# libDtPrint_la_LIBADD = @X_LIBS@ -lXt $(X_PRE_LIBS) -lX11 $(X_EXTRA_LIBS)


dtincludedir=	$(includedir)/Dt
dtinclude_DATA=	Print.h

AM_CPPFLAGS=	-I$(top_srcdir)/lib \
		-I$(top_builddir)/lib \
		-I$(top_srcdir)/include/Motif-2.1 \
		-I$(top_builddir)/include/Motif-2.1 \
		-I$(top_srcdir)/include \
		-I$(top_builddir)/include \
		$(X_CFLAGS)

libDtPrint_la_SOURCES=	Print.h DtPrintSetupBoxP.h \
			DtPrintSetupBox.c DtPrintSetupData.c

endif

EXTRA_DIST = Print.h DtPrintSetupBoxP.h \
             DtPrintSetupBox.c DtPrintSetupData.c
