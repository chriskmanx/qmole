#
# $Header: /cvsroot/lesstif/lesstif/lib/Mrm-2.1/Makefile.am,v 1.25 2005/11/01 09:02:57 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

libMrm_la_LDFLAGS= -version-info 2:1 -no-undefined
# libdir = $(exec_prefix)/lib

#
# Sources in this directory
#
SRCS_2_1 = \
	Mrm.c lookup.c misc.c \
        Mrm.h lookup.h misc.h uil.h

AM_CPPFLAGS= \
          -I$(top_srcdir)/include/Motif-2.1 \
          -I$(top_builddir)/include/Motif-2.1 \
          -I$(top_srcdir)/include \
          -I$(top_builddir)/include \
	  -I$(top_srcdir)/lib/Mrm-2.1 \
          ${X_CFLAGS} \
          -DX_LIB_PATH=\"@x_libraries@\" \
          -DLESSTIFHOME=\"@prefix@\"

lib_LTLIBRARIES= libMrm.la

libMrm_la_LIBADD = ../Xm-2.1/libXm.la @X_LIBS@ -lXt $(X_PRE_LIBS) -lX11 $(X_EXTRA_LIBS)

libMrm_la_SOURCES= $(SRCS_2_1)
