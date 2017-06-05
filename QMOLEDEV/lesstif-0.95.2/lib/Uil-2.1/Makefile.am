#
# $Header: /cvsroot/lesstif/lesstif/lib/Uil-2.1/Makefile.am,v 1.17 2005/11/01 09:02:57 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in

libUil_la_LDFLAGS= -version-info 2:1 -no-undefined
# libdir = $(exec_prefix)/lib

#
# Sources in this directory
#
SRCS_2_1 = \
	Uil.c  UilData.c  uillex.c  uilparse.c  uilsym.c

AM_CPPFLAGS= \
          -I$(top_srcdir)/include/Motif-2.1 \
          -I$(top_builddir)/include/Motif-2.1 \
          -I$(top_srcdir)/include \
          -I$(top_builddir)/include \
          -I$(top_srcdir)/lib/Uil-2.1 \
          $(X_CFLAGS)

lib_LTLIBRARIES= libUil.la

libUil_la_LIBADD = ../Xm-2.1/libXm.la $(X_LIBS) -lXt

libUil_la_SOURCES=  ${SRCS_2_1}
