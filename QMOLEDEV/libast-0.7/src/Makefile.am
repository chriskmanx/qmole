# $Id: Makefile.am,v 1.6 2001/09/22 16:25:29 mej Exp $

lib_LTLIBRARIES = libast.la

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/include/$(PACKAGE)

libast_la_SOURCES =							\
    array.c builtin_hashes.c conf.c debug.c dlinked_list.c file.c	\
    linked_list.c mbuff.c mem.c msgs.c obj.c objpair.c options.c	\
    regexp.c socket.c str.c strings.c snprintf.c tok.c url.c

libast_la_LDFLAGS = -version-info 2:1:0
MAINTAINERCLEANFILES = Makefile.in
