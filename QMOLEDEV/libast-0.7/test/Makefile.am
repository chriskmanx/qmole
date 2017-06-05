# $Id: Makefile.am,v 1.8 2005/03/07 22:29:08 mej Exp $

EXTRA_PROGRAMS = libast-test libast-perf

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/include/$(PACKAGE)

libast_test_SOURCES = test.c test.h
libast_test_DEPENDENCIES = $(top_builddir)/src/libast.la
libast_test_LDADD = $(top_builddir)/src/libast.la

libast_perf_SOURCES = perf.c perf.h
libast_perf_DEPENDENCIES = $(top_builddir)/src/libast.la
libast_perf_LDADD = $(top_builddir)/src/libast.la

test: libast-test
	$(top_builddir)/test/libast-test

perf: libast-perf
# Uncomment the following to run under gdb
#	vi $(top_builddir)/test/libast-perf
	$(top_builddir)/test/libast-perf $(PERFOPTS)

.PHONY: test
MAINTAINERCLEANFILES = Makefile.in
