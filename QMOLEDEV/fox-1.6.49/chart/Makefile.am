## Process this file with automake to produce Makefile.in

CXXFLAGS = @CXXFLAGS@ @X_CFLAGS@
INCLUDES = -I$(top_srcdir)/include -I$(top_builddir)/include
RESWRAP  = $(top_builddir)/utils/reswrap
SUFFIXES = .gif .bmp
LDADD	 =  libCHART-1.6.la $(top_builddir)/src/libFOX-1.6.la -lm

lib_LTLIBRARIES = libCHART-1.6.la

libCHART_1_6_la_LIBADD  = $(X_LIBS) $(X_BASE_LIBS) $(X_EXTRA_LIBS)

libCHART_1_6_la_LDFLAGS = \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)    \
	-export-dynamic

chartincludedir=$(includedir)/fox-1.6/chart

chartinclude_HEADERS = \
FXChart.h

libCHART_1_6_la_SOURCES = \
FXChart.cpp


noinst_PROGRAMS = chart

ICONS = marble.bmp

chart_SOURCES = chart.cpp icons.h icons.cpp $(ICONS)

chart.cpp: icons.h icons.cpp

icons.h: $(ICONS)
	$(RESWRAP) -i -o icons.h $^

icons.cpp: $(ICONS)
	$(RESWRAP) -e -o icons.cpp $^

CLEANFILES = icons.h icons.cpp


EXTRA_DIST = Makefile.bc
