## Process this file with automake to produce Makefile.in

CXXFLAGS = @CXXFLAGS@ @X_CFLAGS@
INCLUDES = -I$(top_srcdir)/include -I$(top_builddir)/include
RESWRAP  = $(top_builddir)/utils/reswrap
SUFFIXES = .gif .bmp
LDADD = $(top_builddir)/src/libFOX-1.6.la -lm


bin_PROGRAMS = shutterbug

man_MANS = shutterbug.1

ICONS  = \
line_0.gif \
line_1.gif \
line_2.gif \
line_3.gif \
line_4.gif \
line_5.gif \
line_6.gif \
line_7.gif \
line_8.gif \
shutterbug.gif \
tinyshutterbug.gif


shutterbug_SOURCES = \
icons.h \
icons.cpp \
ShutterBug.h \
ShutterBug.cpp \
Snapper.h \
Snapper.cpp \
LICENSE \
$(ICONS)


ShutterBug.cpp: icons.h icons.cpp


icons.h: $(ICONS)
	$(RESWRAP) -i -o icons.h $^


icons.cpp: $(ICONS)
	$(RESWRAP) -e -o icons.cpp $^


CLEANFILES = icons.h icons.cpp

EXTRA_DIST = \
$(man_MANS) \
Makefile.bc \
Makefile.wc \
Makefile.dmc
