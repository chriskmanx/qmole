## Process this file with automake to produce Makefile.in

CXXFLAGS = @CXXFLAGS@ @X_CFLAGS@
INCLUDES = -I$(top_srcdir)/include -I$(top_builddir)/include
RESWRAP  = $(top_builddir)/utils/reswrap
SUFFIXES = .gif .bmp
LDADD = $(top_builddir)/src/libFOX-1.6.la -lm


bin_PROGRAMS = calculator

man_MANS = calculator.1

ICONS  = \
colors.gif \
information.gif \
bigcalc.gif \
constmem.bmp \
question.gif \
tinycalc.gif


calculator_SOURCES = \
icons.h \
icons.cpp \
Calculator.h \
Calculator.cpp \
HelpWindow.h \
HelpWindow.cpp \
Preferences.h \
Preferences.cpp \
help.cpp \
main.cpp \
LICENSE \
$(ICONS)


Calculator.cpp: icons.h icons.cpp


icons.h: $(ICONS)
	$(RESWRAP) -i -o icons.h $^


icons.cpp: $(ICONS)
	$(RESWRAP) -e -o icons.cpp $^


CLEANFILES = icons.h icons.cpp


EXTRA_DIST = $(man_MANS) \
             Makefile.bc \
             Makefile.wc \
             Makefile.dmc

