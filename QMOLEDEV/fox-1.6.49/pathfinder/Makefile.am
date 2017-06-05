## Process this file with automake to produce Makefile.in

CXXFLAGS = @CXXFLAGS@ @X_CFLAGS@
INCLUDES = -I$(top_srcdir)/include -I$(top_builddir)/include
RESWRAP  = $(top_builddir)/utils/reswrap
SUFFIXES = .gif .bmp
LDADD	 = $(top_builddir)/src/libFOX-1.6.la -lm

bin_PROGRAMS	= PathFinder

man_MANS = PathFinder.1

ICONS = \
bigicons.bmp \
closepanel.gif \
clrbook.gif \
copy.bmp \
copyit.gif \
cut.bmp \
deleteit.bmp \
desktop.bmp \
details.bmp \
dirup.bmp \
enter.gif \
file_gif.gif \
foxbig.gif \
foxmini.gif \
goback.bmp \
goforw.bmp \
gotodir.bmp \
home.gif \
hosts.bmp \
iconpath.gif \
linkit.gif \
location.gif \
maphost.bmp \
mimetype.gif \
moveit.gif \
paste.bmp \
pattern_gif.gif \
properties.bmp \
quit_gif.gif \
renameit.gif \
rotateleft.gif \
rotateright.gif \
setbook.gif \
setdir.gif \
smallicons.bmp \
unmaphost.bmp \
work.gif

PathFinder_SOURCES =  \
icons.h \
icons.cpp \
CopyDialog.h \
CopyDialog.cpp \
PathFinder.cpp \
PathFinder.h \
Preferences.cpp \
Preferences.h \
PropertyDialog.cpp \
PropertyDialog.h \
LICENSE \
$(ICONS)

PathFinder.cpp: icons.h icons.cpp

icons.h: $(ICONS)
	$(RESWRAP) -i -o icons.h $^

icons.cpp: $(ICONS)
	$(RESWRAP) -e -o icons.cpp $^

CLEANFILES	= icons.h icons.cpp

EXTRA_DIST = $(man_MANS) \
             Makefile.bc \
             Makefile.wc \
             Makefile.dmc

