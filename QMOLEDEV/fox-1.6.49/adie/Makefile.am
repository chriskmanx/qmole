## Process this file with automake to produce Makefile.in

CXXFLAGS = @CXXFLAGS@ @X_CFLAGS@
INCLUDES = -I$(top_srcdir)/include -I$(top_builddir)/include
RESWRAP  = $(top_builddir)/utils/reswrap
SUFFIXES = .gif .bmp
LDADD = $(top_builddir)/src/libFOX-1.6.la -lm


bin_PROGRAMS = adie

bin_SCRIPTS = Adie.stx

man_MANS = adie.1

ICONS  = \
adie_gif.gif \
big_gif.gif \
bookdel_gif.gif \
booknext_gif.gif \
bookprev_gif.gif \
bookset_gif.gif \
close_gif.gif \
colors_gif.gif \
copy_gif.gif \
cut_gif.gif \
delete_gif.gif \
delimit_gif.gif \
fonts_gif.gif \
help_gif.gif \
indent_gif.gif \
info_gif.gif \
lang_gif.gif \
new_gif.gif \
open_gif.gif \
palette_gif.gif \
pattern_gif.gif \
paste_gif.gif \
print_gif.gif \
quit_gif.gif \
redo_gif.gif \
reload_gif.gif \
saveall_gif.gif \
saveas_gif.gif \
save_gif.gif \
search_gif.gif \
searchnext_gif.gif \
searchprev_gif.gif \
shiftleft_gif.gif \
shiftright_gif.gif \
small_gif.gif \
styles_gif.gif \
syntax_gif.gif \
undo_gif.gif


adie_SOURCES = \
Adie.h \
Adie.cpp \
Commands.cpp \
Commands.h \
help.h \
help.cpp \
HelpWindow.h \
HelpWindow.cpp \
Hilite.h \
Hilite.cpp \
icons.h \
icons.cpp \
main.cpp \
parsesyntax.cpp \
Preferences.h \
Preferences.cpp \
TextWindow.h \
TextWindow.cpp \
LICENSE \
$(ICONS)


Adie.cpp: icons.h icons.cpp


icons.h: $(ICONS)
	$(RESWRAP) -i -o icons.h $^


icons.cpp: $(ICONS)
	$(RESWRAP) -e -o icons.cpp $^


CLEANFILES = icons.h icons.cpp


EXTRA_DIST = \
$(man_MANS) \
Adie.stx \
Makefile.bc \
Makefile.wc \
Makefile.dmc

