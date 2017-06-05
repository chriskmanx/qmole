## Process this file with automake to produce Makefile.in.

INCLUDES = $(LIBGNOME_CFLAGS)

libexec_PROGRAMS = gnome2-info2html

gnome2_info2html_SOURCES = main.c parse.c utils.c html.c data.h html.h parse.h utils.h version.h

gnome2_info2html_LDADD = $(LIBGNOME_LIBS) $(Z_LIBS) $(BZ_LIBS)
