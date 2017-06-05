## Process this file with automake to produce Makefile.in

SUBDIRS = ttsubset

INCLUDES = \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
	-DPACKAGE_LOCALE_DIR=\""$(prefix)/$(DATADIRNAME)/locale"\" \
	@PACKAGE_CFLAGS@

bin_PROGRAMS = xournal

xournal_SOURCES = \
	main.c xournal.h \
	xo-misc.c xo-misc.h \
	xo-file.c xo-file.h \
	xo-paint.c xo-paint.h \
	xo-print.c xo-print.h \
	xo-support.c xo-support.h \
	xo-interface.c xo-interface.h \
	xo-callbacks.c xo-callbacks.h \
	xo-shapes.c xo-shapes.h

xournal_LDADD = ttsubset/libttsubset.a @PACKAGE_LIBS@ $(INTLLIBS)

