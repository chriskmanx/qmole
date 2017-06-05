## Process this file with automake to produce Makefile.in

if PLATFORM_WIN32
no_undefined = -no-undefined
endif

INCLUDES = \
	-I$(top_builddir)				\
	-I$(top_srcdir)					\
	-DG_LOG_DOMAIN=\"GnomeCanvas\"			\
	$(CANVAS_GLADE_CFLAGS)

glademoduledir = $(libdir)/libglade/2.0

if USE_GLADE
glademodule_LTLIBRARIES = libcanvas.la
endif

libcanvas_la_LIBADD = \
	$(top_builddir)/libgnomecanvas/libgnomecanvas-2.la	\
	$(CANVAS_GLADE_LIBS)

libcanvas_la_LDFLAGS = \
	-export-dynamic -module -avoid-version $(no_undefined)

libcanvas_la_SOURCES = \
	glade-canvas.c
