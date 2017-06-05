check_PROGRAMS = \
	test-libglade-canvas

TESTS = $(check_PROGRAMS)

TESTS_ENVIRONMENT = \
	LIBGLADE_MODULE_PATH="$(top_builddir)/glade/.libs:$(LIBGLADE_MODULE_PATH)"

INCLUDES = \
	-I$(top_srcdir)		\
	$(CANVAS_GLADE_CFLAGS)	\
	-DGLADEFILE=\""$(srcdir)/test-libglade-canvas.glade"\"

LDADD = $(CANVAS_GLADE_LIBS)

AM_LDFLAGS = -export-dynamic

test_libglade_canvas_SOURCE = test-libglade-canvas.c

EXTRA_DIST = test-libglade-canvas.glade


