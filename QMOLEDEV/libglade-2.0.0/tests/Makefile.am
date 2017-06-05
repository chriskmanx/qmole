if HAVE_PYTHON
python_tests = test-convert
endif

check_PROGRAMS = test-libglade-gtk test-value-parse

TESTS = $(python_tests) test-value-parse
TESTS_ENVIRONMENT = PYTHON=$(PYTHON)

INCLUDES = -I$(top_srcdir) $(LIBGLADE_CFLAGS)

LDADD = $(top_builddir)/glade/libglade-2.0.la

LDFLAGS = -export-dynamic

test_libglade_gtk_SOURCE = test-libglade-gtk.c
test_value_parse_SOURCE = test-value-parse.c

EXTRA_DIST = test-convert test-libglade-gtk.glade

CLEANFILES = test-libglade-gtk.glade2 test-libglade-gtk-noupgrade.glade2 .memdump
