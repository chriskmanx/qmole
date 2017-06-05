INCLUDES = \
	-I$(top_srcdir)/gobject \
	$(PYTHON_INCLUDES) \
	$(GLIB_CFLAGS)

EXTRA_DIST = \
	$(tests) \
	common.py \
	runtests.py \
	testmodule.py \
	test-thread.h \
	test-unknown.h

noinst_LTLIBRARIES = testhelper.la
linked_LIBS = testhelper.la

testhelper_la_LDFLAGS = -module -avoid-version
testhelper_la_LIBADD = $(GLIB_LIBS)
testhelper_la_SOURCES = \
	testhelpermodule.c \
	test-thread.c \
	test-unknown.c 

tests =  \
	test_conversion.py \
	test_enum.py \
	test_gicon.py \
	test_gio.py \
	test_gobject.py \
	test_gtype.py \
	test_interface.py \
	test_mainloop.py \
	test_properties.py \
	test_signal.py \
	test_subprocess.py \
	test_subtype.py \
	test_unknown.py \
	test_source.py

# This is a hack to make sure a shared library is built
testhelper.la: $(testhelper_la_OBJECTS) $(testhelper_la_DEPENDENCIES)
	$(LINK) -rpath $(pkgpyexecdir) $(testhelper_la_LDFLAGS) $(testhelper_la_OBJECTS) $(testhelper_la_LIBADD) $(LIBS)

check-local: $(top_srcdir)/glib/__init__.py $(top_srcdir)/gobject/__init__.py $(top_srcdir)/gio/__init__.py
	@if test "$(top_builddir)" != "$(top_srcdir)"; then \
	   cp $(top_srcdir)/glib/*.py $(top_builddir)/glib; \
	   cp $(top_srcdir)/gobject/*.py $(top_builddir)/gobject; \
	   cp $(top_srcdir)/gio/*.py $(top_builddir)/gio; \
	fi
	@$(PYTHON) $(srcdir)/runtests.py $(top_builddir) $(top_srcdir)
	@if test "$(top_builddir)" != "$(top_srcdir)"; then \
	   rm -f $(top_builddir)/glib/*.py; \
	   rm -f $(top_builddir)/gobject/*.py; \
	   rm -f $(top_builddir)/gio/*.py; \
	fi
	@rm -fr $(top_builddir)/glib/*.pyc
	@rm -fr $(top_builddir)/gobject/*.pyc
	@rm -fr $(top_builddir)/gio/*.pyc
