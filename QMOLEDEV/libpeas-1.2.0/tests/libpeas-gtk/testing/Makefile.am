noinst_LTLIBRARIES = libtesting.la

INCLUDES = \
	-I$(top_srcdir)				\
	-I$(srcdir)				\
	-I$(srcdir)/../../testing-util		\
	$(PEAS_CFLAGS)				\
	$(PEAS_GTK_CFLAGS)			\
	$(WARN_CFLAGS)				\
	$(DISABLE_DEPRECATED)			\
	-DBUILDDIR="\"$(abs_top_builddir)\""	\
	-DSRCDIR="\"$(abs_top_srcdir)\""

libtesting_la_LDFLAGS = \
	-export-dynamic -no-undefined -export-symbols-regex "^[^_].*"

libtesting_la_LIBADD = \
	$(PEAS_LIBS)					\
	$(PEAS_GTK_LIBS)				\
	$(top_builddir)/libpeas/libpeas-1.0.la		\
	$(top_builddir)/libpeas-gtk/libpeas-gtk-1.0.la	\
	../../testing-util/libtesting-util.la

libtesting_la_SOURCES = testing.c testing.h
