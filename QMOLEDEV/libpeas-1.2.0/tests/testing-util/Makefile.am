INCLUDES = \
	-I$(top_srcdir)				\
	-I$(srcdir)				\
	$(PEAS_CFLAGS)				\
	$(WARN_CFLAGS)				\
	$(DISABLE_DEPRECATED)			\
	-DBUILDDIR="\"$(abs_top_builddir)\""	\
	-DSRCDIR="\"$(abs_top_srcdir)\""

noinst_LTLIBRARIES = libtesting-util.la

libtesting_util_la_LDFLAGS = \
	-export-dynamic -no-undefined -export-symbols-regex "^[^_].*"

libtesting_util_la_LIBADD = \
	$(PEAS_LIBS)					\
	$(top_builddir)/libpeas/libpeas-1.0.la

libtesting_util_la_SOURCES = \
	testing-util.c		\
	testing-util.h
