AM_CPPFLAGS = -I$(top_srcdir) $(LIBGSF_CFLAGS) $(GDK_PIXBUF_CFLAGS) $(LIBXML_CFLAGS)
LDADD = $(top_builddir)/gsf/libgsf-1.la $(LIBGSF_LIBS) $(GDK_PIXBUF_LIBS)

bin_PROGRAMS = gsf-office-thumbnailer

gsf_office_thumbnailer_SOURCES =	\
	main.c

thumbnailerdir = $(datadir)/thumbnailers
thumbnailer_DATA = gsf-office.thumbnailer

EXTRA_DIST = $(thumbnailer_DATA)
