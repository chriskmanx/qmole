include $(top_srcdir)/Makefile.decl

INCLUDES =				\
	-I$(top_srcdir)			\
	-I$(top_srcdir)/gdk-pixbuf	\
	$(GDK_PIXBUF_DEBUG_FLAGS)	\
	$(GDK_PIXBUF_DEP_CFLAGS)

DEPS = \
	$(top_builddir)/gdk-pixbuf/libgdk_pixbuf-$(GDK_PIXBUF_API_VERSION).la

LDADDS = \
	$(top_builddir)/gdk-pixbuf/libgdk_pixbuf-$(GDK_PIXBUF_API_VERSION).la \
	$(GDK_PIXBUF_DEP_LIBS)

noinst_PROGRAMS = \
	pixbuf-read			\
	pixbuf-lowmem			\
	pixbuf-randomly-modified	\
	pixbuf-random			\
	pixbuf-threads

pixbuf_read_LDADD = $(LDADDS)
pixbuf_lowmem_LDADD = $(LDADDS)
pixbuf_randomly_modified_LDADD = $(LDADDS)
pixbuf_random_LDADD = $(LDADDS)
pixbuf_threads_LDADD = $(LDADDS) $(GLIB_LIBS)


-include $(top_srcdir)/git.mk
