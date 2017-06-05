SUBDIRS=plugins

bin_PROGRAMS = peas-demo

INCLUDES =								\
	-I$(top_srcdir)							\
	-I$(srcdir)							\
	$(PEAS_CFLAGS)							\
	$(PEAS_GTK_CFLAGS)						\
	$(WARN_CFLAGS)							\
	$(DISABLE_DEPRECATED)

peas_demo_SOURCES = \
	peas-demo-window.c		\
	peas-demo-window.h		\
	peas-demo.c

peas_demo_LDADD = \
	../libpeas/libpeas-1.0.la		\
	../libpeas-gtk/libpeas-gtk-1.0.la	\
	$(PEAS_LIBS)				\
	$(PEAS_GTK_LIBS)

peas_demo_CFLAGS = \
	$(PEAS_CFLAGS)			\
	-DPEAS_LIBDIR="\"$(libdir)\""	\
	-DPEAS_PREFIX="\"$(prefix)\""

.PHONY: demo
demo: $(bin_PROGRAMS)
	./peas-demo --run-from-build-dir
