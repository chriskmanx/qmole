include $(top_srcdir)/Makefile.decl

NULL =

module_flags = -export_dynamic -avoid-version -module -no-undefined -export-symbols-regex '^g_io_module_(load|unload|query)'

giomodule_LTLIBRARIES = libgiolibproxy.la
giomoduledir = $(GIO_MODULE_DIR)

libgiolibproxy_la_SOURCES = 		\
	glibproxyresolver.c		\
	glibproxyresolver.h		\
	libproxy-module.c		\
	$(NULL)

libgiolibproxy_la_CFLAGS = \
	-DG_LOG_DOMAIN=\"GLib-Net\"	\
	$(GLIB_CFLAGS)			\
	$(LIBPROXY_CFLAGS)		\
	-DGIO_MODULE_DIR=\"$(GIO_MODULE_DIR)\"	\
	-DG_DISABLE_DEPRECATED

libgiolibproxy_la_LDFLAGS = $(module_flags)
libgiolibproxy_la_LIBADD = \
		$(GLIB_LIBS) \
		$(LIBPROXY_LIBS) \
		$(NULL)
