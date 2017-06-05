include $(top_srcdir)/Makefile.decl

NULL =

module_flags = -export_dynamic -avoid-version -module -no-undefined -export-symbols-regex '^g_io_module_(load|unload|query)'

giomodule_LTLIBRARIES = libgiognutls.la
giomoduledir = $(GIO_MODULE_DIR)

gnutls-marshal.h: gnutls-marshal.list
	$(AM_V_GEN) $(GLIB_GENMARSHAL) --prefix=_gnutls_marshal $(srcdir)/gnutls-marshal.list --header --internal > $@.tmp && \
	  mv $@.tmp $@

gnutls-marshal.c: gnutls-marshal.h gnutls-marshal.list
	$(AM_V_GEN) (echo "#include \"gnutls-marshal.h\""; \
	$(GLIB_GENMARSHAL) --prefix=_gnutls_marshal $(srcdir)/gnutls-marshal.list --body --internal) > $@.tmp && \
	  mv $@.tmp $@

libgiognutls_la_SOURCES = 		\
	gnutls-module.c			\
	gnutls-marshal.c		\
	gnutls-marshal.h		\
	gtlsbackend-gnutls.c		\
	gtlsbackend-gnutls.h		\
	gtlscertificate-gnutls.c	\
	gtlscertificate-gnutls.h	\
	gtlsclientconnection-gnutls.c	\
	gtlsclientconnection-gnutls.h	\
	gtlsconnection-gnutls.c		\
	gtlsconnection-gnutls.h		\
	gtlsinputstream-gnutls.c	\
	gtlsinputstream-gnutls.h	\
	gtlsoutputstream-gnutls.c	\
	gtlsoutputstream-gnutls.h	\
	gtlsserverconnection-gnutls.c	\
	gtlsserverconnection-gnutls.h	\
	$(NULL)

$(libgiognutls_la_OBJECTS): gnutls-marshal.h

libgiognutls_la_CFLAGS = \
	-DG_LOG_DOMAIN=\"GLib-Net\"	\
	$(GLIB_CFLAGS)			\
	$(LIBGNUTLS_CFLAGS)		\
	$(LIBGCRYPT_CFLAGS)		\
	-DGIO_MODULE_DIR=\"$(GIO_MODULE_DIR)\"	\
	-DG_DISABLE_DEPRECATED

libgiognutls_la_LDFLAGS = $(module_flags)
libgiognutls_la_LIBADD =		\
	$(GLIB_LIBS)			\
	$(GNUTLS_LIBS)			\
	$(LIBGCRYPT_LIBS)		\
	$(NULL)

BUILT_SOURCES = 		\
	gnutls-marshal.h	\
	gnutls-marshal.c

EXTRA_DIST = 			\
	gnutls-marshal.list

CLEANFILES = 			\
	$(BUILT_SOURCES)
