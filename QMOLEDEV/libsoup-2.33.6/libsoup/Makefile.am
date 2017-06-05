## Process this file with automake to produce Makefile.in

if OS_WIN32
LIBWS2_32 = -lws2_32
endif

INCLUDES = 				\
	-DG_LOG_DOMAIN=\"libsoup\" 	\
	-I$(top_srcdir)			\
	$(SOUP_DEBUG_FLAGS)		\
	$(SOUP_MAINTAINER_FLAGS)	\
	$(GLIB_CFLAGS)			\
	$(XML_CFLAGS)			\
	$(GCONF_CFLAGS)			\
	$(LIBPROXY_CFLAGS)		\
	$(SQLITE_CFLAGS)		\
	$(GNOME_KEYRING_CFLAGS)

MARSHAL_GENERATED = soup-marshal.c soup-marshal.h
MKENUMS_GENERATED = soup-enum-types.c soup-enum-types.h

soup-marshal.h: soup-marshal.list
	$(AM_V_GEN) ( $(GLIB_GENMARSHAL) --prefix=soup_marshal $(srcdir)/soup-marshal.list --header > soup-marshal.tmp \
	&& mv soup-marshal.tmp soup-marshal.h ) \
	|| ( rm -f soup-marshal.tmp && exit 1 )

soup-marshal.c: soup-marshal.h
	$(AM_V_GEN) ( (echo '#include "soup-marshal.h"'; $(GLIB_GENMARSHAL) --prefix=soup_marshal $(srcdir)/soup-marshal.list --body) > soup-marshal.tmp \
	&& mv soup-marshal.tmp soup-marshal.c ) \
	|| ( rm -f soup-marshal.tmp && exit 1 )

soup-enum-types.h: $(soup_headers)
	$(AM_V_GEN) ( cd $(srcdir) && $(GLIB_MKENUMS) --template soup-enum-types.h.tmpl \
		$(soup_headers) ) > soup-enum-types.h.tmp \
	&& mv soup-enum-types.h.tmp soup-enum-types.h \
	|| rm -f soup-enum-type.h.tmp

soup-enum-types.c: $(libsoupinclude_HEADERS)
	$(AM_V_GEN) ( cd $(srcdir) && $(GLIB_MKENUMS) --template soup-enum-types.c.tmpl \
		$(soup_headers) ) > soup-enum-types.c.tmp \
	&& mv soup-enum-types.c.tmp soup-enum-types.c \
	|| rm -f soup-enum-type.c.tmp

BUILT_SOURCES = $(MARSHAL_GENERATED) $(MKENUMS_GENERATED)

CLEANFILES = $(MARSHAL_GENERATED) $(MKENUMS_GENERATED)

libsoupincludedir = $(includedir)/libsoup-2.4/libsoup

soup_headers =			\
	soup.h			\
	soup-address.h		\
	soup-auth.h		\
	soup-auth-domain.h	\
	soup-auth-domain-basic.h  \
	soup-auth-domain-digest.h \
	soup-cache.h		\
	soup-content-decoder.h	\
	soup-content-sniffer.h	\
	soup-cookie.h		\
	soup-cookie-jar.h	\
	soup-cookie-jar-text.h	\
	soup-date.h		\
	soup-form.h		\
	soup-headers.h		\
	soup-logger.h		\
	soup-message.h		\
	soup-message-body.h	\
	soup-message-headers.h	\
	soup-method.h		\
	soup-misc.h     	\
	soup-multipart.h     	\
	soup-password-manager.h	\
	soup-portability.h	\
	soup-proxy-resolver.h	\
	soup-proxy-uri-resolver.h \
	soup-request.h		\
	soup-request-data.h	\
	soup-request-file.h	\
	soup-request-http.h	\
	soup-requester.h	\
	soup-server.h		\
	soup-session.h		\
	soup-session-async.h	\
	soup-session-feature.h	\
	soup-session-sync.h	\
	soup-socket.h		\
	soup-status.h		\
	soup-types.h		\
	soup-uri.h		\
	soup-value-utils.h	\
	soup-xmlrpc.h

libsoupinclude_HEADERS =	\
	$(soup_headers)		\
	soup-enum-types.h

lib_LTLIBRARIES = libsoup-2.4.la

libsoup_2_4_la_LDFLAGS =	\
	-version-info $(SOUP_CURRENT):$(SOUP_REVISION):$(SOUP_AGE) -no-undefined

libsoup_2_4_la_LIBADD =			\
	$(GLIB_LIBS)			\
	$(XML_LIBS)			\
	-lz				\
	$(LIBWS2_32)

libsoup_2_4_la_SOURCES =		\
	$(BUILT_SOURCES)		\
	soup-address.c			\
	soup-auth.c			\
	soup-auth-basic.h		\
	soup-auth-basic.c		\
	soup-auth-digest.h		\
	soup-auth-digest.c		\
	soup-auth-ntlm.h		\
	soup-auth-ntlm.c		\
	soup-auth-domain.c		\
	soup-auth-domain-basic.c	\
	soup-auth-domain-digest.c	\
	soup-auth-manager.h		\
	soup-auth-manager.c		\
	soup-auth-manager-ntlm.h	\
	soup-auth-manager-ntlm.c	\
	soup-cache.c			\
	soup-cache-private.h		\
	soup-connection.h		\
	soup-connection.c		\
	soup-content-decoder.c		\
	soup-content-sniffer.c		\
	soup-cookie.c			\
	soup-cookie-jar.c		\
	soup-cookie-jar-text.c		\
	soup-date.c			\
	soup-directory-input-stream.h	\
	soup-directory-input-stream.c	\
	soup-form.c			\
	soup-headers.c			\
	soup-http-input-stream.h	\
	soup-http-input-stream.c	\
	soup-logger.c			\
	soup-message.c			\
	soup-message-body.c		\
	soup-message-client-io.c	\
	soup-message-headers.c		\
	soup-message-io.c		\
	soup-message-private.h		\
	soup-message-queue.h		\
	soup-message-queue.c		\
	soup-message-server-io.c	\
	soup-method.c     		\
	soup-misc.c     		\
	soup-multipart.c	     	\
	soup-password-manager.c		\
	soup-path-map.h     		\
	soup-path-map.c     		\
	soup-proxy-resolver.c		\
	soup-proxy-resolver-static.h	\
	soup-proxy-resolver-static.c	\
	soup-proxy-uri-resolver.c	\
	soup-request.c			\
	soup-request-data.c		\
	soup-request-file.c		\
	soup-request-http.c		\
	soup-requester.c		\
	soup-server.c			\
	soup-session.c			\
	soup-session-async.c		\
	soup-session-feature.c		\
	soup-session-private.h		\
	soup-session-sync.c		\
	soup-socket.c			\
	soup-ssl.h			\
	soup-ssl.c	     		\
	soup-status.c			\
	soup-uri.c			\
	soup-value-utils.c		\
	soup-xmlrpc.c

if BUILD_LIBSOUP_GNOME

libsoupgnomeincludedir = $(includedir)/libsoup-gnome-2.4/libsoup

libsoupgnomeinclude_HEADERS =	\
	soup-cookie-jar-sqlite.h\
	soup-gnome.h		\
	soup-gnome-features.h

lib_LTLIBRARIES += libsoup-gnome-2.4.la

libsoup_gnome_2_4_la_LDFLAGS = $(libsoup_2_4_la_LDFLAGS)

libsoup_gnome_2_4_la_LIBADD =		\
	libsoup-2.4.la			\
	$(GLIB_LIBS)			\
	$(GCONF_LIBS)			\
	$(LIBPROXY_LIBS)		\
	$(SQLITE_LIBS)			\
	$(GNOME_KEYRING_LIBS)

libsoup_gnome_2_4_la_SOURCES =		\
	soup-cookie-jar-sqlite.c	\
	soup-gnome-features.c		\
	soup-proxy-resolver-gnome.h	\
	soup-proxy-resolver-gnome.c	\
	soup-password-manager-gnome.h	\
	soup-password-manager-gnome.c

endif

#
# Introspection support
#
include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = --add-include-path=.
INTROSPECTION_COMPILER_ARGS = --includedir=.

if HAVE_INTROSPECTION

# Core library
gi_soup_files = \
	$(filter-out soup.h soup-enum-types.% soup-marshal.%,\
	   $(soup_headers) $(filter-out %.h, $(libsoup_2_4_la_SOURCES)))
gi_built_soup_files = soup-enum-types.h

Soup-2.4.gir: libsoup-2.4.la
Soup_2_4_gir_INCLUDES = Gio-2.0
Soup_2_4_gir_CFLAGS = $(INCLUDES)
Soup_2_4_gir_LIBS = libsoup-2.4.la
Soup_2_4_gir_EXPORT_PACKAGES = libsoup-2.4
Soup_2_4_gir_SCANNERFLAGS = --c-include "libsoup/soup.h"
Soup_2_4_gir_FILES = \
	$(addprefix $(srcdir)/, $(gi_soup_files)) \
        $(foreach f,$(gi_built_soup_files), \
	   $(if $(shell test -f $(addprefix $(srcdir)/,$(f)) && echo yes), \
	      $(addprefix $(srcdir)/,$(f)), \
	      $(f)))

INTROSPECTION_GIRS += Soup-2.4.gir

if BUILD_LIBSOUP_GNOME

# GNOME extensions
gi_soup_gnome_files = $(filter-out soup-gnome.h,\
		          $(libsoupgnomeinclude_HEADERS) \
	                  $(filter-out %.h, $(libsoup_gnome_2_4_la_SOURCES)))
SoupGNOME-2.4.gir: libsoup-gnome-2.4.la Soup-2.4.gir
SoupGNOME_2_4_gir_SCANNERFLAGS = --identifier-prefix=Soup --symbol-prefix=soup --c-include "libsoup/soup-gnome.h"
SoupGNOME_2_4_gir_INCLUDES = Soup-2.4
SoupGNOME_2_4_gir_CFLAGS = $(INCLUDES)
SoupGNOME_2_4_gir_LIBS = libsoup-gnome-2.4.la
SoupGNOME_2_4_gir_FILES = $(addprefix $(srcdir)/,$(gi_soup_gnome_files))
SoupGNOME_2_4_gir_EXPORT_PACKAGES = libsoup-gnome-2.4

INTROSPECTION_GIRS += SoupGNOME-2.4.gir

endif

girdir = $(datadir)/gir-1.0
gir_DATA = $(INTROSPECTION_GIRS)

typelibdir = $(libdir)/girepository-1.0
typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES += $(gir_DATA) $(typelib_DATA)

endif

EXTRA_DIST=				\
	soup-marshal.list		\
	soup-enum-types.h.tmpl		\
	soup-enum-types.c.tmpl
