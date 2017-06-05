## Process this file with automake to produce Makefile.in

if OS_WIN32
LIBWS2_32 = -lws2_32
endif

INCLUDES = 				\
	-DG_LOG_DOMAIN=\"libsoup\" 	\
	-I$(top_srcdir)			\
	$(SOUP_DEBUG_FLAGS)		\
	$(GLIB_CFLAGS)			\
	$(XML_CFLAGS)			\
	$(LIBGCRYPT_CFLAGS)		\
	$(LIBGNUTLS_CFLAGS)

MARSHAL_GENERATED = soup-marshal.c soup-marshal.h
MKENUMS_GENERATED = soup-enum-types.c soup-enum-types.h

soup-marshal.h: soup-marshal.list
	( $(GLIB_GENMARSHAL) --prefix=soup_marshal $(srcdir)/soup-marshal.list --header > soup-marshal.tmp \
	&& mv soup-marshal.tmp soup-marshal.h ) \
	|| ( rm -f soup-marshal.tmp && exit 1 )

soup-marshal.c: soup-marshal.h
	( (echo '#include "soup-marshal.h"'; $(GLIB_GENMARSHAL) --prefix=soup_marshal $(srcdir)/soup-marshal.list --body) > soup-marshal.tmp \
	&& mv soup-marshal.tmp soup-marshal.c ) \
	|| ( rm -f soup-marshal.tmp && exit 1 )

soup-enum-types.h: $(soup_headers)
	( cd $(srcdir) && $(GLIB_MKENUMS) --template soup-enum-types.h.tmpl \
		$(soup_headers) ) > soup-enum-types.h.tmp \
	&& mv soup-enum-types.h.tmp soup-enum-types.h \
	|| rm -f soup-enum-type.h.tmp

soup-enum-types.c: $(libsoupinclude_HEADERS)
	( cd $(srcdir) && $(GLIB_MKENUMS) --template soup-enum-types.c.tmpl \
		$(soup_headers) ) > soup-enum-types.c.tmp \
	&& mv soup-enum-types.c.tmp soup-enum-types.c \
	|| rm -f soup-enum-type.c.tmp

BUILT_SOURCES = $(MARSHAL_GENERATED) $(MKENUMS_GENERATED)

CLEANFILES = $(MARSHAL_GENERATED) $(MKENUMS_GENERATED)

libsoupincludedir = $(includedir)/libsoup-$(SOUP_API_VERSION)/libsoup

soup_headers =			\
	soup.h			\
	soup-address.h		\
	soup-auth.h		\
	soup-auth-domain.h	\
	soup-auth-domain-basic.h  \
	soup-auth-domain-digest.h \
	soup-date.h		\
	soup-form.h		\
	soup-headers.h		\
	soup-logger.h		\
	soup-message.h		\
	soup-message-body.h	\
	soup-message-headers.h	\
	soup-method.h		\
	soup-misc.h     	\
	soup-portability.h	\
	soup-server.h		\
	soup-session.h		\
	soup-session-async.h	\
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
	$(LIBGNUTLS_LIBS_STATIC)	\
	$(LIBGNUTLS_LIBS)		\
	$(LIBGCRYPT_LIBS)		\
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
	soup-connection.h		\
	soup-connection.c		\
	soup-date.c			\
	soup-dns.h			\
	soup-dns.c			\
	soup-form.c			\
	soup-gnutls.c			\
	soup-headers.c			\
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
	soup-nossl.c     		\
	soup-path-map.h     		\
	soup-path-map.c     		\
	soup-server.c			\
	soup-session.c			\
	soup-session-async.c		\
	soup-session-private.h		\
	soup-session-sync.c		\
	soup-socket.c			\
	soup-ssl.h			\
	soup-status.c			\
	soup-uri.c			\
	soup-value-utils.c		\
	soup-xmlrpc.c

EXTRA_DIST=				\
	soup-marshal.list		\
	soup-enum-types.h.tmpl		\
	soup-enum-types.c.tmpl
