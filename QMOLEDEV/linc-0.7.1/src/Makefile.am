lib_LTLIBRARIES=liblinc.la

bin_PROGRAMS = linc-cleanup-sockets

INCLUDES =                         \
	-I$(top_builddir)/include  \
	-I$(top_srcdir)/include    \
	$(LINC_CFLAGS)             \
	$(SSL_CFLAGS)              \
	$(WARN_CFLAGS)             \
	-DG_DISABLE_DEPRECATED	   \
	-D_GNU_SOURCE

liblinc_la_SOURCES =		\
	linc.c			\
	linc-connection.c	\
	linc-debug.h		\
	linc-compat.h		\
	linc-private.h		\
	linc-protocols.c	\
	linc-server.c		\
	linc-source.c

liblinc_la_LDFLAGS =                 \
	$(LINC_LIBS)                 \
	$(SSL_LIBS)                  \
	-version-info $(LT_VERSION)

linc_cleanup_sockets_SOURCES = \
	cleanup.c

linc_cleanup_sockets_LDFLAGS = \
	$(LINC_LIBS)