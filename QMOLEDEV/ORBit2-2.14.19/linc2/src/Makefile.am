noinst_LTLIBRARIES=liblinc.la
#lib_LTLIBRARIES=liblinc.la

if OS_WIN32
else
bin_PROGRAMS = linc-cleanup-sockets
endif

#	-I$(top_builddir)/include  
#	-I$(top_srcdir)/include    
INCLUDES =                         \
	-I$(top_builddir)/linc2/include  \
	-I$(top_srcdir)/linc2/include    \
	$(LINC_CFLAGS)             \
	$(WARN_CFLAGS)             \
	-DG_DISABLE_DEPRECATED

if OS_WIN32
else
INCLUDES += -D_GNU_SOURCE
endif

liblinc_la_SOURCES =		\
	linc.c			\
	linc-connection.c	\
	linc-debug.h		\
	linc-compat.h		\
	linc-compat.c		\
	linc-private.h		\
	linc-protocols.c	\
	linc-server.c		\
	linc-source.c

liblinc_la_LDFLAGS =                 \
	$(LINC_LIBS)                 \
	-no-undefined

linc_cleanup_sockets_SOURCES = \
	cleanup.c

linc_cleanup_sockets_LDFLAGS = \
	$(LINC_LIBS)
