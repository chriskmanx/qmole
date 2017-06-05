INCLUDES =				\
	-I$(top_srcdir)/include		\
	-I$(top_builddir)/include	\
	-DORBIT2_INTERNAL_API		\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED_CFLAGS)	\
	$(LOCAL_LINC_CFLAGS)		\
	$(ORBIT_CFLAGS)

noinst_LTLIBRARIES=libGIOP.la

libGIOP_la_SOURCES=giop-connection.c \
	giop-server.c \
	giop.c \
	giop-debug.h \
	giop-private.h \
	giop-send-buffer.c \
	giop-recv-buffer.c
