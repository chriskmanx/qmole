noinst_LTLIBRARIES=liborb-util.la

INCLUDES =						\
	-I$(top_srcdir)/include				\
	-I$(top_builddir)/include			\
	-DORBIT_SYSTEM_RCFILE=\"$(sysconfdir)/orbitrc\"	\
	-DORBIT2_INTERNAL_API				\
	$(WARN_CFLAGS)					\
	$(DISABLE_DEPRECATED_CFLAGS)			\
	$(LOCAL_LINC_CFLAGS)				\
	$(ORBIT_CFLAGS)

liborb_util_la_SOURCES= \
	orbit-purify.h  \
	genrand.c       \
	orbit-options.c \
	orbit-options.h \
	orbit-util.c
