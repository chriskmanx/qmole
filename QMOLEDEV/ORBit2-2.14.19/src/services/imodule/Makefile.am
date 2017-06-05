NULL =

lib_LTLIBRARIES = libORBit-imodule-2.la

INCLUDES =				\
        -I$(top_srcdir)/include		\
        -I$(top_builddir)/include	\
	-DORBIT2_INTERNAL_API		\
	$(WARN_CFLAGS)			\
	$(LOCAL_LINC_CFLAGS)		\
	$(ORBIT_CFLAGS)			\
	$(ORBIT_IDL_CFLAGS)		\
	$(NULL)

LDADD = $(ORBIT_LIBS)				\
	$(ORBIT_IDL_LIBS)			\
	$(top_builddir)/src/orb/libORBit-2.la

imoduleincludedir      = $(includedir)/orbit-2.0/ORBitservices
imoduleinclude_HEADERS = orbit-imodule.h

libORBit_imodule_2_la_SOURCES =		\
	orbit-imodule-utils.c		\
	orbit-imodule-utils.h		\
	orbit-imodule-libidl-utils.c	\
	orbit-imodule-libidl-utils.h	\
	orbit-imodule.c			\
	$(NULL)

libORBit_imodule_2_la_LIBADD = \
	$(top_builddir)/src/orb/libORBit-2.la \
	$(ORBIT_IDL_LIBS) \
	$(ORBIT_LIBS)

libORBit_imodule_2_la_LDFLAGS = -no-undefined
