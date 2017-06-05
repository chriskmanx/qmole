if OS_WIN32
libm=
else
libm=-lm
endif

libORBitservicesincludedir = $(includedir)/orbit-2.0/ORBitservices

lib_LTLIBRARIES=libORBitCosNaming-2.la

noinst_PROGRAMS = name-client-2 orbit-name-server-2

lib_LIBRARIES = libname-server-2.a

libORBitCosNaming_2_la_SOURCES = 					\
	CosNaming-common.c 						\
	CosNaming-stubs.c

libORBitservicesinclude_HEADERS = 					\
	CosNaming.h							\
	CosNaming_impl.h

libORBitCosNaming_2_la_LDFLAGS =					\
	-version-info $(LT_VERSION)					\
	-no-undefined							\
	$(ORBIT_NAME_LIBS)						\
	$(top_builddir)/src/orb/libORBit-2.la

INCLUDES =				\
	-I.				\
	-I$(top_builddir)/include	\
	-I$(top_srcdir)/include		\
	-DORBIT2_INTERNAL_API		\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED_CFLAGS)	\
	$(LOCAL_LINC_CFLAGS)		\
	$(ORBIT_NAME_CFLAGS)

CosNaming_built = 							\
	CosNaming.h 							\
	CosNaming-common.c 						\
	CosNaming-stubs.c 						\
	CosNaming-skels.c

IDL_FLAGS=--showcpperrors
IDL_FILES=CosNaming.idl
include $(top_srcdir)/Makefile.shared

CLEANFILES=my_cosnaming_built $(BUILT_SOURCES)

DEPS = 	$(top_builddir)/src/orb/libORBit-2.la			\
	libORBitCosNaming-2.la

LDADDS =                                                        \
	$(top_builddir)/src/orb/libORBit-2.la			\
	libORBitCosNaming-2.la					\
	$(libm)							\
	$(ORBIT_NAME_LIBS)

name_client_2_SOURCES = name-client.c name-support.c name-support.h
name_client_2_LDFLAGS = $(FLAGS)
name_client_2_DEPENDENCIES = $(DEPS) CosNaming.h
name_client_2_LDADD = $(LDADDS)

libname_server_2_a_SOURCES = orbit-name-server.c CosNaming-skels.c \
			   name-support.c name-support.h
libname_server_2_a_DEPENDENCIES = $(DEPS) CosNaming.h

orbit_name_server_2_SOURCES = boot.c
orbit_name_server_2_LDFLAGS = $(FLAGS)
orbit_name_server_2_DEPENDENCIES = $(DEPS) CosNaming.h
orbit_name_server_2_LDADD = libname-server-2.a $(LDADDS)

$(libname_server_2_a_OBJECTS) $(name_client_2_OBJECTS) $(orbit_name_server_2_OBJECTS) : CosNaming.h

EXTRA_DIST = $(idl_DATA)

idldir   = $(datadir)/idl/orbit-2.0
idl_DATA = CosNaming.idl

BUILT_SOURCES = $(CosNaming_built)

dist-hook:
	cd $(distdir); rm -f $(BUILT_SOURCES)
