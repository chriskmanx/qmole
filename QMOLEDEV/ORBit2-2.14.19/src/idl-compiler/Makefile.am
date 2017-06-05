bin_PROGRAMS = orbit-idl-2

orbit_backends_dir = $(libdir)/orbit-2.0/idl-backends

INCLUDES =							\
	-I$(top_builddir)					\
	-I$(top_builddir)/include				\
	-I$(top_srcdir)						\
	-I$(top_srcdir)/include					\
	-DORBIT_BACKENDS_DIR="\"$(orbit_backends_dir)\""	\
	-DVERSION=\"$(ORBIT_VERSION)\"				\
	-DORBIT2_INTERNAL_API					\
	$(WARN_CFLAGS)						\
	$(DISABLE_DEPRECATED_CFLAGS)				\
	$(ORBIT_IDL_CFLAGS)

orbit_idl_2_LDADD =                     \
	$(ORBIT_IDL_LIBS)		\
	$(LIBM)

orbit_idl_2_SOURCES =			\
	orbit-idl-main.c		\
	orbit-idl-driver.c		\
	orbit-idl-backend.c		\
	orbit-idl-backend.h		\
	orbit-idl-utils.c		\
	orbit-idl2.h			\
	orbit-idl3-types.h		\
        orbit-idl-c-backend.h		\
        orbit-idl-c-backend.c		\
        orbit-idl-c-stubs.c		\
        orbit-idl-c-skels.c		\
        orbit-idl-c-headers.c		\
        orbit-idl-c-common.c		\
	orbit-idl-c-imodule.c		\
        orbit-idl-c-skelimpl.c		\
        orbit-idl-c-utils.c		\
        orbit-idl-c-typecode.c		\
        orbit-idl-c-deps.c		\
	orbit-idl2.h

orbitidlincludedir = $(includedir)/orbit-2.0/orbit-idl
orbitidlinclude_HEADERS = orbit-idl-backend.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ORBit-idl-2.0.pc
