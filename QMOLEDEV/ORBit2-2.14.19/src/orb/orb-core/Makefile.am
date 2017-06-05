noinst_LTLIBRARIES=liborb-core.la

TYPELIB_DIR=$(libdir)/orbit-2.0

INCLUDES =						\
	-I.						\
	-I$(srcdir)					\
	-I$(top_builddir)/include			\
	-I$(top_builddir)/include/orbit/orb-core	\
	-I$(top_srcdir)/include				\
	-I$(top_srcdir)/include/orbit/orb-core	\
	-DORBIT_TYPELIB_DIR=\""$(TYPELIB_DIR)"\"	\
	-DORBIT2_INTERNAL_API				\
	$(WARN_CFLAGS)					\
	$(DISABLE_DEPRECATED_CFLAGS)			\
	$(LOCAL_LINC_CFLAGS)				\
	$(ORBIT_CFLAGS)

HTTP_FILES = orbhttp.c orbhttp.h

if ENABLE_HTTP
ORBHTTP = $(HTTP_FILES)
endif

main_src = \
	corba-orb.c \
	orbit-object.c \
	orbit-small.c \
	orbit-typelib.c \
	orbit-trace.c \
	corba-object.c \
	corba-loc.c \
	corba-policy.c \
	orbit-policy.c \
	orbit-policy.h \
	corba-env.c \
	corba-string.c \
	allocators.c \
	corba-typecode.c \
	corba-types.c \
	corba-any.c \
	corba-context.c \
	orb-core-private.h \
	orb-core-export.h \
	corba-nvlist.c \
	corba-request.c \
	iop-profiles.c \
	iop-profiles.h \
	orbit-debug.h \
	$(ORBHTTP) 

liborb_core_la_SOURCES= \
	$(BUILT_SOURCES) \
	$(main_src)

IDLOUT=corba-defs-common.c
DEFS_IDLOUT=iop-defs-common.c
OPS_IDLOUT_H= corba-ops.h
OPS_IDLOUT_C= corba-ops-stubs.c corba-ops-common.c corba-ops-skels.c
IFACE_IDLOUT=orbit-interface-common.c

$(liborb_core_la_OBJECTS) : $(BUILT_SOURCES)

IDL_FLAGS = -I$(top_srcdir)/src/idl/CORBA_PIDL			\
	-I$(top_srcdir)/src/idl/CORBA				\
	-I$(top_srcdir)/src/idl/misc				\
	-I$(top_srcdir)/src/idl/interop -I.				\
	-D_PRE_3_0_COMPILER_                                    \
	--noskels --nodefskels --nostubs --noidata --noheaders	\
	--define=Object=OObject --define=TypeCode=TTypeCode	\
        --showcpperrors
IDL_FILES=corba-defs.idl iop-defs.idl corba-ops.idl orbit-interface.idl
IDL_DIR=$(top_srcdir)/src/orb/orb-core/
include $(top_srcdir)/Makefile.shared

IDL_FLAGS_CORBA_OPS = --showcpperrors
$(OPS_IDLOUT_C): $(OPS_IDLOUT_H)

$(OPS_IDLOUT_H): corba-ops.idl $(IDL_COMPILER)
	-(rm -f $(OPS_IDLOUT_C) $(OPS_IDLOUT_H) || true) > /dev/null
	$(IDL_COMPILER) $(IDL_FLAGS_CORBA_OPS) --deps .deps/corba-ops.idl.P $<
	for I in $(OPS_IDLOUT_C) $(OPS_IDLOUT_H); do								\
		sed -e 's,ZZZis_a,_is_a,g' -e 's,ZZis_a,is_a,g' $$I > $$I.out; mv $$I.out $$I;	\
	done;											\
	sed -e "s,Z,_,g" corba-ops-skels.c > corba-ops-skels.c.out;				\
	mv corba-ops-skels.c.out corba-ops-skels.c;

IDL_FLAGS_INTERFACE = --nostubs --noskels --showcpperrors --noheaders
$(IFACE_IDLOUT) : orbit-interface.idl $(IDL_COMPILER)
	-(rm -f $(IFACE_IDLOUT) || true) > /dev/null
	$(IDL_COMPILER) $(IDL_FLAGS_INTERFACE) --deps .deps/orbit-interface.idl.P $<

BUILT_SOURCES = $(IFACE_IDLOUT) $(IDLOUT) $(DEFS_IDLOUT) $(OPS_IDLOUT_C) $(OPS_IDLOUT_H)
CLEANFILES = $(BUILT_SOURCES)
EXTRA_DIST = $(IDL_FILES) $(HTTP_FILES)

dist-hook:
	cd $(distdir); rm -f $(BUILT_SOURCES)

$(IDL_COMPILER):
	@cd $(top_builddir)/src/idl-compiler && $(MAKE) $(AM_MAKEFLAGS)
