noinst_LTLIBRARIES=liborb-poa.la

IDL = $(top_builddir)/src/idl-compiler/orbit-idl-2

INCLUDES =						\
	-I$(top_srcdir)/include				\
	-I$(top_builddir)/include			\
	-I$(top_builddir)/src/orb/orb-core		\
	-I$(top_srcdir)/src/orb/orb-core		\
	-I$(top_builddir)/include/orbit/poa		\
	-I$(top_srcdir)/include/orbit/orb-core		\
	-I$(top_builddir)/include/orbit/orb-core	\
	-I$(top_srcdir)/src/orb/poa			\
	-DORBIT2_INTERNAL_API				\
	$(WARN_CFLAGS)					\
	$(DISABLE_DEPRECATED_CFLAGS)			\
	$(LOCAL_LINC_CFLAGS)				\
	$(ORBIT_CFLAGS)

POA_IDLOUT=poa-defs-common.c

main_src = \
	poa.c \
	orbit-poa-export.h \
	orbit-poa.h \
	poa-private.h \
	poa-macros.h \
	poa-manager.c \
	poa-policy.c \
	poa-servants.c \
	orbit-adaptor.c

liborb_poa_la_SOURCES=\
	$(POA_IDLOUT) \
	$(main_src)

IDL_FLAGS= -I$(top_srcdir)/src/idl/CORBA_PIDL			\
	-I$(top_srcdir)/src/idl/CORBA				\
	-I$(top_srcdir)/src/idl/misc				\
	-I$(top_srcdir)/src/orb/orb-core			\
	-D_PRE_3_0_COMPILER_                                    \
	--noskels --nodefskels --nostubs --noidata --noheaders  \
	--define=Object=OObject --define=TypeCode=TTypeCode	\
        --showcpperrors
IDL_FILES=poa-defs.idl
include $(top_srcdir)/Makefile.shared

BUILT_SOURCES = $(POA_IDLOUT)
CLEANFILES = $(BUILT_SOURCES)
EXTRA_DIST = $(IDL_FILES)

dist-hook:
	cd $(distdir); rm -f $(BUILT_SOURCES)
