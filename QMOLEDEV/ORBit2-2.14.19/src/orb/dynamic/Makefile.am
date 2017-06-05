noinst_LTLIBRARIES=liborb-dynamic.la

DYNAMIC_IDLOUT=dynamic-defs-common.c
liborb_dynamic_la_SOURCES= \
	$(DYNAMIC_IDLOUT) \
	dynany.c

INCLUDES =						\
	-I$(top_srcdir)/include				\
	-I$(top_srcdir)/src/orb				\
	-I$(top_builddir)/include			\
	-I$(top_builddir)/include/orbit/dynamic		\
	-DORBIT2_INTERNAL_API				\
	$(WARN_CFLAGS)					\
	$(DISABLE_DEPRECATED_CFLAGS)			\
	$(LOCAL_LINC_CFLAGS)				\
	$(ORBIT_CFLAGS)

IDL_FLAGS= -I$(top_srcdir)/src/idl/CORBA_PIDL			\
	--define=Object=OObject --define=TypeCode=TTypeCode	\
	-I$(top_srcdir)/src/idl/CORBA				\
	-I$(top_srcdir)/src/idl/misc				\
	-I$(top_srcdir)/src/orb/orb-core			\
	--noskels --nodefskels --nostubs --noidata --noheaders  \
	--showcpperrors
IDL_FILES=dynamic-defs.idl
include $(top_srcdir)/Makefile.shared

BUILT_SOURCES = $(DYNAMIC_IDLOUT)
CLEANFILES = $(BUILT_SOURCES)
EXTRA_DIST = $(IDL_FILES)

dist-hook:
	cd $(distdir); rm -f $(BUILT_SOURCES)
