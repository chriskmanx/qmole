dynamicincludedir=$(includedir)/orbit-2.0/orbit/dynamic
dynamicinclude_HEADERS= \
	dynamic.h	\
	dynamic-defs.h

BUILT_SOURCES=dynamic-defs.h

IDL_FLAGS=-I$(top_srcdir)/src/idl/CORBA_PIDL			\
	-I$(top_srcdir)/src/idl/CORBA				\
	-I$(top_srcdir)/src/orb/orb-core			\
	-I$(top_srcdir)/src/idl/misc				\
	--define=Object=OObject --define=TypeCode=TTypeCode	\
	--noskels --nodefskels --nostubs --nocommon --noidata	\
        --showcpperrors 
IDL_DIR=$(top_srcdir)/src/orb/dynamic/
IDL_FILES=dynamic-defs.idl

include $(top_srcdir)/Makefile.shared

CLEANFILES=$(BUILT_SOURCES)

