poaincludedir=$(includedir)/orbit-2.0/orbit/poa
poainclude_HEADERS= \
	poa-types.h	\
	poa.h		\
	poa-basics.h	\
	poa-defs.h	\
	poa-policy.h	\
	portableserver-poa-type.h \
	portableserver-current-type.h \
	orbit-adaptor.h


BUILT_SOURCES = poa-defs.h

IDL_FLAGS =  -I$(top_srcdir)/src/idl/CORBA_PIDL				\
	-I$(top_srcdir)/src/idl/CORBA					\
	-I$(top_srcdir)/src/orb/orb-core				\
	-I$(top_srcdir)/src/idl/misc					\
	--define=Object=OObject --define=TypeCode=TTypeCode		\
	--noskels --nodefskels --nostubs --nocommon --noidata		\
        --showcpperrors
IDL_FILES=poa-defs.idl
IDL_DIR=$(top_srcdir)/src/orb/poa/
include $(top_srcdir)/Makefile.shared

CLEANFILES=$(BUILT_SOURCES)
