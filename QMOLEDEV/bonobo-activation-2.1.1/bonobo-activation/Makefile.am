NULL=

lib_LTLIBRARIES=libbonobo-activation.la

INCLUDES=					\
	-I$(top_srcdir)				\
	-I$(top_builddir)			\
	-DSERVER_LIBEXECDIR=\""$(libexecdir)\"" \
        -DBONOBO_ACTIVATION_LOCALEDIR=\""${prefix}/${DATADIRNAME}/locale"\" \
	$(BONOBO_ACTIVATION_CFLAGS)		\
	-DVERSION=\"$(VERSION)\"		\
	-DG_DISABLE_DEPRECATED			\
	$(NULL)

LDFLAGS=					\
	$(BONOBO_ACTIVATION_LIBS)		\
	@LT_VERSION_INFO@			\
	$(NULL)

bonobo_activationincludedir = $(includedir)/bonobo-activation-2.0/bonobo-activation

bonobo_activationinclude_HEADERS=		\
	Bonobo_Unknown.h			\
	Bonobo_GenericFactory.h			\
	Bonobo_Activation_types.h		\
	bonobo-activation.h			\
	bonobo-activation-async.h		\
	bonobo-activation-base-service.h	\
	bonobo-activation-id.h			\
	bonobo-activation-activate.h		\
	bonobo-activation-init.h		\
	bonobo-activation-shlib.h		\
	bonobo-activation-register.h		\
	bonobo-activation-server-info.h		\
	bonobo-activation-async.h		\
	bonobo-activation-version.h		\
	$(NULL)


libbonobo_activation_la_SOURCES=		\
	$(BUILT_SOURCES)			\
	bonobo-activation-client.c		\
	bonobo-activation-client.h		\
	bonobo-activation-id.c			\
	bonobo-activation-private.h		\
	bonobo-activation-shlib.c		\
	bonobo-activation-activate.c		\
	bonobo-activation-activate-private.h	\
	bonobo-activation-base-service.c	\
	bonobo-activation-register.c		\
	bonobo-activation-server-info.c		\
	bonobo-activation-i18n.h		\
	bonobo-activation-fork-server.c		\
	bonobo-activation-init.c		\
	$(NULL)


IDL_FLAGS = -I$(top_srcdir)/idl
IDL_DIR = $(top_srcdir)/idl/
IDL_FILES = Bonobo_Unknown.idl			\
	    Bonobo_GenericFactory.idl		\
	    Bonobo_Activation_types.idl		\
	    Bonobo_ObjectDirectory.idl		\
	    Bonobo_ActivationContext.idl	\
	    $(NULL)

IDL_HEADER_FILES = $(IDL_FILES:.idl=.h)
IDL_COMMONS = $(IDL_FILES:.idl=-common.c)
IDL_SKELS = $(IDL_FILES:.idl=-skels.c)
IDL_STUBS = $(IDL_FILES:.idl=-stubs.c)

BUILT_SOURCES= $(IDL_HEADER_FILES) $(IDL_COMMONS) $(IDL_SKELS) $(IDL_STUBS) \
	$(NULL)

$(libbonobo_activation_la_OBJECTS): $(BUILT_SOURCES)

%.h %-stubs.c %-skels.c %-common.c %-imodule.c %-skelimpl.c: $(IDL_DIR)%.idl $(ORBIT_IDL)
	echo "top_srcdir: $(top_srcdir)"
	echo "IDL_DIR: $(IDL_DIR)"
	echo "IDL file: $<"
	-(rm -f $(*).h $(*)-stubs.c $(*)-skels.c $(*)-common.c $(*)-imodule.c $(*)-skelimpl.c || true) > /dev/null
	$(ORBIT_IDL) $(IDL_FLAGS) "-D__$(*)_COMPILATION" --deps $(*D)/.deps/$(*F).idl.P $<

if MAINTAINER_MODE
DEPS_MAGIC := $(shell mkdir .deps > /dev/null 2>&1 || :)
IDL_DEP_FILES = $(IDL_FILES:%.idl=.deps/%.idl.P)
-include $(IDL_DEP_FILES)
distclean-local:
	-rm -rf .deps
endif


CLEANFILES=					\
	$(BUILT_SOURCES)			\
	$(NULL)

dist-hook:
	cd $(distdir) ; rm -f $(CLEANFILES)
