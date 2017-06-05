SUBDIRS = . examples

INCLUDES = \
	-I$(top_srcdir)						\
	-I$(top_builddir)					\
	$(DBUS_CFLAGS)						\
	$(DBUS_GLIB_CFLAGS)					\
	$(DBUS_GLIB_TOOL_CFLAGS)				\
	-DDBUS_COMPILATION=1					\
	-DDBUS_LOCALEDIR=\"$(prefix)/@DATADIRNAME@/locale\" 

lib_LTLIBRARIES=libdbus-glib-1.la

CLEANFILES = $(BUILT_SOURCES)

DBUS_GLIB_INTERNALS = \
	dbus-gtype-specialized.c	        \
	dbus-gtype-specialized-priv.h		\
	dbus-gutils.c				\
	dbus-gutils.h				\
	dbus-gsignature.c			\
	dbus-gsignature.h			\
	dbus-gvalue.h				\
	dbus-gvalue-utils.c			\
	dbus-gvalue-utils.h

libdbus_glib_1_la_SOURCES = 			\
	dbus-glib.c				\
	dbus-gmain.c				\
	dbus-gmarshal.c				\
	dbus-gmarshal.h				\
	dbus-gobject.c				\
	dbus-gobject.h				\
	dbus-gproxy.c				\
	dbus-gtest.c				\
	dbus-gtest.h				\
	dbus-gvalue.c				\
	dbus-gvalue.h				\
	dbus-gvalue-parse-variant.c		\
	dbus-gthread.c				\
	$(DBUS_GLIB_INTERNALS)

libdbus_glib_HEADERS =                  \
	dbus-gtype-specialized.h	\
        dbus-gvalue-parse-variant.h	\
	dbus-glib.h			\
        dbus-glib-lowlevel.h

libdbus_glibdir = $(includedir)/dbus-1.0/dbus

libdbus_glib_1_la_LIBADD= $(DBUS_LIBS) $(DBUS_GLIB_LIBS)
## don't export symbols that start with "_" (we use this 
## convention for internal symbols)
libdbus_glib_1_la_LDFLAGS= -export-symbols-regex "^[^_].*" -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) -no-undefined

# convenience lib used here and by dbus-viewer
noinst_LTLIBRARIES=libdbus-gtool.la

libdbus_gtool_la_SOURCES = $(DBUS_GLIB_INTERNALS) \
	dbus-gidl.c				\
	dbus-gidl.h				\
	dbus-gloader-expat.c			\
	dbus-gparser.c				\
	dbus-gparser.h

libdbus_gtool_la_LIBADD = $(DBUS_LIBS) -lexpat

bin_PROGRAMS=dbus-binding-tool

dbus_binding_tool_SOURCES =			\
	dbus-binding-tool-glib.h		\
	dbus-binding-tool-glib.c		\
	dbus-glib-tool.h			\
	dbus-glib-tool.c

dbus_binding_tool_LDADD= $(builddir)/libdbus-gtool.la $(builddir)/libdbus-glib-1.la $(DBUS_LIBS) $(DBUS_GLIB_LIBS) -lexpat

## we just rebuilt these manually and check them into cvs; easier than
## convincing automake/make to do this properly
regenerate-built-sources:
	@GLIB_GENMARSHAL@ --prefix=_dbus_g_marshal dbus-gmarshal.list --header > dbus-gmarshal.h && \
	echo '#include <config.h>' > dbus-gmarshal.c &&						    \
	echo '#include "dbus-gmarshal.h"' >> dbus-gmarshal.c &&					    \
        @GLIB_GENMARSHAL@ --prefix=_dbus_g_marshal dbus-gmarshal.list --body >> dbus-gmarshal.c


completiondir = $(sysconfdir)/bash_completion.d
if DBUS_BASH_COMPLETION
libexec_PROGRAMS=dbus-bash-completion-helper
completion_SCRIPTS=dbus-bash-completion.sh
endif

dbus-bash-completion.sh : dbus-bash-completion.sh.in
	@sed -e "s|\@libexecdir\@|$(libexecdir)|" $< > $@
CLEANFILES += dbus-bash-completion.sh

dbus_bash_completion_helper_SOURCES =                 	\
	dbus-bash-completion-helper.c
dbus_bash_completion_helper_LDADD=$(builddir)/libdbus-gtool.la -lexpat $(builddir)/libdbus-glib-1.la $(DBUS_LIBS) $(DBUS_GLIB_LIBS)


EXTRA_DIST=dbus-gmarshal.list dbus-bash-completion.sh.in

if DBUS_BUILD_TESTS

## we use noinst_PROGRAMS not check_PROGRAMS for TESTS so that we
## build even when not doing "make check"
noinst_PROGRAMS= $(TESTS)

## note that TESTS has special meaning (stuff to use in make check)
## so if adding tests not to be run in make check, don't add them to 
## TESTS
TESTS_ENVIRONMENT=DBUS_TEST_DATA=$(top_builddir)/test/data DBUS_TEST_HOMEDIR=$(top_builddir)/dbus
TESTS=dbus-glib-test

## FIXME we aren't running dbus-glib-tool --self-test

dbus_glib_test_SOURCES=			\
	dbus-gtest-main.c

dbus_glib_test_LDADD= $(builddir)/libdbus-glib-1.la

else
### not building tests
TESTS=

endif

