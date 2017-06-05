INCLUDES = \
	-I$(top_srcdir)				\
	-I$(top_srcdir)/dbus			\
	-I$(top_builddir)			\
	-I$(top_builddir)/dbus			\
	$(DBUS_CFLAGS)				\
	$(DBUS_GLIB_CFLAGS)			\
	-DDBUS_COMPILATION

LDADD = \
	$(DBUS_GLIB_THREADS_LIBS) \
	$(DBUS_GLIB_LIBS) \
	$(DBUS_LIBS) \
	$(top_builddir)/dbus/libdbus-glib-1.la

tool_ldadd = \
	$(LDADD) \
	$(DBUS_GLIB_TOOL_LIBS) \
	$(top_builddir)/dbus/libdbus-gtool.la

## note that TESTS has special meaning (stuff to use in make check)
## so if adding tests not to be run in make check, don't add them to 
## TESTS
if DBUS_BUILD_TESTS
TESTS_ENVIRONMENT=DBUS_TOP_BUILDDIR=$(ABSOLUTE_TOP_BUILDDIR)
TESTS=run-test.sh run-peer-test.sh
else
TESTS=
endif

EXTRA_DIST=run-test.sh run-peer-test.sh test-service-glib.xml my-object-marshal.list test-service-glib-subclass.xml

if DBUS_BUILD_TESTS

if HAVE_GLIB_THREADS
THREAD_APPS=test-thread-server test-thread-client test-profile

test_thread_server_SOURCES=				\
	test-thread-server.c				\
	test-thread.h

test_thread_client_SOURCES=				\
	test-thread-client.c				\
	test-thread.h
endif

## we use noinst_PROGRAMS not check_PROGRAMS for TESTS so that we
## build even when not doing "make check"
noinst_PROGRAMS = \
	test-dbus-glib \
	test-service-glib \
	$(THREAD_APPS) \
	peer-server \
	peer-client \
	test-types \
	test-30574 \
	test-proxy-peer \
	test-registrations \
	test-variant-recursion \
	test-gvariant

test_30574_SOURCES = \
	30574.c

test_proxy_peer_SOURCES = \
	my-object-marshal.c \
	my-object.c \
	my-object.h \
	proxy-peer.c

test_registrations_SOURCES = \
	my-object.c \
	my-object.h \
	my-object-subclass.c \
	my-object-subclass.h \
	my-object-marshal.c \
	registrations.c

test_dbus_glib_SOURCES=				\
	my-object.c \
	my-object.h \
	my-object-marshal.c \
	test-dbus-glib.c

test_dbus_glib_LDADD= $(tool_ldadd)

test_variant_recursion_SOURCES=test-variant-recursion.c

test_variant_recursion_LDADD= $(tool_ldadd)

BUILT_SOURCES = test-service-glib-glue.h test-service-glib-subclass-glue.h test-service-glib-bindings.h my-object-marshal.c my-object-marshal.h 

test_service_glib_SOURCES=				\
	my-object.c                             \
	my-object.h                             \
	my-object-subclass.c                    \
	my-object-subclass.h                    \
	my-object-marshal.c                             \
	test-service-glib.c 

test-service-glib-glue.h: test-service-glib.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DEBUG) $(DBUS_BINDING_TOOL) --prefix=my_object --mode=glib-server --output=test-service-glib-glue.h $(srcdir)/test-service-glib.xml

test-service-glib-subclass-glue.h: test-service-glib-subclass.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DEBUG) $(DBUS_BINDING_TOOL) --prefix=my_object_subclass --mode=glib-server --output=test-service-glib-subclass-glue.h $(srcdir)/test-service-glib-subclass.xml

test-service-glib-bindings.h: test-service-glib.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DEBUG) $(DBUS_BINDING_TOOL) --prefix=my_object --mode=glib-client --output=test-service-glib-bindings.h $(srcdir)/test-service-glib.xml

my-object-marshal.c: Makefile my-object-marshal.list
	echo "#include <config.h>" > $@.tmp
	@GLIB_GENMARSHAL@ --prefix=my_object_marshal $(srcdir)/my-object-marshal.list --header --body >> $@.tmp
	mv $@.tmp $@

my-object-marshal.h: Makefile my-object-marshal.list
	@GLIB_GENMARSHAL@ --prefix=my_object_marshal $(srcdir)/my-object-marshal.list --header > my-object-marshal.h


peer_server_SOURCES = \
	my-object.c                             \
	my-object.h                             \
	my-object-subclass.c                    \
	my-object-subclass.h                    \
	my-object-marshal.c                             \
	peer-server.c

peer_client_SOURCES = \
	peer-client.c

test_types_SOURCES = \
	test-types.c

test_gvariant_SOURCES = \
	test-gvariant.c

CLEANFILES = \
	$(BUILT_SOURCES) \
	run-with-tmp-session-bus.conf

else
### not building tests

if HAVE_GLIB_THREADS
noinst_PROGRAMS=test-profile
endif

endif

if HAVE_GLIB_THREADS
test_profile_SOURCES=				\
	test-profile.c
endif
