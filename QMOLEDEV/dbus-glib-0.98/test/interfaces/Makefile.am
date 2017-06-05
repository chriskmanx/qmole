INCLUDES = \
	-I$(top_srcdir)				\
	-I$(top_builddir)			\
	-I$(top_builddir)/dbus			\
	$(DBUS_CFLAGS)				\
	$(DBUS_GLIB_CFLAGS)			\
	-DDBUS_COMPILATION

LDADD = $(DBUS_GLIB_LIBS) \
	$(top_builddir)/dbus/libdbus-glib-1.la \
	$(top_builddir)/dbus/libdbus-gtool.la

## note that TESTS has special meaning (stuff to use in make check)
## so if adding tests not to be run in make check, don't add them to 
## TESTS
if DBUS_BUILD_TESTS
TESTS_ENVIRONMENT = \
	DBUS_TOP_BUILDDIR=$(ABSOLUTE_TOP_BUILDDIR) \
	DBUS_BINDING_TOOL=$(DBUS_BINDING_TOOL) \
	$(NULL)
TESTS=run-test.sh
else
TESTS=
endif

EXTRA_DIST = \
	invalid-annotated-node.xml \
	invalid-nested-annotation.xml \
	run-test.sh \
	test-goodbye.xml \
	test-hello.xml \
	test-song.xml \
	test-dup-prop-a.xml \
	test-dup-prop-b.xml \
	valid-annotations.xml \
	$(NULL)

if DBUS_BUILD_TESTS

## we use noinst_PROGRAMS not check_PROGRAMS for TESTS so that we
## build even when not doing "make check"
noinst_PROGRAMS = test-service test-client

test_service_SOURCES = \
	test-interfaces.c \
	test-interfaces.h \
	test-dup-prop.c \
	test-dup-prop.h \
	test-objects.c \
	test-objects.h \
	test-server.c

test_client_SOURCES = \
	test-client.c

BUILT_SOURCES = \
	test-song-glue.h		\
	test-hello-glue.h		\
	test-goodbye-glue.h	\
	test-dup-prop-a-glue.h	\
	test-dup-prop-b-glue.h	\
	test-song-bindings.h	\
	test-hello-bindings.h	\
	test-goodbye-bindings.h	\
	test-dup-prop-a-bindings.h \
	test-dup-prop-a-bindings.h \
	test-dup-prop-b-bindings.h \
	test-dup-prop-b-bindings.h \
	valid-annotations-glue.h \
	valid-annotations-bindings.h \
	$(NULL)

test-song-glue.h: test-song.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_song --mode=glib-server --output=test-song-glue.h $(srcdir)/test-song.xml

test-song-bindings.h: test-song.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_song --mode=glib-client --output=test-song-bindings.h $(srcdir)/test-song.xml

test-hello-glue.h: test-hello.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_hello --mode=glib-server --output=test-hello-glue.h $(srcdir)/test-hello.xml

test-hello-bindings.h: test-hello.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_hello --mode=glib-client --output=test-hello-bindings.h $(srcdir)/test-hello.xml

test-goodbye-glue.h: test-goodbye.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_goodbye --mode=glib-server --output=test-goodbye-glue.h $(srcdir)/test-goodbye.xml

test-goodbye-bindings.h: test-goodbye.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_goodbye --mode=glib-client --output=test-goodbye-bindings.h $(srcdir)/test-goodbye.xml

test-dup-prop-a-glue.h: test-dup-prop-a.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_dup_prop_a --mode=glib-server --output=test-dup-prop-a-glue.h $(srcdir)/test-dup-prop-a.xml

test-dup-prop-a-bindings.h: test-dup-prop-a.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_dup_prop_a --mode=glib-client --output=test-dup-prop-a-bindings.h $(srcdir)/test-dup-prop-a.xml

test-dup-prop-b-glue.h: test-dup-prop-b.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_dup_prop_b --mode=glib-server --output=test-dup-prop-b-glue.h $(srcdir)/test-dup-prop-b.xml

test-dup-prop-b-bindings.h: test-dup-prop-b.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_dup_prop_b --mode=glib-client --output=test-dup-prop-b-bindings.h $(srcdir)/test-dup-prop-b.xml

valid-annotations-glue.h: valid-annotations.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_annotated --mode=glib-server --output=$@ $<

valid-annotations-bindings.h: valid-annotations.xml $(top_builddir)/dbus/dbus-binding-tool$(EXEEXT)
	$(DBUS_BINDING_TOOL) --prefix=test_annotated --mode=glib-client --output=$@ $<

CLEANFILES = \
	$(BUILT_SOURCES) \
	run-with-tmp-session-bus.conf

else
### not building tests


endif

