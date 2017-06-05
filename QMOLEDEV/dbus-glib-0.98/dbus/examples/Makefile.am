SUBDIRS = . statemachine

INCLUDES = \
	-I$(top_srcdir)				\
	-I$(top_builddir)			\
	-I$(top_builddir)/dbus			\
	$(DBUS_CFLAGS)				\
	$(DBUS_GLIB_CFLAGS)			\
	-DDBUS_COMPILATION

LDADD = \
	$(DBUS_GLIB_LIBS) \
	$(top_builddir)/dbus/libdbus-glib-1.la

## Makefile.am bits for sample client/server pair

noinst_PROGRAMS= example-client example-service 

example_client_SOURCES= example-client.c

example_service_SOURCES= example-service.c

BUILT_SOURCES = example-service-glue.h

example-service-glue.h: example-service.xml
	$(LIBTOOL) --mode=execute $(DBUS_BINDING_TOOL) --prefix=some_object --mode=glib-server --output=example-service-glue.h $(srcdir)/example-service.xml


## Makefile.am bits for another client/server pair

noinst_PROGRAMS += example-signal-recipient example-signal-emitter

example_signal_recipient_SOURCES= example-signal-recipient.c

example_signal_emitter_SOURCES= example-signal-emitter.c

BUILT_SOURCES += example-signal-emitter-glue.h

example-signal-emitter-glue.h: example-signal-emitter.xml
	$(LIBTOOL) --mode=execute $(DBUS_BINDING_TOOL) --prefix=test_object --mode=glib-server --output=example-signal-emitter-glue.h $(srcdir)/example-signal-emitter.xml

CLEANFILES = $(BUILT_SOURCES)

EXTRA_DIST = example-service.xml example-signal-emitter.xml 
