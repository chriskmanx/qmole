INCLUDES = \
	-I$(top_srcdir)				\
	-I$(top_builddir)			\
	-I$(top_builddir)/dbus			\
	$(DBUS_CFLAGS)				\
	$(DBUS_GLIB_CFLAGS)			\
	$(DBUS_GTK_THREADS_CFLAGS)		\
	-DDBUS_COMPILATION

LDADD = $(top_builddir)/dbus/libdbus-glib-1.la \
	$(DBUS_GLIB_LIBS)

## Makefile.am bits for sample client/server pair

noinst_PROGRAMS= statemachine-server

#if HAVE_GTK
#noinst_PROGRAMS += statemachine-client
#endif

EXTRA_DIST = statemachine.h statemachine-server.h sm-marshal.list statemachine-server.xml statemachine.xml

statemachine_server_SOURCES= statemachine-server.c sm-marshal.c statemachine.c

#statemachine_client_SOURCES= statemachine-client.c sm-marshal.c statemachine.h
#statemachine_client_LDADD= $(LDADD) $(DBUS_GTK_THREADS_LIBS)

BUILT_SOURCES = statemachine-server-glue.h statemachine-glue.h

statemachine-server-glue.h: statemachine-server.xml
	$(LIBTOOL) --mode=execute $(DBUS_BINDING_TOOL) --prefix=sm_server --mode=glib-server --output=$@ $<

statemachine-glue.h: statemachine.xml
	$(LIBTOOL) --mode=execute $(DBUS_BINDING_TOOL) --prefix=sm_object --mode=glib-server --output=$@ $<

sm-marshal.c: Makefile sm-marshal.list
	echo "#include <config.h>" > $@.tmp
	@GLIB_GENMARSHAL@ --prefix=sm_marshal $(srcdir)/sm-marshal.list --header --body >> $@.tmp
	mv $@.tmp $@

sm-marshal.h: Makefile sm-marshal.list
	@GLIB_GENMARSHAL@ --prefix=sm_marshal $(srcdir)/sm-marshal.list --header > $@.tmp && mv $@.tmp $@

BUILT_SOURCES += sm-marshal.c sm-marshal.h

CLEANFILES = $(BUILT_SOURCES)
