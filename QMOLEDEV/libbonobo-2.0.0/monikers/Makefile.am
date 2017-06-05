INCLUDES = \
	-I$(top_srcdir) 				\
	$(WARN_CFLAGS)					\
	$(LIBBONOBO_CFLAGS)				\
	-DPLUGIN_DIR=\""$(PLUGIN_DIR)"\"		\
        -DBONOBO_LIBDIR=\""$(libdir)"\" 		\
        -DBONOBO_DATADIR=\""$(datadir)"\" 		\
        -DBONOBO_PIXMAPDIR=\""$(datadir)/pixmaps"\"	\
        -DBONOBO_BINDIR=\""$(bindir)"\" 		\
        -DBONOBO_LOCALSTATEDIR=\""$(localstatedir)"\" 	\
        -DBONOBO_LOCALEDIR=\""$(gnomelocaledir)"\" 	\
	-DG_LOG_DOMAIN=\"Bonobo\"			\
	-DVERSION=\"$(VERSION)\"

Bonobo_Moniker_std.server.in: Bonobo_Moniker_std.server.in.in
	sed -e "s|\@MONIKER_LIBDIR\@|$(monikerdir)|" $< > $@

CLEANFILES = Bonobo_Moniker_std.server Bonobo_Moniker_std.server.in

serverdir = $(libdir)/bonobo/servers
server_DATA = Bonobo_Moniker_std.server

@INTLTOOL_SERVER_RULE@

monikerdir = $(libdir)/bonobo/monikers
moniker_LTLIBRARIES = libmoniker_std_2.la

libmoniker_std_2_la_SOURCES =			\
	bonobo-moniker-extender-stream.c  	\
	bonobo-moniker-item.c			\
	bonobo-moniker-new.c			\
	bonobo-moniker-ior.c			\
	bonobo-moniker-oaf.c			\
	bonobo-moniker-cache.c			\
	bonobo-stream-cache.h			\
	bonobo-stream-cache.c			\
	bonobo-moniker-query.c			\
	bonobo-moniker-query.h			\
	bonobo-moniker-std.h            	\
	bonobo-moniker-std.c			

libmoniker_std_2_la_LDFLAGS = -module -avoid-version 
libmoniker_std_2_la_LIBADD = $(top_builddir)/bonobo/libbonobo-2.la

EXTRA_DIST = Bonobo_Moniker_std.server.in.in
