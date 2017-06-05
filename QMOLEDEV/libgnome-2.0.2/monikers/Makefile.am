INCLUDES = \
	-I$(top_srcdir) 				\
	$(LIBGNOME_CFLAGS)				\
	-DG_LOG_DOMAIN=\"libgnome\"			\
	-DVERSION=\"$(VERSION)\"

GNOME_Moniker_std.server.in: GNOME_Moniker_std.server.in.in
	sed -e "s|\@MONIKER_LIBDIR\@|$(monikerdir)|" $< > $@

CLEANFILES+=GNOME_Moniker_std.server GNOME_Moniker_std.server.in

serverdir = $(libdir)/bonobo/servers
server_DATA = GNOME_Moniker_std.server

@INTLTOOL_SERVER_RULE@

monikerdir = $(libdir)/bonobo/monikers
moniker_LTLIBRARIES = libmoniker_extra_2.la

libmoniker_extra_2_la_SOURCES =			\
	bonobo-config-bag.c			\
	bonobo-config-bag.h			\
	bonobo-moniker-config.c			\
	bonobo-moniker-conf-indirect.c		\
	bonobo-moniker-extra.h            	\
	bonobo-moniker-extra.c			

libmoniker_extra_2_la_LDFLAGS = -module -avoid-version $(LIBGNOME_LIBS)

EXTRA_DIST = GNOME_Moniker_std.server.in.in
