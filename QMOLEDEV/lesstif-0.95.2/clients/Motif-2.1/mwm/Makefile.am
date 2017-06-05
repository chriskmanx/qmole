#
# $Header: /cvsroot/lesstif/lesstif/clients/Motif-2.1/mwm/Makefile.am,v 1.20 2005/03/26 06:52:24 dannybackx Exp $
#

bin_PROGRAMS=	mwm

MAINTAINERCLEANFILES=Makefile.in mwmlex.c mwmparse.c mwmparse.h

mwm_SOURCES = \
  	colormaps.c cursors.c decorate.c defaults.c desktop.c \
	events.c functions.c icons.c menus.c misc.c move.c \
	mwm.c pager.c pan.c props.c resize.c resources.c screens.c \
	windows.c gethostname.c \
        mwmparse.y mwmlex.l \
	mwm.h mwmstrings.h

EXTRA_DIST=	Mwm.ad system.mwmrc alt.map README gethostname.c COPYING

# non-local stuff
#BUILT_SOURCES = mwmlex.c mwmparse.c mwmparse.h $(mwmd_DATA) Mwm.ad


# Special dependencies
mwmlex.c:	mwmparse.h
mwmparse.h:	mwmparse.c


AM_CPPFLAGS=	\
                -I$(top_srcdir)/include/Motif-2.1 \
		-I../../../include/Motif-2.1 \
		$(X_CFLAGS) \
		-DMWM_DDIR=\"$(mwmddir)\" \
		-DXINCL_PATH=\"@x_includes@\"

LDADD=		../../../lib/Xm-2.1/libXm.la \
		$(X_LIBS) $(XPLIB) -lXt $(X_PRE_LIBS) -lXext -lX11 $(X_EXTRA_LIBS)

appdir=		$(libdir)/X11/app-defaults

mwmddir=	$(libdir)/X11/mwm
mwmd_DATA=	system.mwmrc alt.map README


AM_YFLAGS=		-d

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(appdir)
	$(INSTALL_DATA) $(srcdir)/Mwm.ad $(DESTDIR)$(appdir)/Mwm
