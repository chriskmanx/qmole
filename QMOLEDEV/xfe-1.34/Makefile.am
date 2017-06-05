man_MANS=xfe.1 xfv.1 xfi.1 xfp.1 xfw.1
SUBDIRS = intl po m4 src icons
DIST_SUBDIRS = intl po m4 src icons

rcdir = $(prefix)/share/xfe
rc_DATA = xferc

EXTRA_DIST = autogen.sh iconslink.sh config.h i18n.h xfe.1 xfv.1 xfi.1 xfp.1 xfw.1 \
             ABOUT-NLS TODO BUGS xfe.spec.in xferc.in xfe.spec *.desktop.in.in *.png *.xpm *.svg \
	         $(top_srcdir)/icons/* $(top_srcdir)/debian

icondir = $(prefix)/share/pixmaps
icon_DATA = xfe.png xfv.png xfi.png xfp.png xfw.png xfe.xpm xfv.xpm xfi.xpm xfp.xpm xfw.xpm

desktopdir = $(prefix)/share/applications
desktop_in_files = xfe.desktop.in xfw.desktop.in xfi.desktop.in xfv.desktop.in xfp.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

dist-hook:
	cd po && $(MAKE) update-po
	cp -b po/*.po* $(distdir)/po
	rm -rf $(distdir)/po/*~
	rm -rf $(distdir)/debian/xfe

install-data-hook:
	sh iconslink.sh $(top_srcdir) $(DESTDIR)$(rcdir)
	
uninstall-hook:
	rm -rf $(DESTDIR)$(rcdir)

ACLOCAL_AMFLAGS = -I m4
