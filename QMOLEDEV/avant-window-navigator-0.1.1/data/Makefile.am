SUBDIRS = active

desktopdir   = $(datadir)/applications
desktop_DATA = avant-window-navigator.desktop
@INTLTOOL_DESKTOP_RULE@


$(desktop_DATA).in: $(desktop_DATA).in.in
	sed -e "s|\@PKGDATADIR\@|$(pkgdatadir)|g" $< > $@
	
icon_DATA =  \
	avant-window-navigator.svg	\
	avant-window-navigator-24.png	\
	avant-window-navigator-48.png

icondir = $(pkgdatadir)

schemasdir   = /etc/gconf/schemas
schemas_DATA = avant-window-navigator.schemas
@INTLTOOL_SCHEMAS_RULE@


$(schemas_DATA).in: $(schemas_DATA).in.in
	sed -e "s|\@PKGDATADIR\@|$(pkgdatadir)|g" $< > $@

install-data-local:
	if [ -z "$(DESTDIR)" ]; then \
		GCONF_CONFIG_SOURCE="" $(GCONFTOOL) --makefile-install-rule $(builddir) $(schemas_DATA); \
	fi

EXTRA_DIST = $(schemas_DATA).in.in $(icon_DATA) $(desktop_DATA).in.in 

CLEANFILES = $(schemas_DATA) $(schemas_DATA).in $(desktop_DATA) $(desktop_DATA).in
