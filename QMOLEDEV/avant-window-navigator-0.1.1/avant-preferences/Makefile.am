glade_DATA = window.glade
gladedir = $(pkgdatadir)

desktopdir   = $(datadir)/applications
desktop_DATA = avant-preferences.desktop
@INTLTOOL_DESKTOP_RULE@


$(desktop_DATA).in: $(desktop_DATA).in.in
	sed -e "s|\@PKGDATADIR\@|$(pkgdatadir)|g" $< > $@

install-exec-local:
	sed -e "s|\@PKGDATADIR\@|$(pkgdatadir)|g" avant-preferences.in.py > avant-preferences.py
	$(INSTALL) -m 755 avant-preferences.py $(DESTDIR)$(bindir)/avant-preferences

EXTRA_DIST = $(glade_DATA) avant-preferences.in.py $(desktop_DATA).in.in

clean:
	rm -f *.pyo *.pyc *.bak *.gladep $(desktop_DATA) $(desktop_DATA).in
