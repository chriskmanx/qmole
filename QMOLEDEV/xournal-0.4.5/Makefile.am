## Process this file with automake to produce Makefile.in

SUBDIRS = src po

EXTRA_DIST = \
	autogen.sh \
	xournal.glade xournal.gladep \
	xournal.xml x-xoj.desktop xournal.desktop

install-data-local:
	@$(NORMAL_INSTALL)
	if test -d $(srcdir)/pixmaps; then \
	  $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/pixmaps; \
	  for pixmap in $(srcdir)/pixmaps/*; do \
	    if test -f $$pixmap; then \
	      $(INSTALL_DATA) $$pixmap $(DESTDIR)$(pkgdatadir)/pixmaps; \
	    fi \
	  done \
	fi; \
	if test -d $(srcdir)/html-doc; then \
	  $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/html-doc; \
	  for docfile in $(srcdir)/html-doc/*; do \
	    if test -f $$docfile; then \
	      $(INSTALL_DATA) $$docfile $(DESTDIR)$(pkgdatadir)/html-doc; \
	    fi \
	  done; \
	  if test ! -e $(DESTDIR)$(pkgdatadir)/html-doc/pixmaps; then \
	    ln -s ../pixmaps $(DESTDIR)$(pkgdatadir)/html-doc/pixmaps; \
	  fi \
	fi; \
	echo "*** Desktop files, icons, MIME types not installed. Run 'make desktop-install'"; \
	echo "*** (or 'make home-desktop-install' for installation in a home directory)."

desktop-install:
	if test "$(datadir)" = "/usr/share"; then \
	  desktopdir=/usr/share; \
	else \
	  desktopdir=/usr/local/share; \
	fi; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/icons/hicolor/scalable/apps; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/mime/packages; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/applications; \
	$(mkinstalldirs) $(DESTDIR)/usr/share/mimelnk/application; \
	$(INSTALL_DATA) $(srcdir)/pixmaps/xournal.svg $(DESTDIR)$$desktopdir/icons/hicolor/scalable/apps; \
	$(INSTALL_DATA) $(srcdir)/pixmaps/xoj.svg $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes; \
	if test ! -e $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes/gnome-mime-application-x-xoj.svg; then \
	  ln -s xoj.svg $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes/gnome-mime-application-x-xoj.svg; \
	fi; \
	$(INSTALL_DATA) $(srcdir)/xournal.xml $(DESTDIR)$$desktopdir/mime/packages; \
	$(INSTALL_DATA) $(srcdir)/xournal.desktop $(DESTDIR)$$desktopdir/applications; \
	$(INSTALL_DATA) $(srcdir)/x-xoj.desktop $(DESTDIR)/usr/share/mimelnk/application; \
	if test -z "$(DESTDIR)"; then \
	  echo "Updating desktop, mime, and icon databases."; \
	  update-desktop-database; \
	  update-mime-database $$desktopdir/mime; \
	  gtk-update-icon-cache -f -t $$desktopdir/icons/hicolor; \
	else \
	  echo "*** Not updating desktop, mime, and icon databases. After install, run:"; \
	  echo "***   update-desktop-database"; \
	  echo "***   update-mime-database $$desktopdir/mime"; \
	  echo "***   gtk-update-icon-cache -f -t $$desktopdir/icons/hicolor"; \
	fi

home-desktop-install:
	desktopdir=$(HOME)/.local/share; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/icons/hicolor/scalable/apps; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/mime/packages; \
	$(mkinstalldirs) $(DESTDIR)$$desktopdir/applications; \
	$(mkinstalldirs) $(DESTDIR)$(HOME)/.kde/share/mimelnk/application; \
	$(INSTALL_DATA) $(srcdir)/pixmaps/xournal.svg $(DESTDIR)$$desktopdir/icons/hicolor/scalable/apps; \
	$(INSTALL_DATA) $(srcdir)/pixmaps/xoj.svg $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes; \
	if test ! -e $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes/gnome-mime-application-x-xoj.svg; then \
	  ln -s xoj.svg $(DESTDIR)$$desktopdir/icons/hicolor/scalable/mimetypes/gnome-mime-application-x-xoj.svg; \
	fi; \
	$(INSTALL_DATA) $(srcdir)/xournal.xml $(DESTDIR)$$desktopdir/mime/packages; \
	$(INSTALL_DATA) $(srcdir)/xournal.desktop $(DESTDIR)$$desktopdir/applications; \
	$(INSTALL_DATA) $(srcdir)/x-xoj.desktop $(DESTDIR)$(HOME)/.kde/share/mimelnk/application; \
	if test -z "$(DESTDIR)"; then \
	  echo "Updating desktop, mime, and icon databases."; \
	  XDG_DATA_DIRS=$(HOME)/.local/share update-desktop-database; \
	  update-mime-database $$desktopdir/mime; \
	  gtk-update-icon-cache -f -t $$desktopdir/icons/hicolor; \
	else \
	  echo "*** Not updating desktop, mime, and icon databases. After install, run:"; \
	  echo "***   update-desktop-database"; \
	  echo "***   update-mime-database $$desktopdir/mime"; \
	  echo "***   gtk-update-icon-cache -f -t $$desktopdir/icons/hicolor"; \
	fi

dist-hook:
	if test -d pixmaps; then \
	  mkdir $(distdir)/pixmaps; \
	  for pixmap in pixmaps/*; do \
	    if test -f $$pixmap; then \
	      cp -p $$pixmap $(distdir)/pixmaps; \
	    fi \
	  done \
	fi; \
	if test -d html-doc; then \
	  mkdir $(distdir)/html-doc; \
	  for docfile in html-doc/*; do \
	    if test -f $$docfile; then \
	      cp -p $$docfile $(distdir)/html-doc; \
	    fi \
	  done; \
	  if test ! -e $(distdir)/html-doc/pixmaps; then \
	    ln -s ../pixmaps $(distdir)/html-doc/pixmaps; \
	  fi \
	fi

