# $Id: Makefile.am 13745 2005-03-15 21:48:14Z mej $

THEMES = Eterm auto Escreen trans

EXTRA_DIST = Eterm/theme.cfg.in Escreen/theme.cfg.in trans/theme.cfg.in auto/theme.cfg.in \
             Eterm/menus.cfg Escreen/menus.cfg trans/menus.cfg auto/menus.cfg

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/themes
	-@for i in $(THEMES) ; do \
	  if test -d $(DESTDIR)$(pkgdatadir)/themes/$$i -a "@REMOVE_THEMES@" = "yes"; then \
	    echo "Removing $$i theme in $(DESTDIR)$(pkgdatadir)/themes as requested." ; \
	    $(RM) -rf $(DESTDIR)$(pkgdatadir)/themes/$$i ; \
	    if test -d $(DESTDIR)$(pkgdatadir)/themes/$$i ; then \
	      echo "ERROR:  Unable to remove theme." ; \
	    fi ; \
	  fi ; \
	  if test ! -d $(DESTDIR)$(pkgdatadir)/themes/$$i ; then \
	    echo "Installing $$i theme in $(DESTDIR)$(pkgdatadir)/themes" ; \
	    $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/themes/$$i ; \
	    $(CP) $(srcdir)/$$i/???* $(DESTDIR)$(pkgdatadir)/themes/$$i ; \
	    $(CHMOD) 755 $(DESTDIR)$(pkgdatadir)/themes/$$i ; \
	    $(CHMOD) 644 $(DESTDIR)$(pkgdatadir)/themes/$$i/???* ; \
	    rm -f $(DESTDIR)$(pkgdatadir)/themes/$$i/theme.cfg.in ; \
	    rm -f $(DESTDIR)$(pkgdatadir)/themes/$$i/theme.cfg.in.orig ; \
	  else \
	    echo "ALERT!  Not overwriting $$i theme in $(DESTDIR)$(pkgdatadir)/themes.  You will need to update this theme manually." ; \
	  fi ; \
	done

uninstall-hook:
	rm -rf $(DESTDIR)$(pkgdatadir)/themes
MAINTAINERCLEANFILES=Makefile.in
