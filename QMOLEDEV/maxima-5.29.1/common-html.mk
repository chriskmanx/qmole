# Installation/uninstallation and distribution for .html files.
# htmlname -- html base name (e.g. maxima or xmaxima)
# htmlinstdir -- html installation directory  

install-data-local: install-maxima-html
install-maxima-html: $(htmlname).html
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(htmlinstdir)
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/$(htmlname).html $(srcdir)/$(htmlname)_*.html" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  if test -f $(srcdir)/$$f; then \
            if test ! -d `dirname $(DESTDIR)$(htmlinstdir)/$$f`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(htmlinstdir)/$$f`; \
            fi; \
	    echo " $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(htmlinstdir)/$$f"; \
	    $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(htmlinstdir)/$$f; \
	  else if test -f $$f; then \
            if test ! -d `dirname $(DESTDIR)$(htmlinstdir)/$$f`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(htmlinstdir)/$$f`; \
            fi; \
	    echo " $(INSTALL_DATA) $$f $(DESTDIR)$(htmlinstdir)/$$f"; \
	    $(INSTALL_DATA) $$f $(DESTDIR)$(htmlinstdir)/$$f; \
	  fi; fi; \
	done

uninstall-local: uninstall-maxima-html
uninstall-maxima-html:
	@$(NORMAL_UNINSTALL)
	rm -f $(DESTDIR)$(htmlinstdir)/$(htmlname).html 
	rm -f $(DESTDIR)$(htmlinstdir)/$(htmlname)_*.html

dist-hook: dist-maxima-html
dist-maxima-html: $(htmlname).html
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/$(htmlname).html $(srcdir)/$(htmlname)_*.html" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  test -f $(distdir)/$$f || cp -p $(srcdir)/$$f $(distdir)/$$f; \
	done


