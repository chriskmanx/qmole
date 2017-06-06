# $Id: Makefile.am 13745 2005-03-15 21:48:14Z mej $

EXTRA_DIST =  bar_horizontal_1.png bar_horizontal_2.png bar_horizontal_3.png bar_vertical_1.png bar_vertical_2.png \
              bar_vertical_3.png button_arrow_down_1.png button_arrow_down_2.png button_arrow_down_3.png \
              button_arrow_up_1.png button_arrow_up_2.png button_arrow_up_3.png menu1.png menu2.png menu3.png \
              thumb_1.png thumb_2.png help.png exit.png

pixmapdir = $(pkgdatadir)/pix

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(pixmapdir)
	@for i in $(EXTRA_DIST) ; do \
	  echo $(INSTALL_DATA) $(srcdir)/$$i $(DESTDIR)${pixmapdir} ; \
	  $(INSTALL_DATA) $(srcdir)/$$i $(DESTDIR)${pixmapdir} ; \
	done

uninstall-hook:
	rm -rf $(DESTDIR)$(pkgdatadir)/pix
MAINTAINERCLEANFILES=Makefile.in
