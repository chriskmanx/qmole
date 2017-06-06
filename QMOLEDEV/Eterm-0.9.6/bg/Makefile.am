# $Id: Makefile.am 13745 2005-03-15 21:48:14Z mej $

DIRS = tile scale
UPDATE_SCRIPT = @top_srcdir@/utils/Etbg_update_list

EXTRA_DIST =  tile/circuit.jpg scale/Neopolis-horizon.jpg README.backgrounds
pixmapdir = $(pkgdatadir)/pix
themedir = $(pkgdatadir)/themes

install-data-hook:
	@(for j in $(DIRS) ; do echo "Installing pixmaps in $$j" ; \
	  $(mkinstalldirs) $(DESTDIR)$(pixmapdir)/$$j ; (cd $(srcdir)/$$j ; \
	  for i in `ls -1 | grep -v CVS | grep -v \.cvsignore` ; do \
	    echo $(INSTALL_DATA) $$i $(DESTDIR)${pixmapdir}/$$j/ ; \
	    $(INSTALL_DATA) $$i $(DESTDIR)${pixmapdir}/$$j/ ; \
	  done) ; \
	done)
	$(UPDATE_SCRIPT) $(DESTDIR)${pixmapdir}
MAINTAINERCLEANFILES=Makefile.in
