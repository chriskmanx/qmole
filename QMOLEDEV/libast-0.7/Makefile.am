# $Id: Makefile.am,v 1.13 2005/05/12 12:58:22 azundris Exp $

AUTOMAKE_OPTIONS = foreign
SUBDIRS = include src test

bin_SCRIPTS = libast-config

MACROS = libast.m4
macrodir = $(datadir)/aclocal

EXTRA_DIST = DESIGN README $(MACROS) libast.spec
MAINTAINERCLEANFILES = Makefile.in *~ aclocal.m4 config.guess config.sub configure depcomp \
    install-sh ltconfig ltmain.sh missing mkinstalldirs stamp-h* *.log config.h.in

test: all
	cd test && $(MAKE) test

perf: all
	cd test && $(MAKE) perf

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(macrodir)
	for i in $(MACROS) ; do \
	    $(INSTALL_DATA) $(srcdir)/$$i $(DESTDIR)$(macrodir)/ ; \
	done

uninstall-hook:
	for i in $(MACROS) ; do \
	    rm -f $(DESTDIR)$(macrodir)/$$i ; \
	done

.PHONY: test
