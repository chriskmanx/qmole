# Makefile.  Generated from Makefile.in by configure.

# Package-related substitution variables
package        = x2
version        = 1.1.0
tarname        = x2
distdir        = $(tarname)-$(version)

# Prefix-related substitution variables
prefix         = /usr/local
exec_prefix    = ${prefix}
bindir         = ${exec_prefix}/bin

# Tool-related substitution variables
CC             = /usr/bin/gcc
DEFS           = -DHAVE_CONFIG_H
LIBS           = -lvte -lpng14 -lm -lfreetype -lfontconfig -lcairo -lX11 
CFLAGS         = -g -O2
CPPFLAGS       = 
INSTALL        = /usr/bin/install -c
#INSTALL_DATA   = ${INSTALL} -m 644
INSTALL_PROGRAM= ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}

# VPATH-related substitution variables
top_builddir   = .
builddir       = .
srcdir         = .


SUBDIRS = src data

all clean check install uninstall x2:
	$(MAKE) -C src $@

dist: $(distdir).tar.gz

$(distdir).tar.gz: FORCE $(distdir)
	tar chof - $(distdir) | gzip -9 -c >$(distdir).tar.gz
	rm -rf $(distdir)

$(distdir):
	mkdir -p $(distdir)/src
	mkdir -p $(distdir)/data
	cp $(srcdir)/data/x2.desktop $(distdir)/data
	cp $(srcdir)/data/x2.1 $(distdir)/data
	cp $(srcdir)/data/x2.png $(distdir)/data
	cp $(srcdir)/configure $(distdir)
	cp $(srcdir)/config.h.in $(distdir)
	cp $(srcdir)/install-sh $(distdir)
	cp $(srcdir)/Makefile.in $(distdir)
	cp $(srcdir)/src/Makefile.in $(distdir)/src
	cp $(srcdir)/src/main.c $(distdir)/src
	cp $(srcdir)/src/callbacks.c $(distdir)/src
	cp $(srcdir)/src/data.h $(distdir)/src
	cp $(srcdir)/src/func_proto.h $(distdir)/src
	cp $(srcdir)/src/prefs.c $(distdir)/src
	cp $(srcdir)/src/templates.c $(distdir)/src
	cp $(srcdir)/ChangeLog $(distdir)/
	cp $(srcdir)/COPYING $(distdir)/
	cp $(srcdir)/INSTALL $(distdir)/
	cp $(srcdir)/NEWS $(distdir)/
	cp $(srcdir)/README $(distdir)/

distcheck: $(distdir).tar.gz
	gzip -cd $+ | tar xvf -
	cd $(distdir); ./configure
	$(MAKE) -C $(distdir) all check
	$(MAKE) -C $(distdir) DESTDIR=$${PWD}/$(distdir)/_inst install uninstall
	$(MAKE) -C $(distdir) clean
	rm -rf $(distdir)
	@echo ""
	@echo "$(distdir).tar.gz is ready for distribution."

Makefile: Makefile.in config.status
	./config.status $@

config.status: configure
	./config.status --recheck

FORCE:
	-rm -rf $(distdir) &>/dev/null
	-rm $(distdir).tar.gz &>/dev/null

.PHONY: FORCE all clean check dist distcheck install uninstall
