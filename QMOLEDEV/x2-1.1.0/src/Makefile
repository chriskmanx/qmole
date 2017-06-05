# src/Makefile.  Generated from Makefile.in by configure.

# Package-related substitution variables
package        = x2
version        = 1.1.0
tarname        = x2
distdir        = $(tarname)-$(version)

# Prefix-related substitution variables
prefix         = /usr/local
exec_prefix    = ${prefix}
bindir         = ${exec_prefix}/bin
datadir	       = ${datarootdir}
datarootdir    = ${prefix}/share

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
top_builddir   = ..
builddir       = .
srcdir         = .


all: x2

x2: main.c
	gcc -Wall -g -O0 -o $@ $+ `pkg-config --libs --cflags vte gtksourceview-2.0`
clean:
	-rm x2 &>/dev/null

install:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL) -d $(DESTDIR)$(datadir)
	$(INSTALL) -d $(DESTDIR)$(datadir)/applications
	$(INSTALL) -d $(DESTDIR)$(datadir)/man/man1
	$(INSTALL) -d $(DESTDIR)$(datadir)/pixmaps/x2/
	$(INSTALL_PROGRAM) -m 0755 x2 $(DESTDIR)$(bindir)/x2
	$(INSTALL_PROGRAM) -m 0755 ../data/x2.desktop $(DESTDIR)$(datadir)/applications/x2.desktop
	$(INSTALL_PROGRAM) -m 0755 ../data/x2.1 $(DESTDIR)$(datadir)/man/man1/x2.1
	$(INSTALL_PROGRAM) -m 0755 ../data/x2.png $(DESTDIR)$(datadir)/pixmaps/x2/x2.png

uninstall:
	-rm $(DESTDIR)$(bindir)/x2
	-rm $(DESTDIR)$(datadir)/{applications/x2.desktop,man/man1/x2.1,pixmaps/x2/x2.png}

Makefile: Makefile.in ../config.status
	cd .. && ./config.status src/$@

../config.status: ../configure
	cd .. && ./config.status --recheck

.PHONY: all clean check install uninstall
