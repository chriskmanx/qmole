## Process this file with automake to produce Makefile.in

# For non-gcc compilers, change the options to "no-dependencies"
AUTOMAKE_OPTIONS = foreign dist-zip

SUBDIRS    = utils include src chart doc tests shutterbug adie pathfinder calculator windows
EXTRA_DIST = ADDITIONS AUTHORS INSTALL LICENSE LICENSE_ADDENDUM TRACING README fox.lsm index.html aclocal.m4

bin_SCRIPTS = fox-config

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = fox.pc

CLEANFILES = fox.tar.gz

snapshot: dist-all
	cp $(distdir).tar.gz /net/linkyftp/pub/fox-snapshot.tar.gz
	cp $(distdir).zip /net/linkyftp/pub/fox-snapshot.zip



web: distdir
	cd $(distdir)/doc && doxygen doxygen.cfg
	cp -r $(distdir)/doc/* /net/linky/mnt/html

drop: dist-all
	cp $(distdir).tar.gz /net/webby/ftp/pub
	cp $(distdir).zip /net/webby/ftp/pub


