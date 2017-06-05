DISTCHECK_CONFIGURE_FLAGS = --enable-gtk-doc

SUBDIRS=linc2 src include test docs

bin_SCRIPTS=orbit2-config

EXTRA_DIST =		\
	ORBit2.m4	\
	ORBit.spec	\
	orbit2-zip.in	\
	HACKING		\
	MAINTAINERS	\
	README.win32	\
	Makefile.shared \
	gtk-doc.make	\
	ORBit2.doap

rpms: distcheck
	rpm -ta $(top_builddir)/@PACKAGE@-@VERSION@.tar.gz

m4datadir=$(datadir)/aclocal
m4data_DATA=ORBit2.m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = \
	ORBit-2.0.pc \
	ORBit-CosNaming-2.0.pc \
	ORBit-imodule-2.0.pc
