ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgsf-1.pc

if WITH_WIN32
  pkgconfig_DATA += libgsf-win32-1.pc
endif

SUBDIRS = po gsf gsf-win32 doc tools tests m4 thumbnailer

if WITH_PYTHON
  SUBDIRS += python
endif

EXTRA_DIST = BUGS HACKING	\
	intltool-extract.in	\
	intltool-merge.in	\
	intltool-update.in	\
	libgsf-1.spec		\
	dumpdef.pl

DISTCLEANFILES = intltool-extract intltool-merge intltool-update

DISTCHECK_CONFIGURE_FLAGS = --disable-scrollkeeper --enable-gtk-doc --enable-schemas-install
