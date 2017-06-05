AUTOMAKE_OPTIONS = 1.7

DISTCHECK_CONFIGURE_FLAGS = --enable-gtk-doc

SUBDIRS = libgnomecanvas glade demos po docs

if USE_GLADE
SUBDIRS += tests
endif

EXTRA_DIST = \
	COPYING.LIB		\
	NEWS			\
	intltool-extract.in	\
	intltool-merge.in	\
	intltool-update.in	\
	libgnomecanvas-zip.in

DISTCLEANFILES = \
	intltool-extract \
	intltool-merge \
	intltool-update
