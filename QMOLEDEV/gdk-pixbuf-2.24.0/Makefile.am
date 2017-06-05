include $(top_srcdir)/Makefile.decl

SUBDIRS = gdk-pixbuf po docs tests contrib build

EXTRA_DIST += config.h.win32

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = gdk-pixbuf-2.0.pc

ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}

DISTCLEANFILES = 		\
	gdk-pixbuf-2.0.pc

DISTCHECK_CONFIGURE_FLAGS = 	\
	--enable-gtk-doc	\
	--enable-man		\
	--enable-introspection

MAINTAINERCLEANFILES = \
	ABOUT-NLS \
	aclocal.m4 \
	config.guess \
	config.h.in \
	config.rpath \
	config.sub \
	depcomp \
	gtk-doc.make \
	install-sh \
	ltmain.sh \
	missing


-include $(top_srcdir)/git.mk
