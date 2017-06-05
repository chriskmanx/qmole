ACLOCAL_AMFLAGS = -I m4

SUBDIRS = include src tests

EXTRA_DIST =			\
	doxygen.cfg			\
	autogen.sh

include_HEADERS = include/basedir.h include/basedir_fs.h

include $(top_srcdir)/aminclude.am

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=pkgconfig/libxdg-basedir.pc
