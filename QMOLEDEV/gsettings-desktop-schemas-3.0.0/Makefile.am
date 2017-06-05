SUBDIRS = headers schemas po

ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}

pkgconfigdir = $(datadir)/pkgconfig
pkgconfig_DATA = gsettings-desktop-schemas.pc

EXTRA_DIST =			\
	HACKING			\
	MAINTAINERS

MAINTAINERCLEANFILES = \
	$(srcdir)/m4/* \
	$(srcdir)/INSTALL \
	$(srcdir)/aclocal.m4 \
	$(srcdir)/config.guess \
	$(srcdir)/config.sub \
	$(srcdir)/install-sh \
	$(srcdir)/missing \
	$(srcdir)/mkinstalldirs \
	`find "$(srcdir)" -type f -name Makefile.in -print` \
	$(srcdir)/configure

-include $(top_srcdir)/git.mk
