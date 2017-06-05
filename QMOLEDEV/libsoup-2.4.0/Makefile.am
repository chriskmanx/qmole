## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = 1.6

SUBDIRS = libsoup tests docs

EXTRA_DIST = libsoup.pc.in gtk-doc.make libsoup-zip.in

DISTCHECK_CONFIGURE_FLAGS = --enable-gtk-doc

pkgconfigdir = $(libdir)/pkgconfig

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL) -m 644 libsoup.pc $(DESTDIR)$(pkgconfigdir)/libsoup-$(SOUP_API_VERSION).pc

uninstall-local:
	rm -f $(DESTDIR)$(pkgconfigdir)/libsoup-$(SOUP_API_VERSION).pc
