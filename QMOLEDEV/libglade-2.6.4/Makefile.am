SUBDIRS = glade doc tests

ACLOCAL_AMFLAGS = -I m4

INCLUDES = $(LIBGLADE_CFLAGS)

# build documentation when doing a distcheck.
DISTCHECK_CONFIGURE_FLAGS = --enable-gtk-doc

if HAVE_PYTHON
bin_SCRIPTS = libglade-convert
endif

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libglade-2.0.pc

noinst_PROGRAMS = test-libglade

test_libglade_LDADD = glade/libglade-2.0.la $(LIBGLADE_LIBS)
test_libglade_SOURCES = test-libglade.c

# handling of the XML catalog

xmldir = $(datadir)/xml/libglade
xml_DATA = glade-2.0.dtd

if HAVE_XMLCATALOG
install-data-local:
	-$(XMLCATALOG) --noout --add "system" "http://glade.gnome.org/glade-2.0.dtd" $(xmldir)/glade-2.0.dtd $(XML_CATALOG_FILE)
uninstall-local:
	-$(XMLCATALOG) --noout --del $(xmldir)/glade-2.0.dtd $(XML_CATALOG_FILE)
endif

#small hack to get distcheck to work
clean-local:
	rm -f intl/po2tbl.sed .memdump

EXTRA_DIST = \
  gtk-doc.make \
  glade-2.0.dtd \
  libglade.spec.in \
  libglade.spec \
  libglade-zip.in \
  libglade-2.0.pc.in \
  libglade-2.0-uninstalled.pc.in \
  libglade-convert.in \
  examples/simple.glade \
  examples/first.glade \
  examples/example.glade \
  examples/bonobo.glade
