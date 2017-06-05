SUBDIRS = glade doc tests

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

XMLCATALOG = /etc/xml/catalog

xmldir = $(datadir)/xml/libglade
xml_DATA = glade-2.0.dtd

install-data-local:
	-xmlcatalog --noout --add "system" "http://glade.gnome.org/glade-2.0.dtd" $(xmldir)/glade-2.0.dtd $(XMLCATALOG)
uninstall-local:
	-xmlcatalog --noout --del $(xmldir)/glade-2.0.dtd $(XMLCATALOG)

#small hack to get distcheck to work
clean-local:
	rm -f intl/po2tbl.sed .memdump

EXTRA_DIST = \
  glade-2.0.dtd \
  libglade.spec.in \
  libglade.spec \
  libglade-2.0.pc.in \
  libglade-convert.in \
  examples/simple.glade \
  examples/first.glade \
  examples/example.glade \
  examples/bonobo.glade \
  config.h.win32 \
  makefile.mingw
