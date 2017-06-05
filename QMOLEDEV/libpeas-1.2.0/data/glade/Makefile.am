catalogdir = $(GLADE_CATALOG_DIR)
catalog_in_files = libpeas-gtk.xml.in
catalog_DATA = $(catalog_in_files:.xml.in=.xml)

EXTRA_DIST = \
	$(catalog_in_files)

validate: $(catalog_DATA)
	xmllint --dtdvalid $(GLADE_CATALOG_DIR)/glade-catalog.dtd --noout $<

CLEANFILES = $(catalog_DATA)

@INTLTOOL_XML_NOMERGE_RULE@
