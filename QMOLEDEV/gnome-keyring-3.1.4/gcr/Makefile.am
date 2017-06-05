
if WITH_TESTS
TESTS_DIR = tests
TESTABLE_LIB = libgcr-testable.la
else
TESTS_DIR =
TESTABLE_LIB =
endif

SUBDIRS = . icons $(TESTS_DIR)

# ------------------------------------------------------------------
# UI BUILDER
# 

uidir = $(datadir)/gcr-@GCR_MAJOR@/ui/

ui_DATA = \
	gcr-import-dialog.ui \
	gcr-unlock-options-widget.ui

# ------------------------------------------------------------------
# HEADERS

incdir = $(includedir)/gcr-@GCR_MAJOR@/gcr

inc_HEADERS = \
	gcr.h \
	gcr-certificate.h \
	gcr-certificate-basics-widget.h \
	gcr-certificate-chain.h \
	gcr-certificate-details-widget.h \
	gcr-certificate-renderer.h \
	gcr-certificate-widget.h \
	gcr-collection.h \
	gcr-collection-model.h \
	gcr-column.h \
	gcr-combo-selector.h \
	gcr-comparable.h \
	gcr-enum-types.h \
	gcr-key-renderer.h \
	gcr-key-widget.h \
	gcr-importer.h \
	gcr-library.h \
	gcr-list-selector.h \
	gcr-parser.h \
	gcr-pkcs11-certificate.h \
	gcr-renderer.h \
	gcr-simple-certificate.h \
	gcr-simple-collection.h \
	gcr-tree-selector.h \
	gcr-trust.h \
	gcr-types.h \
	gcr-unlock-options.h \
	gcr-unlock-options-widget.h \
	gcr-viewer.h

# ------------------------------------------------------------------
# LIBRARY

INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	$(GTK_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(LIBTASN1_CFLAGS) \
	$(P11_KIT_CFLAGS)

BUILT_SOURCES = \
	gcr-marshal.c gcr-marshal.h \
	gcr-enum-types.c gcr-enum-types.h

lib_LTLIBRARIES = libgcr-@GCR_MAJOR@.la

libgcr_@GCR_MAJOR@_la_SOURCES = \
	gcr-certificate.c gcr-certificate.h \
	gcr-certificate-basics-widget.h gcr-certificate-basics-widget.c \
	gcr-certificate-chain.c gcr-certificate-chain.h \
	gcr-certificate-details-widget.h gcr-certificate-details-widget.c \
	gcr-certificate-renderer.c gcr-certificate-renderer.h \
	gcr-certificate-exporter.c gcr-certificate-exporter.h \
	gcr-certificate-extensions.c gcr-certificate-extensions.h \
	gcr-certificate-widget.c gcr-certificate-widget.h \
	gcr-collection.c gcr-collection.h \
	gcr-collection-model.c gcr-collection-model.h \
	gcr-combo-selector.c gcr-combo-selector.h \
	gcr-comparable.c gcr-comparable.h \
	gcr-debug.c gcr-debug.h \
	gcr-display-scrolled.c gcr-display-scrolled.h \
	gcr-display-view.c gcr-display-view.h \
	gcr-gnupg-collection.c gcr-gnupg-collection.h \
	gcr-gnupg-key.c gcr-gnupg-key.h \
	gcr-fingerprint.c gcr-fingerprint.h \
	gcr-gnupg-process.c gcr-gnupg-process.h \
	gcr-gnupg-util.c gcr-gnupg-util.h \
	gcr-icons.c gcr-icons.h \
	gcr-import-dialog.c gcr-import-dialog.h \
	gcr-importer.c gcr-importer.h  \
	gcr-internal.h \
	gcr-key-renderer.c gcr-key-renderer.h \
	gcr-key-widget.c gcr-key-widget.h \
	gcr-library.c gcr-library.h \
	gcr-list-selector.c gcr-list-selector.h gcr-list-selector-private.h \
	gcr-live-search.c gcr-live-search.h \
	gcr-memory-icon.c gcr-memory-icon.h \
	gcr-parser.c gcr-parser.h \
	gcr-pkcs11-certificate.c gcr-pkcs11-certificate.h \
	gcr-record.c gcr-record.h \
	gcr-renderer.c gcr-renderer.h \
	gcr-simple-certificate.c gcr-simple-certificate.h \
	gcr-simple-collection.c gcr-simple-collection.h \
	gcr-tree-selector.c gcr-tree-selector.h \
	gcr-trust.c gcr-trust.h \
	gcr-types.h \
	gcr-unlock-options.h \
	gcr-unlock-options-widget.c gcr-unlock-options-widget.h \
	gcr-util.c gcr-util.h \
	gcr-viewer.c gcr-viewer.h \
	$(BUILT_SOURCES)

libgcr_@GCR_MAJOR@_la_CFLAGS = \
	-DGCK_API_SUBJECT_TO_CHANGE \
	-DGCR_API_SUBJECT_TO_CHANGE \
	-DP11_KIT_API_SUBJECT_TO_CHANGE \
	-DGCR_COMPILATION \
	-DUIDIR=\""$(uidir)"\"

libgcr_@GCR_MAJOR@_la_LDFLAGS = \
	-version-info $(GCR_LT_RELEASE) \
	-no-undefined \
	-export-symbols-regex '^gcr_*'

libgcr_@GCR_MAJOR@_la_LIBADD = \
	$(top_builddir)/egg/libegg.la \
	$(top_builddir)/egg/libegg-entry-buffer.la \
	$(top_builddir)/gck/libgck-@GCK_MAJOR@.la \
	$(GOBJECT_LIBS) \
	$(GLIB_LIBS) \
	$(LIBGCRYPT_LIBS) \
	$(GTK_LIBS) \
	$(P11_KIT_LIBS)

noinst_LTLIBRARIES = $(TESTABLE_LIB)
libgcr_testable_la_SOURCES =
libgcr_testable_la_LIBADD = $(libgcr_@GCR_MAJOR@_la_OBJECTS) \
	$(libgcr_@GCR_MAJOR@_la_LIBADD)
libgcr_testable_la_DEPENDENCIES = $(libgcr_@GCR_MAJOR@_la_OBJECTS)

gcr-marshal.h: gcr-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) $(GLIB_GENMARSHAL) $< --header --prefix=_gcr_marshal > $@

gcr-marshal.c: gcr-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) ( echo "#include \"gcr-marshal.h\"" > $@ && \
		$(GLIB_GENMARSHAL) $< --body --prefix=_gcr_marshal >> $@ )

gcr-enum-types.h: $(inc_HEADERS) gcr-enum-types.h.template
	$(AM_V_GEN) $(GLIB_MKENUMS) --template $(srcdir)/gcr-enum-types.h.template \
		$(inc_HEADERS) > $@

gcr-enum-types.c: $(inc_HEADERS) gcr-enum-types.c.template
	$(AM_V_GEN) $(GLIB_MKENUMS) --template $(srcdir)/gcr-enum-types.c.template \
		$(inc_HEADERS) > $@

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = gcr-$(GCR_MAJOR).pc

gcr-$(GCR_MAJOR).pc: gcr.pc
	cp gcr.pc gcr-$(GCR_MAJOR).pc


# ----------------------------------------------------------------

EXTRA_DIST = \
	gcr.pc.in \
	gcr-marshal.list \
	$(ui_DATA) \
	$(conf_DATA) \
	gcr-enum-types.h.template \
	gcr-enum-types.c.template

CLEANFILES = \
	$(BUILT_SOURCES) \
	$(pkgconfig_DATA)

DISTCLEANFILES = \
	$(pkgconfig_DATA)

symbols: libgcr-$(VERSION).symbols

libgcr-$(VERSION).symbols: .libs/libgcr-@GCR_MAJOR@.so
	nm -D .libs/libgcr-@GCR_MAJOR@.so | grep -F ' T ' | \
		cut -d ' ' -f 3 | sort > $@
