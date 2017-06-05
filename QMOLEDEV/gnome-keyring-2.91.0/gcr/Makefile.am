# ------------------------------------------------------------------
# UI BUILDER
# 

uidir = $(datadir)/gcr@GCR_VERSION_SUFFIX@/ui/

ui_DATA = \
	gcr-certificate-basics-widget.ui \
	gcr-import-dialog.ui \
	gcr-unlock-options-widget.ui

# ------------------------------------------------------------------
# HEADERS

incdir = $(includedir)/gcr@GCR_VERSION_SUFFIX@/gcr

inc_HEADERS = \
	gcr.h \
	gcr-certificate.h \
	gcr-certificate-basics-widget.h \
	gcr-certificate-details-widget.h \
	gcr-importer.h \
	gcr-parser.h \
	gcr-simple-certificate.h \
	gcr-types.h \
	gcr-unlock-options.h \
	gcr-unlock-options-widget.h

# ------------------------------------------------------------------
# LIBRARY

INCLUDES = \
	-I$(top_builddir) \
    	-I$(top_srcdir) \
    	$(GTK_CFLAGS) \
    	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(LIBTASN1_CFLAGS)

BUILT_SOURCES = \
	gcr-marshal.c gcr-marshal.h

lib_LTLIBRARIES = libgcr@GCR_VERSION_SUFFIX@.la

libgcr@GCR_VERSION_SUFFIX@_la_SOURCES = \
	gcr-certificate.c gcr-certificate.h \
	gcr-certificate-basics-widget.c gcr-certificate-basics-widget.h \
	gcr-certificate-details-widget.c gcr-certificate-details-widget.h \
	gcr-import-dialog.c gcr-import-dialog.h \
	gcr-importer.c gcr-importer.h  \
	gcr-internal.h \
	gcr-library.c \
	gcr-parser.c gcr-parser.h \
	gcr-simple-certificate.c gcr-simple-certificate.h \
	gcr-types.h \
	gcr-unlock-options.h \
	gcr-unlock-options-widget.c gcr-unlock-options-widget.h \
	$(BUILT_SOURCES)

libgcr@GCR_VERSION_SUFFIX@_la_CFLAGS = \
	-DPKCS11_MODULE_PATH=\""$(libdir)/gnome-keyring/gnome-keyring-pkcs11.so"\" \
	-DGCK_API_SUBJECT_TO_CHANGE \
	-DGCR_API_SUBJECT_TO_CHANGE \
	-DUIDIR=\""$(uidir)"\"
    
libgcr@GCR_VERSION_SUFFIX@_la_LDFLAGS = \
    	-version-info $(GCR_LT_RELEASE) \
	-no-undefined -export-symbols-regex 'gcr_*'
    
libgcr@GCR_VERSION_SUFFIX@_la_LIBADD = \
	$(top_builddir)/egg/libegg.la \
	$(top_builddir)/egg/libegg-entry-buffer.la \
	$(top_builddir)/gck/libgck.la \
	$(GOBJECT_LIBS) \
	$(GLIB_LIBS) \
	$(LIBGCRYPT_LIBS) \
	$(GTK_LIBS)

gcr-marshal.h: gcr-marshal.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --header --prefix=_gcr_marshal > $@

gcr-marshal.c: gcr-marshal.list $(GLIB_GENMARSHAL)
	echo "#include \"gcr-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=_gcr_marshal >> $@

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = gcr-$(GCR_MAJOR).pc

gcr-$(GCR_MAJOR).pc: gcr.pc
	cp gcr.pc gcr-$(GCR_MAJOR).pc

# ----------------------------------------------------------------

EXTRA_DIST = \
	gcr.pc.in \
	gcr-marshal.list \
	$(ui_DATA)
    
CLEANFILES = \
	$(BUILT_SOURCES) \
    	$(pkgconfig_DATA)
    	
DISTCLEANFILES = \
	$(pkgconfig_DATA)
    
if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR = 
endif

SUBDIRS = . \
	$(TESTS_DIR)
