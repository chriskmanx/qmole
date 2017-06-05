
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-I$(top_srcdir)/pkcs11 \
	$(GOBJECT_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(GLIB_CFLAGS)


# ------------------------------------------------------------------------------
# The gnome2-store component code

noinst_LTLIBRARIES = \
	libgkm-gnome2-store.la

libgkm_gnome2_store_la_SOURCES = \
	gkm-gnome2-store.h \
	gkm-gnome2-file.c gkm-gnome2-file.h \
	gkm-gnome2-module.c gkm-gnome2-module.h \
	gkm-gnome2-private-key.c gkm-gnome2-private-key.h \
	gkm-gnome2-public-key.c gkm-gnome2-public-key.h \
	gkm-gnome2-storage.c gkm-gnome2-storage.h

# ------------------------------------------------------------------------------
# The standalone module

moduledir = $(pkcs11standalonedir)

module_LTLIBRARIES = \
	gkm-gnome2-store-standalone.la

gkm_gnome2_store_standalone_la_LDFLAGS = \
	-module -avoid-version \
	-no-undefined -export-symbols-regex 'C_GetFunctionList'

gkm_gnome2_store_standalone_la_SOURCES = \
	gkm-gnome2-standalone.c

gkm_gnome2_store_standalone_la_LIBADD = \
	libgkm-gnome2-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(GLIB_LIBS) \
	$(LIBGCRYPT_LIBS)


# -------------------------------------------------------------------------------

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR =
endif

SUBDIRS = . $(TESTS_DIR)
