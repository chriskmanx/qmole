
INCLUDES = \
	-I$(top_builddir) \
    	-I$(top_srcdir) \
    	-I$(top_srcdir)/pkcs11 \
    	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS)

# ------------------------------------------------------------------------------
# The secret store component code

noinst_LTLIBRARIES = \
	libgkm-secret-store.la

libgkm_secret_store_la_SOURCES = \
	gkm-secret-binary.c gkm-secret-binary.h \
	gkm-secret-collection.h gkm-secret-collection.c \
	gkm-secret-compat.h gkm-secret-compat.c \
	gkm-secret-data.h gkm-secret-data.c \
	gkm-secret-fields.h gkm-secret-fields.c \
	gkm-secret-item.h gkm-secret-item.c \
	gkm-secret-module.h gkm-secret-module.c \
	gkm-secret-object.h gkm-secret-object.c \
	gkm-secret-search.h gkm-secret-search.c \
	gkm-secret-store.h \
	gkm-secret-types.h \
	gkm-secret-textual.c gkm-secret-textual.h

# ------------------------------------------------------------------------------
# The standalone module

moduledir = $(libdir)/gnome-keyring/standalone/

module_LTLIBRARIES = \
	gkm-secret-store-standalone.la

gkm_secret_store_standalone_la_LDFLAGS = \
	-module -avoid-version \
	-no-undefined -export-symbols-regex 'C_GetFunctionList'

gkm_secret_store_standalone_la_SOURCES = \
	gkm-secret-standalone.c

gkm_secret_store_standalone_la_LIBADD = \
	libgkm-secret-store.la \
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
