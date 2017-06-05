
INCLUDES = \
	-I$(top_builddir) \
    	-I$(top_srcdir) \
    	-I$(top_srcdir)/pkcs11 \
    	$(GOBJECT_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(GLIB_CFLAGS)


# ------------------------------------------------------------------------------
# The user-store component code

noinst_LTLIBRARIES = \
	libgkm-user-store.la

libgkm_user_store_la_SOURCES = \
	gkm-user-store.h \
	gkm-user-module.c gkm-user-module.h \
	gkm-user-private-key.c gkm-user-private-key.h \
	gkm-user-public-key.c gkm-user-public-key.h \
	gkm-user-storage.c gkm-user-storage.h

# ------------------------------------------------------------------------------
# The standalone module

moduledir = $(libdir)/gnome-keyring/devel/

module_LTLIBRARIES = \
	gkm-user-store-standalone.la

gkm_user_store_standalone_la_LDFLAGS = \
	-module -avoid-version \
	-no-undefined -export-symbols-regex 'C_GetFunctionList'

gkm_user_store_standalone_la_SOURCES = \
	gkm-user-standalone.c

gkm_user_store_standalone_la_LIBADD = \
	libgkm-user-store.la \
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
