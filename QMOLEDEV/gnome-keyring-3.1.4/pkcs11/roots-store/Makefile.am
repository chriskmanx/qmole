
INCLUDES = \
	-I$(top_builddir) \
    	-I$(top_srcdir) \
    	-I$(top_srcdir)/pkcs11 \
    	$(GOBJECT_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GMODULE_CFLAGS)



# ------------------------------------------------------------------------------
# The roots component code

noinst_LTLIBRARIES = \
	libgkm-roots-store.la

libgkm_roots_store_la_SOURCES = \
	gkm-roots-store.h \
	gkm-roots-module.c gkm-roots-module.h \
	gkm-roots-certificate.c gkm-roots-certificate.h \
	gkm-roots-trust.c gkm-roots-trust.h

# ------------------------------------------------------------------------------
# The standalone module

moduledir = $(pkcs11standalonedir)

module_LTLIBRARIES = \
	gkm-roots-store-standalone.la

gkm_roots_store_standalone_la_LDFLAGS = \
	-module -avoid-version \
	-no-undefined -export-symbols-regex 'C_GetFunctionList|g_module_check_init'

gkm_roots_store_standalone_la_SOURCES = \
	gkm-roots-standalone.c

gkm_roots_store_standalone_la_LIBADD = \
	libgkm-roots-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(GLIB_LIBS) \
	$(LIBGCRYPT_LIBS) \
	$(GMODULE_LIBS)


# -------------------------------------------------------------------------------

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR =
endif

SUBDIRS = . $(TESTS_DIR)
