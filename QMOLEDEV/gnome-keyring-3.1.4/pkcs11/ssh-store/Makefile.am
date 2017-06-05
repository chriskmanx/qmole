
INCLUDES = \
	-I$(top_builddir) \
    	-I$(top_srcdir) \
    	-I$(top_srcdir)/pkcs11 \
    	$(GOBJECT_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GMODULE_CFLAGS)


# ------------------------------------------------------------------------------
# The ssh-store component code

noinst_LTLIBRARIES = \
	libgkm-ssh-store.la

libgkm_ssh_store_la_SOURCES = \
	gkm-ssh-store.h \
	gkm-ssh-module.c gkm-ssh-module.h \
	gkm-ssh-openssh.c gkm-ssh-openssh.h \
	gkm-ssh-private-key.c gkm-ssh-private-key.h \
	gkm-ssh-public-key.c gkm-ssh-public-key.h

# ------------------------------------------------------------------------------
# The standalone module

moduledir = $(pkcs11standalonedir)

module_LTLIBRARIES = \
	gkm-ssh-store-standalone.la

gkm_ssh_store_standalone_la_LDFLAGS = \
	-module -avoid-version \
	-no-undefined -export-symbols-regex 'C_GetFunctionList|g_module_check_init'

gkm_ssh_store_standalone_la_SOURCES = \
	gkm-ssh-standalone.c

gkm_ssh_store_standalone_la_LIBADD = \
	libgkm-ssh-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(GLIB_LIBS) \
	$(GMODULE_LIBS) \
	$(LIBGCRYPT_LIBS)


# -------------------------------------------------------------------------------

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR =
endif

SUBDIRS = . $(TESTS_DIR)
