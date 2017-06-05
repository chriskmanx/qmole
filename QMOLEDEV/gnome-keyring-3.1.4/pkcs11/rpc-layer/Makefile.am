
noinst_LTLIBRARIES = \
	libgkm-rpc-layer.la

noinst_PROGRAMS = \
	gkm-rpc-daemon-standalone

INCLUDES = -I. \
	-I$(top_srcdir) \
	-I$(top_builddir)

# ------------------------------------------------------------------------------
# The dispatch code

libgkm_rpc_layer_la_SOURCES = \
	gkm-rpc-dispatch.c \
	gkm-rpc-layer.h \
	gkm-rpc-message.c \
	gkm-rpc-private.h \
	gkm-rpc-util.c

libgkm_rpc_layer_la_LIBADD = \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(GOBJECT_LIBS) \
    	$(GTHREAD_LIBS) \
	$(GLIB_LIBS)

libgkm_rpc_layer_la_CFLAGS = \
	$(GOBJECT_CFLAGS) \
    	$(GTHREAD_CFLAGS) \
	$(GLIB_CFLAGS)

# ------------------------------------------------------------------------------
# The module code, built as the public gnome-keyring module

# This is the location that some older software looks for modules
moduledir = $(pkcs11dir)

module_LTLIBRARIES = \
	gnome-keyring-pkcs11.la

gnome_keyring_pkcs11_la_SOURCES = \
	gkm-rpc-private.h \
	gkm-rpc-module.c \
	gkm-rpc-message.c \
	gkm-rpc-util.c

gnome_keyring_pkcs11_la_LIBADD = \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la

gnome_keyring_pkcs11_la_LDFLAGS = \
	-module -avoid-version \
	-no-undefined -export-symbols-regex 'C_GetFunctionList'

# This is the configuration file that p11-kit uses to load the module
configdir = $(P11_SYSTEM_MODULES)

config_DATA = \
	gnome-keyring-module

# -----------------------------------------------------------------------------
# The test daemon

gkm_rpc_daemon_standalone_SOURCES = \
	gkm-rpc-daemon-standalone.c

gkm_rpc_daemon_standalone_LDADD = \
	$(DL_LIBS) libgkm-rpc-layer.la
