
noinst_LTLIBRARIES = \
	libgkm-wrap-layer.la

INCLUDES = -I. \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	-I$(top_srcdir)/pkcs11

# ------------------------------------------------------------------------------
# The code

libgkm_wrap_layer_la_SOURCES = \
	gkm-wrap-layer.c gkm-wrap-layer.h \
	gkm-wrap-login.c gkm-wrap-login.h \
	gkm-wrap-prompt.c gkm-wrap-prompt.h

libgkm_wrap_layer_la_LIBADD = \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(GTHREAD_LIBS) \
	$(GLIB_LIBS)

libgkm_wrap_layer_la_CFLAGS = \
	$(GTHREAD_CFLAGS) \
	$(GLIB_CFLAGS)

# ------------------------------------------------------------------------------

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR =
endif

SUBDIRS = . $(TESTS_DIR)