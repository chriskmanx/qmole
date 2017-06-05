
noinst_LTLIBRARIES = \
	libegg.la \
	libegg-asn1x.la \
	libegg-buffer.la \
	libegg-creds.la \
	libegg-dbus.la \
	libegg-secure.la \
	libegg-prompt.la \
	libegg-entry-buffer.la \
	libegg-hex.la

BUILT_SOURCES = \
	asn1-def-pk.c asn1-def-pkix.c

INCLUDES = \
	-I$(top_srcdir) 
	-I$(top_builddir) 

libegg_la_CFLAGS = \
	$(LIBTASN1_CFLAGS) \
	$(GLIB_CFLAGS)

libegg_la_SOURCES = \
	egg-asn1x.c egg-asn1x.h \
	egg-buffer.c egg-buffer.h \
	egg-cleanup.c egg-cleanup.h \
	egg-dh.c egg-dh.h \
	egg-dn.c egg-dn.h \
	egg-error.h \
	egg-hex.c egg-hex.h \
	egg-libgcrypt.c egg-libgcrypt.h \
	egg-mkdtemp.c egg-mkdtemp.h \
	egg-oid.c egg-oid.h \
	egg-padding.c egg-padding.h \
	egg-openssl.c egg-openssl.h \
	egg-unix-credentials.c egg-unix-credentials.h \
	egg-secure-memory.c egg-secure-memory.h \
	egg-spawn.c egg-spawn.h \
	egg-symkey.c egg-symkey.h \
	egg-asn1-defs.h \
	$(BUILT_SOURCES)

asn1-def-pk.c: pk.asn
	$(ASN1PARSER) -o asn1-def-pk.c $(srcdir)/pk.asn

asn1-def-pkix.c: pkix.asn
	$(ASN1PARSER) -o asn1-def-pkix.c $(srcdir)/pkix.asn

EXTRA_DIST = \
	pkix.asn \
	pk.asn

DISTCLEANFILES = \
	$(BUILT_SOURCES)
	
# --------------------------------------------------------------------
# COMMON STUFF COMPILED INTO SMALLER COMPONENTS

libegg_asn1x_la_SOURCES = \
	egg-asn1x.c egg-asn1x.h \
	$(BUILT_SOURCES)

libegg_asn1x_la_CFLAGS = \
	$(LIBTASN1_CFLAGS) \
	$(GLIB_CFLAGS)

libegg_secure_la_SOURCES = \
	egg-secure-memory.c egg-secure-memory.h

libegg_entry_buffer_la_SOURCES = \
	egg-entry-buffer.c egg-entry-buffer.h

libegg_entry_buffer_la_CFLAGS = \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GTK_CFLAGS)

libegg_buffer_la_SOURCES = \
	egg-buffer.c egg-buffer.h 

libegg_creds_la_SOURCES = \
	egg-unix-credentials.c egg-unix-credentials.h	

libegg_dbus_la_SOURCES = \
	egg-dbus.c egg-dbus.h

libegg_dbus_la_CFLAGS = \
	$(DBUS_CFLAGS) \
	$(GLIB_CFLAGS)

libegg_dbus_la_LIBADD = \
	$(DBUS_LIBS) \
	$(GLIB_LIBS)

libegg_prompt_la_SOURCES = \
	egg-dh.c egg-dh.h \
	egg-hex.c egg-hex.h \
	egg-libgcrypt.c egg-libgcrypt.h \
	egg-padding.c egg-padding.h \
	egg-secure-memory.c egg-secure-memory.h

libegg_prompt_la_CFLAGS = \
	-DEGG_DH_NO_ASN1=1 \
	$(LIBGCRYPT_CFLAGS) \
	$(GLIB_CFLAGS)

libegg_prompt_la_LIBS = \
	$(LIBGCRYPT_LIBS) \
	$(GLIB_LIBS)

libegg_hex_la_SOURCES = \
	egg-hex.c egg-hex.h

libegg_hex_la_CFLAGS = \
	$(GLIB_CFLAGS)

libegg_hex_la_LIBS = \
	$(GLIB_LIBS)

# -------------------------------------------------------------------

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR = 
endif

SUBDIRS = . $(TESTS_DIR)
