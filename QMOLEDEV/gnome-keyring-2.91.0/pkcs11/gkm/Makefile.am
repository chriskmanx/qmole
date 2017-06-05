
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	$(GOBJECT_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(GLIB_CFLAGS)

# ------------------------------------------------------------------------------
# The GKM library

noinst_LTLIBRARIES = \
	libgkm.la

BUILT_SOURCES = \
	gkm-marshal.c gkm-marshal.h \
	asn1-def-pk.h asn1-def-pkix.h

libgkm_la_SOURCES = \
	gkm-aes-key.c gkm-aes-key.h \
	gkm-aes-mechanism.c gkm-aes-mechanism.h \
	gkm-attributes.c gkm-attributes.h \
	gkm-certificate.c gkm-certificate.h \
	gkm-certificate-key.c gkm-certificate-key.h \
	gkm-certificate-trust.c gkm-certificate-trust.h \
	gkm-credential.c gkm-credential.h \
	gkm-crypto.c gkm-crypto.h \
	gkm-data-asn1.c gkm-data-asn1.h \
	gkm-data-der.c gkm-data-der.h \
	gkm-data-file.c gkm-data-file.h \
	gkm-data-types.h \
	gkm-dh-key.c gkm-dh-key.h \
	gkm-dh-mechanism.c gkm-dh-mechanism.h \
	gkm-dh-private-key.c gkm-dh-private-key.h \
	gkm-dh-public-key.c gkm-dh-public-key.h \
	gkm-dsa-mechanism.c gkm-dsa-mechanism.h \
	gkm-factory.c gkm-factory.h \
	gkm-file-tracker.c gkm-file-tracker.h \
	gkm-manager.c gkm-manager.h \
	gkm-memory-store.c gkm-memory-store.h \
	gkm-mock.c gkm-mock.h \
	gkm-module.c gkm-module.h gkm-module-ep.h \
	gkm-null-key.c gkm-null-key.h \
	gkm-null-mechanism.c gkm-null-mechanism.h \
	gkm-object.c gkm-object.h \
	gkm-private-xsa-key.c gkm-private-xsa-key.h \
	gkm-public-xsa-key.c gkm-public-xsa-key.h \
	gkm-rsa-mechanism.c gkm-rsa-mechanism.h \
	gkm-secret.c gkm-secret.h \
	gkm-secret-key.c gkm-secret-key.h \
	gkm-serializable.c gkm-serializable.h \
	gkm-session.c gkm-session.h \
	gkm-sexp.c gkm-sexp.h \
	gkm-sexp-key.c gkm-sexp-key.h \
	gkm-store.c gkm-store.h \
	gkm-test.c gkm-test.h \
	gkm-timer.c gkm-timer.h \
	gkm-transaction.c gkm-transaction.h \
	gkm-types.h \
	gkm-util.c gkm-util.h \
	$(BUILT_SOURCES)

libgkm_la_CFLAGS = \
	$(GOBJECT_CFLAGS) \
	$(LIBGCRYPT_CFLAGS) \
	$(LIBTASN1_CFLAGS) \
	$(GLIB_CFLAGS)

libgkm_la_LIBADD = \
	$(top_builddir)/egg/libegg.la \
	$(GOBJECT_LIBS) \
	$(LIBGCRYPT_LIBS) \
	$(GLIB_LIBS)

gkm-marshal.h: gkm-marshal.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --header --prefix=gkm_marshal > $@

gkm-marshal.c: gkm-marshal.list $(GLIB_GENMARSHAL)
	echo "#include \"gkm-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=gkm_marshal >> $@

asn1-def-pk.h: pk.asn
	$(ASN1PARSER) -o asn1-def-pk.h $<

asn1-def-pkix.h: pkix.asn
	$(ASN1PARSER) -o asn1-def-pkix.h $<

EXTRA_DIST = \
	gkm-marshal.list \
	pkix.asn \
	pk.asn

# -------------------------------------------------------------------------------

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR =
endif

SUBDIRS = . $(TESTS_DIR)
