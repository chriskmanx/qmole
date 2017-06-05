
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-I$(top_srcdir)/pkcs11 \
	-DSRCDIR="\"$(srcdir)\"" \
	$(GLIB_CFLAGS) \
	$(LIBGCRYPT_CFLAGS)

LDADD = \
	libgkm-mock-xdg-module.a \
	$(top_builddir)/pkcs11/xdg-store/libgkm-xdg-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(top_builddir)/egg/libegg.la \
	$(GLIB_LIBS) \
	$(LIBGCRYPT_LIBS)

if WITH_P11_TESTS
CHECK_PROGS = check-xdg-module
else
CHECK_PROGS =
endif

TEST_PROGS = \
	test-xdg-module \
	test-xdg-trust

check_PROGRAMS = $(TEST_PROGS)

test: $(TEST_PROGS) $(CHECK_PROGS)
	gtester -k --verbose -m $(TEST_MODE) --g-fatal-warnings $(TEST_PROGS)
	@for prog in $(CHECK_PROGS); do SRCDIR='.' ./$$prog || exit 1; done

check-local: test

all-local: $(check_PROGRAMS)

EXTRA_DIST = \
	p11-tests.conf \
	files

noinst_PROGRAMS = \
	frob-trust-file \
	dump-trust-file \
	$(CHECK_PROGS)

check_xdg_module_CFLAGS = $(P11_TESTS_CFLAGS)
check_xdg_module_LDADD = $(P11_TESTS_LIBS) $(LDADD)

noinst_LIBRARIES = libgkm-mock-xdg-module.a

libgkm_mock_xdg_module_a_SOURCES = \
	mock-xdg-module.c mock-xdg-module.h
