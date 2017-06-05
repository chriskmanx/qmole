
INCLUDES = \
	-I$(top_srcdir)/egg \
	$(LIBRARY_CFLAGS)

LDADD =  \
	$(top_builddir)/egg/libegg.la \
	$(LIBRARY_LIBS)

TEST_PROGS = \
	test-dh \
	test-hkdf \
	test-secmem

check_PROGRAMS = $(TEST_PROGS)

test: ${TEST_PROGS}
	gtester -k --verbose ${TEST_PROGS}

check-local: test

all-local: $(check_PROGRAMS)
