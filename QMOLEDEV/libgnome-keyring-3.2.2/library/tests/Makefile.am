
INCLUDES = \
	-I$(top_srcdir)/library \
	$(LIBRARY_CFLAGS)

LDADD =  \
	$(top_builddir)/egg/libegg.la \
	$(top_builddir)/library/libgnome-keyring.la \
	$(LIBRARY_LIBS)

TEST_PROGS = \
	test-memory \
	test-keyrings \
	test-other

check_PROGRAMS = \
	test-prompting \
	$(TEST_PROGS)

noinst_PROGRAMS = \
	frob-any-daemon \
	frob-unlock-keyring \
	frob-list-modified

test: $(TEST_PROGS)
	gtester -k --verbose $(TEST_PROGS)

all-local: $(check_PROGRAMS)

check-local: test
