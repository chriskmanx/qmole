noinst_PROGRAMS=					\
	test-inhibit

INCLUDES =                           \
	-I$(top_srcdir)/include	     \
	-I$(top_builddir)/include    \
	$(WARN_CFLAGS)               \
	$(LOCAL_LINC_CFLAGS)         \
	$(ORBIT_CFLAGS)

LDADD = $(top_builddir)/src/orb/libORBit-2.la $(ORBIT_LIBS)

FOO_IDLOUT=foo.h foo-stubs.c foo-skels.c foo-common.c
BAA_IDLOUT=baa.h baa-stubs.c baa-skels.c baa-common.c

test_inhibit_SOURCES=$(FOO_IDLOUT) $(BAA_IDLOUT) test-inhibit.c

IDL_FLAGS=--showcpperrors -I$(srcdir)
IDL_FILES=foo.idl baa.idl
include $(top_srcdir)/Makefile.shared

TESTS=test-inhibit

BUILT_SOURCES = $(BAA_IDLOUT) $(FOO_IDLOUT)
CLEANFILES = $(BUILT_SOURCES)
EXTRA_DIST = $(IDL_FILES)

dist-hook:
	cd $(distdir); rm -f $(BUILT_SOURCES)
