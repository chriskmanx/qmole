SUBDIRS = everything inhibit poa

INCLUDES =                          \
	-I$(top_srcdir)/include     \
	-I$(top_srcdir)/src/orb     \
	-I$(top_builddir)/include   \
	-DORBIT2_INTERNAL_API       \
	$(WARN_CFLAGS)              \
	$(LOCAL_LINC_CFLAGS)        \
	$(ORBIT_CFLAGS)

bin_PROGRAMS=ior-decode-2 typelib-dump
noinst_PROGRAMS=test1 \
	echo-client echo-client-t echo-server \
	empty-client empty-server \
	test-any-client test-any-server \
	timeout-client timeout-server 

check_PROGRAMS = test-dynany test-mem test-performance test-giop test-corbaloc

TESTS =  timeout.sh $(check_PROGRAMS)

LDADD = $(top_builddir)/src/orb/libORBit-2.la $(ORBIT_LIBS)

test_corbaloc_SOURCES = test-corbaloc.c

test_performance_SOURCES = 	\
	test-performance.c	\
	test1.h			\
	test1-skels.c		\
	test1-common.c

test_giop_SOURCES =		\
	test-giop.c		\
	test-giop-frag.h

TEST_ANY_IDLOUT=test-any.h test-any-common.c test-any-stubs.c test-any-skels.c
test_any_client_SOURCES=test-any-client.c $(TEST_ANY_IDLOUT)
test_any_server_SOURCES=test-any-server.c $(TEST_ANY_IDLOUT)
$(srcdir)/test-any-client.c $(srcdir)/test-any-server.c: test-any.h

ECHO_IDLOUT=echo.h echo-common.c echo-stubs.c echo-skels.c
echo_client_SOURCES=echo-client.c $(ECHO_IDLOUT)
echo_client_t_SOURCES=echo-client-t.c $(ECHO_IDLOUT)
echo_server_SOURCES=echo-server.c echo-srv.c $(ECHO_IDLOUT) echo-share.h$
$(srcdir)/echo-client-t.c $(srcdir)/echo-client.c $(srcdir)/echo-server.c $(srcdir)/echo-srv.c: echo.h

EMPTY_IDLOUT=empty.h empty-common.c empty-stubs.c empty-skels.c
empty_client_SOURCES=empty-client.c $(EMPTY_IDLOUT)
empty_server_SOURCES=empty-server.c $(EMPTY_IDLOUT)
$(srcdir)/empty-client.c $(srcdir)/empty-server.c: empty.h

TIMEOUT_IDLOUT=timeout.h timeout-common.c timeout-stubs.c timeout-skels.c
timeout_client_SOURCES=timeout-client.c $(TIMEOUT_IDLOUT)
timeout_server_SOURCES=timeout-server.c $(TIMEOUT_IDLOUT)
$(srcdir)/timeout-client.c $(srcdir)/timeout-server.c: timeout.h

IDLOUT=test1-stubs.c test1-skels.c test1-common.c test1.h
test1_SOURCES=test1.c $(IDLOUT)
$(srcdir)/test1.c: test1.h

test_mem_SOURCES=test-mem.c $(IDLOUT)

ior_decode_2_SOURCES=ior-decode.c

typelib_dump_SOURCES=typelib-dump.c

DYNANY_IDLOUT=dynany.h dynany-common.c dynany-skels.c dynany-stubs.c
test_dynany_SOURCES=$(DYNANY_IDLOUT) test-dynany.c
test_dynany_LDFLAGS=$(LIBM)

IDL_FLAGS =  --showcpperrors
IDL_FILES=echo.idl empty.idl test-any.idl test1.idl dynany.idl timeout.idl
include $(top_srcdir)/Makefile.shared

BUILT_SOURCES =            \
	$(EMPTY_IDLOUT)    \
	$(ECHO_IDLOUT)     \
	$(IDLOUT)          \
	$(TEST_ANY_IDLOUT) \
	$(DYNANY_IDLOUT)   \
	$(TIMEOUT_IDLOUT)

CLEANFILES = $(BUILT_SOURCES) timeout-server.iorfile
EXTRA_DIST = $(IDL_FILES) timeout_impl.c timeout.sh

dist-hook:
	cd $(distdir); rm -f $(BUILT_SOURCES)
