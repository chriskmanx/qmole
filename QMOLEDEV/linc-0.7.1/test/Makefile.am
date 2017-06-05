noinst_PROGRAMS = test-linc

INCLUDES =				\
	-I$(top_srcdir)/include		\
	-I$(top_srcdir)/src		\
	-I$(top_builddir)/include	\
	$(LINC_CFLAGS)			\
	$(WARN_CFLAGS)

TESTS = test-linc

test_linc_SOURCES = test-linc.c
test_linc_LDADD = ../src/liblinc.la $(LINC_LIBS) $(SSL_LIBS)
