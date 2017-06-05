noinst_PROGRAMS = test-linc

#	-I$(top_srcdir)/include
#	-I$(top_builddir)/include
INCLUDES =				\
	-I$(top_srcdir)/linc2/include	\
	-I$(top_builddir)/linc2/include \
	-I$(top_srcdir)/linc2/src	\
	$(LINC_CFLAGS)			\
	$(WARN_CFLAGS)

TESTS = test-linc

test_linc_SOURCES = test-linc.c
test_linc_LDADD = ../src/liblinc.la $(LINC_LIBS)
