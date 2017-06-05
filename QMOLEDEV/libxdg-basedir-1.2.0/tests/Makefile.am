AM_CFLAGS = -I$(top_srcdir)/include -Wall
AUTOMAKE_OPTIONS = color-tests

check_PROGRAMS = testdump testfind testquery

QUERYTESTS = \
	querycd.1 \
	querycd.2 \
	querycd.3 \
	querycd.4 \
	querycd.5 \
	querycf.1 \
	querycf.2 \
	querycs.1 \
	querycs.2 \
	querycs.3 \
	querycs.4 \
	querycs.5 \
	querydd.1 \
	querydd.2 \
	querydd.3 \
	querydd.4 \
	querydd.5 \
	querydf.1 \
	querydf.2 \
	querydh.1 \
	querydh.2 \
	querydh.3 \
	queryds.1 \
	queryds.2 \
	queryds.3 \
	queryds.4 \
	queryds.5 \
	queryds.6 \
	queryrd.1 \
	queryrd.2 \
	#

TESTS = testdump ${QUERYTESTS}

EXTRA_DIST = query-harness.sh ${QUERYTESTS}

TESTS_ENVIRONMENT = env top_srcdir=$(top_srcdir) top_builddir=$(top_builddir)

testdump_SOURCES = testdump.c
testdump_LDFLAGS = $(all_libraries)
testdump_LDADD = $(top_builddir)/src/libxdg-basedir.la

testfind_SOURCES = testfind.c
testfind_LDFLAGS = $(all_libraries)
testfind_LDADD = $(top_builddir)/src/libxdg-basedir.la

testquery_SOURCES = testquery.c
testquery_LDFLAGS = $(all_libraries)
testquery_LDADD = $(top_builddir)/src/libxdg-basedir.la
