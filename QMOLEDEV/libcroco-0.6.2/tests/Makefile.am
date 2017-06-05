SUBDIRS=test-inputs test-output-refs
EXTRA_DIST=testctl vg.supp global-vars.sh.in valgrind-version.sh \
test-prop-ident.sh test-unknown-at-rule.sh test-unknown-at-rule2.sh \
test-several-media.sh


#the list of all possible tests goes here.

EXTRALDFLAGS=@GLIB2_LIBS@ @LIBXML2_LIBS@
testprogs=test0 test1 test2 test3 test4 test5 test6
noinst_PROGRAMS=$(testprogs)
test0_SOURCES=test0-main.c
test0_LDFLAGS=$(EXTRALDFLAGS)
test1_SOURCES=test1-main.c
test1_LDFLAGS=$(EXTRALDFLAGS)
test2_SOURCES=test2-main.c cr-test-utils.c cr-test-utils.h
test2_LDFLAGS=$(EXTRALDFLAGS)
test3_SOURCES=test3-main.c cr-test-utils.c cr-test-utils.h
test3_LDFLAGS=$(EXTRALDFLAGS)
test4_SOURCES=test4-main.c cr-test-utils.c cr-test-utils.h
test4_LDFLAGS=$(EXTRALDFLAGS)
test5_SOURCES=test5-main.c cr-test-utils.c cr-test-utils.h
test5_LDFLAGS=$(EXTRALDFLAGS)
test6_SOURCES=test6-main.c cr-test-utils.c cr-test-utils.h
test6_LDFLAGS=$(EXTRALDFLAGS)

croco_lib=$(top_builddir)/src/@CROCO_LIB@
LDADD=$(croco_lib)

INCLUDES=-I$(top_srcdir)/intl -I$(top_srcdir)/src -I$(top_builddir)/src

AM_CFLAGS=-I. @LIBXML2_CFLAGS@ @GLIB2_CFLAGS@
