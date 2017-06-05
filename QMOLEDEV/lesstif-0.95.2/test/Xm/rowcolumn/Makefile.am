#
# $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/Makefile.am,v 1.64 2008/01/02 19:48:22 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

LTTEST  = $(top_builddir)/common/libLtTest.a
AUX_OBJS =
noinst_PROGRAMS= test1  test2  test3  test4  test5  test6  test7  test8  \
		 test9  test10 test11 test12 test13 test14 test15 test16 \
		 test17 test18 test19 test20 test21 test22 test23 test24 \
		 test25 test26 test27 test28 test29 test30 test31 test32 \
		 test33 test34 test35 test36 test37 test38 test39 test40 \
		 test41 test42 test43 test44 test45 test46 test47 test48 \
		 test49 test50 test51 test52 test53 test54 test55 test56 \
		 test57 test58 test59 test60 test61 test62 test63 test64 \
		 test65


LDADD	= $(AUX_OBJS)  \
	  $(LTTEST)  \
	  $(top_builddir)/../lib/Xm$(version_suffix)/libXm.la $(XPLIB) \
	  ${X_LIBS} -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS} 

AM_CPPFLAGS= -I$(top_srcdir)/../include/Motif$(include_version_suffix)  \
	  -I$(top_builddir)/../include/Motif$(include_version_suffix)  \
	  -I$(top_srcdir)/../include/Motif-1.2  \
	  -I$(top_builddir)/../include/Motif-1.2  \
	  $(X_CFLAGS) 

$(LTTEST): 
	( cd $(top_builddir)/common ; $(MAKE) )

SUFFIXES = .motif .mo

if MotifTests

motif_TESTS = $(OBJECTS:.o=.motif)

.c.mo: 
	$(CC) $(DEFS) $(MOTIF_CFLAGS) $(X_CFLAGS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -c -o $@ $< 

.mo.motif: 
	${LINK} $< $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) ${X_LIBS} -lXm -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS} 

motif-tests: ${motif_TESTS}

${motif_TESTS}: $(AUX_OBJS)

endif
clean-local:
	rm -rf *.motif *.mo LessTif LessTif.sum LessTif.log site.exp xlab/*.p*
