#
# $Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/print/Makefile.am,v 1.22 2008/01/02 19:48:24 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

AUX_OBJS =
noinst_PROGRAMS = test1 test2 test3 test4 test5 test6 test7 test8 test9 test10

noinst_HEADERS =

LTTEST	= $(top_builddir)/common/libLtTest.a

LDADD	= $(AUX_OBJS) \
	  $(top_builddir)/../lib/Dt/libDtPrint.la \
	  $(LTTEST) \
	  $(top_builddir)/../lib/Xm$(version_suffix)/libXm.la $(XPLIB) \
	  @X_LIBS@ -lXt -lX11


AM_CPPFLAGS= -I$(top_srcdir)/../include/Motif-2.1 \
  	  -I$(top_builddir)/../include/Motif-2.1 \
        -I$(top_srcdir)/../include/Motif-2.0 \
	  -I$(top_builddir)/../include/Motif-2.0 \
	  -I$(top_srcdir)/../include/Motif-1.2 \
	  -I$(top_builddir)/../include/Motif-1.2 \
	  -I$(top_srcdir)/../lib \
	  -I$(top_builddir)/../lib \
	  -I$(top_builddir)/common

$(LTTEST): 
	( cd $(top_builddir)/common ; $(MAKE) )

SUFFIXES= .motif .mo

clean-local:
	rm -rf *.motif .mo LessTif LessTif.sum LessTif.log site.exp xlab/*.p*

if MotifTests

motif_TESTS = $(OBJECTS:.o=.motif)

.c.mo:
	$(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) -g @MOTIF_CFLAGS@ $<

#
# Only link -lDtPrint when required
#
test2.motif:	test2.mo
	${CC} -R@motif_libraries@ -o $@ $< $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) -lDtPrint -lXm @X_LIBS@ -lXt -lX11

test4.motif:	test4.mo
	${CC} -R@motif_libraries@ -o $@ $< $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) -lDtPrint -lXm @X_LIBS@ -lXt -lX11

.mo.motif:
	${CC} -Wl,-rpath @motif_libraries@ -o $@ $< $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) -lXm @X_LIBS@ -lXt -lX11

motif-tests: ${motif_TESTS}

${motif_TESTS}: $(AUX_OBJS)

endif
