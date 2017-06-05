#
# $Header: /cvsroot/lesstif/lesstif/test/Mrm/mrm/Makefile.am,v 1.22 2008/01/02 19:48:20 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in \
         hello_world.uid test2a.uid test2b.uid test2c.uid test3.uid

SUFFIXES = .uid .uil .motif .mo

noinst_PROGRAMS= test1 test2 test3
noinst_SCRIPTS = hello_world.uid test2a.uid test2b.uid test2c.uid test3.uid

EXTRA_DIST=hello_world.uil  test2a.uil  test2b.uil  test2c.uil  test3.uil


%.uid: %.uil
	if test -x $(top_builddir)/../clients/Motif$(include_version_suffix)/uil/uil ; 	then \
	$(top_builddir)/../clients/Motif$(include_version_suffix)/uil/uil -o  $@  $< ; else \
	uil -o $@ $< ; 	fi


LTTEST= $(top_builddir)/common/libLtTest.a
LDADD	= \
	  $(LTTEST)  \
	  $(top_builddir)/../lib/Mrm$(version_suffix)/libMrm.la  \
	  $(top_builddir)/../lib/Xm$(version_suffix)/libXm.la $(XPLIB) \
	  ${X_LIBS} -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS}

AM_CPPFLAGS= -I$(top_srcdir)/../include/Motif$(include_version_suffix) \
	  -I$(top_builddir)/../include/Motif$(include_version_suffix) \
	  -I$(top_srcdir)/../include/Motif-1.2 \
	  -I$(top_builddir)/../include/Motif-1.2

#SUFFIXES = .motif .mo

$(LTTEST): 
	( cd $(top_builddir)/common ; $(MAKE) )

clean-local:
	rm -rf *.motif *.mo LessTif LessTif.sum LessTif.log site.exp xlab/*.p* *.uid

if MotifTests

motif_TESTS = $(OBJECTS:.o=.motif)

.c.mo:
	$(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) -g ${MOTIF_CFLAGS} ${X_CFLAGS} $<

.mo.motif:
	${LINK} $< $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) -lXm ${X_LIBS} -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS}

motif-tests: ${motif_TESTS}

${motif_TESTS}: $(AUX_OBJS)
endif
