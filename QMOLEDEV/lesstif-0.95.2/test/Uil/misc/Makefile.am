#
# $Header: /cvsroot/lesstif/lesstif/test/Uil/misc/Makefile.am,v 1.8 2005/03/26 06:52:27 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

AUX_OBJS =
noinst_PROGRAMS= test1

LTTEST= $(top_builddir)/common/libLtTest.a
LDADD	= $(AUX_OBJS) \
        $(LTTEST) \
	  $(top_builddir)/../lib/Uil$(version_suffix)/libUil.la  \
	  $(top_builddir)/../lib/Mrm$(version_suffix)/libMrm.la  \
	  $(top_builddir)/../lib/Xm$(version_suffix)/libXm.la $(XPLIB) \
	  ${X_LIBS} -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS}

AM_CPPFLAGS= -I$(top_srcdir)/../include/Motif$(include_version_suffix) \
	  -I$(top_builddir)/../include/Motif$(include_version_suffix) \
	  -I$(top_srcdir)/../include/Motif-2.0 \
	  -I$(top_builddir)/../include/Motif-2.0 \
	  -I$(top_srcdir)/../include/Motif-1.2 \
	  -I$(top_builddir)/../include/Motif-1.2

SUFFIXES = .motif .mo

$(LTTEST): 
	( cd $(top_builddir)/common ; $(MAKE) )

clean-local:
	rm -rf *.motif *.mo LessTif LessTif.sum LessTif.log site.exp xlab/*.p*

if MotifTests

motif_TESTS = $(OBJECTS:.o=.motif)

.c.mo:
	$(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) -g ${MOTIF_CFLAGS} ${X_CFLAGS} $<

.mo.motif:
	${LINK} $< $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) -lXm ${X_LIBS} -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS}

motif-tests: ${motif_TESTS}

${motif_TESTS}: $(AUX_OBJS)
endif
