#
# $Header: /cvsroot/lesstif/lesstif/test/Mrm/xmp/Makefile.am,v 1.10 2005/03/26 06:52:26 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in demo.uid

AUX_OBJS =
noinst_PROGRAMS= demo

EXTRA_DIST=Xmp.uil demo.uil

demo_SOURCES = XmpMrm.c Grid.c String.c Blob.c demo.c \
               Blob.h  BlobP.h  Grid.h  GridP.h  String.h  StringP.h  XmpMrm.h

demo.uid: demo.uil
	if test -x $(top_builddir)/../clients/Motif$(include_version_suffix)/uil/uil ; 	then \
	libtool --mode=execute $(top_builddir)/../clients/Motif$(include_version_suffix)/uil/uil \
                               $(top_srcdir)/Mrm/xmp/demo.uil -o demo.uid ; 	else \
	uil $(top_srcdir)/Mrm/xmp/demo.uil -o demo.uid ; 	fi
 
LTTEST= $(top_builddir)/common/libLtTest.a
LDADD	= $(AUX_OBJS) \
	  $(top_builddir)/../lib/Mrm$(version_suffix)/libMrm.la  \
	  $(top_builddir)/../lib/Xm$(version_suffix)/libXm.la $(XPLIB) \
	  ${X_LIBS} -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS}

AM_CPPFLAGS= -I$(top_srcdir)/../include/Motif$(include_version_suffix) \
	  -I$(top_builddir)/../include/Motif$(include_version_suffix) \
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
