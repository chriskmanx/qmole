#
# $Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/translations/Makefile.am,v 1.4 2005/03/26 06:52:34 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

AUX_OBJS =
if Version_2_1
noinst_PROGRAMS = test1  test2
else
noinst_PROGRAMS =
endif

LDADD	= $(AUX_OBJS) \
	  $(top_builddir)/../lib/Xm$(version_suffix)/libXm.la $(XPLIB) \
	  @X_LIBS@ -lXt -lX11


AM_CPPFLAGS	= -I$(top_srcdir)/../include/Motif-2.1 \
		  -I$(top_builddir)/../include/Motif-2.1 \
 		  -I$(top_srcdir)/../include/Motif-2.0 \
		  -I$(top_srcdir)/../include/Motif-1.2
SUFFIXES = .motif .mo

clean-local:
	rm -f *.motif .mo

if MotifTests

motif_TESTS = $(OBJECTS:.o=.motif)

.c.mo:
	$(CC) -c -o $@ $(CPPFLAGS) -g @MOTIF_CFLAGS@ $<

.mo.motif:
	${LINK} $< $(AUX_OBJS) @MOTIF_LIBS@ -lXm @X_LIBS@ -lXt -lX11

motif-tests: ${motif_TESTS}

${motif_TESTS}: $(AUX_OBJS)

endif
