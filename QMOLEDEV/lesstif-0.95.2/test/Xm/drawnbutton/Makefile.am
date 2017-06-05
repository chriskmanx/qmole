#
# $Header: /cvsroot/lesstif/lesstif/test/Xm/drawnbutton/Makefile.am,v 1.35 2008/01/02 19:48:21 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

LTTEST  = $(top_builddir)/common/libLtTest.a
AUX_OBJS =
noinst_PROGRAMS = test1  test2  test3  test4  test5  test6  test7  test8  \
                  test9  test10 test11


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

clean-local:
	rm -rf *.motif *.mo LessTif LessTif.sum LessTif.log site.exp xlab/*.p*

#test11_LDADD	= `xft-config --libs`
#test11_CFLAGS	= `xft-config --cflags`
test11_LDADD	= -L/usr/local/lib -lXft -lfreetype \
	-L/usr/local/lib -lfontconfig -L/usr/X11R6/lib -lXrender \
	${LDADD}
test11_CFLAGS	= -I/usr/local/include -I/usr/include/freetype2 \
	-I/usr/local/include -I/usr/X11R6/include

if MotifTests

motif_TESTS = $(OBJECTS:.o=.motif)

.c.mo: 
	$(CC) $(DEFS) $(MOTIF_CFLAGS) $(X_CFLAGS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -c -o $@ $< 

.mo.motif: 
	${LINK} $< $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) ${X_LIBS} -lXm -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS} 

motif-tests: ${motif_TESTS}

${motif_TESTS}: $(AUX_OBJS)
endif
