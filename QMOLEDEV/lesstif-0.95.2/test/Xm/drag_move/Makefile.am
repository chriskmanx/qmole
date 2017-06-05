#
# $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_move/Makefile.am,v 1.29 2008/01/02 19:48:21 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

AUX_OBJS =
noinst_PROGRAMS= test1
test1_SOURCES = test1.c callbacks.c drag_bitmap.c export_bitmap.c import_bitmap.c transfer_bitmap.c

noinst_HEADERS = bitmap.h transfer.h

LTTEST= $(top_builddir)/common/libLtTest.a

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

if MotifTests

motif_TESTS = $(OBJECTS:.o=.motif)
motif_OBJ = $(OBJECTS:.o=.mo)

.c.mo: 
	$(CC) $(DEFS) $(MOTIF_CFLAGS) $(X_CFLAGS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -c -o $@ $< 

.mo.motif: 
	${LINK} $(motif_OBJ) $(AUX_OBJS) @MOTIF_LIBS@ $(LTTEST) ${X_LIBS} -lXm -lXt ${X_PRE_LIBS} -lX11 ${X_EXTRA_LIBS} 

motif-tests: ${motif_TESTS}

${motif_TESTS}: $(AUX_OBJS) $(motif_OBJ)
endif
