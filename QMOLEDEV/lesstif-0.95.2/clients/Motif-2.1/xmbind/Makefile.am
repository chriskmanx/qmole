#
# $Header: /cvsroot/lesstif/lesstif/clients/Motif-2.1/xmbind/Makefile.am,v 1.9 2005/03/26 06:52:25 dannybackx Exp $
#

if Version_2_1
bin_PROGRAMS=	xmbind
endif

MAINTAINERCLEANFILES=Makefile.in

SRCS_1_2 = \
  	xmbind.c
SRCS_2_0 = 
SRCS_2_1 = 

# non-local stuff
BUILT_SOURCES =

xmbind_SOURCES =  ${SRCS_1_2:%=../../Motif-1.2/xmbind/%} \
                  ${SRCS_2_0:%=../../Motif-2.0/xmbind/%} \
		  $(SRCS_2_1)


$(SRCS_1_2):
	rm -f $@
	$(LN_S) $(srcdir)/../../Motif-1.2/xmbind/$@ $@

$(SRCS_2_0):
	rm -f $@
	$(LN_S) $(srcdir)/../../Motif-2.0/xmbind/$@ $@


AM_CPPFLAGS=	-I$(top_srcdir)/clients/Motif-2.0/xmbind \
                -I$(top_srcdir)/clients/Motif-1.2/xmbind \
                -I$(top_srcdir)/include/Motif-2.1 \
		-I../../../include/Motif-2.1 \
		-I$(top_srcdir)/include/Motif-2.0 \
		-I../../../include/Motif-2.0 \
		-I$(top_srcdir)/include/Motif-1.2 \
		-I../../../include/Motif-1.2 \
		$(X_CFLAGS)

LDADD=		../../../lib/Xm-2.1/libXm.la \
		$(X_LIBS) $(XPLIB) -lXt $(X_PRE_LIBS) -lX11 $(X_EXTRA_LIBS)


clean-local:
	@for i in $(BUILT_SOURCES) $(SRCS_1_2) $(SRCS_2_0) ; do \
	   if test -r $$i; then \
	      rm $$i; \
	   fi ; \
	   done
