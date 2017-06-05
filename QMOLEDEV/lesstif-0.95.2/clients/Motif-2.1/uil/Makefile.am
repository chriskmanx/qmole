#
# $Header: /cvsroot/lesstif/lesstif/clients/Motif-2.1/uil/Makefile.am,v 1.15 2005/03/26 06:52:24 dannybackx Exp $
#

bin_PROGRAMS=	uil
noinst_PROGRAMS   = newuil

MAINTAINERCLEANFILES=Makefile.in lex.c yacc.c yacc.h

uil_SOURCES = \
		Attribute.c Callback.c Expression.c ExpressionList.c \
		FakeWidget.c Include.c MrmTypes.c glue.c yacc.y lex.l \
		main.c \
		Attribute.h Callback.h Expression.h ExpressionList.h \
		FakeWidget.h Include.h MrmTypes.h glue.h main.h

# non-local stuff
BUILT_SOURCES = lex.c yacc.c yacc.h

# Special dependencies
yacc.h:	yacc.c
lex.o:	yacc.h

AM_CPPFLAGS=	\
                -I$(top_srcdir)/include/Motif-2.1 \
		-I../../../include/Motif-2.1 \
		-I$(top_srcdir)/lib/Mrm-2.1 \
		-I../../../lib/Mrm-2.1 \
		$(X_CFLAGS)

LDADD=		../../../lib/Mrm-2.1/libMrm.la \
		../../../lib/Xm-2.1/libXm.la \
		$(X_LIBS) $(XPLIB) -lXt $(X_PRE_LIBS) -lX11 $(X_EXTRA_LIBS)

newuil_LDADD = ../../../lib/Uil-2.1/libUil.la $(LDADD)
newuil_SOURCES=	newuil.c

AM_YFLAGS=		-d
