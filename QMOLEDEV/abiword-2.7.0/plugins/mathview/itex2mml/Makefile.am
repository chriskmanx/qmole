
noinst_LTLIBRARIES = libitex2mml.la

AM_CPPFLAGS =

LEX_YY_C = lex.yy.c
Y_TAB_C = y.tab.c
Y_TAB_H = y.tab.h

libitex2mml_la_SOURCES = \
	itex2MML.h \
	$(LEX_YY_C) \
	$(Y_TAB_C) \
	$(Y_TAB_H)

YYPREFIX=itex2MML_yy

BUILT_SOURCES = \
	$(LEX_YY_C) \
	$(Y_TAB_C)

EXTRA_DIST = \
	itex2MML.l \
	itex2MML.y \
	README.itex2MML \
	test-itex.html

DISTCLEANFILES = \
	$(BUILT_SOURCES) \
	$(Y_TAB_H)

$(Y_TAB_H):	$(Y_TAB_C)

$(Y_TAB_C):	$(srcdir)/itex2MML.y
		$(YACC) -p $(YYPREFIX) -d $<

$(LEX_YY_C):	$(srcdir)/itex2MML.l
		$(LEX) -P$(YYPREFIX) -o$@ $<

