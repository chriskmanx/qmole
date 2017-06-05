
noinst_LTLIBRARIES = libwin.la

AM_CPPFLAGS = \
	$(AIKSAURUS_CFLAGS)

libwin_la_LIBADD = \
	Aiksaurus.lo

libwin_la_DEPENDENCIES = \
	Aiksaurus.lo

libwin_la_SOURCES = \
	AiksaurusApp.cpp \
	AiksaurusApp.h \
	AiksaurusDlg.cpp \
	AiksaurusDlg.h \
	resource.h

EXTRA_DIST = \
	Aiksaurus.ico \
	Aiksaurus.rc \
	back_hover.ico \
	back.ico \
	forward_hover.ico \
	forward.ico \
	readme.txt \
	search_hover.ico \
	search.ico

Aiksaurus.lo: $(srcdir)/Aiksaurus.rc
	$(top_srcdir)/lt-compile-resource $< $@

