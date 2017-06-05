
SUBDIRS = linkgrammarwrap xp

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = grammar.la

grammar_la_LIBADD = \
	linkgrammarwrap/liblinkgrammarwrap.la \
	xp/libxp.la

grammar_la_LDFLAGS = \
	$(GRAMMAR_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

grammar_la_SOURCES = 

if TOOLKIT_WIN

plugin_DATA = \
	grammar.dll

CLEANFILES = \
	grammar.def \
	grammar.dll \
	grammar.dll.a

grammar.dll: .libs/libgrammar.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(GRAMMAR_LIBS)

endif # TOOLKIT_WIN

