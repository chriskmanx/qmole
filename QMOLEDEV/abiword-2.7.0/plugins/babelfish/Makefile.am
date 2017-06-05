
SUBDIRS = xp

if BABELFISH_BUILTIN
noinst_LTLIBRARIES = babelfish.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = babelfish.la

if TOOLKIT_WIN

plugin_DATA = \
	babelfish.dll

CLEANFILES = \
	babelfish.def \
	babelfish.dll \
	babelfish.dll.a

babelfish.dll: .libs/libbabelfish.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

babelfish_la_LIBADD = \
	xp/libxp.la

babelfish_la_LDFLAGS = \
	$(BABELFISH_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

babelfish_la_SOURCES =

nodist_EXTRA_babelfish_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

