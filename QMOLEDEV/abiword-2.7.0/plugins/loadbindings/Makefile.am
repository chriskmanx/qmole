
SUBDIRS = xp

if LOADBINDINGS_BUILTIN
noinst_LTLIBRARIES = loadbindings.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = loadbindings.la

if TOOLKIT_WIN

plugin_DATA = \
	loadbindings.dll

CLEANFILES = \
	loadbindings.def \
	loadbindings.dll \
	loadbindings.dll.a

loadbindings.dll: .libs/libloadbindings.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(LOADBINDINGS_LIBS)

endif # TOOLKIT_WIN

endif

loadbindings_la_LIBADD = \
	xp/libxp.la

loadbindings_la_LDFLAGS = \
	$(LOADBINDINGS_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

loadbindings_la_SOURCES = 
nodist_EXTRA_loadbindings_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

