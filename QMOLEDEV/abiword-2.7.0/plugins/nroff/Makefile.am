
SUBDIRS = xp

if NROFF_BUILTIN
noinst_LTLIBRARIES = nroff.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = nroff.la

if TOOLKIT_WIN

plugin_DATA = \
	nroff.dll

CLEANFILES = \
	nroff.def \
	nroff.dll \
	nroff.dll.a

nroff.dll: .libs/libnroff.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

nroff_la_LIBADD = \
	xp/libxp.la

nroff_la_LDFLAGS = \
	$(PLUGIN_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

nroff_la_SOURCES = 
nodist_EXTRA_nroff_la_SOURCES =  dummy.cpp

EXTRA_DIST = \
	plugin.m4

