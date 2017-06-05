
SUBDIRS = xp

if GIMP_BUILTIN
noinst_LTLIBRARIES = gimp.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = gimp.la

if TOOLKIT_WIN

plugin_DATA = \
	gimp.dll

CLEANFILES = \
	gimp.def \
	gimp.dll \
	gimp.dll.a

gimp.dll: .libs/libgimp.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

gimp_la_LIBADD = \
	xp/libxp.la

gimp_la_LDFLAGS = \
	$(GIMP_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

gimp_la_SOURCES = 
nodist_EXTRA_gimp_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

