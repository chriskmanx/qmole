
SUBDIRS = xp

if APPLIX_BUILTIN
noinst_LTLIBRARIES = applix.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = applix.la

if TOOLKIT_WIN

plugin_DATA = \
	applix.dll

CLEANFILES = \
	applix.def \
	applix.dll \
	applix.dll.a

applix.dll: .libs/libapplix.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(APPLIX_LIBS)

endif # TOOLKIT_WIN

endif # APPLIX_BUILTIN

applix_la_LIBADD = \
	xp/libxp.la

applix_la_LDFLAGS = \
	$(APPLIX_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

applix_la_SOURCES = 

# needed to call the C++ ld driver
nodist_EXTRA_applix_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

