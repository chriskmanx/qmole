
SUBDIRS = xp

if EML_BUILTIN
noinst_LTLIBRARIES = eml.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = eml.la

if TOOLKIT_WIN

plugin_DATA = \
	eml.dll

CLEANFILES = \
	eml.def \
	eml.dll \
	eml.dll.a

eml.dll: .libs/libeml.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

eml_la_LIBADD = \
	xp/libxp.la

eml_la_LDFLAGS = \
	$(EML_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

eml_la_SOURCES = 
nodist_EXTRA_eml_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

