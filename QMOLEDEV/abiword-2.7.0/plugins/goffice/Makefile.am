
SUBDIRS = unix

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = goffice.la

goffice_la_LIBADD = \
	unix/libunix.la

goffice_la_LDFLAGS = \
	$(GOFFICE_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

goffice_la_SOURCES = 

EXTRA_DIST = \
	plugin.m4

if TOOLKIT_WIN

plugin_DATA = \
	goffice.dll

CLEANFILES = \
	goffice.def \
	goffice.dll \
	goffice.dll.a

goffice.dll: .libs/libgoffice.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(GOFFICE_LIBS)

endif # TOOLKIT_WIN

