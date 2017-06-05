
if TOOLKIT_COCOA
SUBDIRS =
else
SUBDIRS = backends core

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = collab.la

collab_la_LIBADD = \
	backends/libbackends.la \
	core/libcore.la

collab_la_LDFLAGS = \
	$(COLLAB_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

collab_la_SOURCES = 
nodist_EXTRA_collab_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

if TOOLKIT_WIN

plugin_DATA = \
	collab.dll

CLEANFILES = \
	collab.def \
	collab.dll \
	collab.dll.a

collab.dll: .libs/libcollab.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(COLLAB_LIBS)

endif # TOOLKIT_WIN

endif # TOOLKIT_COCOA
