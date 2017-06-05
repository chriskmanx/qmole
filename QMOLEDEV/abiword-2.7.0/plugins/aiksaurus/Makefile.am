
DIST_SUBDIRS = win xp
SUBDIRS = xp

if TOOLKIT_WIN
SUBDIRS += win
platform_lib = win/libwin.la
endif

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = aiksaurus.la

aiksaurus_la_LIBADD = \
	$(platform_lib) \
	xp/libxp.la

aiksaurus_la_LDFLAGS = \
	$(AIKSAURUS_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

aiksaurus_la_SOURCES = 

if TOOLKIT_WIN

plugin_DATA = \
	aiksaurus.dll

CLEANFILES = \
	aiksaurus.def \
	aiksaurus.dll \
	aiksaurus.dll.a

aiksaurus.dll: .libs/libaiksaurus.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(AIKSAURUS_LIBS)

endif # TOOLKIT_WIN

