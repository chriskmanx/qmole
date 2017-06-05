
SUBDIRS = data itex2mml xp

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = mathview.la

mathview_la_LIBADD = \
	itex2mml/libitex2mml.la \
	xp/libxp.la

mathview_la_LDFLAGS = \
	$(MATHVIEW_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

mathview_la_SOURCES = 

EXTRA_DIST = \
	plugin.m4

if TOOLKIT_WIN

plugin_DATA = \
	mathview.dll

CLEANFILES = \
	mathview.def \
	mathview.dll \
	mathview.dll.a

mathview.dll: .libs/libmathview.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(MATHVIEW_LIBS)

endif # TOOLKIT_WIN

