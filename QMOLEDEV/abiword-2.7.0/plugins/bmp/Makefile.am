
SUBDIRS = xp

if BMP_BUILTIN
noinst_LTLIBRARIES = bmp.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = bmp.la

if TOOLKIT_WIN

plugin_DATA = \
	bmp.dll

CLEANFILES = \
	bmp.def \
	bmp.dll \
	bmp.dll.a

bmp.dll: .libs/libbmp.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(BMP_LIBS)

endif # TOOLKIT_WIN

endif

bmp_la_LIBADD = \
	xp/libxp.la

bmp_la_LDFLAGS = \
	$(BMP_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

bmp_la_SOURCES = 

EXTRA_DIST = \
	plugin.m4

