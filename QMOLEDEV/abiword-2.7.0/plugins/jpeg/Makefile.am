
SUBDIRS = xp

if JPEG_BUILTIN
noinst_LTLIBRARIES = jpeg.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = jpeg.la

if TOOLKIT_WIN

plugin_DATA = \
	jpeg.dll

CLEANFILES = \
	jpeg.def \
	jpeg.dll \
	jpeg.dll.a

jpeg.dll: .libs/libjpeg.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(JPEG_LIBS)

endif # TOOLKIT_WIN

endif

jpeg_la_LIBADD = \
	xp/libxp.la

jpeg_la_LDFLAGS = \
	$(JPEG_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

jpeg_la_SOURCES = 

EXTRA_DIST = \
	plugin.m4

