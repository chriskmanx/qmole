
SUBDIRS = xp

if MIF_BUILTIN
noinst_LTLIBRARIES = mif.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = mif.la

if TOOLKIT_WIN

plugin_DATA = \
	mif.dll

CLEANFILES = \
	mif.def \
	mif.dll \
	mif.dll.a

mif.dll: .libs/libmif.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

mif_la_LIBADD = \
	xp/libxp.la

mif_la_LDFLAGS = \
	$(MIF_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

mif_la_SOURCES = 
nodist_EXTRA_mif_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

