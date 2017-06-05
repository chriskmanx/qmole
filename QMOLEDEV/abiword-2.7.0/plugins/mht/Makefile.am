
SUBDIRS = xp

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = mht.la

mht_la_LIBADD = \
	xp/libxp.la

mht_la_LDFLAGS = \
	$(MHT_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

mht_la_SOURCES = 
nodist_EXTRA_mht_la_SOURCES =  dummy.cpp

EXTRA_DIST = \
	plugin.m4

if TOOLKIT_WIN

plugin_DATA = \
	mht.dll

CLEANFILES = \
	mht.def \
	mht.dll \
	mht.dll.a

mht.dll: .libs/libmht.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(MHT_LIBS)

endif # TOOLKIT_WIN

