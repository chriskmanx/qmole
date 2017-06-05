
SUBDIRS = xp

if ISCII_BUILTIN
noinst_LTLIBRARIES = iscii.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = iscii.la

if TOOLKIT_WIN

plugin_DATA = \
	iscii.dll

CLEANFILES = \
	iscii.def \
	iscii.dll \
	iscii.dll.a

iscii.dll: .libs/libiscii.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

iscii_la_LIBADD = \
	xp/libxp.la

iscii_la_LDFLAGS = \
	$(ISCII_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

iscii_la_SOURCES = 
nodist_EXTRA_iscii_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

