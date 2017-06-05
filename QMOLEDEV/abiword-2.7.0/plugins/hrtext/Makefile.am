
SUBDIRS = xp

if HRTEXT_BUILTIN
noinst_LTLIBRARIES = hrtext.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = hrtext.la

if TOOLKIT_WIN

plugin_DATA = \
	hrtext.dll

CLEANFILES = \
	hrtext.def \
	hrtext.dll \
	hrtext.dll.a

hrtext.dll: .libs/libhrtext.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

hrtext_la_LIBADD = \
	xp/libxp.la

hrtext_la_LDFLAGS = \
	$(HRTEXT_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

hrtext_la_SOURCES = 
nodist_EXTRA_hrtext_la_SOURCES = dummy.cpp 

EXTRA_DIST = \
	plugin.m4

