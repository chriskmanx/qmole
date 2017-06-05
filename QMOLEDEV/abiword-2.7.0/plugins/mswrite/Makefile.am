
SUBDIRS = xp

if MSWRITE_BUILTIN
noinst_LTLIBRARIES = mswrite.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = mswrite.la

if TOOLKIT_WIN

plugin_DATA = \
	mswrite.dll

CLEANFILES = \
	mswrite.def \
	mswrite.dll \
	mswrite.dll.a

mswrite.dll: .libs/libmswrite.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(MSWRITE_LIBS)

endif # TOOLKIT_WIN

endif

mswrite_la_LIBADD = \
	xp/libxp.la

mswrite_la_LDFLAGS = \
	$(MSWRITE_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

mswrite_la_SOURCES = 
nodist_EXTRA_mswrite_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

