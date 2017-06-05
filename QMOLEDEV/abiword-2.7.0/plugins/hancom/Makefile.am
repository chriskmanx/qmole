
SUBDIRS = xp

if HANCOM_BUILTIN
noinst_LTLIBRARIES = hancom.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = hancom.la

if TOOLKIT_WIN

plugin_DATA = \
	hancom.dll

CLEANFILES = \
	hancom.def \
	hancom.dll \
	hancom.dll.a

hancom.dll: .libs/libhancom.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(HANCOM_LIBS)

endif # TOOLKIT_WIN

endif

hancom_la_LIBADD = \
	xp/libxp.la

hancom_la_LDFLAGS = \
	$(HANCOM_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

hancom_la_SOURCES = 
nodist_EXTRA_hancom_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

