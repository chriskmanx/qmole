
SUBDIRS = unix

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = gda.la

gda_la_LIBADD = \
	unix/libunix.la

gda_la_LDFLAGS = \
	$(GDA_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

gda_la_SOURCES = 

EXTRA_DIST = \
	plugin.m4

if TOOLKIT_WIN

plugin_DATA = \
	gda.dll

CLEANFILES = \
	gda.def \
	gda.dll \
	gda.dll.a

gda.dll: .libs/libgda.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(GDA_LIBS)

endif # TOOLKIT_WIN

