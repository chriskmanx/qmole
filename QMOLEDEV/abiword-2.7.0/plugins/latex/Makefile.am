
if HAVE_LIBXSLT
SUBDIRS = xp xsltml
else
SUBDIRS = xp
endif

if LATEX_BUILTIN
noinst_LTLIBRARIES = latex.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = latex.la

if TOOLKIT_WIN

plugin_DATA = \
	latex.dll

CLEANFILES = \
	latex.def \
	latex.dll \
	latex.dll.a

latex.dll: .libs/liblatex.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(PLUGIN_LIBS)

endif # TOOLKIT_WIN

endif

latex_la_LIBADD = \
	xp/libxp.la

latex_la_LDFLAGS = \
	$(LATEX_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

latex_la_SOURCES = 
nodist_EXTRA_latex_la_SOURCES = dummy.cpp

EXTRA_DIST = \
	plugin.m4

