
SUBDIRS = common exp imp

if OPENDOCUMENT_BUILTIN
noinst_LTLIBRARIES = opendocument.la

else

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = opendocument.la

if TOOLKIT_WIN

plugin_DATA = \
	opendocument.dll

CLEANFILES = \
	opendocument.def \
	opendocument.dll \
	opendocument.dll.a

opendocument.dll: .libs/libopendocument.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(OPENDOCUMENT_LIBS)

endif # TOOLKIT_WIN

endif # OPENDOCUMENT_BUILTIN

opendocument_la_LIBADD = \
	common/libcommon.la \
	exp/libexp.la \
	imp/libimp.la

opendocument_la_LDFLAGS = \
	$(OPENDOCUMENT_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

opendocument_la_SOURCES = 

# needed to call the C++ ld driver
nodist_EXTRA_opendocument_la_SOURCES = dummy.cpp

#install-exec-hook:
#	install_name_tool -change $(prefix)/lib/libabiword-@ABIWORD_SERIES@.dylib \
#		@executable_path/../Frameworks/libabiword-@ABIWORD_SERIES@.dylib \
#		$(DESTDIR)/$(plugindir)/opendocument.so

EXTRA_DIST = \
	plugin.m4

