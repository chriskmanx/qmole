if TOOLKIT_COCOA

else
SUBDIRS = xp

plugindir = $(ABIWORD_PLUGINSDIR)
plugin_LTLIBRARIES = command.la

command_la_LIBADD = \
	xp/libxp.la

command_la_LDFLAGS = \
	$(COMMAND_LIBS) \
	-avoid-version \
	-module \
	-no-undefined

command_la_SOURCES = 

if TOOLKIT_WIN

plugin_DATA = \
	command.dll

CLEANFILES = \
	command.def \
	command.dll \
	command.dll.a

command.dll: .libs/libcommand.a
	$(CXX) -shared $^ -o $@ -Wl,--enable-auto-import -Wl,--out-implib,$@.a $(COMMAND_LIBS)

endif # TOOLKIT_WIN
endif
