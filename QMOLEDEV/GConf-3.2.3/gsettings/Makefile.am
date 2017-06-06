module_flags = -export_dynamic -avoid-version -module -no-undefined -export-symbols-regex '^g_io_module_(load|unload|query)'

giomodule_LTLIBRARIES = libgsettingsgconfbackend.la
giomoduledir = $(GIO_MODULE_DIR)

libgsettingsgconfbackend_la_SOURCES = \
	gconfsettingsbackend-module.c \
	gconfsettingsbackend.c        \
	gconfsettingsbackend.h

libgsettingsgconfbackend_la_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(GSETTINGS_CFLAGS)

libgsettingsgconfbackend_la_LDFLAGS = \
	$(module_flags)               \
	$(NULL)

libgsettingsgconfbackend_la_LIBADD =        \
	$(top_builddir)/gconf/libgconf-2.la \
	$(GSETTINGS_LIBS)                   \
	$(NULL)

dist_bin_SCRIPTS = gsettings-schema-convert
bin_PROGRAMS = gsettings-data-convert

AM_CPPFLAGS = \
	-I$(top_srcdir)         \
	-I$(top_builddir)       \
	-DDATADIR=\"$(datadir)\" \
	$(GSETTINGS_CFLAGS)

gsettings_data_convert_SOURCES = gsettings-data-convert.c
gsettings_data_convert_LDADD =              \
	$(top_builddir)/gconf/libgconf-2.la \
	$(GSETTINGS_LIBS)                   \
	$(NULL)

@INTLTOOL_DESKTOP_RULE@
autostartdir = $(sysconfdir)/xdg/autostart
autostart_in_files = gsettings-data-convert.desktop.in
autostart_DATA = $(autostart_in_files:.desktop.in=.desktop)

man_MANS =                       \
	gsettings-schema-convert.1 \
	gsettings-data-convert.1 \
	$(NULL)

%.1 : %.xml
	xsltproc -nonet http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl $<

EXTRA_DIST = gsettings-data-convert.xml gsettings-schema-convert.xml gsettings-data-convert.1 gsettings-schema-convert.1 gsettings-data-convert.desktop.in

dist-hook-local: $(BUILT_EXTRA_DIST)
	files='$(BUILT_EXTRA_DIST)';                      \
	for f in $$files; do                              \
	  if test -f $$f; then d=.; else d=$(srcdir); fi; \
	  cp $$d/$$f $(distdir) || exit 1; done

install-data-hook:
	if test -z "$(DESTDIR)" -a "$(GIO_QUERYMODULES)" != "no" ; then                         \
		$(GIO_QUERYMODULES) $(GIO_MODULE_DIR) ;                                         \
	fi

CLEANFILES = $(autostart_DATA)
