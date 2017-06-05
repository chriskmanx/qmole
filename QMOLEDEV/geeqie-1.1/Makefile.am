## Process this file with automake to produce Makefile.in.

include $(top_srcdir)/aminclude.am

SUBDIRS = src po doc plugins
DIST_SUBDIRS = src po doc plugins

man_MANS = geeqie.1

readmedir = @readmedir@
readme_DATA = README COPYING ChangeLog TODO README.lirc

desktopdir = $(datadir)/applications
desktop_in_files = geeqie.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

icondir = $(datadir)/pixmaps
icon_DATA = geeqie.png

EXTRA_DIST = \
	$(readme_DATA)	\
	$(desktop_DATA)	\
	$(desktop_in_files)	\
	$(icon_DATA)	\
	geeqie.spec.in	\
	geeqie.1	\
	doxygen.conf

dist-hook: geeqie.spec
	cp $(top_builddir)/geeqie.spec $(distdir)

DISTCLEANFILES = config.report
