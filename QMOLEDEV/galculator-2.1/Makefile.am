## Process this file with automake to produce Makefile.in

desktopdir = $(datadir)/applications
desktop_in_files = galculator.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

CLEANFILES = $(desktop_DATA)

SUBDIRS = src po ui doc pixmaps

# things we want to include in the source code distribution

EXTRA_DIST = galculator.spec \
	galculator.spec.in \
	galculator.desktop.in \
	intltool-merge.in \
	intltool-extract.in \
	intltool-update.in \
	ABOUT-NLS

