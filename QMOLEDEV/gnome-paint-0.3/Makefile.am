## Process this file with automake to produce Makefile.in
## Created by Anjuta

SUBDIRS = src po \
	data

gnome_paintdocdir = ${prefix}/doc/gnome-paint
gnome_paintdoc_DATA = \
	README\
	COPYING\
	AUTHORS\
	ChangeLog\
	INSTALL\
	NEWS


INTLTOOL_FILES = intltool-extract.in \
	intltool-merge.in \
	intltool-update.in

EXTRA_DIST = $(gnome_paintdoc_DATA) \
	$(INTLTOOL_FILES)\
	$(ui_DATA)

DISTCLEANFILES = intltool-extract \
	intltool-merge \
	intltool-update \
	po/.intltool-merge-cache

# Remove doc directory on uninstall
uninstall-local:
	-rm -r $(gnome_paintdocdir)
