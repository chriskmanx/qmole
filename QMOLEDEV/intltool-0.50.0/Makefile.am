## Process this file with automake to produce Makefile.in

SUBDIRS = tests doc

# Scripts which are bundled with all intltoolized packages:
distributed_scripts =				\
	intltool-extract			\
	intltool-merge				\
	intltool-update

# The intltool scripts
bin_SCRIPTS =					\
	$(distributed_scripts)			\
	intltoolize				\
	intltool-prepare

# These are required by intltoolize
dist_pkgdata_DATA =	Makefile.in.in

# This macro file should be visible to Automake's aclocal.
dist_aclocal_DATA = intltool.m4

CLEANFILES =					\
	$(bin_SCRIPTS)				\
	$(EXTRA_SCRIPTS)

EXTRA_DIST =					\
	$(distributed_scripts:=.in)		\
	intltool-prepare.in

edit = sed -e 's|[@]PACKAGE@|$(PACKAGE)|g' \
	   -e 's|[@]VERSION@|$(VERSION)|g' \
	   -e "s|[@]INTLTOOL_PERL@|$(PERL)|g" \
	   -e "s|[@]INTLTOOL_LIBDIR@|$(libdir)|g"

%: %.in Makefile
	$(edit) < $< > $@
	chmod +x $@
