#
# $Header: /cvsroot/lesstif/lesstif/scripts/Makefile.am,v 1.15 2005/01/16 10:24:04 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in typedefs.c

SUBDIRS= . FreeBSD RedHat Slackware autoconf OS2

EXTRA_DIST = LTsysinfo

dist_bin_SCRIPTS=motif-config

if LT_BUILD_MAN2HTML
noinst_PROGRAMS=man2html
endif

# alternative approach:
#
#EXTRA_DIST= man2html.c
# Rule to build the man2html utility on the build host, not for the
# target system!
# If this fails one has to build and install it manually, afterwards
# configure should use that installed one ...
# The source for our man2html is believed to be clean enough that
# such a simple rule should work out.
#if LT_BUILD_MAN2HTML
#noinst_SCRIPTS   = man2html
#man2html: man2html.c
#	cc -o man2html $<
#endif

EXTRA_PROGRAMS    = cvs2patch typedefs mkinline

typedefs_SOURCES  = typedefs.l
cvs2patch_SOURCES = cvs2patch.c
