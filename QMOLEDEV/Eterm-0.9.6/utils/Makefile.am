# $Id: Makefile.am 13745 2005-03-15 21:48:14Z mej $

bin_PROGRAMS = Esetroot Etbg Ettable
bin_SCRIPTS = Etcolors Etsearch kEsetroot Etbg_update_list

Esetroot_SOURCES = Esetroot.c
Esetroot_LDFLAGS = -rpath $(libdir):$(pkglibdir)

Etbg_SOURCES = Etbg.c
Ettable_SOURCES = Ettable.c

DISTCLEANFILES = kEsetroot
EXTRA_DIST = Etcolors Etsearch kEsetroot.in Etbg_update_list
MAINTAINERCLEANFILES=Makefile.in
