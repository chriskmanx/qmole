#
# $Header: /cvsroot/lesstif/lesstif/lib/config/Makefile.am,v 1.7 2003/03/17 20:34:18 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in
bin_SCRIPTS=	mxmkmf

configdir=	$(libdir)/LessTif/config

EXTRA_DIST =    LessTif.rules LessTif.tmpl host.def Imake.tmpl

if XIsR6
config_DATA=	LessTif.rules LessTif.tmpl host.def
else
# so being R5 
config_DATA=	Imake.tmpl LessTif.rules LessTif.tmpl
endif
