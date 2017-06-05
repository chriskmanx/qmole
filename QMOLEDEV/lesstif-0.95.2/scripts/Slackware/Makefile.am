#
# $Header: /cvsroot/lesstif/lesstif/scripts/Slackware/Makefile.am,v 1.2 1998/03/02 22:56:33 miers Exp $
#
MAINTAINERCLEANFILES=Makefile.in

EXTRA_DIST=	install.end maketag tagfile

RELEASE=@major_shared@.@minor_shared@

@PACKAGE@-@VERSION@-slackware-bin.tgz:	disklesstif1 doinst.sh install.end maketag tagfile
	install -d lesstif/usr/include/Xm
	install ../../include/Motif-1.2/Xm/*.h lesstif/usr/include/Xm
	install -d lesstif/usr/include/Mrm
	install ../../include/Motif-1.2/Mrm/*.h lesstif/usr/include/Mrm
	install -d lesstif/usr/X11/lib
	install -s ../../lib/Xm/libXm.so.${RELEASE} lesstif/usr/X11/lib
	install -s ../../lib/Mrm/libMrm.so.${RELEASE} lesstif/usr/X11/lib
	install -s ../../lib/Uil/libUil.so.${RELEASE} lesstif/usr/X11/lib
	install -d lesstif/usr/X11/bin
	install -s ../../clients/Motif-1.2/mwm/mwm lesstif/usr/X11/bin
	install -s ../../clients/Motif-1.2/uil/uil lesstif/usr/X11/bin
	install -s ../../clients/Motif-1.2/xmbind/xmbind lesstif/usr/X11/bin
	install -d lesstif/usr/lib/X11/app-defaults
	install ../../clients/Motif-1.2/mwm/Mwm lesstif/usr/lib/X11/app-defaults
	install -d lesstif/usr/lib/X11/mwm
	install ../../clients/Motif-1.2/mwm/system.mwmrc lesstif/usr/lib/X11/mwm
	install -d lesstif/install
	install doinst.sh lesstif/install
	tar zcf lesstif/lesstif.tgz -C lesstif usr install
	rm -r lesstif/usr lesstif/install
	install disklesstif1 lesstif
	install install.end lesstif
	install maketag lesstif
	install tagfile lesstif
	install tagfile lesstif/tagfile.org
	tar zcf $@ lesstif
	rm -r lesstif
