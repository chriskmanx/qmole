#
# $Header: /cvsroot/lesstif/lesstif/scripts/FreeBSD/Makefile.am,v 1.11 2001/08/29 16:06:48 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

EXTRA_DIST=	COMMENT DESCR
CLEANFILES=	lesstif-@VERSION@-freebsd.tgz PLIST

package:	lesstif-@VERSION@-freebsd

lesstif-@VERSION@-freebsd:	PLIST
	/usr/sbin/pkg_create -c $(srcdir)/COMMENT -d $(srcdir)/DESCR -f PLIST $@

PLIST:
	echo "@cwd " @prefix@ >PLIST
	echo "@srcdir " @prefix@ >>PLIST
	echo "@mode 0755" >>PLIST
	find @prefix@/bin -type f -print | sed -e "s:@prefix@/::" >>PLIST
	echo @ACLOCALDIR@/ac_find_xbae.m4 >>PLIST
	echo @ACLOCALDIR@/ac_find_xlt.m4 >>PLIST
	echo @ACLOCALDIR@/ac_find_xpm.m4 >>PLIST
	echo @ACLOCALDIR@/ac_find_motif.m4 >>PLIST
	echo "@mode 0644" >>PLIST
	find @prefix@/include @prefix@/lib @prefix@/LessTif -type f -print | sed -e "s:@prefix@/::" >>PLIST
if Version_2_1
	echo "@exec ln -s %D/LessTif/Motif-2.1/lib/libXm.so.2 %D/lib/libXm.so.2" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.1/lib/libXm.so.2 %D/lib/libXm.so" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.1/lib/libMrm.so.2 %D/lib/libMrm.so.2" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.1/lib/libMrm.so.2 %D/lib/libMrm.so" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.1/lib/libXm.la %D/lib/libXm.la" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.1/lib/libMrm.la %D/lib/libMrm.la" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.1/include/Xm %D/include/Xm" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.1/include/Mrm %D/include/Mrm" >>PLIST
endif
if Version_2_0
	echo "@exec ln -s %D/LessTif/Motif-2.0/lib/libXm.so.2 %D/lib/libXm.so.2" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.0/lib/libXm.so.2 %D/lib/libXm.so" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.0/lib/libMrm.so.2 %D/lib/libMrm.so.2" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.0/lib/libMrm.so.2 %D/lib/libMrm.so" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.0/lib/libXm.la %D/lib/libXm.la" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.0/lib/libMrm.la %D/lib/libMrm.la" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.0/include/Xm %D/include/Xm" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-2.0/include/Mrm %D/include/Mrm" >>PLIST
endif
if Version_1_2
	echo "@exec ln -s %D/LessTif/Motif-1.2/lib/libXm.so.1 %D/lib/libXm.so.1" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-1.2/lib/libXm.so.1 %D/lib/libXm.so" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-1.2/lib/libMrm.so.1 %D/lib/libMrm.so.1" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-1.2/lib/libMrm.so.1 %D/lib/libMrm.so" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-1.2/lib/libXm.la %D/lib/libXm.la" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-1.2/lib/libMrm.la %D/lib/libMrm.la" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-1.2/include/Xm %D/include/Xm" >>PLIST
	echo "@exec ln -s %D/LessTif/Motif-1.2/include/Mrm %D/include/Mrm" >>PLIST
endif
	echo "@exec /sbin/ldconfig -m %D/lib" >>PLIST
	echo "@exec more %D/README" >>PLIST
