#
# $Header: /cvsroot/lesstif/lesstif/test/Makefile.am,v 1.24 2002/05/18 14:28:47 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in

bin_SCRIPTS   = testall

DIST_SUBDIRS = common config Xm Xm-2.0 Xm-2.1 Mrm Uil extra

CommonDirs = . common config Xm Mrm Uil

if Version_2_1
    SUBDIRS = $(CommonDirs) Xm-2.0 Xm-2.1
else
if Version_2_0
    SUBDIRS = $(CommonDirs) Xm-2.0
else
    SUBDIRS = $(CommonDirs)
endif
endif


if MotifTests

motif-tests:
	for subdir in $(SUBDIRS); do \
		(cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) motif-tests); \
	done

endif
