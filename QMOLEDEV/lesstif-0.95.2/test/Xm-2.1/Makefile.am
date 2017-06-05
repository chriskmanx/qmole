#
# $Header: /cvsroot/lesstif/lesstif/test/Xm-2.1/Makefile.am,v 1.7 2002/02/18 14:07:19 amai Exp $
#

MAINTAINERCLEANFILES=Makefile.in

SUBDIRS	= . messages print translations

if MotifTests

motif-tests:
	for subdir in $(SUBDIRS); do \
		(cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) motif-tests); \
	done

endif

bin_SCRIPTS   = testall
testall: ../testall.in
	(cd .. && $(MAKE) testall)
	cp ../testall ./testall
	chmod 755 testall
