#
# $Header: /cvsroot/lesstif/lesstif/test/Mrm/Makefile.am,v 1.5 2002/02/18 14:07:13 amai Exp $
#
MAINTAINERCLEANFILES=Makefile.in

SUBDIRS	=  . ch22 mrm xmp

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
