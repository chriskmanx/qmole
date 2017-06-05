#!/bin/sh

#
# For running some tests with the newly built compiler
# Author: Matthew Danish (md AT cs DOT bu DOT edu)
# Time: 2010
#

######

PACKAGE_TARNAME=ats-anairiats
PACKAGE_VERSION=0.2.6

######

CURDIR="`pwd`"
ATSHOME="$CURDIR"
ATSHOMERELOC="ATS-$PACKAGE_VERSION"

TESTDIR=doc/EXAMPLE/TEST
make -C ${TESTDIR} cleanall
make -C ${TESTDIR} checkall
EXITCODE=$?
exit $EXITCODE

###### end of [test.sh.in] ######
