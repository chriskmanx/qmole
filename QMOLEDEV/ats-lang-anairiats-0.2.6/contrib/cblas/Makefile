#
# API for cblas in ATS
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Author: Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
# Time: July, 2010
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc -Wall

LD=ld
LIBFLAGS=-L/usr/lib/atlas

######

all: atsctrb_cblas.o clean

######

atsctrb_cblas.o: \
  cblas_dats.o cblas_extra_dats.o ; $(LD) -r -o $@ $^

######

cblas_dats.o: \
  DATS/cblas.dats; $(ATSCC) $(LIBFLAGS) -o $@ -c DATS/cblas.dats
cblas_extra_dats.o: \
  DATS/cblas_extra.dats; $(ATSCC) $(LIBFLAGS) -o $@ -c DATS/cblas_extra.dats

######

clean::
	rm -f *_?ats.c *_?ats.o

cleanall: clean
	rm -f atsctrb_cblas.o

###### end of [Makefile] ######
