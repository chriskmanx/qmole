#
# API for clapack in ATS
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
LIBFLAGS=-L/usr/lib/atlas

######

all: atsctrb_clapack.o clean

######

atsctrb_clapack.o: clapack_dats.o
	ld -r -o $@ clapack_dats.o

######

clapack_dats.o: DATS/clapack.dats
	$(ATSCC) $(LIBFLAGS) -o $@ -c DATS/clapack.dats

######

clean::
	rm -f *_?ats.c *_?ats.o

cleanall: clean
	rm -f atsctrb_clapack.o

###### end of [Makefile] ######
