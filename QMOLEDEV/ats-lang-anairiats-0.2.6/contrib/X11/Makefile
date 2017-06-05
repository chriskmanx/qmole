#
# API for Xlib in ATS
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Time: January, 2010
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSHOME),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc -Wall

######

all: atsctrb_X11.o clean

######

atsctrb_X11.o: \
  X_sats.o \
  Xlib_sats.o Xlib_dats.o
	ld -r -o $@ \
  X_sats.o \
  Xlib_sats.o Xlib_dats.o

######

X_sats.o: SATS/X.sats
	$(ATSCC) -IX11 -o $@ -c SATS/X.sats

######

Xlib_sats.o: SATS/Xlib.sats
	$(ATSCC) -IX11 -o $@ -c SATS/Xlib.sats

Xlib_dats.o: DATS/Xlib.dats
	$(ATSCC) -IX11 -o $@ -c DATS/Xlib.dats

######

clean::
	rm -f X_sats.c X_sats.o
	rm -f Xlib_sats.c Xlib_sats.o
	rm -f Xlib_dats.c Xlib_dats.o

cleanall: clean
	rm -f atsctrb_Xlib.o

###### end of [Makefile] ######
