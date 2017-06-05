#
# API for glib in ATS
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Time: February, 2010
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc -Wall
GLIBCFLAGS = `pkg-config glib-2.0 --cflags`

######

GCC=gcc

######

all:: glibconfig
all:: atsctrb_glib.o
all:: clean

######

glibconfig: HATS/glibconfig_hats
	$(GCC) $(GLIBCFLAGS) -E -P -o HATS/glibconfig.hats -x c $<

######

atsctrb_glib.o: \
  glib_sats.o glib_dats.o
	ld -r -o $@ \
  glib_sats.o glib_dats.o

######

glib_sats.o: SATS/glib.sats
	$(ATSCC) $(GLIBCFLAGS) -o $@ -c SATS/glib.sats

glib_dats.o: DATS/glib.dats
	$(ATSCC) $(GLIBCFLAGS) -o $@ -c DATS/glib.dats

######

clean::
	rm -f glib_sats.c glib_sats.o
	rm -f glib_dats.c glib_dats.o

cleanall: clean
	rm -f atsctrb_glib.o

###### end of [Makefile] ######
