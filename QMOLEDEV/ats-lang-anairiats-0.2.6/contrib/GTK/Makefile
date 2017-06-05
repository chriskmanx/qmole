#
# API for GTK in ATS
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Time: May, 2010
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif # end of [ifeq]

######

ATSCC=$(ATSUSRQ)/bin/atscc -Wall
GTKCFLAGS = `pkg-config gtk+-2.0 --cflags`

######

all:: atsctrb_GTK.o
all:: clean

######

atsctrb_GTK.o: GTK_dats.o
	ld -r -o $@ GTK_dats.o

######

GTK_dats.o: DATS/gtk.dats
	$(ATSCC) $(GTKCFLAGS) -o $@ -c DATS/gtk.dats

######

RMF=rm -f

######

clean:: ; $(RMF) *_?ats.c *_?ats.o

cleanall:: clean
cleanall:: ; $(RMF) atsctrb_GTK.o

###### end of [Makefile] ######
