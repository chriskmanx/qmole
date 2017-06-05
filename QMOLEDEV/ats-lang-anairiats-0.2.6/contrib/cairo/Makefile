#
# API for cairo in ATS
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Time: December, 2009
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc -Wall
XRCFLAGS=`pkg-config cairo --cflags`

######

all:: atsctrb_cairo.o
all:: clean

######

atsctrb_cairo.o: \
  cairo_dats.o cairo_extra_dats.o
	ld -r -o $@ $^

######

cairo_dats.o: DATS/cairo.dats
	$(ATSCC) $(XRCFLAGS) -o $@ -c DATS/cairo.dats

cairo_extra_dats.o: DATS/cairo_extra.dats
	$(ATSCC) $(XRCFLAGS) -o $@ -c DATS/cairo_extra.dats

######

clean::
	rm -f *_?ats.c *_?ats.o

cleanall: clean
	rm -f atsctrb_cairo.o

###### end of [Makefile] ######
