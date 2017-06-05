#
# Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
# Time: May, 2011
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

ATSCC=$(ATSUSRQ)/bin/atscc

######

all: atsctrb_GLES2.o clean

######

atsctrb_GLES2.o: gl2_sats.o
	ld -r -o $@ gl2_sats.o

######

gl2_sats.o: SATS/gl2.sats
	$(ATSCC) -o $@ -c SATS/gl2.sats

######

clean::
	rm -f gl2_sats.c gl2_sats.o

cleanall: clean
	rm -f atsctrb_GLES2.o

###### end of [Makefile] ######
