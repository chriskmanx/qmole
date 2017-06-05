##
## Makefile for [twentyfour.dats]
##

######
  
ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSOPT=$(ATSUSRQ)/bin/atsopt

######

#GCFLAG=-D_ATS_gc // no longer in use
#GCFLAG=-D_ATS_gcats // no longer in use
GCFLAG=-D_ATS_GCATS
#GCFLAG=-D_ATS_GCBDW

######

.PHONY: all
all: checkall cleanall

######

checkall::
cleanall:: clean

######

checkall:: twentyfour
twentyfour: twentyfour.dats rational.dats rational.sats
	$(ATSCC) -O2 $(GCFLAG) -o twentyfour twentyfour.dats rational.dats rational.sats -lm \
     && ./twentyfour
######

clean:
	rm -f  *~
	rm -f *_dats.c *_sats.c
cleanall:: ; rm -f twentyfour

###### end of [Makefile] ######
