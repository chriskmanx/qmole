#
#
# Makefile for K&R examples in Chapter 4
#
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSOPT=$(ATSUSRQ)/bin/atsopt

######

.PHONY: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean
cleanall:: ; $(RMF) *_?ats.html 

######

checkall:: atof
cleanall:: ; $(RMF) atof
atof: atof.dats ; $(ATSCC) -o atof atof.dats

checkall:: printd
cleanall:: ; $(RMF) printd
printd: printd.dats ; $(ATSCC) -o printd printd.dats

checkall:: qsort
cleanall:: ; $(RMF) qsort
qsort: qsort.dats ; $(ATSCC) -o qsort qsort.dats

######

html:: ; $(ATSOPT) --posmark_html -d atof.dats > atof_dats.html
html:: ; $(ATSOPT) --posmark_html -d printd.dats > printd_dats.html
html:: ; $(ATSOPT) --posmark_html -d qsort.dats > qsort_dats.html

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
