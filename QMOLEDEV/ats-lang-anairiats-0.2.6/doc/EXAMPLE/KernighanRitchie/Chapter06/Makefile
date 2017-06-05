#
#
# Makefile for K&R examples in Chapter 6
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

checkall:: getword
cleanall:: ; $(RMF) getword
getword: getword.dats ; $(ATSCC) -o getword getword.dats

######

html:: ; $(ATSOPT) --posmark_html -d getword.dats > getword_dats.html

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
