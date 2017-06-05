#
#
# Makefile for K&R examples in Chapter 7
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

checkall:: lower
cleanall:: ; $(RMF) lower
lower: lower.dats ; $(ATSCC) -o lower lower.dats

checkall:: fgets
cleanall:: ; $(RMF) fgets
fgets: fgets.dats ; $(ATSCC) -o fgets fgets.dats

checkall:: filecopy
cleanall:: ; $(RMF) filecopy
filecopy: filecopy.dats ; $(ATSCC) -o filecopy filecopy.dats

######

html:: ; $(ATSOPT) --posmark_html -d lower.dats > lower_dats.html
html:: ; $(ATSOPT) --posmark_html -d fgets.dats > fgets_dats.html
html:: ; $(ATSOPT) --posmark_html -d filecopy.dats > filecopy_dats.html

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
