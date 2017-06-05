#
#
# Makefile for K&R examples in Chapter 3
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

checkall:: atoi
cleanall:: ; $(RMF) atoi
atoi: atoi.dats ; $(ATSCC) -o atoi atoi.dats

checkall:: reverse
cleanall:: ; $(RMF) reverse
reverse: reverse.dats ; $(ATSCC) -o reverse reverse.dats

checkall:: itoa
cleanall:: ; $(RMF) itoa
itoa: itoa.dats ; $(ATSCC) -o itoa itoa.dats

######

html:: ; $(ATSOPT) --posmark_html -d atoi.dats > atoi_dats.html
html:: ; $(ATSOPT) --posmark_html -d reverse.dats > reverse_dats.html
html:: ; $(ATSOPT) --posmark_html -d itoa.dats > itoa_dats.html

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
