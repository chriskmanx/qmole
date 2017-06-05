#
#
# Makefile for K&R examples in Chapter 5
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

checkall:: getint
cleanall:: ; $(RMF) getint
getint: getint.dats ; $(ATSCC) -o getint getint.dats

checkall:: strlen
cleanall:: ; $(RMF) strlen
strlen: strlen.dats ; $(ATSCC) -o strlen strlen.dats

checkall:: strcpy
cleanall:: ; $(RMF) strcpy
strcpy: strcpy.dats ; $(ATSCC) -o strcpy strcpy.dats

checkall:: strcmp
cleanall:: ; $(RMF) strcmp
strcmp: strcmp.dats ; $(ATSCC) -o strcmp strcmp.dats

checkall:: strcmp_alt
cleanall:: ; $(RMF) strcmp_alt
strcmp_alt: strcmp_alt.dats; $(ATSCC) -o strcmp_alt strcmp_alt.dats

checkall:: month_name
cleanall:: ; $(RMF) month_name
month_name: month_name.dats ; $(ATSCC) -o month_name month_name.dats

checkall:: month_day
cleanall:: ; $(RMF) month_day
month_day: month_day.dats ; $(ATSCC) -o month_day month_day.dats

######

html:: ; $(ATSOPT) --posmark_html -d getint.dats > getint_dats.html
html:: ; $(ATSOPT) --posmark_html -d strlen.dats > strlen_dats.html
html:: ; $(ATSOPT) --posmark_html -d strcpy.dats > strcpy_dats.html
html:: ; $(ATSOPT) --posmark_html -d strcmp.dats > strcmp_dats.html
html:: ; $(ATSOPT) --posmark_html -d strcmp_alt.dats > strcmp_alt_dats.html
html:: ; $(ATSOPT) --posmark_html -d month_name.dats > month_name_dats.html
html:: ; $(ATSOPT) --posmark_html -d month_day.dats > month_day_dats.html

######

RMF=rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
