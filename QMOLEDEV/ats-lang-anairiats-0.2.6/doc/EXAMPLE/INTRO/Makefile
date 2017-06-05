#
#
# Some introductory examples written in ATS
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
cleanall:: ; $(RMF) ./XREF/*
cleanall:: ; $(RMF) *_?ats.html

######

checkall:: HelloWorld
cleanall:: ; $(RMF) HelloWorld
HelloWorld: HelloWorld.dats
	$(ATSCC) -o HelloWorld HelloWorld.dats && ./HelloWorld

checkall:: f91
cleanall:: ; $(RMF) f91
f91: f91.dats
	$(ATSCC) -o f91 f91.dats && ./f91 0

checkall:: fact1
cleanall:: ; $(RMF) fact1
fact1: fact1.dats
	$(ATSCC) -o fact1 fact1.dats && ./fact1 10

checkall:: fact2
cleanall:: ; $(RMF) fact2
fact2: fact2.dats
	$(ATSCC) -o fact2 fact2.dats && ./fact2 10

checkall:: fact3
cleanall:: ; $(RMF) fact3
fact3: fact3.dats
	$(ATSCC) -D_ATS_PROOFCHECK -o fact3 fact3.dats -lats -lgmp && ./fact3 100

checkall:: fact4
cleanall:: ; $(RMF) fact4
fact4: fact4.dats
	$(ATSCC) -D_ATS_PROOFCHECK -o fact4 fact4.dats -lats -lgmp && ./fact4 10

checkall:: fact_in_c
cleanall:: ; $(RMF) fact_in_c
fact_in_c: fact_in_c.dats
	$(ATSCC) -o fact_in_c fact_in_c.dats && ./fact_in_c 10

checkall:: fibs
cleanall:: ; $(RMF) fibs
fibs: fibs.dats
	$(ATSCC) -o fibs fibs.dats -lats -lgmp && ./fibs 20

checkall:: revarr
cleanall:: ; $(RMF) revarr
revarr: revarr.dats
	$(ATSCC) -o revarr revarr.dats && ./revarr

checkall:: revstr
cleanall:: ; $(RMF) revstr
revstr: revstr.dats
	$(ATSCC) -o revstr revstr.dats && ./revstr "abcdefghijklmnopqrstuvwxyz"

checkall:: tally
cleanall:: ; $(RMF) tally
tally: tally.dats
	$(ATSCC) -o tally tally.dats -lats && ./tally

######

html:: ; $(ATSOPT) --posmark_html -d HelloWorld.dats > HelloWorld_dats.html
html:: ; $(ATSOPT) --posmark_html -d f91.dats > f91_dats.html
html:: ; $(ATSOPT) --posmark_html -d fact1.dats > fact1_dats.html
html:: ; $(ATSOPT) --posmark_html -d fact2.dats > fact2_dats.html
html:: ; $(ATSOPT) --posmark_html -d fact3.dats > fact3_dats.html
html:: html_fact4
html_fact4: ; $(ATSOPT) --posmark_html -d fact4.dats > fact4_dats.html
html:: ; $(ATSOPT) --posmark_html -d fact_in_c.dats > fact_in_c_dats.html
html:: ; $(ATSOPT) --posmark_html -d fibs.dats > fibs_dats.html
html:: ; $(ATSOPT) --posmark_html -d revarr.dats > revarr_dats.html
html:: ; $(ATSOPT) --posmark_html -d revstr.dats > revstr_dats.html
html:: ; $(ATSOPT) --posmark_html -d tally.dats > tally_dats.html

######

xref:: ; $(ATSOPT) --posmark_xref=XREF -d \
  $(ATSHOME)/utils/xref/ats_prelude_xref.dats > /dev/null
xref:: ; $(ATSOPT) --posmark_xref=XREF -d HelloWorld.dats > HelloWorld_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d f91.dats > f91_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d fact1.dats > fact1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d fact2.dats > fact2_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d fact3.dats > fact3_dats.html
xref:: xref_fact4
xref_fact4: ; $(ATSOPT) --posmark_xref=XREF -d fact4.dats > fact4_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d fact_in_c.dats > fact_in_c_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d fibs.dats > fibs_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d revarr.dats > revarr_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d revstr.dats > revstr_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d tally.dats > tally_dats.html

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) ./INTRO/*
	$(RMF) *_dats.c *_sats.c

###### end of [Makefile] ######
