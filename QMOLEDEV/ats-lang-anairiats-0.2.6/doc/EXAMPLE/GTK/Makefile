#
#
# Some Examples in ATS involving GTK+ programming
#
#

######

ATSUSRQ="$(ATSHOME)"
ATSLIBQ="$(ATSHOME)"
ifeq ("$(ATSHOME)","")
ATSUSRQ="/usr"
ATSLIBQ="/usr/lib/ats-anairiats-0.2.3"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSOPT=$(ATSUSRQ)/bin/atsopt

######

GTKFLAGS=`pkg-config gtk+-2.0 --cflags --libs`

######

.PHONY: all
all: checkall

######

checkall: compall cleanall

######

#
# Please do the following
#   cd $ATSHOME/contrib/GTK; make atsctrb_GTK.o
# if "atsctrb_GTK.o" is not available
#
LIBATSGTK=$(ATSLIBQ)/contrib/GTK/atsctrb_GTK.o
compall:: GameOf24
GameOf24: GameOf24.dats
	$(ATSCC) -D_ATS_GCATS -o $@ GameOf24.dats $(LIBATSGTK) $(GTKFLAGS) -lm

######

compall:: gtkcairoclock
gtkcairoclock: gtkcairoclock.dats
	$(ATSCC) -Wl,-rpath,$(ATSLIBQ)/ccomp/lib -o $@ gtkcairoclock.dats $(GTKFLAGS) -lm

######

compall:: gtkcairopascal
gtkcairopascal: gtkcairopascal.dats
	$(ATSCC) -D_ATS_GCATS -o $@ gtkcairopascal.dats $(GTKFLAGS) -lm

######

ATSCTRB=$(ATSLIBQ)/contrib/cairo/atsctrb_cairo.o
compall:: gtkcairodisc
gtkcairodisc: gtkcairodisc.dats
	$(ATSCC) -D_ATS_GCATS $(ATSCTRB) -o $@ gtkcairodisc.dats $(GTKFLAGS) -lm

######

ATSCTRB=$(ATSLIBQ)/contrib/cairo/atsctrb_cairo.o
compall:: bsearch_demo
bsearch_demo: bsearch_demo.dats
	$(ATSCC) -D_ATS_GCATS $(ATSCTRB) -o $@ bsearch_demo.dats $(GTKFLAGS) -lm

######

html:: ; $(ATSOPT) --posmark_html -d GameOf24.dats > GameOf24_dats.html
html:: ; $(ATSOPT) --posmark_html -d gtkcairoclock.dats > gtkcairoclock_dats.html
html:: ; $(ATSOPT) --posmark_html -d gtkcairopascal.dats > gtkcairopascal_dats.html
html:: ; $(ATSOPT) --posmark_html -d gtkcairopascal.dats > gtkcairopascal_dats.html
html:: ; $(ATSOPT) --posmark_html -d bsearch_demo.dats > bsearch_demo_dats.html

######

xref:: ; $(ATSOPT) --posmark_xref=XREF -d \
  $(ATSHOME)/utils/xref/ats_prelude_xref.dats > /dev/null
xref:: ; $(ATSOPT) --posmark_xref=XREF -d GameOf24.dats > GameOf24_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d gtkcairoclock.dats > gtkcairoclock_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d gtkcairopascal.dats > gtkcairopascal_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d gtkcairopascal.dats > gtkcairopascal_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d bsearch_demo.dats > bsearch_demo_dats.html

######

RMF=rm -f

######

clean::
	$(RMF) *~
	$(RMF) *_?ats.c

cleanall:: clean
	$(RMF) ./XREF/*
	$(RMF) ./*_?ats.html
	$(RMF) GameOf24
	$(RMF) gtkcairoclock
	$(RMF) gtkcairopascal
	$(RMF) gtkcairodisc
	$(RMF) bsearch_demo

###### end of [Makefile] ######
