##
## A simple Makefile
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

.PHONY: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean
cleanall:: ; $(RMF) ./*_?ats.html
cleanall:: ; $(RMF) ./XREF/*.html

######

checkall:: fib_mt
fib_mt: fib_mt.dats
	$(ATSCC) -o fib_mt -O3 -D_ATS_MULTITHREAD fib_mt.dats -lpthread
cleanall:: ; $(RMF) fib_mt

######

checkall:: nsieve_mt
nsieve_mt: nsieve_mt.dats
	$(ATSCC) -o nsieve_mt -O3 -D_ATS_MULTITHREAD nsieve_mt.dats -lm -lpthread
cleanall:: ; $(RMF) nsieve_mt

checkall:: nsieve2_mt
nsieve2_mt: nsieve2_mt.dats
	$(ATSCC) -o nsieve2_mt -O3 -D_ATS_MULTITHREAD nsieve2_mt.dats -lm -lpthread
cleanall:: ; $(RMF) nsieve2_mt

######

checkall:: partial-sums_mt
partial-sums_mt: partial-sums_mt.dats
	$(ATSCC) -o partial-sums_mt -O3 -D_ATS_MULTITHREAD partial-sums_mt.dats -lm -lpthread
cleanall:: ; $(RMF) partial-sums_mt

######

checkall:: qsort_mt_flt
qsort_mt_flt: qsort_mt_flt.dats
	$(ATSCC) -o qsort_mt_flt -O3 -D_ATS_MULTITHREAD qsort_mt_flt.dats -lm -lpthread
cleanall:: ; $(RMF) qsort_mt_flt

checkall:: qsort_mt_int
qsort_mt_int: qsort_mt_int.dats
	$(ATSCC) -o qsort_mt_int -O3 -D_ATS_MULTITHREAD qsort_mt_int.dats -lm -lpthread
cleanall:: ; $(RMF) qsort_mt_int

######

checkall:: randcompec_mt
randcompec_mt: randcompec_mt.dats
	$(ATSCC) -o randcompec_mt -O3 -D_ATS_MULTITHREAD randcompec_mt.dats -lpthread
cleanall:: ; $(RMF) randcompec_mt

checkall:: randcompec2_mt
randcompec2_mt: randcompec2_mt.dats
	$(ATSCC) -o randcompec2_mt -O3 -D_ATS_MULTITHREAD randcompec2_mt.dats -lpthread
cleanall:: ; $(RMF) randcompec2_mt

checkall:: randcompec3_mt
randcompec3_mt: randcompec3_mt.dats
	$(ATSCC) -o randcompec3_mt -O3 -D_ATS_MULTITHREAD randcompec3_mt.dats -lpthread
cleanall:: ; $(RMF) randcompec3_mt

######

xref:: ; $(ATSOPT) --posmark_xref=XREF -d \
  $(ATSHOME)/utils/xref/ats_prelude_xref.dats > /dev/null
xref:: ; $(ATSOPT) --posmark_xref=XREF -d fib_mt.dats > fib_mt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d nsieve_mt.dats > nsieve_mt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d nsieve2_mt.dats > nsieve2_mt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d partial-sums_mt.dats > partial-sums_mt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d qsort_mt.dats > qsort_mt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d qsort_mt_int.dats > qsort_mt_int_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d qsort_mt_flt.dats > qsort_mt_flt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d randcompec_mt.dats > randcompec_mt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d randcompec2_mt.dats > randcompec2_mt_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d randcompec3_mt.dats > randcompec3_mt_dats.html

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c
	$(RMF) *_?ats.o

###### end of [Makefile] ######
