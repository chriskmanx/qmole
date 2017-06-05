#
#
# Makefile for some BLAS testing code 
#
#

######

ATSUSRQ="$(ATSHOME)"
ATSLIBQ="$(ATSHOME)"
ifeq ("$(ATSHOME)","")
ATSUSRQ="/usr"
ATSLIBQ="/usr/lib/ats-anairiats-0.2.5"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSOPT=$(ATSUSRQ)/bin/atsopt
LIBFLAGS=-L/usr/lib/atlas -L/usr/lib64/atlas
ATSCTRB=$(ATSLIBQ)/contrib/cblas/atsctrb_cblas.o

######

.PHONY: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean

######

checkall:: test_cblas
test_cblas: test_cblas.dats
	$(ATSCC) $(LIBFLAGS) -O2 -o $@ test_cblas.dats $(ATSCTRB) -lcblas -lm \
     && ./test_cblas
cleanall:: ; $(RMF) test_cblas

######

checkall:: test_gemm1
test_gemm1: test_gemm1.dats
	$(ATSCC) $(LIBFLAGS) -O2 -o $@ test_gemm1.dats $(ATSCTRB) -lcblas -lm \
     && ./test_gemm1
cleanall:: ; $(RMF) test_gemm1

checkall:: test_gemm2
test_gemm2: test_gemm2.dats
	$(ATSCC) $(LIBFLAGS) -O2 -o $@ test_gemm2.dats $(ATSCTRB) -lcblas -lm \
     && ./test_gemm2
cleanall:: ; $(RMF) test_gemm2

######

checkall:: test_symm
test_symm: test_symm.dats
	$(ATSCC) $(LIBFLAGS) -O2 -o $@ test_symm.dats $(ATSCTRB) -lcblas -lm \
     && ./test_symm
cleanall:: ; $(RMF) test_symm

######

checkall:: test_lu
test_lu: test_lu.dats
	$(ATSCC) $(LIBFLAGS) -O2 -o $@ test_lu.dats $(ATSCTRB) -lcblas -lm \
     && ./test_lu
cleanall:: ; $(RMF) test_lu

######

html:: ; $(ATSOPT) --posmark_html -d test_cblas.dats > test_cblas_dats.html
cleanall:: ; $(RMF) *_?ats.html

######

RMF=rm -f

######

clean::
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
