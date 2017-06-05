######
#
# Makefile for the [parcomb] package
#
######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif # end of [ifeq]

######

ATSCC=$(ATSUSRQ)/bin/atscc -Wall

######

SOURCES := \
  dynloadall.dats \
  parcomb.sats parcomb.dats \
  posloc.sats posloc.dats \
  tokenize.sats tokenize.dats \

OBJECTS := $(SOURCES)
OBJECTS := $(patsubst %.dats, %_dats.o, $(OBJECTS))
OBJECTS := $(patsubst %.sats, %_sats.o, $(OBJECTS))

######

all:: atsctrb_parcomb.o
all:: clean

######

atsctrb_parcomb.o: $(OBJECTS); ld -r -o $@ $(OBJECTS)

######

dynloadall_dats.o: dynloadall.dats; $(ATSCC) -O2 -c -o $@ $<

######

parcomb_sats.o: SATS/parcomb.sats
	$(ATSCC) -O2 -c -o $@ $<
parcomb_dats.o: DATS/parcomb.dats
	$(ATSCC) -O2 -c -o $@ $<

######

posloc_sats.o: SATS/posloc.sats
	$(ATSCC) -O2 -c -o $@ $<
posloc_dats.o: DATS/posloc.dats
	$(ATSCC) -O2 -c -o $@ $<

tokenize_sats.o: SATS/tokenize.sats
	$(ATSCC) -O2 -c -o $@ $<
tokenize_dats.o: DATS/tokenize.dats
	$(ATSCC) -O2 -c -o $@ $<

######

RMF=rm -f

######

clean:: ; $(RMF) *~
clean:: ; $(RMF) *_?ats.c
clean:: ; $(RMF) *_?ats.o

cleanall: clean
	$(RMF) atsctrb_parcomb.o

###### end of [Makefile] ######
