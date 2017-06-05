#
#
# Some Examples in ATS involving GTK+ programming
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

XRCFLAGS=`pkg-config cairo --cflags`
GTKCLFLAGS=`pkg-config gtk+-2.0 --cflags --libs`

######

.PHONY: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean

######

checkall:: blurring
cleanall:: ; $(RMF) blurring
blurring: blurring.dats blur_image_surface_dats.o
	$(ATSCC) -o $@ blurring.dats blur_image_surface_dats.o $(GTKCLFLAGS) -lm
blur_image_surface_dats.o: blur_image_surface.dats
	$(ATSCC) -c -o $@ blur_image_surface.dats $(XRCFLAGS)

######

checkall:: clip-all
cleanall:: ; $(RMF) clip-all
clip-all: clip-all.dats
	$(ATSCC) -o $@ clip-all.dats $(GTKCLFLAGS)

######

checkall:: push-group
cleanall:: ; $(RMF) push-group
push-group: push-group.dats
	$(ATSCC) -g -o $@ push-group.dats $(GTKCLFLAGS)

######

checkall:: stroke-image
cleanall:: ; $(RMF) stroke-image
stroke-image: stroke-image.dats
	$(ATSCC) -o $@ stroke-image.dats $(GTKCLFLAGS)

######

checkall:: testcairo
cleanall:: ; $(RMF) testcairo
testcairo: testcairo.dats
	$(ATSCC) -o $@ testcairo.dats $(GTKCLFLAGS) -lm

######

checkall:: text-rotate
cleanall:: ; $(RMF) text-rotate
text-rotate: text-rotate.dats
	$(ATSCC) -o $@ text-rotate.dats $(GTKCLFLAGS) -lm

checkall:: text-transform
cleanall:: ; $(RMF) text-transform
text-transform: text-transform.dats
	$(ATSCC) -o $@ text-transform.dats $(GTKCLFLAGS) -lm

######

RMF=rm -f

######

clean::
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
