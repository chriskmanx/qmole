#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Time: December, 2009
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

ATSCC=$(ATSUSRQ)/bin/atscc

######

all:: atsctrb_GL.o
all:: atsctrb_glut.o
all:: clean

######

atsctrb_GL.o: gl_sats.o glu_sats.o ; ld -r -o $@ $^
atsctrb_glut.o: glut_sats.o glut_dats.o ; ld -r -o $@ $^

######

gl_sats.o: SATS/gl.sats
	$(ATSCC) -o $@ -c SATS/gl.sats

glu_sats.o: SATS/glu.sats
	$(ATSCC) -o $@ -c SATS/glu.sats

######

glut_sats.o: SATS/glut.sats
	$(ATSCC) -o $@ -c SATS/glut.sats
glut_dats.o: DATS/glut.dats
	$(ATSCC) -o $@ -c DATS/glut.dats

######

RMF=rm -f

######

clean:: ; $(RMF) *_?ats.c *_?ats.o

cleanall:: clean
cleanall:: ; $(RMF) atsctrb_GL.o atsctrb_glut.o

###### end of [Makefile] ######
