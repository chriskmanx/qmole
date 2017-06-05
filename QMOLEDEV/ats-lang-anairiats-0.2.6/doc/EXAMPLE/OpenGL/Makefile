#
#
# A simple Makefile for some programs involving OpenGL
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
#
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSCTRB=$(ATSUSRQ)/contrib/GL/atsctrb_GL.o
ATSCTRBGTK=$(ATSUSRQ)/contrib/GTK/atsctrb_GTK.o

######

.PHONY: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean

######

checkall:: glHello
cleanall:: ; $(RMF) glHello
glHello: glHello.dats
	$(ATSCC) -o glHello glHello.dats $(ATSCTRB) -lGL -lglut

######

checkall:: glDoubleBuffer
cleanall:: ; $(RMF) glDoubleBuffer
glDoubleBuffer: glDoubleBuffer.dats
	$(ATSCC) -o glDoubleBuffer glDoubleBuffer.dats $(ATSCTRB) -lGL -lglut

checkall:: glIcosahedron
cleanall:: ; $(RMF) glIcosahedron
glIcosahedron: glIcosahedron.dats
	$(ATSCC) -D_ATS_GCATS -o glIcosahedron glIcosahedron.dats $(ATSCTRB) -lGL -lGLU -lglut -lm

checkall:: glClipping
cleanall:: ; $(RMF) glClipping
glClipping: glClipping.dats
	$(ATSCC) -o glClipping glClipping.dats $(ATSCTRB) -lGL -lGLU -lglut

checkall:: glCubeView
cleanall:: ; $(RMF) glCubeView
glCubeView: glCubeView.dats
	$(ATSCC) -o glCubeView glCubeView.dats $(ATSCTRB) -lGL -lGLU -lglut

checkall:: glPlanet
cleanall:: ; $(RMF) glPlanet
glPlanet: glPlanet.dats
	$(ATSCC) -o glPlanet glPlanet.dats $(ATSCTRB) -lGL -lGLU -lglut

checkall:: glColoredTriangle
cleanall:: ; $(RMF) glColoredTriangle
glColoredTriangle: glColoredTriangle.dats
	$(ATSCC) -o glColoredTriangle glColoredTriangle.dats $(ATSCTRB) -lGL -lGLU -lglut -lm

checkall:: glLitSphere
cleanall:: ; $(RMF) glLitSphere
glLitSphere: glLitSphere.dats
	$(ATSCC) -o glLitSphere glLitSphere.dats $(ATSCTRB) -lGL -lglut

checkall:: glLightMove1
cleanall:: ; $(RMF) glLightMove1
glLightMove1: glLightMove1.dats
	$(ATSCC) -o glLightMove1 glLightMove1.dats $(ATSCTRB) -lGL -lGLU -lglut

checkall:: glLightMove2
cleanall:: ; $(RMF) glLightMove2
glLightMove2: glLightMove2.dats
	$(ATSCC) -o glLightMove2 glLightMove2.dats $(ATSCTRB) -lGL -lGLU -lglut

checkall:: glBlending1
cleanall:: ; $(RMF) glBlending1
glBlending1: glBlending1.dats
	$(ATSCC) -o glBlending1 glBlending1.dats $(ATSCTRB) -lGL -lGLU -lglut

checkall:: glBlending2
cleanall:: ; $(RMF) glBlending2
glBlending2: glBlending2.dats
	$(ATSCC) -o glBlending2 glBlending2.dats $(ATSCTRB) -lGL -lglut

checkall:: glDisplayList1
cleanall:: ; $(RMF) glDisplayList1
glDisplayList1: glDisplayList1.dats
	$(ATSCC) -o glDisplayList1 glDisplayList1.dats $(ATSCTRB) -lGL -lGLU -lglut -lm

######

#
# A little bit animation for fun
#
checkall:: glFaces
cleanall:: ; $(RMF) glFaces
glFaces: glFaces.dats
	$(ATSCC) -o glFaces glFaces.dats $(ATSCTRB) -lGL -lglut -lm

######

#
# A simple game of Tetris implemented in ATS
#
checkall:: glTetrix
cleanall:: ; $(RMF) glTetrix
glTetrix: glTetrix.dats
	$(ATSCC) -O2 -o glTetrix glTetrix.dats $(ATSCTRB) -lGL -lglut -lm

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c
	$(RMF) *_?ats.o

###### end of [Makefile] ######
