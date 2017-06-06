#
# Makefile for Magick++ demo programs
#
# Copyright Bob Friesenhahn, 1999
#
AUTOMAKE_OPTIONS = 1.2

INCLUDES = -I../lib

noinst_PROGRAMS = button demo flip shapes

LDADD = ../lib/libMagick++.la

button_SOURCES = button.cpp
demo_SOURCES = demo.cpp
flip_SOURCES = flip.cpp
shapes_SOURCES = shapes.cpp

EXTRA_DIST = Generic.ttf model.gif smile.gif smile_anim.gif tile.gif \
	button.dsp demo.dsp flip.dsp shapes.dsp
