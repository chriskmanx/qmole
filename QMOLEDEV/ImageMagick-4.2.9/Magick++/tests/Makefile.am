#
# Makefile for Magick++ tests
#
# Copyright Bob Friesenhahn, 1999
#
AUTOMAKE_OPTIONS = 1.2 

INCLUDES = -I../lib

#
# Execute 'make check' to compile and run a test program
#
check_PROGRAMS = color attributes manipulate readwrite

LDADD = ../lib/libMagick++.la

attributes_SOURCES = attributes.cpp

color_SOURCES = color.cpp

manipulate_SOURCES = manipulate.cpp

readwrite_SOURCES = readwrite.cpp

EXTRA_DIST = Generic.ttf test_image.miff test_image_anim.gif \
	attributes.dsp color.dsp manipulate.dsp readwrite.dsp

check:
	./color
	./attributes
	./manipulate
	./readwrite
