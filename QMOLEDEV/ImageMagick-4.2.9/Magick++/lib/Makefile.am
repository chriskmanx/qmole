#
# Makefile for Magick++ library
#
# Copyright Bob Friesenhahn, 1999
#
AUTOMAKE_OPTIONS = 1.2

SUBDIRS = Magick++

lib_LTLIBRARIES    = libMagick++.la
INTERFACE_CURRENT  = @INTERFACE_CURRENT@
INTERFACE_REVISION = @INTERFACE_REVISION@
INTERFACE_AGE      = @INTERFACE_AGE@

libMagick___la_SOURCES = Blob.cpp Color.cpp Drawable.cpp \
	Exception.cpp Functions.cpp Geometry.cpp \
	Image.cpp LastError.cpp Montage.cpp \
	Options.cpp

libMagick___la_LDFLAGS = -version-info $(INTERFACE_CURRENT):$(INTERFACE_REVISION):$(INTERFACE_AGE)

include_HEADERS = Magick++.h

EXTRA_DIST = Magick++.dsp
