#  Makefile for building ImageMagick library
#
#  Copyright 1999 E. I. du Pont de Nemours and Company
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files ("ImageMagick"),
#  to deal in ImageMagick without restriction, including without limitation 
#  the rights to use, copy, modify, merge, publish, distribute, sublicense, 
#  and/or sell copies of ImageMagick, and to permit persons to whom the 
#  ImageMagick is furnished to do so, subject to the following conditions:
#   
#  The above copyright notice and this permission notice shall be included in 
#  all copies or substantial portions of ImageMagick. 
#   
#  The software is provided "as is", without warranty of any kind, express or 
#  implied, including but not limited to the warranties of merchantability, 
#  fitness for a particular purpose and noninfringement.In no event shall 
#  E. I. du Pont de Nemours and Company be liable for any claim, damages or 
#  other liability, whether in an action of contract, tort or otherwise,
#  arising from, out of or in connection with ImageMagick or the use or other 
#  dealings in ImageMagick. 
#   
#  Except as contained in this notice, the name of the E. I. du Pont de 
#  Nemours and Company shall not be used in advertising or otherwise to 
#  promote the sale, use or other dealings in ImageMagick without prior 
#  written authorization from the E. I. du Pont de Nemours and Company. 
#
#

# Don't require all the GNU mandated files
AUTOMAKE_OPTIONS = 1.2 foreign


lib_LTLIBRARIES = libMagick.la

MAJOR_VERSION=@MAJOR_VERSION@
MINOR_VERSION=@MINOR_VERSION@
MICRO_VERSION=@MICRO_VERSION@

INCLUDES = -I$(top_srcdir)

libMagick_la_LDFLAGS = -version-info $(MAJOR_VERSION):$(MINOR_VERSION)$(MICRO_VERSION):0

# Library sources
libMagick_la_SOURCES = PreRvIcccm.c animate.c annotate.c avs.c blob.c bmp.c \
  cmyk.c colors.c compress.c dcm.c decorate.c delegates.c display.c dps.c \
  draw.c effects.c enhance.c ept.c error.c fax.c fits.c fpx.c gems.c gif.c \
  gradation.c gray.c hdf.c histogram.c html.c icc.c icon.c image.c \
  iptc.c jbig.c jpeg.c label.c logo.c magick.c map.c matte.c memory.c \
  miff.c monitor.c mono.c montage.c mtv.c null.c pcd.c pcl.c pcx.c \
  pdf.c pict.c pix.c plasma.c png.c pnm.c preview.c ps.c ps2.c ps3.c psd.c \
  pwp.c quantize.c rgb.c rla.c rle.c segment.c sct.c sfw.c sgi.c shear.c \
  signature.c stegano.c sun.c tga.c tiff.c tile.c tim.c transform.c \
  ttf.c txt.c uil.c utility.c uyvy.c vicar.c vid.c viff.c widget.c x.c \
  xbm.c xc.c xpm.c xwd.c xwindows.c yuv.c zoom.c

# Source files not used for Unix library but which are distributed
EXTRA_libMagick_la_SOURCES = mac.c nt.c vms.c

# Binary scripts
bin_SCRIPTS = Magick-config

# Headers which are installed
include_HEADERS = magick.h api.h classify.h image.h quantize.h gems.h \
	blob.h compress.h utility.h monitor.h error.h memory.h delegates.h \
	version.h config.h

# Headers which are not installed but which are distributed
noinst_HEADERS = animate.h display.h mac.h nt.h vms.h defines.h mng.h \
	PreRvIcccm.h widget.h xwindows.h

# Additional files to distribute
EXTRA_DIST = Imakefile Magick-config.in Make.com

# Ensure that configuration header is not distributed
dist-hook:
	-rm $(distdir)/config.h

DEFS = @DEFS@ -I../
