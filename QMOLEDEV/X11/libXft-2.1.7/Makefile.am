# 
#  $Id: Makefile.am,v 1.11 2004/04/15 01:45:47 keithp Exp $
# 
#  Copyright © 2003 Keith Packard, Noah Levitt
# 
#  Permission to use, copy, modify, distribute, and sell this software and its
#  documentation for any purpose is hereby granted without fee, provided that
#  the above copyright notice appear in all copies and that both that
#  copyright notice and this permission notice appear in supporting
#  documentation, and that the name of Keith Packard not be used in
#  advertising or publicity pertaining to distribution of the software without
#  specific, written prior permission.  Keith Packard makes no
#  representations about the suitability of this software for any purpose.  It
#  is provided "as is" without express or implied warranty.
# 
#  KEITH PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
#  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
#  EVENT SHALL KEITH PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
#  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
#  DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
#  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
#  PERFORMANCE OF THIS SOFTWARE.

bin_SCRIPTS = xft-config

man_MANS = Xft.3

AM_CFLAGS = $(FONTCONFIG_CFLAGS) $(FREETYPE_CFLAGS) $(XRENDER_CFLAGS) \
	$(WARN_CFLAGS)

lib_LTLIBRARIES = libXft.la

libXft_la_SOURCES = Xft.h \
                    XftCompat.h \
                    xftint.h \
                    xftcolor.c \
                    xftcore.c \
                    xftdbg.c \
                    xftdpy.c \
                    xftdraw.c \
                    xftextent.c \
                    xftfont.c \
                    xftfreetype.c \
                    xftglyphs.c \
                    xftinit.c \
                    xftlist.c \
                    xftname.c \
                    xftrender.c \
                    xftstr.c \
                    xftswap.c \
                    xftxlfd.c

libXft_la_LIBADD = @FONTCONFIG_LIBS@ @FREETYPE_LIBS@ @XRENDER_LIBS@

# -version-number requires libtool >= 1.5
libXft_la_LDFLAGS = -version-number 2:1:2 -no-undefined

libXftincludedir = $(includedir)/X11/Xft
libXftinclude_HEADERS = Xft.h XftCompat.h 

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = xft.pc

EXTRA_DIST = xft-config.in xft.pc.in Xft.3.in $(man_MANS) autogen.sh

