MAINTAINERCLEANFILES = Makefile.in
AUTOMAKE_OPTIONS     = 1.4 foreign

INCLUDES             = -I. \
                       -I$(top_srcdir) \
                       -I$(top_srcdir)/src/lib \
                       -I$(top_srcdir)/src/modules/loaders \
                       @my_includes@

pkgdir               = $(libdir)/imlib2/loaders

if BUILD_JPEG_LOADER
JPEG_L               = jpeg.la
endif
if BUILD_PNG_LOADER
PNG_L                = png.la
endif
if BUILD_TIFF_LOADER
TIFF_L               = tiff.la
endif
if BUILD_GIF_LOADER
GIF_L                = gif.la
endif
if BUILD_ZLIB_LOADER
ZLIB_L               = zlib.la
endif
if BUILD_BZ2_LOADER
BZ2_L                = bz2.la
endif
if BUILD_ID3_LOADER
ID3_L                = id3.la
endif

pkg_LTLIBRARIES      = \
$(JPEG_L) \
$(PNG_L) \
$(TIFF_L) \
$(GIF_L) \
$(ZLIB_L) \
$(BZ2_L) \
$(ID3_L) \
pnm.la \
argb.la \
bmp.la \
xpm.la \
tga.la \
lbm.la

EXTRA_DIST = loader_common.h

jpeg_la_SOURCES      = loader_jpeg.c
jpeg_la_LDFLAGS      = -module -avoid-version
jpeg_la_LIBADD       = @JPEGLIBS@ $(top_builddir)/src/lib/libImlib2.la
jpeg_la_LIBTOOLFLAGS = --tag=disable-static

png_la_SOURCES       = loader_png.c
png_la_CPPFLAGS      = @PNG_CFLAGS@
png_la_LDFLAGS       = -module -avoid-version
png_la_LIBADD        = @PNG_LIBS@ $(top_builddir)/src/lib/libImlib2.la
png_la_LIBTOOLFLAGS = --tag=disable-static

tiff_la_SOURCES      = loader_tiff.c
tiff_la_LDFLAGS      = -module -avoid-version
tiff_la_LIBADD       = @TIFFLIBS@ $(top_builddir)/src/lib/libImlib2.la
tiff_la_LIBTOOLFLAGS = --tag=disable-static

gif_la_SOURCES       = loader_gif.c
gif_la_LDFLAGS       = -module -avoid-version
gif_la_LIBADD        = @GIFLIBS@ $(top_builddir)/src/lib/libImlib2.la
gif_la_LIBTOOLFLAGS = --tag=disable-static

zlib_la_SOURCES      = loader_zlib.c
zlib_la_LDFLAGS      = -module -avoid-version
zlib_la_LIBADD       = @ZLIBLIBS@ $(top_builddir)/src/lib/libImlib2.la
zlib_la_LIBTOOLFLAGS = --tag=disable-static

bz2_la_SOURCES       = loader_bz2.c
bz2_la_LDFLAGS       = -module -avoid-version
bz2_la_LIBADD        = @BZ2LIBS@ $(top_builddir)/src/lib/libImlib2.la
bz2_la_LIBTOOLFLAGS = --tag=disable-static

id3_la_SOURCES       = loader_id3.c
id3_la_LDFLAGS       = -module -avoid-version
id3_la_LIBADD        = @ID3LIBS@ $(top_builddir)/src/lib/libImlib2.la
id3_la_LIBTOOLFLAGS = --tag=disable-static

pnm_la_SOURCES       = loader_pnm.c
pnm_la_LDFLAGS       = -module -avoid-version
pnm_la_LIBADD        = $(top_builddir)/src/lib/libImlib2.la
pnm_la_LIBTOOLFLAGS = --tag=disable-static

argb_la_SOURCES      = loader_argb.c
argb_la_LDFLAGS      = -module -avoid-version
argb_la_LIBADD       = $(top_builddir)/src/lib/libImlib2.la
argb_la_LIBTOOLFLAGS = --tag=disable-static

bmp_la_SOURCES       = loader_bmp.c
bmp_la_LDFLAGS       = -module -avoid-version
bmp_la_LIBADD        = $(top_builddir)/src/lib/libImlib2.la
bmp_la_LIBTOOLFLAGS = --tag=disable-static

xpm_la_SOURCES       = loader_xpm.c
xpm_la_LDFLAGS       = -module -avoid-version
xpm_la_LIBADD        = $(top_builddir)/src/lib/libImlib2.la
xpm_la_LIBTOOLFLAGS = --tag=disable-static

tga_la_SOURCES       = loader_tga.c
tga_la_LDFLAGS       = -module -avoid-version
tga_la_LIBADD        = $(top_builddir)/src/lib/libImlib2.la
tga_la_LIBTOOLFLAGS = --tag=disable-static

lbm_la_SOURCES       = loader_lbm.c
lbm_la_LDFLAGS       = -module -avoid-version
lbm_la_LIBADD        = $(top_builddir)/src/lib/libImlib2.la
lbm_la_LIBTOOLFLAGS = --tag=disable-static
