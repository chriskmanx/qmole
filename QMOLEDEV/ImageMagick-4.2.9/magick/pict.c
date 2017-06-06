/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        PPPP   IIIII   CCCC  TTTTT                           %
%                        P   P    I    C        T                             %
%                        PPPP     I    C        T                             %
%                        P        I    C        T                             %
%                        P      IIIII   CCCC    T                             %
%                                                                             %
%                                                                             %
%                    Read/Write ImageMagick Image Format.                     %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                                 July 1992                                   %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentation files ("ImageMagick"),  %
%  to deal in ImageMagick without restriction, including without limitation   %
%  the rights to use, copy, modify, merge, publish, distribute, sublicense,   %
%  and/or sell copies of ImageMagick, and to permit persons to whom the       %
%  ImageMagick is furnished to do so, subject to the following conditions:    %
%                                                                             %
%  The above copyright notice and this permission notice shall be included in %
%  all copies or substantial portions of ImageMagick.                         %
%                                                                             %
%  The software is provided "as is", without warranty of any kind, express or %
%  implied, including but not limited to the warranties of merchantability,   %
%  fitness for a particular purpose and noninfringement.  In no event shall   %
%  E. I. du Pont de Nemours and Company be liable for any claim, damages or   %
%  other liability, whether in an action of contract, tort or otherwise,      %
%  arising from, out of or in connection with ImageMagick or the use or other %
%  dealings in ImageMagick.                                                   %
%                                                                             %
%  Except as contained in this notice, the name of the E. I. du Pont de       %
%  Nemours and Company shall not be used in advertising or otherwise to       %
%  promote the sale, use or other dealings in ImageMagick without prior       %
%  written authorization from the E. I. du Pont de Nemours and Company.       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"

/*
  ImageMagick Macintosh PICT Methods.
*/
#define ReadPixmap(pixmap) \
{ \
  pixmap.version=MSBFirstReadShort(image); \
  pixmap.pack_type=MSBFirstReadShort(image); \
  pixmap.pack_size=MSBFirstReadLong(image); \
  pixmap.horizontal_resolution=MSBFirstReadLong(image); \
  pixmap.vertical_resolution=MSBFirstReadLong(image); \
  pixmap.pixel_type=MSBFirstReadShort(image); \
  pixmap.bits_per_pixel=MSBFirstReadShort(image); \
  pixmap.component_count=MSBFirstReadShort(image); \
  pixmap.component_size=MSBFirstReadShort(image); \
  pixmap.plane_bytes=MSBFirstReadLong(image); \
  pixmap.table=MSBFirstReadLong(image); \
  pixmap.reserved=MSBFirstReadLong(image); \
}

#define ReadRectangle(rectangle) \
{ \
  rectangle.top=MSBFirstReadShort(image); \
  rectangle.left=MSBFirstReadShort(image); \
  rectangle.bottom=MSBFirstReadShort(image); \
  rectangle.right=MSBFirstReadShort(image); \
}

typedef struct _PICTCode
{
  char
    *name;

  int
    length;

  char
    *description;
} PICTCode;

typedef struct _PICTPixmap
{
  short
    version,
    pack_type;

  long int
    pack_size,
    horizontal_resolution,
    vertical_resolution;

  short
    pixel_type,
    bits_per_pixel,
    component_count,
    component_size;

  long int
    plane_bytes,
    table,
    reserved;
} PICTPixmap;

typedef struct _PICTRectangle
{
  short
    top,
    left,
    bottom,
    right;
} PICTRectangle;

const PICTCode
  codes[] =
  {
    /* 0x00 */  { "NOP",            0, "nop" },
    /* 0x01 */  { "Clip",           0, "clip" },
    /* 0x02 */  { "BkPat",          8, "background pattern" },
    /* 0x03 */  { "TxFont",         2, "text font (word)" },
    /* 0x04 */  { "TxFace",         1, "text face (byte)" },
    /* 0x05 */  { "TxMode",         2, "text mode (word)" },
    /* 0x06 */  { "SpExtra",        4, "space extra (fixed point)" },
    /* 0x07 */  { "PnSize",         4, "pen size (point)" },
    /* 0x08 */  { "PnMode",         2, "pen mode (word)" },
    /* 0x09 */  { "PnPat",          8, "pen pattern" },
    /* 0x0a */  { "FillPat",        8, "fill pattern" },
    /* 0x0b */  { "OvSize",         4, "oval size (point)" },
    /* 0x0c */  { "Origin",         4, "dh, dv (word)" },
    /* 0x0d */  { "TxSize",         2, "text size (word)" },
    /* 0x0e */  { "FgColor",        4, "foreground color (longword)" },
    /* 0x0f */  { "BkColor",        4, "background color (longword)" },
    /* 0x10 */  { "TxRatio",        8, "numerator (point), denominator (point)" },
    /* 0x11 */  { "Version",        1, "version (byte)" },
    /* 0x12 */  { "BkPixPat",       0, "color background pattern" },
    /* 0x13 */  { "PnPixPat",       0, "color pen pattern" },
    /* 0x14 */  { "FillPixPat",     0, "color fill pattern" },
    /* 0x15 */  { "PnLocHFrac",     2, "fractional pen position" },
    /* 0x16 */  { "ChExtra",        2, "extra for each character" },
    /* 0x17 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x18 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x19 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x1a */  { "RGBFgCol",       6, "RGB foreColor" },
    /* 0x1b */  { "RGBBkCol",       6, "RGB backColor" },
    /* 0x1c */  { "HiliteMode",     0, "hilite mode flag" },
    /* 0x1d */  { "HiliteColor",    6, "RGB hilite color" },
    /* 0x1e */  { "DefHilite",      0, "Use default hilite color" },
    /* 0x1f */  { "OpColor",        6, "RGB OpColor for arithmetic modes" },
    /* 0x20 */  { "Line",           8, "pnLoc (point), newPt (point)" },
    /* 0x21 */  { "LineFrom",       4, "newPt (point)" },
    /* 0x22 */  { "ShortLine",      6, "pnLoc (point, dh, dv (-128 .. 127))" },
    /* 0x23 */  { "ShortLineFrom",  2, "dh, dv (-128 .. 127)" },
    /* 0x24 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x25 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x26 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x27 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x28 */  { "LongText",       0, "txLoc (point), count (0..255), text" },
    /* 0x29 */  { "DHText",         0, "dh (0..255), count (0..255), text" },
    /* 0x2a */  { "DVText",         0, "dv (0..255), count (0..255), text" },
    /* 0x2b */  { "DHDVText",       0, "dh, dv (0..255), count (0..255), text" },
    /* 0x2c */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x2d */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x2e */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x2f */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x30 */  { "frameRect",      8, "rect" },
    /* 0x31 */  { "paintRect",      8, "rect" },
    /* 0x32 */  { "eraseRect",      8, "rect" },
    /* 0x33 */  { "invertRect",     8, "rect" },
    /* 0x34 */  { "fillRect",       8, "rect" },
    /* 0x35 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x36 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x37 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x38 */  { "frameSameRect",  0, "rect" },
    /* 0x39 */  { "paintSameRect",  0, "rect" },
    /* 0x3a */  { "eraseSameRect",  0, "rect" },
    /* 0x3b */  { "invertSameRect", 0, "rect" },
    /* 0x3c */  { "fillSameRect",   0, "rect" },
    /* 0x3d */  { "reserved",       0, "reserved for Apple use" },
    /* 0x3e */  { "reserved",       0, "reserved for Apple use" },
    /* 0x3f */  { "reserved",       0, "reserved for Apple use" },
    /* 0x40 */  { "frameRRect",     8, "rect" },
    /* 0x41 */  { "paintRRect",     8, "rect" },
    /* 0x42 */  { "eraseRRect",     8, "rect" },
    /* 0x43 */  { "invertRRect",    8, "rect" },
    /* 0x44 */  { "fillRRrect",     8, "rect" },
    /* 0x45 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x46 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x47 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x48 */  { "frameSameRRect", 0, "rect" },
    /* 0x49 */  { "paintSameRRect", 0, "rect" },
    /* 0x4a */  { "eraseSameRRect", 0, "rect" },
    /* 0x4b */  { "invertSameRRect",0, "rect" },
    /* 0x4c */  { "fillSameRRect",  0, "rect" },
    /* 0x4d */  { "reserved",       0, "reserved for Apple use" },
    /* 0x4e */  { "reserved",       0, "reserved for Apple use" },
    /* 0x4f */  { "reserved",       0, "reserved for Apple use" },
    /* 0x50 */  { "frameOval",      8, "rect" },
    /* 0x51 */  { "paintOval",      8, "rect" },
    /* 0x52 */  { "eraseOval",      8, "rect" },
    /* 0x53 */  { "invertOval",     8, "rect" },
    /* 0x54 */  { "fillOval",       8, "rect" },
    /* 0x55 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x56 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x57 */  { "reserved",       8, "reserved for Apple use" },
    /* 0x58 */  { "frameSameOval",  0, "rect" },
    /* 0x59 */  { "paintSameOval",  0, "rect" },
    /* 0x5a */  { "eraseSameOval",  0, "rect" },
    /* 0x5b */  { "invertSameOval", 0, "rect" },
    /* 0x5c */  { "fillSameOval",   0, "rect" },
    /* 0x5d */  { "reserved",       0, "reserved for Apple use" },
    /* 0x5e */  { "reserved",       0, "reserved for Apple use" },
    /* 0x5f */  { "reserved",       0, "reserved for Apple use" },
    /* 0x60 */  { "frameArc",      12, "rect, startAngle, arcAngle" },
    /* 0x61 */  { "paintArc",      12, "rect, startAngle, arcAngle" },
    /* 0x62 */  { "eraseArc",      12, "rect, startAngle, arcAngle" },
    /* 0x63 */  { "invertArc",     12, "rect, startAngle, arcAngle" },
    /* 0x64 */  { "fillArc",       12, "rect, startAngle, arcAngle" },
    /* 0x65 */  { "reserved",      12, "reserved for Apple use" },
    /* 0x66 */  { "reserved",      12, "reserved for Apple use" },
    /* 0x67 */  { "reserved",      12, "reserved for Apple use" },
    /* 0x68 */  { "frameSameArc",   4, "rect, startAngle, arcAngle" },
    /* 0x69 */  { "paintSameArc",   4, "rect, startAngle, arcAngle" },
    /* 0x6a */  { "eraseSameArc",   4, "rect, startAngle, arcAngle" },
    /* 0x6b */  { "invertSameArc",  4, "rect, startAngle, arcAngle" },
    /* 0x6c */  { "fillSameArc",    4, "rect, startAngle, arcAngle" },
    /* 0x6d */  { "reserved",       4, "reserved for Apple use" },
    /* 0x6e */  { "reserved",       4, "reserved for Apple use" },
    /* 0x6f */  { "reserved",       4, "reserved for Apple use" },
    /* 0x70 */  { "framePoly",      0, "poly" },
    /* 0x71 */  { "paintPoly",      0, "poly" },
    /* 0x72 */  { "erasePoly",      0, "poly" },
    /* 0x73 */  { "invertPoly",     0, "poly" },
    /* 0x74 */  { "fillPoly",       0, "poly" },
    /* 0x75 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x76 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x77 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x78 */  { "frameSamePoly",  0, "poly (NYI)" },
    /* 0x79 */  { "paintSamePoly",  0, "poly (NYI)" },
    /* 0x7a */  { "eraseSamePoly",  0, "poly (NYI)" },
    /* 0x7b */  { "invertSamePoly", 0, "poly (NYI)" },
    /* 0x7c */  { "fillSamePoly",   0, "poly (NYI)" },
    /* 0x7d */  { "reserved",       0, "reserved for Apple use" },
    /* 0x7e */  { "reserved",       0, "reserved for Apple use" },
    /* 0x7f */  { "reserved",       0, "reserved for Apple use" },
    /* 0x80 */  { "frameRgn",       0, "region" },
    /* 0x81 */  { "paintRgn",       0, "region" },
    /* 0x82 */  { "eraseRgn",       0, "region" },
    /* 0x83 */  { "invertRgn",      0, "region" },
    /* 0x84 */  { "fillRgn",        0, "region" },
    /* 0x85 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x86 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x87 */  { "reserved",       0, "reserved for Apple use" },
    /* 0x88 */  { "frameSameRgn",   0, "region (NYI)" },
    /* 0x89 */  { "paintSameRgn",   0, "region (NYI)" },
    /* 0x8a */  { "eraseSameRgn",   0, "region (NYI)" },
    /* 0x8b */  { "invertSameRgn",  0, "region (NYI)" },
    /* 0x8c */  { "fillSameRgn",    0, "region (NYI)" },
    /* 0x8d */  { "reserved",       0, "reserved for Apple use" },
    /* 0x8e */  { "reserved",       0, "reserved for Apple use" },
    /* 0x8f */  { "reserved",       0, "reserved for Apple use" },
    /* 0x90 */  { "BitsRect",       0, "copybits, rect clipped" },
    /* 0x91 */  { "BitsRgn",        0, "copybits, rgn clipped" },
    /* 0x92 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x93 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x94 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x95 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x96 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x97 */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x98 */  { "PackBitsRect",   0, "packed copybits, rect clipped" },
    /* 0x99 */  { "PackBitsRgn",    0, "packed copybits, rgn clipped" },
    /* 0x9a */  { "Opcode_9A",      0, "the mysterious opcode 9A" },
    /* 0x9b */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x9c */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x9d */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x9e */  { "reserved",      -1, "reserved for Apple use" },
    /* 0x9f */  { "reserved",      -1, "reserved for Apple use" },
    /* 0xa0 */  { "ShortComment",   2, "kind (word)" },
    /* 0xa1 */  { "LongComment",    0, "kind (word), size (word), data" }
  };


/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e c o d e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DecodeImage decompresses an image via Macintosh pack bits
%  decoding for Macintosh PICT images.
%
%  The format of the DecodeImage method is:
%
%      unsigned char* DecodeImage(Image *image,int bytes_per_line,
%        const int bits_per_pixel)
%
%  A description of each parameter follows:
%
%    o status:  Method DecodeImage returns True if all the pixels are
%      uncompressed without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%    o bytes_per_line: This integer identifies the number of bytes in a
%      scanline.
%
%    o bits_per_pixel: The number of bits in a pixel.
%
%
*/

static unsigned char *ExpandBuffer(unsigned char *pixels,int *bytes_per_line,
  const int bits_per_pixel)
{
  register int
    i;

  register unsigned char
    *p,
    *q;

  static unsigned char
    scanline[8*256];

  p=pixels;
  q=scanline;
  switch (bits_per_pixel)
  {
    case 8:
    case 16:
    case 32:
      return(pixels);
    case 4:
    {
      for (i=0; i < *bytes_per_line; i++)
      {
        *q++=(*p >> 4) & 0xff;
        *q++=(*p & 15);
        p++;
      }
      *bytes_per_line*=2;
      break;
    }
    case 2:
    {
      for (i=0; i < *bytes_per_line; i++)
      {
        *q++=(*p >> 6) & 0x03;
        *q++=(*p >> 4) & 0x03;
        *q++=(*p >> 2) & 0x03;
        *q++=(*p & 3);
        p++;
      }
      *bytes_per_line*=4;
      break;
    }
    case 1:
    {
      for (i=0; i < *bytes_per_line; i++)
      {
        *q++=(*p >> 7) & 0x01;
        *q++=(*p >> 6) & 0x01;
        *q++=(*p >> 5) & 0x01;
        *q++=(*p >> 4) & 0x01;
        *q++=(*p >> 3) & 0x01;
        *q++=(*p >> 2) & 0x01;
        *q++=(*p >> 1) & 0x01;
        *q++=(*p & 0x01);
        p++;
      }
      *bytes_per_line*=8;
      break;
    }
    default:
      break;
  }
  return(scanline);
}

static unsigned char* DecodeImage(Image *image,int bytes_per_line,
  const int bits_per_pixel)
{
  int
    bytes_per_pixel,
    k,
    length,
    number_pixels,
    packets,
    scanline_length,
    width;

  register int
    i,
    j;

  register unsigned char
    *p,
    *q;

  unsigned char
    *pixels,
    *scanline;

  /*
    Determine pixel buffer size.
  */
  if (bits_per_pixel <= 8)
    bytes_per_line&=0x7fff;
  width=image->columns;
  bytes_per_pixel=1;
  if (bits_per_pixel == 16)
    {
      bytes_per_pixel=2;
      width*=2;
    }
  else
    if (bits_per_pixel == 32)
      width*=image->matte ? 4 : 3;
  if (bytes_per_line == 0)
    bytes_per_line=width;
  packets=width;
  if (bytes_per_line < 8)
    packets=8*bytes_per_line;
  /*
    Allocate pixel and scanline buffer.
  */
  pixels=(unsigned char *)
    AllocateMemory((packets*image->rows+8)*sizeof(unsigned char));
  if (pixels == (unsigned char *) NULL)
    return((unsigned char *) NULL);
  scanline=(unsigned char *)
    AllocateMemory((bytes_per_line+65535)*sizeof(unsigned char));
  if (scanline == (unsigned char *) NULL)
    return((unsigned char *) NULL);
  if (bytes_per_line < 8)
    {
      /*
        Pixels are already uncompressed.
      */
      for (i=0; i < (int) image->rows; i++)
      {
        q=pixels+i*width;
        number_pixels=bytes_per_line;
        (void) ReadBlob(image,number_pixels,(char *) scanline);
        p=ExpandBuffer(scanline,&number_pixels,bits_per_pixel);
        for (j=0; j < number_pixels; j++)
          *q++=(*p++);
      }
      FreeMemory(scanline);
      return(pixels);
    }
  /*
    Uncompress RLE pixels into uncompressed pixel buffer.
  */
  for (i=0; i < (int) image->rows; i++)
  {
    q=pixels+i*width;
    if ((bytes_per_line > 250) || (bits_per_pixel > 8))
      scanline_length=MSBFirstReadShort(image);
    else
      scanline_length=ReadByte(image);
    (void) ReadBlob(image,scanline_length,(char *) scanline);
    for (j=0; j < scanline_length; )
      if ((scanline[j] & 0x80) == 0)
        {
          length=(scanline[j] & 0xff)+1;
          number_pixels=length*bytes_per_pixel;
          p=ExpandBuffer(scanline+j+1,&number_pixels,bits_per_pixel);
          for (k=0; k < number_pixels; k++)
            *q++=(*p++);
          j+=length*bytes_per_pixel+1;
        }
      else
        {
          int
            l;

          length=((scanline[j] ^ 0xff) & 0xff)+2;
          number_pixels=bytes_per_pixel;
          p=ExpandBuffer(scanline+j+1,&number_pixels,bits_per_pixel);
          for (k=0; k < length; k++)
          {
            for (l=0; l < number_pixels; l++)
              *q++=(*p++);
            p-=number_pixels;
          }
          j+=bytes_per_pixel+1;
        }
  }
  FreeMemory(scanline);
  return(pixels);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E n c o d e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method EncodeImage compresses an image via Macintosh pack bits encoding
%  for Macintosh PICT images.
%
%  The format of the EncodeImage method is:
%
%      status=EncodeImage(image,scanline,bytes_per_line,pixels)
%
%  A description of each parameter follows:
%
%    o status:  Method EncodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%    o scanline: A pointer to an array of characters to pack.
%
%    o bytes_per_line: The number of bytes in a scanline.
%
%    o pixels: A pointer to an array of characters where the packed
%      characters are stored.
%
%
*/
static unsigned int EncodeImage(Image *image,const unsigned char *scanline,
  const int bytes_per_line,unsigned char *pixels)
{
#define MaxCount  128
#define MaxPackbitsRunlength  128

  int
    count,
    repeat_count,
    runlength;

  register const unsigned char
    *p;

  register int
    i;

  register long
    packets;

  register unsigned char
    *q;

  unsigned char
    index;

  /*
    Pack scanline.
  */
  assert(image != (Image *) NULL);
  assert(scanline != (unsigned char *) NULL);
  assert(pixels != (unsigned char *) NULL);
  count=0;
  runlength=0;
  p=scanline+(bytes_per_line-1);
  q=pixels;
  index=(*p);
  for (i=bytes_per_line-1; i >= 0; i--)
  {
    if (index == *p)
      runlength++;
    else
      {
        if (runlength < 3)
          while (runlength > 0)
          {
            *q++=index;
            runlength--;
            count++;
            if (count == MaxCount)
              {
                *q++=MaxCount-1;
                count-=MaxCount;
              }
          }
        else
          {
            if (count > 0)
              *q++=count-1;
            count=0;
            while (runlength > 0)
            {
              repeat_count=runlength;
              if (repeat_count > MaxPackbitsRunlength)
                repeat_count=MaxPackbitsRunlength;
              *q++=index;
              *q++=257-repeat_count;
              runlength-=repeat_count;
            }
          }
        runlength=1;
      }
    index=(*p);
    p--;
  }
  if (runlength < 3)
    while (runlength > 0)
    {
      *q++=index;
      runlength--;
      count++;
      if (count == MaxCount)
        {
          *q++=MaxCount-1;
          count-=MaxCount;
        }
    }
  else
    {
      if (count > 0)
        *q++=count-1;
      count=0;
      while (runlength > 0)
      {
        repeat_count=runlength;
        if (repeat_count > MaxPackbitsRunlength)
          repeat_count=MaxPackbitsRunlength;
        *q++=index;
        *q++=257-repeat_count;
        runlength-=repeat_count;
      }
    }
  if (count > 0)
    *q++=count-1;
  /*
    Write the number of and the packed packets.
  */
  packets=(int) (q-pixels);
  if (bytes_per_line > 250)
    {
      MSBFirstWriteShort(image,(unsigned short) packets);
      packets+=2;
    }
  else
    {
      (void) WriteByte(image,packets);
      packets++;
    }
  while (q != pixels)
  {
    q--;
    (void) WriteByte(image,*q);
  }
  return(packets);
}

#if !defined(macintosh)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d P I C T I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadPICTImage reads an Apple Macintosh QuickDraw/PICT image file
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadPICTImage method is:
%
%      Image *ReadPICTImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadPICTImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadPICTImage(const ImageInfo *image_info)
{
  char
    geometry[MaxTextExtent];

  Image
    *image,
    *tiled_image;

  int
    c,
    code,
    flags,
    version;

  long
    length;

  PICTRectangle
    frame;

  PICTPixmap
    pixmap;

  register int
    i;

  register RunlengthPacket
    *q;

  unsigned int
    status;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  status=OpenBlob(image_info,image,ReadBinaryType);
  if (status == False)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  /*
    Read PICT header.
  */
  for (i=0; i < 512; i++)
    (void) ReadByte(image);  /* skip header */
  (void) MSBFirstReadShort(image);  /* skip picture size */
  ReadRectangle(frame);
  while ((c=ReadByte(image)) == 0);
  if (c != 0x11)
    ReaderExit(CorruptImageWarning,"Not a PICT image file",image);
  version=ReadByte(image);
  if (version == 2)
    {
      c=ReadByte(image);
      if (c != 0xff)
        ReaderExit(CorruptImageWarning,"Not a PICT image file",image);
    }
  else
    if (version != 1)
      ReaderExit(CorruptImageWarning,"Not a PICT image file",image);
  /*
    Create black canvas.
  */
  flags=0;
  image->columns=frame.right-frame.left;
  image->rows=frame.bottom-frame.top;
  if (image_info->ping)
    {
      CloseBlob(image);
      return(image);
    }
  image->packets=((Max(image->columns*image->rows,1)-1)/(MaxRunlength+1))+1;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  q=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    q->red=0;
    q->green=0;
    q->blue=0;
    q->index=0;
    SetRunlengthEncoder(q);
    q++;
  }
  q--;
  q->length=image->columns*image->rows-(MaxRunlength+1)*(image->packets-1)-1;
  /*
    Interpret PICT opcodes.
  */
  code=0;
  for ( ; ; )
  {
    if ((version == 1) || ((TellBlob(image) % 2) != 0))
      code=ReadByte(image);
    if (version == 2)
      code=MSBFirstReadShort(image);
    if ((code == 0xff) || (code == 0xffff))
      break;
    if (code < 0xa2)
      switch (code)
      {
        case 0x01:
        {
          /*
            Clipping rectangle.
          */
          length=MSBFirstReadShort(image);
          if (length != 0x000a)
            {
              for (i=0; i < (length-2); i++)
                (void) ReadByte(image);
              break;
            }
          ReadRectangle(frame);
          if ((frame.left & 0x8000) || (frame.top & 0x8000))
            break;
          image->columns=frame.right-frame.left;
          image->rows=frame.bottom-frame.top;
          image->packets=
            ((Max(image->columns*image->rows,1)-1)/(MaxRunlength+1))+1;
          image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
            image->pixels,image->packets*sizeof(RunlengthPacket));
          if (image->pixels == (RunlengthPacket *) NULL)
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          q=image->pixels;
          for (i=0; i < (int) image->packets; i++)
          {
            q->index=0;
            SetRunlengthEncoder(q);
            q++;
          }
          q--;
          q->length=image->columns*image->rows-(MaxRunlength+1)*
            (image->packets-1)-1;
          SyncImage(image);
          break;
        }
        case 0x12:
        case 0x13:
        case 0x14:
        {
          int
            pattern;

          unsigned int
            height,
            width;

          /*
            Skip pattern definition.
          */
          pattern=MSBFirstReadShort(image);
          for (i=0; i < 8; i++)
            (void) ReadByte(image);
          if (pattern == 2)
            {
              for (i=0; i < 5; i++)
                (void) ReadByte(image);
              break;
            }
          if (pattern != 1)
            ReaderExit(CorruptImageWarning,"Unknown pattern type",image);
          length=MSBFirstReadShort(image);
          ReadRectangle(frame);
          ReadPixmap(pixmap);
          (void) MSBFirstReadLong(image);
          flags=MSBFirstReadShort(image);
          length=MSBFirstReadShort(image);
          for (i=0; i <= length; i++)
            (void) MSBFirstReadLong(image);
          width=frame.bottom-frame.top;
          height=frame.right-frame.left;
          if (pixmap.bits_per_pixel <= 8)
            length&=0x7fff;
          if (pixmap.bits_per_pixel == 16)
            width<<=1;
          if (length == 0)
            length=width;
          if (length < 8)
            {
              for (i=0; i < (int) (length*height); i++)
                (void) ReadByte(image);
            }
          else
            for (i=0; i < (int) height; i++)
              if (length > 250)
                for (i=0; i < (int) MSBFirstReadShort(image); i++)
                  (void) ReadByte(image);
              else
                for (i=0; i < ReadByte(image); i++)
                  (void) ReadByte(image);
          break;
        }
        case 0x1b:
        {
          /*
            Initialize image background color.
          */
          image->background_color.red=XDownScale(MSBFirstReadShort(image));
          image->background_color.green=XDownScale(MSBFirstReadShort(image));
          image->background_color.blue=XDownScale(MSBFirstReadShort(image));
          break;
        }
        case 0x70:
        case 0x71:
        case 0x72:
        case 0x73:
        case 0x74:
        case 0x75:
        case 0x76:
        case 0x77:
        {
          /*
            Skip polygon or region.
          */
          length=MSBFirstReadShort(image);
          for (i=0; i < (length-2); i++)
            (void) ReadByte(image);
          break;
        }
        case 0x90:
        case 0x91:
        case 0x98:
        case 0x99:
        case 0x9a:
        {
          int
            bytes_per_line,
            j,
            y;

          PICTRectangle
            destination;

          Quantum
            blue,
            green,
            red;

          register int
            x;

          register long
            packets;

          register unsigned char
            *p;

          unsigned char
            *pixels;

          unsigned short
            index;

          /*
            Pixmap clipped by a rectangle.
          */
          bytes_per_line=0;
          if (code != 0x9a)
            bytes_per_line=MSBFirstReadShort(image);
          else
            {
              (void) MSBFirstReadShort(image);
              (void) MSBFirstReadShort(image);
              (void) MSBFirstReadShort(image);
            }
          ReadRectangle(frame);
          /*
            Initialize tiled_image.
          */
          tiled_image=AllocateImage(image_info);
          if (tiled_image == (Image *) NULL)
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          tiled_image->file=image->file;
          tiled_image->columns=frame.right-frame.left;
          tiled_image->rows=frame.bottom-frame.top;
          if ((code == 0x9a) || (bytes_per_line & 0x8000))
            {
              ReadPixmap(pixmap);
              tiled_image->matte=pixmap.component_count == 4;
            }
          if (code != 0x9a)
            {
              /*
                Initialize colormap.
              */
              tiled_image->colors=2;
              if (bytes_per_line & 0x8000)
                {
                  (void) MSBFirstReadLong(image);
                  flags=MSBFirstReadShort(image);
                  tiled_image->colors=MSBFirstReadShort(image)+1;
                }
              tiled_image->class=PseudoClass;
              tiled_image->colormap=(ColorPacket *)
                AllocateMemory(tiled_image->colors*sizeof(ColorPacket));
              if (tiled_image->colormap == (ColorPacket *) NULL)
                {
                  DestroyImage(tiled_image);
                  ReaderExit(ResourceLimitWarning,"Memory allocation failed",
                    image);
                }
              for (i=0; i < (int) tiled_image->colors; i++)
              {
                tiled_image->colormap[i].red=(Quantum)
                  ((long) (MaxRGB*i)/(tiled_image->colors-1));
                tiled_image->colormap[i].green=(Quantum)
                  ((long) (MaxRGB*i)/(tiled_image->colors-1));
                tiled_image->colormap[i].blue=(Quantum)
                  ((long) (MaxRGB*i)/(tiled_image->colors-1));
              }
              if (bytes_per_line & 0x8000)
                for (i=0; i < (int) tiled_image->colors; i++)
                {
                  j=MSBFirstReadShort(image) % tiled_image->colors;
                  if (flags & 0x8000)
                    j=i;
                  tiled_image->colormap[j].red=
                    XDownScale(MSBFirstReadShort(image));
                  tiled_image->colormap[j].green=
                    XDownScale(MSBFirstReadShort(image));
                  tiled_image->colormap[j].blue=
                    XDownScale(MSBFirstReadShort(image));
                }
            }
          ReadRectangle(destination);
          ReadRectangle(destination);
          (void) MSBFirstReadShort(image);
          if ((code == 0x91) || (code == 0x99))
            {
              /*
                Skip region.
              */
              length=MSBFirstReadShort(image);
              for (i=0; i < (length-2); i++)
                (void) ReadByte(image);
            }
          packets=0;
          tiled_image->pixels=(RunlengthPacket *) AllocateMemory(
            tiled_image->columns*tiled_image->rows*sizeof(RunlengthPacket));
          if ((code != 0x9a) && (bytes_per_line & 0x8000) == 0)
            pixels=DecodeImage(tiled_image,bytes_per_line,1);
          else
            pixels=DecodeImage(tiled_image,bytes_per_line,
              pixmap.bits_per_pixel);
          if ((tiled_image->pixels == (RunlengthPacket *) NULL) ||
              (pixels == (unsigned char *) NULL))
            {
              DestroyImage(tiled_image);
              ReaderExit(ResourceLimitWarning,"Memory allocation failed",
                image);
            }
          /*
            Convert PICT tiled image to runlength-encoded packets.
          */
          red=0;
          green=0;
          blue=0;
          index=0;
          p=pixels;
          q=tiled_image->pixels;
          SetRunlengthEncoder(q);
          for (y=0; y < (int) tiled_image->rows; y++)
          {
            for (x=0; x < (int) tiled_image->columns; x++)
            {
              if (tiled_image->class == PseudoClass)
                index=(*p++);
              else
                {
                  if (pixmap.bits_per_pixel == 16)
                    {
                      i=(*p++);
                      j=(*p);
                      red=UpScale((i & 0x7c) << 1);
                      green=UpScale(((i & 0x03) << 6) | ((j & 0xe0) >> 2));
                      blue=UpScale((j & 0x1f) << 3);
                    }
                  else
                    if (!tiled_image->matte)
                      {
                        red=UpScale(*p);
                        green=UpScale(*(p+tiled_image->columns));
                        blue=UpScale(*(p+2*tiled_image->columns));
                      }
                    else
                      {
                        index=UpScale(*p);
                        red=UpScale(*(p+tiled_image->columns));
                        green=UpScale(*(p+2*tiled_image->columns));
                        blue=UpScale(*(p+3*tiled_image->columns));
                      }
                  p++;
                }
              if ((red == q->red) && (green == q->green) &&
                  (blue == q->blue) && (index == q->index) &&
                  ((int) q->length < MaxRunlength))
                q->length++;
              else
                {
                  if (packets != 0)
                    q++;
                  packets++;
                  q->red=red;
                  q->green=green;
                  q->blue=blue;
                  q->index=index;
                  q->length=0;
                }
            }
            if ((tiled_image->class == DirectClass) &&
                (pixmap.bits_per_pixel != 16))
              p+=(pixmap.component_count-1)*tiled_image->columns;
            if (destination.bottom == (int) image->rows)
              if (QuantumTick(y,tiled_image->rows))
                ProgressMonitor(LoadImageText,y,tiled_image->rows);
          }
          SetRunlengthPackets(tiled_image,packets);
          if (tiled_image->class == PseudoClass)
            SyncImage(tiled_image);
          (void) FreeMemory((char *) pixels);
          CompositeImage(image,ReplaceCompositeOp,tiled_image,destination.left,
            destination.top);
          tiled_image->file=(FILE *) NULL;
          DestroyImage(tiled_image);
          if (destination.bottom != (int) image->rows)
            ProgressMonitor(LoadImageText,destination.bottom,image->rows);
          break;
        }
        case 0xa1:
        {
          /*
            Comment.
          */
          (void) MSBFirstReadShort(image);
          length=MSBFirstReadShort(image);
          if (length == 0)
            break;
          if (image->comments != (char *) NULL)
            FreeMemory((char *) image->comments);
          image->comments=(char *) AllocateMemory((length+1)*sizeof(char));
          if (image->comments == (char *) NULL)
            break;
          for (i=0; i < length; i++)
            image->comments[i]=ReadByte(image);
          image->comments[i]='\0';
          break;
        }
        default:
        {
          /*
            Skip to next op code.
          */
          if (codes[code].length == -1)
            (void) MSBFirstReadShort(image);
          else
            for (i=0; i < codes[code].length; i++)
              (void) ReadByte(image);
        }
      }
    if (code == 0xc00)
      {
        /*
          Skip header.
        */
        for (i=0; i < 24; i++)
          (void) ReadByte(image);
        continue;
      }
    if (((code >= 0xb0) && (code <= 0xcf)) ||
        ((code >= 0x8000) && (code <= 0x80ff)))
      continue;
    if (code == 0x8200)
      {
        FILE
          *file;

        ImageInfo
          *local_info;

        /*
          Embedded JPEG.
        */
        local_info=CloneImageInfo(image_info);
        if (local_info == (ImageInfo *) NULL)
          ReaderExit(FileOpenWarning,"Unable to write file",image);
        GetBlobInfo(&(local_info->blob));
        TemporaryFilename(local_info->filename);
        file=fopen(local_info->filename,WriteBinaryType);
        if (file == (FILE *) NULL)
          ReaderExit(FileOpenWarning,"Unable to write file",image);
        length=MSBFirstReadLong(image);
        for (i=0; i < 6; i++)
          (void) MSBFirstReadLong(image);
        ReadRectangle(frame);
        for (i=0; i < 122; i++)
          (void) ReadByte(image);
        for (i=0; i < (length-154); i++)
        {
          c=ReadByte(image);
          (void) putc(c,file);
        }
        (void) fclose(file);
        tiled_image=ReadJPEGImage(local_info);
        DestroyImageInfo(local_info);
        (void) remove(local_info->filename);
        if (tiled_image == (Image *) NULL)
          continue;
        FormatString(geometry,"%ux%u",Max(image->columns,tiled_image->columns),
          Max(image->rows,tiled_image->rows));
        TransformImage(&image,(char *) NULL,geometry);
        CompositeImage(image,ReplaceCompositeOp,tiled_image,
          frame.left,frame.right);
        tiled_image->file=(FILE *) NULL;
        DestroyImage(tiled_image);
        continue;
      }
    if (((code >= 0xd0) && (code <= 0xfe)) ||
        ((code >= 0x8100) && (code <= 0xffff)))
      {
        /*
          Skip reserved.
        */
        length=MSBFirstReadShort(image);
        for (i=0; i < length; i++)
          (void) ReadByte(image);
        continue;
      }
    if ((code >= 0x100) && (code <= 0x7fff))
      {
        /*
          Skip reserved.
        */
        length=(code >> 7) & 0xff;
        for (i=0; i < length; i++)
          (void) ReadByte(image);
        continue;
      }
  }
  CondenseImage(image);
  return(image);
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e P I C T I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WritePICTImage writes an image to a file in the Apple Macintosh
%  QuickDraw/PICT image format.
%
%  The format of the WritePICTImage method is:
%
%      unsigned int WritePICTImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WritePICTImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WritePICTImage(const ImageInfo *image_info,Image *image)
{
#define MaxCount  128
#define PictCropRegionOp  0x01
#define PictEndOfPictureOp  0xff
#define PictHeaderOp  0x0C00
#define PictHeaderSize  512
#define PictPixmapOp  0x9A
#define PictPICTOp  0x98
#define PictVersion  0x11

  int
    count,
    x,
    y;

  PICTPixmap
    pixmap;

  PICTRectangle
    bounds,
    crop_rectangle,
    destination_rectangle,
    frame_rectangle,
    size_rectangle,
    source_rectangle;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  unsigned char
    *buffer,
    *packed_scanline,
    *scanline;

  unsigned int
    bytes_per_line,
    status;

  unsigned long
    horizontal_resolution,
    vertical_resolution;

  unsigned short
    base_address,
    row_bytes,
    transfer_mode;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  TransformRGBImage(image,RGBColorspace);
  /*
    Allocate memory.
  */
  bytes_per_line=image->columns;
  if ((Latin1Compare(image_info->magick,"PICT24") == 0) || 
      !IsPseudoClass(image))
    bytes_per_line*=image->matte ? 4 : 3;
  buffer=(unsigned char *)
    AllocateMemory(PictHeaderSize*sizeof(unsigned char));
  packed_scanline=(unsigned char *) AllocateMemory((bytes_per_line+
    bytes_per_line/MaxCount+1)*sizeof(unsigned char));
  scanline=(unsigned char *)
    AllocateMemory(bytes_per_line*sizeof(unsigned char));
  if ((buffer == (unsigned char *) NULL) ||
      (packed_scanline == (unsigned char *) NULL) ||
      (scanline == (unsigned char *) NULL))
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Initialize image info.
  */
  size_rectangle.top=0;
  size_rectangle.left=0;
  size_rectangle.right=image->rows;
  size_rectangle.bottom=image->columns;
  frame_rectangle=size_rectangle;
  crop_rectangle=size_rectangle;
  source_rectangle=size_rectangle;
  destination_rectangle=size_rectangle;
  horizontal_resolution=0x00480000;
  vertical_resolution=0x00480000;
  base_address=0xff;
  row_bytes=image->columns | 0x8000;
  bounds.top=0;
  bounds.left=0;
  bounds.right=image->rows;
  bounds.bottom=image->columns;
  pixmap.version=0;
  pixmap.pack_type=0;
  pixmap.pack_size=0;
  pixmap.horizontal_resolution=horizontal_resolution;
  pixmap.vertical_resolution=vertical_resolution;
  pixmap.pixel_type=0;
  pixmap.bits_per_pixel=8;
  pixmap.component_count=1;
  pixmap.component_size=8;
  pixmap.plane_bytes=0;
  pixmap.table=0;
  pixmap.reserved=0;
  transfer_mode=0;
  if ((Latin1Compare(image_info->magick,"PICT24") == 0) ||
      (image->class == DirectClass))
    {
      pixmap.component_count=image->matte ? 4 : 3;
      pixmap.pixel_type=16;
      pixmap.bits_per_pixel=32;
      pixmap.pack_type=0x04;
      transfer_mode=0x40;
      row_bytes=(4*image->columns) | 0x8000;
    }
  /*
    Write header, header size, size bounding box, version, and reserved.
  */
  for (i=0; i < PictHeaderSize; i++)
    buffer[i]=0;
  (void) WriteBlob(image,PictHeaderSize,(char *) buffer);
  MSBFirstWriteShort(image,0);
  MSBFirstWriteShort(image,size_rectangle.top);
  MSBFirstWriteShort(image,size_rectangle.left);
  MSBFirstWriteShort(image,size_rectangle.right);
  MSBFirstWriteShort(image,size_rectangle.bottom);
  MSBFirstWriteShort(image,PictVersion);
  MSBFirstWriteShort(image,0x02ff);
  MSBFirstWriteShort(image,PictHeaderOp);
  /*
    Write full size of the file, resolution, frame bounding box, and reserved.
  */
  MSBFirstWriteLong(image,0xFFFE0000UL);
  MSBFirstWriteLong(image,horizontal_resolution);
  MSBFirstWriteLong(image,vertical_resolution);
  MSBFirstWriteShort(image,frame_rectangle.top);
  MSBFirstWriteShort(image,frame_rectangle.left);
  MSBFirstWriteShort(image,frame_rectangle.right);
  MSBFirstWriteShort(image,frame_rectangle.bottom);
  MSBFirstWriteLong(image,0x00000000L);
  /*
    Write crop region opcode and crop bounding box.
  */
  MSBFirstWriteShort(image,PictCropRegionOp);
  MSBFirstWriteShort(image,0xa);
  MSBFirstWriteShort(image,crop_rectangle.top);
  MSBFirstWriteShort(image,crop_rectangle.left);
  MSBFirstWriteShort(image,crop_rectangle.right);
  MSBFirstWriteShort(image,crop_rectangle.bottom);
  /*
    Write picture opcode, row bytes, and picture bounding box, and version.
  */
  if ((Latin1Compare(image_info->magick,"PICT24") != 0) &&
      (image->class == PseudoClass))
    MSBFirstWriteShort(image,PictPICTOp);
  else
    {
      MSBFirstWriteShort(image,PictPixmapOp);
      MSBFirstWriteLong(image,(unsigned long) base_address);
    }
  MSBFirstWriteShort(image,row_bytes | 0x8000);
  MSBFirstWriteShort(image,bounds.top);
  MSBFirstWriteShort(image,bounds.left);
  MSBFirstWriteShort(image,bounds.right);
  MSBFirstWriteShort(image,bounds.bottom);
  /*
    Write pack type, pack size, resolution, pixel type, and pixel size.
  */
  MSBFirstWriteShort(image,pixmap.version);
  MSBFirstWriteShort(image,pixmap.pack_type);
  MSBFirstWriteLong(image,pixmap.pack_size);
  MSBFirstWriteLong(image,pixmap.horizontal_resolution);
  MSBFirstWriteLong(image,pixmap.vertical_resolution);
  MSBFirstWriteShort(image,pixmap.pixel_type);
  MSBFirstWriteShort(image,pixmap.bits_per_pixel);
  /*
    Write component count, size, plane bytes, table size, and reserved.
  */
  MSBFirstWriteShort(image,pixmap.component_count);
  MSBFirstWriteShort(image,pixmap.component_size);
  MSBFirstWriteLong(image,(unsigned long) pixmap.plane_bytes);
  MSBFirstWriteLong(image,(unsigned long) pixmap.table);
  MSBFirstWriteLong(image,(unsigned long) pixmap.reserved);
  if ((Latin1Compare(image_info->magick,"PICT24") != 0) &&
      (image->class == PseudoClass))
    {
      unsigned short
        red,
        green,
        blue;

      /*
        Write image colormap.
      */
      MSBFirstWriteLong(image,0x00000000L);  /* color seed */
      MSBFirstWriteShort(image,0L);  /* color flags */
      MSBFirstWriteShort(image,(unsigned short) (image->colors-1));
      for (i=0; i < (int) image->colors; i++)
      {
        red=((unsigned long) (image->colormap[i].red*65535L)/MaxRGB);
        green=((unsigned long) (image->colormap[i].green*65535L)/MaxRGB);
        blue=((unsigned long) (image->colormap[i].blue*65535L)/MaxRGB);
        MSBFirstWriteShort(image,(unsigned int) i);
        MSBFirstWriteShort(image,red);
        MSBFirstWriteShort(image,green);
        MSBFirstWriteShort(image,blue);
      }
    }
  /*
    Write source and destination rectangle.
  */
  MSBFirstWriteShort(image,source_rectangle.top);
  MSBFirstWriteShort(image,source_rectangle.left);
  MSBFirstWriteShort(image,source_rectangle.right);
  MSBFirstWriteShort(image,source_rectangle.bottom);
  MSBFirstWriteShort(image,destination_rectangle.top);
  MSBFirstWriteShort(image,destination_rectangle.left);
  MSBFirstWriteShort(image,destination_rectangle.right);
  MSBFirstWriteShort(image,destination_rectangle.bottom);
  MSBFirstWriteShort(image,transfer_mode);
  /*
    Write picture data.
  */
  count=0;
  x=0;
  y=0;
  p=image->pixels;
  if ((Latin1Compare(image_info->magick,"PICT24") != 0) &&
      (image->class == PseudoClass))
    {
      register unsigned char
        *index;

      index=scanline;
      for (i=0; i < (int) image->packets; i++)
      {
        for (j=0; j <= ((int) p->length); j++)
        {
          *index++=(unsigned char) p->index;
          x++;
          if (x == (int) image->columns)
            {
              count+=
                EncodeImage(image,scanline,row_bytes & 0x7FFF,packed_scanline);
              if (QuantumTick(y,image->rows))
                ProgressMonitor(SaveImageText,y,image->rows);
              index=scanline;
              x=0;
              y++;
            }
        }
        p++;
      }
    }
  else
    {
      register unsigned char
        *blue,
        *green,
        *index,
        *red;

      red=scanline;
      green=scanline+image->columns;
      blue=scanline+2*image->columns;
      index=(unsigned char *) NULL;
      if (image->matte)
        {
          index=scanline;
          red=scanline+image->columns;
          green=scanline+2*image->columns;
          blue=scanline+3*image->columns;
        }
      for (i=0; i < (int) image->packets; i++)
      {
        for (j=0; j <= ((int) p->length); j++)
        {
          *red++=DownScale(p->red);
          *green++=DownScale(p->green);
          *blue++=DownScale(p->blue);
          if (image->matte)
            *index++=DownScale(p->index);
          x++;
          if (x == (int) image->columns)
            {
              if (QuantumTick(y,image->rows))
                ProgressMonitor(SaveImageText,y,image->rows);
              red=scanline;
              green=scanline+image->columns;
              blue=scanline+2*image->columns;
              if (image->matte)
                {
                  index=scanline;
                  red=scanline+image->columns;
                  green=scanline+2*image->columns;
                  blue=scanline+3*image->columns;
                }
              count+=
                EncodeImage(image,scanline,row_bytes & 0x7FFF,packed_scanline);
              x=0;
              y++;
            }
        }
        p++;
      }
    }
  if (count & 0x1)
    (void) WriteByte(image,'\0');
  MSBFirstWriteShort(image,PictEndOfPictureOp);
  FreeMemory((char *) scanline);
  FreeMemory((char *) packed_scanline);
  FreeMemory((char *) buffer);
  CloseBlob(image);
  return(True);
}
