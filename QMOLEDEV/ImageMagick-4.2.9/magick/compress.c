/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%           CCCC   OOO   M   M  PPPP   RRRR   EEEEE   SSSSS  SSSSS            %
%          C      O   O  MM MM  P   P  R   R  E       SS     SS               %
%          C      O   O  M M M  PPPP   RRRR   EEE      SSS    SSS             %
%          C      O   O  M   M  P      R R    E          SS     SS            %
%           CCCC   OOO   M   M  P      R  R   EEEEE   SSSSS  SSSSS            %
%                                                                             %
%                                                                             %
%                  Image Compression/Decompression Methods                    %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                              May  1993                                      %
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
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"
#if defined(HasTIFF)
#include "tiffio.h"
#endif
#if defined(HasZLIB)
#include "zlib.h"
#endif

/*
  Define declarations.
*/
#define LoadImageText  "  Loading image...  "
#define SaveImageText  "  Saving image...  "

/*
  Typedef declarations.
*/
typedef struct HuffmanTable
{
  int
    id,
    code,
    length,
    count;
} HuffmanTable;

typedef struct _ScanlinePacket
{
  unsigned char
    pixel;

  int
    state;
} ScanlinePacket;

/*
  Huffman coding declarations.
*/
#define TWId  23
#define MWId  24
#define TBId  25
#define MBId  26
#define EXId  27

static const HuffmanTable
  MBTable[]=
  {
    { MBId, 0x0f, 10, 64 }, { MBId, 0xc8, 12, 128 },
    { MBId, 0xc9, 12, 192 }, { MBId, 0x5b, 12, 256 },
    { MBId, 0x33, 12, 320 }, { MBId, 0x34, 12, 384 },
    { MBId, 0x35, 12, 448 }, { MBId, 0x6c, 13, 512 },
    { MBId, 0x6d, 13, 576 }, { MBId, 0x4a, 13, 640 },
    { MBId, 0x4b, 13, 704 }, { MBId, 0x4c, 13, 768 },
    { MBId, 0x4d, 13, 832 }, { MBId, 0x72, 13, 896 },
    { MBId, 0x73, 13, 960 }, { MBId, 0x74, 13, 1024 },
    { MBId, 0x75, 13, 1088 }, { MBId, 0x76, 13, 1152 },
    { MBId, 0x77, 13, 1216 }, { MBId, 0x52, 13, 1280 },
    { MBId, 0x53, 13, 1344 }, { MBId, 0x54, 13, 1408 },
    { MBId, 0x55, 13, 1472 }, { MBId, 0x5a, 13, 1536 },
    { MBId, 0x5b, 13, 1600 }, { MBId, 0x64, 13, 1664 },
    { MBId, 0x65, 13, 1728 }, { MBId, 0x00, 0, 0 }
  };

static const HuffmanTable
  EXTable[]=
  {
    { EXId, 0x08, 11, 1792 }, { EXId, 0x0c, 11, 1856 },
    { EXId, 0x0d, 11, 1920 }, { EXId, 0x12, 12, 1984 },
    { EXId, 0x13, 12, 2048 }, { EXId, 0x14, 12, 2112 },
    { EXId, 0x15, 12, 2176 }, { EXId, 0x16, 12, 2240 },
    { EXId, 0x17, 12, 2304 }, { EXId, 0x1c, 12, 2368 },
    { EXId, 0x1d, 12, 2432 }, { EXId, 0x1e, 12, 2496 },
    { EXId, 0x1f, 12, 2560 }, { EXId, 0x00, 0, 0 }
  };

static const HuffmanTable
  MWTable[]=
  {
    { MWId, 0x1b, 5, 64 }, { MWId, 0x12, 5, 128 },
    { MWId, 0x17, 6, 192 }, { MWId, 0x37, 7, 256 },
    { MWId, 0x36, 8, 320 }, { MWId, 0x37, 8, 384 },
    { MWId, 0x64, 8, 448 }, { MWId, 0x65, 8, 512 },
    { MWId, 0x68, 8, 576 }, { MWId, 0x67, 8, 640 },
    { MWId, 0xcc, 9, 704 }, { MWId, 0xcd, 9, 768 },
    { MWId, 0xd2, 9, 832 }, { MWId, 0xd3, 9, 896 },
    { MWId, 0xd4, 9, 960 }, { MWId, 0xd5, 9, 1024 },
    { MWId, 0xd6, 9, 1088 }, { MWId, 0xd7, 9, 1152 },
    { MWId, 0xd8, 9, 1216 }, { MWId, 0xd9, 9, 1280 },
    { MWId, 0xda, 9, 1344 }, { MWId, 0xdb, 9, 1408 },
    { MWId, 0x98, 9, 1472 }, { MWId, 0x99, 9, 1536 },
    { MWId, 0x9a, 9, 1600 }, { MWId, 0x18, 6, 1664 },
    { MWId, 0x9b, 9, 1728 }, { MWId, 0x00, 0, 0 }
  };

static const HuffmanTable
  TBTable[]=
  {
    { TBId, 0x37, 10, 0 }, { TBId, 0x02, 3, 1 }, { TBId, 0x03, 2, 2 },
    { TBId, 0x02, 2, 3 }, { TBId, 0x03, 3, 4 }, { TBId, 0x03, 4, 5 },
    { TBId, 0x02, 4, 6 }, { TBId, 0x03, 5, 7 }, { TBId, 0x05, 6, 8 },
    { TBId, 0x04, 6, 9 }, { TBId, 0x04, 7, 10 }, { TBId, 0x05, 7, 11 },
    { TBId, 0x07, 7, 12 }, { TBId, 0x04, 8, 13 }, { TBId, 0x07, 8, 14 },
    { TBId, 0x18, 9, 15 }, { TBId, 0x17, 10, 16 }, { TBId, 0x18, 10, 17 },
    { TBId, 0x08, 10, 18 }, { TBId, 0x67, 11, 19 }, { TBId, 0x68, 11, 20 },
    { TBId, 0x6c, 11, 21 }, { TBId, 0x37, 11, 22 }, { TBId, 0x28, 11, 23 },
    { TBId, 0x17, 11, 24 }, { TBId, 0x18, 11, 25 }, { TBId, 0xca, 12, 26 },
    { TBId, 0xcb, 12, 27 }, { TBId, 0xcc, 12, 28 }, { TBId, 0xcd, 12, 29 },
    { TBId, 0x68, 12, 30 }, { TBId, 0x69, 12, 31 }, { TBId, 0x6a, 12, 32 },
    { TBId, 0x6b, 12, 33 }, { TBId, 0xd2, 12, 34 }, { TBId, 0xd3, 12, 35 },
    { TBId, 0xd4, 12, 36 }, { TBId, 0xd5, 12, 37 }, { TBId, 0xd6, 12, 38 },
    { TBId, 0xd7, 12, 39 }, { TBId, 0x6c, 12, 40 }, { TBId, 0x6d, 12, 41 },
    { TBId, 0xda, 12, 42 }, { TBId, 0xdb, 12, 43 }, { TBId, 0x54, 12, 44 },
    { TBId, 0x55, 12, 45 }, { TBId, 0x56, 12, 46 }, { TBId, 0x57, 12, 47 },
    { TBId, 0x64, 12, 48 }, { TBId, 0x65, 12, 49 }, { TBId, 0x52, 12, 50 },
    { TBId, 0x53, 12, 51 }, { TBId, 0x24, 12, 52 }, { TBId, 0x37, 12, 53 },
    { TBId, 0x38, 12, 54 }, { TBId, 0x27, 12, 55 }, { TBId, 0x28, 12, 56 },
    { TBId, 0x58, 12, 57 }, { TBId, 0x59, 12, 58 }, { TBId, 0x2b, 12, 59 },
    { TBId, 0x2c, 12, 60 }, { TBId, 0x5a, 12, 61 }, { TBId, 0x66, 12, 62 },
    { TBId, 0x67, 12, 63 }, { TBId, 0x00, 0, 0 }
  };

static const HuffmanTable
  TWTable[]=
  {
    { TWId, 0x35, 8, 0 }, { TWId, 0x07, 6, 1 }, { TWId, 0x07, 4, 2 },
    { TWId, 0x08, 4, 3 }, { TWId, 0x0b, 4, 4 }, { TWId, 0x0c, 4, 5 },
    { TWId, 0x0e, 4, 6 }, { TWId, 0x0f, 4, 7 }, { TWId, 0x13, 5, 8 },
    { TWId, 0x14, 5, 9 }, { TWId, 0x07, 5, 10 }, { TWId, 0x08, 5, 11 },
    { TWId, 0x08, 6, 12 }, { TWId, 0x03, 6, 13 }, { TWId, 0x34, 6, 14 },
    { TWId, 0x35, 6, 15 }, { TWId, 0x2a, 6, 16 }, { TWId, 0x2b, 6, 17 },
    { TWId, 0x27, 7, 18 }, { TWId, 0x0c, 7, 19 }, { TWId, 0x08, 7, 20 },
    { TWId, 0x17, 7, 21 }, { TWId, 0x03, 7, 22 }, { TWId, 0x04, 7, 23 },
    { TWId, 0x28, 7, 24 }, { TWId, 0x2b, 7, 25 }, { TWId, 0x13, 7, 26 },
    { TWId, 0x24, 7, 27 }, { TWId, 0x18, 7, 28 }, { TWId, 0x02, 8, 29 },
    { TWId, 0x03, 8, 30 }, { TWId, 0x1a, 8, 31 }, { TWId, 0x1b, 8, 32 },
    { TWId, 0x12, 8, 33 }, { TWId, 0x13, 8, 34 }, { TWId, 0x14, 8, 35 },
    { TWId, 0x15, 8, 36 }, { TWId, 0x16, 8, 37 }, { TWId, 0x17, 8, 38 },
    { TWId, 0x28, 8, 39 }, { TWId, 0x29, 8, 40 }, { TWId, 0x2a, 8, 41 },
    { TWId, 0x2b, 8, 42 }, { TWId, 0x2c, 8, 43 }, { TWId, 0x2d, 8, 44 },
    { TWId, 0x04, 8, 45 }, { TWId, 0x05, 8, 46 }, { TWId, 0x0a, 8, 47 },
    { TWId, 0x0b, 8, 48 }, { TWId, 0x52, 8, 49 }, { TWId, 0x53, 8, 50 },
    { TWId, 0x54, 8, 51 }, { TWId, 0x55, 8, 52 }, { TWId, 0x24, 8, 53 },
    { TWId, 0x25, 8, 54 }, { TWId, 0x58, 8, 55 }, { TWId, 0x59, 8, 56 },
    { TWId, 0x5a, 8, 57 }, { TWId, 0x5b, 8, 58 }, { TWId, 0x4a, 8, 59 },
    { TWId, 0x4b, 8, 60 }, { TWId, 0x32, 8, 61 }, { TWId, 0x33, 8, 62 },
    { TWId, 0x34, 8, 63 }, { TWId, 0x00, 0, 0 }
  };

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   A S C I I 8 5 E n c o d e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ASCII85Encode encodes data in ASCII base-85 format.  ASCII base-85
%  encoding produces five ASCII printing characters from every four bytes of
%  binary data.
%
%  The format of the ASCII85Encode method is:
%
%      void Ascii85Initialize(void)
%
%  A description of each parameter follows:
%
%    o code: a binary unsigned char to encode to ASCII 85.
%
%    o file: write the encoded ASCII character to this file.
%
%
*/
#define MaxLineExtent  36

static int
  offset,
  line_break;

static unsigned char
  ascii85_buffer[10];

static char *Ascii85Tuple(unsigned char *data)
{
  static char
    tuple[6];

  register unsigned int
    word,
    x;

  register unsigned short
    y;

  word=(((data[0] << 8) | data[1]) << 16) | (data[2] << 8) | data[3];
  if (word == 0L)
    {
      tuple[0]='z';
      tuple[1]='\0';
      return(tuple);
    }
  x=(unsigned int) (word/(85L*85*85*85));
  tuple[0]=x+'!';
  word-=x*(85L*85*85*85);
  x=(unsigned int) (word/(85L*85*85));
  tuple[1]=x+'!';
  word-=x*(85L*85*85);
  x=(unsigned int) (word/(85*85));
  tuple[2]=x+'!';
  y=(unsigned short) (word-x*(85L*85));
  tuple[3]=(y/85)+'!';
  tuple[4]=(y % 85)+'!';
  tuple[5]='\0';
  return(tuple);
}

Export void Ascii85Initialize(void)
{
  line_break=MaxLineExtent << 1;
  offset=0;
}

Export void Ascii85Flush(Image *image)
{
  register char
    *tuple;

  assert(image != (Image *) NULL);
  if (offset > 0)
    {
      ascii85_buffer[offset]=0;
      ascii85_buffer[offset+1]=0;
      ascii85_buffer[offset+2]=0;
      tuple=Ascii85Tuple(ascii85_buffer);
      (void) WriteBlob(image,offset+1,*tuple == 'z' ? "!!!!" : tuple);
    }
  (void) WriteByte(image,'~');
  (void) WriteByte(image,'>');
  (void) WriteByte(image,'\n');
}

Export void Ascii85Encode(Image *image,const unsigned int code)
{
  int
    n;

  register char
    *q;

  register unsigned char
    *p;

  assert(image != (Image *) NULL);
  ascii85_buffer[offset]=code;
  offset++;
  if (offset < 4)
    return;
  p=ascii85_buffer;
  for (n=offset; n >= 4; n-=4)
  {
    for (q=Ascii85Tuple(p); *q; q++)
    {
      line_break--;
      if ((line_break < 0) && (*(q+1) != '%'))
        {
          (void) WriteByte(image,'\n');
          line_break=2*MaxLineExtent;
        }
      (void) WriteByte(image,*q);
    }
    p+=8;
  }
  offset=n;
  p-=4;
  for (n=0; n < 4; n++)
    ascii85_buffer[n]=(*p++);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   H u f f m a n D e c o d e I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method HuffmanDecodeImage uncompresses an image via Huffman-coding.
%
%  The format of the HuffmanDecodeImage method is:
%
%      unsigned int HuffmanDecodeImage(Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method HuffmanDecodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int HuffmanDecodeImage(Image *image)
{
#define HashSize  1021
#define MBHashA  293
#define MBHashB  2695
#define MWHashA  3510
#define MWHashB  1178

#define InitializeHashTable(hash,table,a,b) \
{ \
  entry=table; \
  while (entry->code != 0) \
  {  \
    hash[((entry->length+a)*(entry->code+b)) % HashSize]=(HuffmanTable *) entry; \
    entry++; \
  } \
}

#define InputBit(bit)  \
{  \
  if ((mask & 0xff) == 0)  \
    {  \
      byte=getc(image->file);  \
      mask=0x80;  \
    }  \
  runlength++;  \
  bit=byte & mask ? 1 : 0; \
  mask>>=1;  \
  if (bit)  \
    runlength=0;  \
  if (EOFBlob(image))  \
    break;  \
}

  const HuffmanTable
    *entry;

  HuffmanTable
    **mb_hash,
    **mw_hash;

  int
    bail,
    code,
    color,
    count,
    length,
    null_lines,
    runlength,
    x,
    y;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    bit,
    byte,
    mask,
    *scanline;

  unsigned long
    packets,
    max_packets;

  unsigned short
    index;

  /*
    Allocate buffers.
  */
  assert(image != (Image *) NULL);
  mb_hash=(HuffmanTable **) AllocateMemory(HashSize*sizeof(HuffmanTable *));
  mw_hash=(HuffmanTable **) AllocateMemory(HashSize*sizeof(HuffmanTable *));
  scanline=(unsigned char *)
    AllocateMemory(image->columns*sizeof(unsigned char));
  if ((mb_hash == (HuffmanTable **) NULL) ||
      (mw_hash == (HuffmanTable **) NULL) ||
      (scanline == (unsigned char *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  /*
    Initialize Huffman tables.
  */
  for (i=0; i < HashSize; i++)
  {
    mb_hash[i]=(HuffmanTable *) NULL;
    mw_hash[i]=(HuffmanTable *) NULL;
  }
  InitializeHashTable(mw_hash,TWTable,MWHashA,MWHashB);
  InitializeHashTable(mw_hash,MWTable,MWHashA,MWHashB);
  InitializeHashTable(mw_hash,EXTable,MWHashA,MWHashB);
  InitializeHashTable(mb_hash,TBTable,MBHashA,MBHashB);
  InitializeHashTable(mb_hash,MBTable,MBHashA,MBHashB);
  InitializeHashTable(mb_hash,EXTable,MBHashA,MBHashB);
  /*
    Uncompress 1D Huffman to runlength encoded pixels.
  */
  byte=0;
  mask=0;
  null_lines=0;
  runlength=0;
  while (runlength < 11)
   InputBit(bit);
  do { InputBit(bit); } while (bit == 0);
  packets=0;
  max_packets=image->packets;
  image->x_resolution=204.0;
  image->y_resolution=196.0;
  image->units=PixelsPerInchResolution;
  image->packets=0;
  q=image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; ((y < (int) image->rows) && (null_lines < 3)); )
  {
    /*
      Initialize scanline to white.
    */
    p=scanline;
    for (x=0; x < (int) image->columns; x++)
      *p++=0;
    /*
      Decode Huffman encoded scanline.
    */
    color=True;
    code=0;
    count=0;
    length=0;
    runlength=0;
    x=0;
    for ( ; ; )
    {
      if (x >= (int) image->columns)
        {
          while (runlength < 11)
           InputBit(bit);
          do { InputBit(bit); } while (bit == 0);
          break;
        }
      bail=False;
      do
      {
        if (runlength < 11)
          InputBit(bit)
        else
          {
            InputBit(bit);
            if (bit)
              {
                null_lines++;
                if (x != 0)
                  null_lines=0;
                bail=True;
                break;
              }
          }
        code=(code << 1)+bit;
        length++;
      } while (code <= 0);
      if (bail)
        break;
      if (length > 13)
        {
          while (runlength < 11)
           InputBit(bit);
          do { InputBit(bit); } while (bit == 0);
          break;
        }
      if (color)
        {
          if (length < 4)
            continue;
          entry=mw_hash[((length+MWHashA)*(code+MWHashB)) % HashSize];
        }
      else
        {
          if (length < 2)
            continue;
          entry=mb_hash[((length+MBHashA)*(code+MBHashB)) % HashSize];
        }
      if (!entry)
        continue;
      if ((entry->length != length) || (entry->code != code))
        continue;
      switch (entry->id)
      {
        case TWId:
        case TBId:
        {
          count+=entry->count;
          if ((x+count) > (int) image->columns)
            count=image->columns-x;
          if (count > 0)
            {
              if (color)
                {
                  x+=count;
                  count=0;
                }
              else
                for ( ; count > 0; count--)
                  scanline[x++]=1;
            }
          color=!color;
          break;
        }
        case MWId:
        case MBId:
        case EXId:
        {
          count+=entry->count;
          break;
        }
        default:
          break;
      }
      code=0;
      length=0;
    }
    /*
      Transfer scanline to image pixels.
    */
    p=scanline;
    for (x=0; x < (int) image->columns; x++)
    {
      index=(unsigned short) (*p++);
      if ((index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (packets != 0)
            q++;
          packets++;
          if (packets == max_packets)
            {
              max_packets<<=1;
              image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
                image->pixels,max_packets*sizeof(RunlengthPacket));
              if (image->pixels == (RunlengthPacket *) NULL)
                {
                  MagickWarning(ResourceLimitWarning,
                    "Memory allocation failed",(char *) NULL);
                  return(False);
                }
              q=image->pixels+packets-1;
            }
          q->index=index;
          q->length=0;
        }
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
    y++;
  }
  image->packets=packets;
  image->rows=y;
  SyncImage(image);
  image->compression=FaxCompression;
  /*
    Free decoder memory.
  */
  FreeMemory((char *) mw_hash);
  FreeMemory((char *) mb_hash);
  FreeMemory((char *) scanline);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   H u f f m a n E n c o d e I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method HuffmanEncodeImage compresses an image via Huffman-coding.
%
%  The format of the HuffmanEncodeImage method is:
%
%      unsigned int HuffmanEncodeImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method HuffmanEncodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
*/
Export unsigned int HuffmanEncodeImage(const ImageInfo *image_info,Image *image)
{
#define HuffmanOutputCode(entry)  \
{  \
  mask=1 << (entry->length-1);  \
  while (mask != 0)  \
  {  \
    OutputBit((entry->code & mask ? 1 : 0));  \
    mask>>=1;  \
  }  \
}

#define OutputBit(count)  \
{  \
  if(count > 0)  \
    byte=byte | bit;  \
  bit>>=1;  \
  if ((bit & 0xff) == 0)   \
    {  \
      if (Latin1Compare(image_info->magick,"FAX") == 0) \
        (void) WriteByte(image,byte);  \
      else \
        Ascii85Encode(image,(unsigned int) byte); \
      byte=0;  \
      bit=0x80;  \
    }  \
}

  const HuffmanTable
    *entry;

  int
    i,
    k,
    runlength;

  Image
    *huffman_image;

  register int
    j,
    n,
    x;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  register unsigned short
    polarity;

  unsigned char
    bit,
    byte,
    *scanline;

  unsigned int
    mask,
    width;

  /*
    Allocate scanline buffer.
  */
  assert(image != (Image *) NULL);
  width=image->columns;
  if (Latin1Compare(image_info->magick,"FAX") == 0)
    width=Max(image->columns,1728);
  scanline=(unsigned char *) AllocateMemory((width+1)*sizeof(unsigned char));
  if (scanline == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  huffman_image=(Image *) image;
  if (!IsMonochromeImage(huffman_image))
    {
      QuantizeInfo
        quantize_info;

      /*
        Convert image to monochrome.
      */
      ((Image *) image)->orphan=True;
      huffman_image=CloneImage(image,image->columns,image->rows,True);
      ((Image *) image)->orphan=False;
      if (huffman_image == (Image *) NULL)
        return(False);
      GetQuantizeInfo(&quantize_info);
      quantize_info.number_colors=2;
      quantize_info.dither=image_info->dither;
      quantize_info.colorspace=GRAYColorspace;
      (void) QuantizeImage(&quantize_info,huffman_image);
      SyncImage(huffman_image);
    }
  byte=0;
  bit=0x80;
  if (Latin1Compare(image_info->magick,"FAX") != 0)
    Ascii85Initialize();
  else
    {
      /*
        End of line.
      */
      for (k=0; k < 11; k++)
        OutputBit(0);
      OutputBit(1);
    }
  /*
    Compress runlength encoded to 1D Huffman pixels.
  */
  polarity=0;
  if (huffman_image->colors == 2)
    polarity=(Intensity(huffman_image->colormap[0]) >
      Intensity(huffman_image->colormap[1]) ? 0 : 1);
  q=scanline;
  for (i=0; i < (int) width; i++)
    *q++=(unsigned char) polarity;
  p=huffman_image->pixels;
  q=scanline;
  x=0;
  for (i=0; i < (int) huffman_image->packets; i++)
  {
    for (j=0; j <= ((int) p->length); j++)
    {
      *q++=(unsigned char)
        (p->index == polarity ? (int) polarity : (int) !polarity);
      x++;
      if (x < (int) huffman_image->columns)
        continue;
      /*
        Huffman encode scanline.
      */
      q=scanline;
      for (n=width; n > 0; )
      {
        /*
          Output white run.
        */
        for (runlength=0; ((*q == polarity) && (n > 0)); n--)
        {
          q++;
          runlength++;
        }
        if (runlength >= 64)
          {
            entry=MWTable+((runlength/64)-1);
            if (runlength >= 1792)
              entry=EXTable+(Min(runlength,2560)-1792)/64;
            runlength-=entry->count;
            HuffmanOutputCode(entry);
          }
        entry=TWTable+Min(runlength,63);
        HuffmanOutputCode(entry);
        if (n != 0)
          {
            /*
              Output black run.
            */
            for (runlength=0; ((*q != polarity) && (n > 0)); n--)
            {
              q++;
              runlength++;
            }
            if (runlength >= 64)
              {
                entry=MBTable+((runlength/64)-1);
                if (runlength >= 1792)
                  entry=EXTable+(Min(runlength,2560)-1792)/64;
                runlength-=entry->count;
                HuffmanOutputCode(entry);
              }
            entry=TBTable+Min(runlength,63);
            HuffmanOutputCode(entry);
          }
      }
      /*
        End of line.
      */
      for (k=0; k < 11; k++)
        OutputBit(0);
      OutputBit(1);
      x=0;
      q=scanline;
    }
    p++;
    if (huffman_image->previous == (Image *) NULL)
      if (QuantumTick(i,huffman_image->packets))
        ProgressMonitor(SaveImageText,i,huffman_image->packets);
  }
  /*
    End of page.
  */
  for (i=0; i < 6; i++)
  {
    for (k=0; k < 11; k++)
      OutputBit(0);
    OutputBit(1);
  }
  /*
    Flush bits.
  */
  if (bit != 0x80)
    {
      if (Latin1Compare(image_info->magick,"FAX") == 0)
        (void) WriteByte(image,byte);
      else
        Ascii85Encode(image,(unsigned int) byte);
    }
  if (Latin1Compare(image_info->magick,"FAX") != 0)
    Ascii85Flush(image);
  if (huffman_image != image)
    DestroyImage(huffman_image);
  FreeMemory((char *) scanline);
  return(True);
}

#if defined(HasTIFF)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   H u f f m a n 2 D E n c o d e I m a g e                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method Huffman2DEncodeImage compresses an image via two-dimensional
%  Huffman-coding.
%
%  The format of the Huffman2DEncodeImage method is:
%
%      unsigned int Huffman2DEncodeImage(ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method Huffman2DEncodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
*/
Export unsigned int Huffman2DEncodeImage(ImageInfo *image_info,Image *image)
{
  char
    filename[MaxTextExtent];

  Image
    *huffman_image;

  ImageInfo
    *local_info;

  int
    count;

  register int
    i,
    j;

  TIFF
    *tiff;


  uint16
    fillorder;

  unsigned char
    *buffer;

  unsigned int
    *byte_count,
    status,
    strip_size;

  /*
    Write image as CCITTFax4 TIFF image to a temporary file.
  */
  assert(image_info != (ImageInfo *) NULL);
  assert(image != (Image *) NULL);
  ((Image *) image)->orphan=True;
  huffman_image=CloneImage(image,image->columns,image->rows,True);
  ((Image *) image)->orphan=False;
  if (huffman_image == (Image *) NULL)
    return(False);
  if (!IsMonochromeImage(huffman_image))
    {
      QuantizeInfo
        quantize_info;

      /*
        Convert image to monochrome.
      */
      GetQuantizeInfo(&quantize_info);
      quantize_info.number_colors=2;
      quantize_info.dither=image_info->dither;
      quantize_info.colorspace=GRAYColorspace;
      (void) QuantizeImage(&quantize_info,huffman_image);
      SyncImage(huffman_image);
    }
  TemporaryFilename(filename);
  (void) strcpy(huffman_image->filename,filename);
  (void) strcpy(huffman_image->magick,"TIFF");
  local_info=CloneImageInfo(image_info);
  local_info->compression=Group4Compression;
  status=WriteImage(local_info,huffman_image);
  DestroyImageInfo(local_info);
  DestroyImage(huffman_image);
  if (status == False)
    return(False);
  tiff=TIFFOpen(filename,ReadBinaryType);
  (void) remove(filename);
  if (tiff == (TIFF *) NULL)
    {
      MagickWarning(FileOpenWarning,"Unable to open file",image_info->filename);
      return(False);
    }
  /*
    Allocate raw strip buffer.
  */
  TIFFGetField(tiff,TIFFTAG_STRIPBYTECOUNTS,&byte_count);
  strip_size=byte_count[0];
  for (i=1; i < (int) TIFFNumberOfStrips(tiff); i++)
    if (byte_count[i] > strip_size)
      strip_size=byte_count[i];
  buffer=(unsigned char *) AllocateMemory(strip_size*sizeof(unsigned char));
  if (buffer == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      TIFFClose(tiff);
      return(False);
    }
  /*
    Compress runlength encoded to 2D Huffman pixels.
  */
  TIFFGetFieldDefaulted(tiff,TIFFTAG_FILLORDER,&fillorder);
  for (i=0; i < (int) TIFFNumberOfStrips(tiff); i++)
  {
    Ascii85Initialize();
    count=TIFFReadRawStrip(tiff,i,buffer,byte_count[i]);
    if (fillorder == FILLORDER_LSB2MSB)
      TIFFReverseBits(buffer,count);
    for (j=0; j < count; j++)
      Ascii85Encode(image,(unsigned int) buffer[j]);
    Ascii85Flush(image);
  }
  FreeMemory((char *) buffer);
  TIFFClose(tiff);
  return(True);
}
#else
Export unsigned int Huffman2DEncodeImage(ImageInfo *image_info,Image *image)
{
  MagickWarning(MissingDelegateWarning,"TIFF library is not available",
    image_info->filename);
  return(False);
}
#endif

#if defined(HasLZW)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   L Z W E n c o d e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method LZWEncodeImage compresses an image via LZW-coding specific to
%  Postscript Level II or Portable Document Format.  To ensure portability, the
%  binary LZW bytes are encoded as ASCII base-85.
%
%  The format of the LZWEncodeImage method is:
%
%      unsigned int LZWEncodeImage(Image *image,
%        const unsigned int number_pixels,unsigned char *pixels)
%
%  A description of each parameter follows:
%
%    o status:  Method LZWEncodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%    o number_pixels:  An unsigned interger that specifies the number of
%      pixels to compress.
%
%    o pixels: The address of an unsigned array of characters containing the
%      pixels to compress.
%
%
*/
Export unsigned int LZWEncodeImage(Image *image,
  const unsigned int number_pixels,unsigned char *pixels)
{
#define LZWClr  256  /* Clear Table Marker */
#define LZWEod  257  /* End of Data marker */
#define OutputCode(code) \
{ \
    accumulator+=((long) code) << (32-code_width-number_bits); \
    number_bits+=code_width; \
    while (number_bits >= 8) \
    { \
        Ascii85Encode(image,(unsigned int) (accumulator >> 24)); \
        accumulator=accumulator << 8; \
        number_bits-=8; \
    } \
}

  typedef struct _TableType
  {
    short
      prefix,
      suffix,
      next;
  } TableType;

  int
    index;

  register int
    i;

  short
    number_bits,
    code_width,
    last_code,
    next_index;

  TableType
    *table;

  unsigned long
    accumulator;

  /*
    Allocate string table.
  */
  assert(image != (Image *) NULL);
  assert(pixels != (unsigned char *) NULL);
  table=(TableType *) AllocateMemory((1 << 12)*sizeof(TableType));
  if (table == (TableType *) NULL)
    return(False);
  /*
    Initialize variables.
  */
  accumulator=0;
  code_width=9;
  number_bits=0;
  last_code=0;
  Ascii85Initialize();
  OutputCode(LZWClr);
  for (index=0; index < 256; index++)
  {
    table[index].prefix=(-1);
    table[index].suffix=index;
    table[index].next=(-1);
  }
  next_index=LZWEod+1;
  code_width=9;
  last_code=pixels[0];
  for (i=1; i < (int) number_pixels; i++)
  {
    /*
      Find string.
    */
    index=last_code;
    while (index != -1)
      if ((table[index].prefix != last_code) ||
          (table[index].suffix != pixels[i]))
        index=table[index].next;
      else
        {
          last_code=index;
          break;
        }
    if (last_code != index)
      {
        /*
          Add string.
        */
        OutputCode(last_code);
        table[next_index].prefix=last_code;
        table[next_index].suffix=pixels[i];
        table[next_index].next=table[last_code].next;
        table[last_code].next=next_index;
        next_index++;
        /*
          Did we just move up to next bit width?
        */
        if ((next_index >> code_width) != 0)
          {
            code_width++;
            if (code_width > 12)
              {
                /*
                  Did we overflow the max bit width?
                */
                code_width--;
                OutputCode(LZWClr);
                for (index=0; index < 256; index++)
                {
                  table[index].prefix=(-1);
                  table[index].suffix=index;
                  table[index].next=(-1);
                }
                next_index=LZWEod+1;
                code_width=9;
              }
            }
          last_code=pixels[i];
      }
  }
  /*
    Flush tables.
  */
  OutputCode(last_code);
  OutputCode(LZWEod);
  if (number_bits != 0)
    Ascii85Encode(image,accumulator >> 24);
  Ascii85Flush(image);
  FreeMemory(table);
  return(True);
}
#else
Export unsigned int LZWEncodeImage(Image *image,
  const unsigned int number_pixels,unsigned char *pixels)
{
  MagickWarning(MissingDelegateWarning,"LZW library is not available",
    (char *) NULL);
  return(False);
}
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   P a c k b i t s E n c o d e I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method PackbitsEncodeImage compresses an image via Macintosh Packbits
%  encoding specific to Postscript Level II or Portable Document Format.  To
%  ensure portability, the binary Packbits bytes are encoded as ASCII Base-85.
%
%  The format of the PackbitsEncodeImage method is:
%
%      unsigned int PackbitsEncodeImage(Image *image,
%        const unsigned int number_pixels,unsigned char *pixels)
%
%  A description of each parameter follows:
%
%    o status:  Method PackbitsEncodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%    o number_pixels:  An unsigned integer that specifies the number of
%      pixels to compress.
%
%    o pixels: The address of an unsigned array of characters containing the
%      pixels to compress.
%
%
*/
Export unsigned int PackbitsEncodeImage(Image *image,
  const unsigned int number_pixels,unsigned char *pixels)
{
  register int
    count,
    i,
    j;

  unsigned char
    *packbits;

  /*
    Compress pixels with Packbits encoding.
  */
  assert(image != (Image *) NULL);
  assert(pixels != (unsigned char *) NULL);
  packbits=(unsigned char *) AllocateMemory(128*sizeof(unsigned char));
  if (packbits == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  Ascii85Initialize();
  i=number_pixels;
  while (i != 0)
  {
    switch (i)
    {
      case 1:
      {
        i--;
        Ascii85Encode(image,0);
        Ascii85Encode(image,*pixels);
        break;
      }
      case 2:
      {
        i-=2;
        Ascii85Encode(image,1);
        Ascii85Encode(image,*pixels);
        Ascii85Encode(image,pixels[1]);
        break;
      }
      case 3:
      {
        i-=3;
        if ((*pixels == *(pixels+1)) && (*(pixels+1) == *(pixels+2)))
          {
            Ascii85Encode(image,(256-3)+1);
            Ascii85Encode(image,*pixels);
            break;
          }
        Ascii85Encode(image,2);
        Ascii85Encode(image,*pixels);
        Ascii85Encode(image,pixels[1]);
        Ascii85Encode(image,pixels[2]);
        break;
      }
      default:
      {
        if ((*pixels == *(pixels+1)) && (*(pixels+1) == *(pixels+2)))
          {
            /*
              Packed run.
            */
            count=3;
            while ((count < i) && (*pixels == *(pixels+count)))
            {
              count++;
              if (count >= 127)
                break;
            }
            i-=count;
            Ascii85Encode(image,(256-count)+1);
            Ascii85Encode(image,*pixels);
            pixels+=count;
            break;
          }
        /*
          Literal run.
        */
        count=0;
        while ((*(pixels+count) != *(pixels+count+1)) ||
               (*(pixels+count+1) != *(pixels+count+2)))
        {
          packbits[count+1]=pixels[count];
          count++;
          if ((count >= (int) (i-3)) || (count >= 127))
            break;
        }
        i-=count;
        *packbits=count-1;
        for (j=0; j <= count; j++)
          Ascii85Encode(image,packbits[j]);
        pixels+=count;
        break;
      }
    }
  }
  Ascii85Encode(image,128);  /* EOD marker */
  Ascii85Flush(image);
  FreeMemory((char *) packbits);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R u n l e n g t h D e c o d e I m a g e                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method RunlengthDecodeImage unpacks the packed image pixels into
%  runlength-encoded pixel packets.  The packed image pixel memory is then
%  freed.
%
%  The format of the RunlengthDecodeImage method is:
%
%      unsigned int RunlengthDecodeImage(Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method RunlengthDecodeImage return True if the image is
%      decoded.  False is returned if there is an error occurs.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int RunlengthDecodeImage(Image *image)
{
  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned long
    count;

  unsigned short
    value;

  assert(image != (Image *) NULL);
  if (image->packed_pixels == (unsigned char *) NULL)
    return(True);
  /*
    Allocate pixels.
  */
  if (image->pixels == (RunlengthPacket *) NULL)
    image->pixels=(RunlengthPacket *)
      AllocateMemory((unsigned int) image->packets*sizeof(RunlengthPacket));
  else
    image->pixels=(RunlengthPacket *) ReallocateMemory((char *) image->pixels,
      image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to unpack pixels",
        "Memory allocation failed");
      return(False);
    }
  /*
    Unpack the packed image pixels into runlength-encoded pixel packets.
  */
  p=image->packed_pixels;
  q=image->pixels;
  count=0;
  if (image->class == DirectClass)
    {
      if (image->compression == RunlengthEncodedCompression)
        for (i=0; i < (int) image->packets; i++)
        {
          ReadQuantum(q->red,p);
          ReadQuantum(q->green,p);
          ReadQuantum(q->blue,p);
          q->index=0;
          if (image->matte || (image->colorspace == CMYKColorspace))
            ReadQuantum(q->index,p);
          q->length=(*p++);
          count+=(q->length+1);
          q++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(LoadImageText,i,image->packets);
        }
      else
        for (i=0; i < (int) image->packets; i++)
        {
          ReadQuantum(q->red,p);
          ReadQuantum(q->green,p);
          ReadQuantum(q->blue,p);
          q->index=0;
          if (image->matte || (image->colorspace == CMYKColorspace))
            ReadQuantum(q->index,p);
          q->length=0;
          count++;
          q++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(LoadImageText,i,image->packets);
        }
    }
  else
    {
      register unsigned short
        index;

      if (image->compression == RunlengthEncodedCompression)
        {
          if (image->colors <= 256)
            for (i=0; i < (int) image->packets; i++)
            {
              q->index=(unsigned short) (*p++);
              q->length=(*p++);
              count+=(q->length+1);
              q++;
              if (image->previous == (Image *) NULL)
                if (QuantumTick(i,image->packets))
                  ProgressMonitor(LoadImageText,i,image->packets);
            }
          else
            for (i=0; i < (int) image->packets; i++)
            {
              index=(*p++) << 8;
              index|=(*p++);
              q->index=index;
              q->length=(*p++);
              count+=(q->length+1);
              q++;
              if (image->previous == (Image *) NULL)
                if (QuantumTick(i,image->packets))
                  ProgressMonitor(LoadImageText,i,image->packets);
            }
        }
      else
        if (image->colors <= 256)
          for (i=0; i < (int) image->packets; i++)
          {
            q->index=(unsigned short) (*p++);
            q->length=0;
            count++;
            q++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(LoadImageText,i,image->packets);
          }
        else
          for (i=0; i < (int) image->packets; i++)
          {
            index=(*p++) << 8;
            index|=(*p++);
            q->index=index;
            q->length=0;
            count++;
            q++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(LoadImageText,i,image->packets);
          }
      SyncImage(image);
    }
  /*
    Free packed pixels memory.
  */
  FreeMemory((char *) image->packed_pixels);
  image->packed_pixels=(unsigned char *) NULL;
  /*
    Guarentee the correct number of pixel packets.
  */
  if (count > (image->columns*image->rows))
    {
      MagickWarning(CorruptImageWarning,"insufficient image data in file",
        image->filename);
      return(False);
    }
  else
    if (count < (image->columns*image->rows))
      {
        MagickWarning(CorruptImageWarning,"too much image data in file",
          image->filename);
        return(False);
      }
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R u n l e n g t h E n c o d e I m a g e                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method RunlengthEncodeImage packs the runlength-encoded pixel packets
%  into the minimum number of bytes.
%
%  The format of the RunlengthEncodeImage method is:
%
%      unsigned int RunlengthEncodeImage(Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method RunlengthEncodeImage return the number of bytes the
%      image consumes.  Zero is returned if an error occurs.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int RunlengthEncodeImage(Image *image)
{
#define SpecialRunlength  255

  register int
    i,
    j,
    runlength;

  register long
    packets;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  unsigned long
    count;

  unsigned short
    value;

  assert(image != (Image *) NULL);
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      MagickWarning(CorruptImageWarning,"Unable to pack pixels",
        "no image pixels");
      return(0);
    }
  if (image->compression == RunlengthEncodedCompression)
    {
      register RunlengthPacket
        *q;

      /*
        Compress image.
      */
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        if (p->length > SpecialRunlength)
          {
            /*
              Uncompress image to allow in-place compression.
            */
            if (!UncondenseImage(image))
              return(0);
            break;
          }
        p++;
      }
      p=image->pixels;
      runlength=p->length+1;
      packets=0;
      q=image->pixels;
      q->length=SpecialRunlength;
      if (image->matte || (image->colorspace == CMYKColorspace))
        for (i=0; i < (int) (image->columns*image->rows); i++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          if ((p->red == q->red) && (p->green == q->green) &&
              (p->blue == q->blue) && (p->index == q->index) &&
              (q->length < SpecialRunlength))
            q->length++;
          else
            {
              if (packets != 0)
                q++;
              packets++;
              *q=(*p);
              q->length=0;
            }
        }
      else
        for (i=0; i < (int) (image->columns*image->rows); i++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          if ((p->red == q->red) && (p->green == q->green) &&
              (p->blue == q->blue) && (q->length < SpecialRunlength))
            q->length++;
          else
            {
              if (packets != 0)
                q++;
              packets++;
              *q=(*p);
              q->length=0;
            }
        }
      image->packets=packets;
      image->pixels=(RunlengthPacket *) ReallocateMemory((char *) image->pixels,
        image->packets*sizeof(RunlengthPacket));
      /*
        Runlength-encode only if it consumes less memory than no compression.
      */
      if (image->class == DirectClass)
        {
          if (image->packets >= ((image->columns*image->rows*3) >> 2))
            image->compression=NoCompression;
        }
      else
        if (image->packets >= ((image->columns*image->rows) >> 1))
          image->compression=NoCompression;
    }
  /*
    Determine packed packet size.
  */
  if (image->class == PseudoClass)
    {
      image->packet_size=1;
      if (image->colors > 256)
        image->packet_size++;
    }
  else
    {
      image->packet_size=3*(image->depth >> 3);
      if (image->matte || (image->colorspace == CMYKColorspace))
        image->packet_size+=image->depth >> 3;
    }
  if (image->compression == RunlengthEncodedCompression)
    image->packet_size++;
  /*
    Allocate packed pixel memory.
  */
  packets=image->packets;
  if (image->compression != RunlengthEncodedCompression)
    packets=image->columns*image->rows;
  if (image->packed_pixels == (unsigned char *) NULL)
    image->packed_pixels=(unsigned char *) AllocateMemory((unsigned int)
      packets*image->packet_size*sizeof(unsigned char));
  else
    image->packed_pixels=(unsigned char *) ReallocateMemory((char *)
      image->packed_pixels,packets*image->packet_size*sizeof(unsigned char));
  if (image->packed_pixels == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to pack pixels",
        "Memory allocation failed");
      return(0);
    }
  /*
    Packs the runlength-encoded pixel packets into the minimum number of bytes.
  */
  p=image->pixels;
  q=image->packed_pixels;
  count=0;
  if (image->class == DirectClass)
    {
      if (image->compression == RunlengthEncodedCompression)
        for (i=0; i < (int) image->packets; i++)
        {
          WriteQuantum(p->red,q);
          WriteQuantum(p->green,q);
          WriteQuantum(p->blue,q);
          if (image->matte || (image->colorspace == CMYKColorspace))
            WriteQuantum(p->index,q);
          *q++=p->length;
          count+=(p->length+1);
          p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
        }
      else
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            WriteQuantum(p->red,q);
            WriteQuantum(p->green,q);
            WriteQuantum(p->blue,q);
            if (image->matte || (image->colorspace == CMYKColorspace))
              WriteQuantum(p->index,q);
          }
          count+=(p->length+1);
          p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
        }
    }
  else
    if (image->compression == RunlengthEncodedCompression)
      {
        if (image->colors <= 256)
          for (i=0; i < (int) image->packets; i++)
          {
            *q++=(unsigned char) p->index;
            *q++=p->length;
            count+=(p->length+1);
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
        else
          for (i=0; i < (int) image->packets; i++)
          {
            *q++=p->index >> 8;
            *q++=(unsigned char) p->index;
            *q++=p->length;
            count+=(p->length+1);
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
      }
    else
      if (image->colors <= 256)
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
            *q++=p->index;
          count+=(p->length+1);
          p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
        }
      else
        {
          register unsigned char
            xff00,
            xff;

          for (i=0; i < (int) image->packets; i++)
          {
            xff00=p->index >> 8;
            xff=p->index;
            for (j=0; j <= ((int) p->length); j++)
            {
              *q++=xff00;
              *q++=xff;
            }
            count+=(p->length+1);
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
        }
  /*
    Guarentee the correct number of pixel packets.
  */
  if (count < (image->columns*image->rows))
    {
      MagickWarning(CorruptImageWarning,"insufficient image data in",
        image->filename);
      return(0);
    }
  else
    if (count > (image->columns*image->rows))
      {
        MagickWarning(CorruptImageWarning,"too much image data in",
          image->filename);
        return(0);
      }
  return((unsigned int) packets);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S e t R u n l e n g t h E n c o d e r                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SetRunlengthEncoder initializes the runlength encoder.
%
%  The format of the SetRunlengthEncoder function is:
%
%      SetRunlengthEncoder(packet)
%
%  A description of each parameter follows:
%
%    o packet: Specifies a RunlengthPacket type.
%
%
*/
Export void SetRunlengthEncoder(RunlengthPacket *packet)
{
  packet->red=0;
  packet->green=0;
  packet->blue=0;
  packet->index=0;
  packet->length=MaxRunlength;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S e t R u n l e n g t h P a c k e t s                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SetRunlengthPackets initializes the runlength encoder.
%
%  The format of the SetRunlengthPackets function is:
%
%      SetRunlengthPackets(image,packets)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o packets: The number of runlength packets.
%
%
*/
Export void SetRunlengthPackets(Image *image,const unsigned long packets)
{
  image->packets=packets;
  image->pixels=(RunlengthPacket *) ReallocateMemory((char *) image->pixels,
    image->packets*sizeof(RunlengthPacket));
}

#if defined(HasZLIB)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   Z L I B E n c o d e I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ZLIBEncodeImage compresses an image via ZLIB-coding specific to
%  Postscript Level II or Portable Document Format.  To ensure portability, the
%  binary ZLIB bytes are encoded as ASCII base-85.
%
%  The format of the ZLIBEncodeImage method is:
%
%      unsigned int ZLIBEncodeImage(Image *image,
%        const unsigned long number_pixels,const unsigned int quality,
%        unsigned char *pixels)
%
%  A description of each parameter follows:
%
%    o status:  Method ZLIBEncodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o file: The address of a structure of type FILE.  ZLIB encoded pixels
%      are written to this file.
%
%    o number_pixels:  An unsigned integer that specifies the number of
%      pixels to compress.
%
%    o quality: the compression level (0-100).
%
%    o pixels: The address of an unsigned array of characters containing the
%      pixels to compress.
%
%
*/
Export unsigned int ZLIBEncodeImage(Image *image,
  const unsigned long number_pixels,const unsigned int quality,
  unsigned char *pixels)
{
  int
    status;

  register int
    i;

  unsigned char
    *compressed_pixels;

  unsigned long
    compressed_packets;

  z_stream
    stream;

  compressed_packets=(unsigned long) (1.001*number_pixels+12);
  compressed_pixels=(unsigned char *)
    AllocateMemory(compressed_packets*sizeof(unsigned char));
  if (compressed_pixels == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  stream.next_in=pixels;
  stream.avail_in=number_pixels;
  stream.next_out=compressed_pixels;
  stream.avail_out=compressed_packets;
  stream.zalloc=(alloc_func) NULL;
  stream.zfree=(free_func) NULL;
  stream.opaque=(voidpf) NULL;
  status=deflateInit(&stream,Min(quality/10,9));
  if (status == Z_OK)
    {
      status=deflate(&stream,Z_FINISH);
      if (status == Z_STREAM_END)
        status=deflateEnd(&stream);
      else
        (void) deflateEnd(&stream);
      compressed_packets=stream.total_out;
    }
  if (status)
    MagickWarning(DelegateWarning,"Unable to Zip compress image",(char *) NULL);
  else
    {
      Ascii85Initialize();
      for (i=0; i < (int) compressed_packets; i++)
        Ascii85Encode(image,compressed_pixels[i]);
      Ascii85Flush(image);
    }
  FreeMemory((char *) compressed_pixels);
  return(!status);
}
#else
Export unsigned int ZLIBEncodeImage(Image *image,
  const unsigned long number_pixels,const unsigned int quality,
  unsigned char *pixels)
{
  MagickWarning(MissingDelegateWarning,"ZLIB library is not available",
    (char *) NULL);
  return(False);
}
#endif
