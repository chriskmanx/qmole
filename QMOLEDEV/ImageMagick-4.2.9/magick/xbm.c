/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            X   X  BBBB   M   M                              %
%                             X X   B   B  MM MM                              %
%                              X    BBBB   M M M                              %
%                             X X   B   B  M   M                              %
%                            X   X  BBBB   M   M                              %
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d X B M I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadXBMImage reads an X11 bitmap image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadXBMImage method is:
%
%      Image *ReadXBMImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadXBMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static int XBMInteger(Image *image,short int *hex_digits)
{
  int
    c,
    flag,
    value;

  value=0;
  flag=0;
  for ( ; ; )
  {
    c=ReadByte(image);
    if (c == EOF)
      {
        value=(-1);
        break;
      }
    c&=0xff;
    if (isxdigit(c))
      {
        value=(value << 4)+hex_digits[c];
        flag++;
        continue;
      }
    if ((hex_digits[c]) < 0 && flag)
      break;
  }
  return(value);
}

Export Image *ReadXBMImage(const ImageInfo *image_info)
{
  char
    buffer[MaxTextExtent],
    name[MaxTextExtent];

  Image
    *image;

  register int
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  register long
    packets;

  short int
    hex_digits[256];

  unsigned char
    bit,
    *data;

  unsigned int
    byte,
    bytes_per_line,
    padding,
    status,
    value,
    version;

  unsigned short
    index;

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
    Read X bitmap header.
  */
  while (GetStringBlob(image,buffer) != (char *) NULL)
    if (sscanf(buffer,"#define %s %u",name,&image->columns) == 2)
      if ((strlen(name) >= 6) &&
          (Latin1Compare(name+strlen(name)-6,"_width") == 0))
          break;
  while (GetStringBlob(image,buffer) != (char *) NULL)
    if (sscanf(buffer,"#define %s %u",name,&image->rows) == 2)
      if ((strlen(name) >= 7) &&
          (Latin1Compare(name+strlen(name)-7,"_height") == 0))
          break;
  image->class=PseudoClass;
  image->colors=2;
  if (image_info->ping)
    {
      CloseBlob(image);
      return(image);
    }
  /*
    Scan until hex digits.
  */
  version=11;
  while (GetStringBlob(image,buffer) != (char *) NULL)
  {
    if (sscanf(buffer,"static short %s = {",name) == 1)
      version=10;
    else
      if (sscanf(buffer,"static unsigned char %s = {",name) == 1)
        version=11;
      else
        if (sscanf(buffer,"static char %s = {",name) == 1)
          version=11;
        else
          continue;
    p=(unsigned char *) strrchr(name,'_');
    if (p == (unsigned char *) NULL)
      p=(unsigned char *) name;
    else
      p++;
    if (Latin1Compare("bits[]",(char *) p) == 0)
      break;
  }
  if ((image->columns == 0) || (image->rows == 0) || EOFBlob(image))
    ReaderExit(CorruptImageWarning,"XBM file is not in the correct format",
      image);
  /*
    Initialize image structure.
  */
  image->colormap=(ColorPacket *)
    AllocateMemory(image->colors*sizeof(ColorPacket));
  packets=0;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->columns*image->rows*sizeof(RunlengthPacket));
  padding=0;
  if ((image->columns % 16) && ((image->columns % 16) < 9)  && (version == 10))
    padding=1;
  bytes_per_line=(image->columns+7)/8+padding;
  data=(unsigned char *)
    AllocateMemory(bytes_per_line*image->rows*sizeof(unsigned char *));
  if ((image->colormap == (ColorPacket *) NULL) ||
      (image->pixels == (RunlengthPacket *) NULL) ||
      (data == (unsigned char *) NULL))
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Initialize colormap.
  */
  image->colormap[0].red=0;
  image->colormap[0].green=0;
  image->colormap[0].blue=0;
  image->colormap[1].red=MaxRGB;
  image->colormap[1].green=MaxRGB;
  image->colormap[1].blue=MaxRGB;
  /*
    Initialize hex values.
  */
  hex_digits['0']=0;
  hex_digits['1']=1;
  hex_digits['2']=2;
  hex_digits['3']=3;
  hex_digits['4']=4;
  hex_digits['5']=5;
  hex_digits['6']=6;
  hex_digits['7']=7;
  hex_digits['8']=8;
  hex_digits['9']=9;
  hex_digits['A']=10;
  hex_digits['B']=11;
  hex_digits['C']=12;
  hex_digits['D']=13;
  hex_digits['E']=14;
  hex_digits['F']=15;
  hex_digits['a']=10;
  hex_digits['b']=11;
  hex_digits['c']=12;
  hex_digits['d']=13;
  hex_digits['e']=14;
  hex_digits['f']=15;
  hex_digits['x']=0;
  hex_digits[' ']=(-1);
  hex_digits[',']=(-1);
  hex_digits['}']=(-1);
  hex_digits['\n']=(-1);
  hex_digits['\t']=(-1);
  /*
    Read hex image data.
  */
  p=data;
  if (version == 10)
    for (x=0; x < (int) (bytes_per_line*image->rows); (x+=2))
    {
      value=XBMInteger(image,hex_digits);
      *p++=value;
      if (!padding || ((x+2) % bytes_per_line))
        *p++=value >> 8;
    }
  else
    for (x=0; x < (int) (bytes_per_line*image->rows); x++)
    {
      value=XBMInteger(image,hex_digits);
      *p++=value;
    }
  /*
    Convert X bitmap image to runlength-encoded packets.
  */
  byte=0;
  p=data;
  q=image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; y < (int) image->rows; y++)
  {
    bit=0;
    for (x=0; x < (int) image->columns; x++)
    {
      if (bit == 0)
        byte=(*p++);
      index=byte & 0x01 ? 0 : 1;
      if ((index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (packets != 0)
            q++;
          packets++;
          q->index=index;
          q->length=0;
        }
      bit++;
      byte>>=1;
      if (bit == 8)
        bit=0;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
  }
  FreeMemory((char *) data);
  SetRunlengthPackets(image,packets);
  SyncImage(image);
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e X B M I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure WriteXBMImage writes an image to a file in the X bitmap format.
%
%  The format of the WriteXBMImage method is:
%
%      unsigned int WriteXBMImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteXBMImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteXBMImage(const ImageInfo *image_info,Image *image)
{
  char
    buffer[MaxTextExtent],
    name[MaxTextExtent];

  int
    x,
    y;

  register int
    i,
    j;

  register char
    *q;

  register RunlengthPacket
    *p;

  register unsigned char
    bit,
    byte,
    polarity;

  unsigned int
    count,
    status;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  TransformRGBImage(image,RGBColorspace);
  /*
    Write X bitmap header.
  */
  (void) strcpy(name,BaseFilename(image->filename));
  q=name;
  while ((*q != '.') && (*q != '\0'))
    q++;
  if (*q == '.')
    *q='\0';
  (void) sprintf(buffer,"#define %.1024s_width %u\n",name,image->columns);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"#define %.1024s_height %u\n",name,image->rows);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"static char %.1024s_bits[] = {\n",name);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer," ");
  (void) WriteBlob(image,strlen(buffer),buffer);
  /*
    Convert MIFF to X bitmap pixels.
  */
  if (!IsMonochromeImage(image))
    {
      QuantizeInfo
        quantize_info;

      GetQuantizeInfo(&quantize_info);
      quantize_info.number_colors=2;
      quantize_info.dither=image_info->dither;
      quantize_info.colorspace=GRAYColorspace;
      (void) QuantizeImage(&quantize_info,image);
      SyncImage(image);
    }
  polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
  if (image->colors == 2)
    polarity=Intensity(image->colormap[0]) > Intensity(image->colormap[1]);
  bit=0;
  byte=0;
  count=0;
  x=0;
  y=0;
  p=image->pixels;
  (void) strcpy(buffer," ");
  (void) WriteBlob(image,strlen(buffer),buffer);
  for (i=0; i < (int) image->packets; i++)
  {
    for (j=0; j <= ((int) p->length); j++)
    {
      byte>>=1;
      if (p->index == polarity)
        byte|=0x80;
      bit++;
      if (bit == 8)
        {
          /*
            Write a bitmap byte to the image file.
          */
          (void) sprintf(buffer,"0x%02x, ",(unsigned int) (byte & 0xff));
          (void) WriteBlob(image,strlen(buffer),buffer);
          count++;
          if (count == 12)
            {
              (void) strcpy(buffer,"\n  ");
              (void) WriteBlob(image,strlen(buffer),buffer);
              count=0;
            };
          bit=0;
          byte=0;
        }
      x++;
      if (x == (int) image->columns)
        {
          if (bit != 0)
            {
              /*
                Write a bitmap byte to the image file.
              */
              byte>>=(8-bit);
              (void) sprintf(buffer,"0x%02x, ",(unsigned int) (byte & 0xff));
              (void) WriteBlob(image,strlen(buffer),buffer);
              count++;
              if (count == 12)
                {
                  (void) strcpy(buffer,"\n  ");
                  (void) WriteBlob(image,strlen(buffer),buffer);
                  count=0;
                };
              bit=0;
              byte=0;
            };
          if (QuantumTick(y,image->rows))
            ProgressMonitor(SaveImageText,y,image->rows);
          x=0;
          y++;
        }
    }
    p++;
  }
  (void) strcpy(buffer,"};\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  CloseBlob(image);
  return(True);
}
