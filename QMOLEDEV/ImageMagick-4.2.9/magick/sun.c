/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            SSSSS  U   U  N   N                              %
%                            SS     U   U  NN  N                              %
%                             SSS   U   U  N N N                              %
%                               SS  U   U  N  NN                              %
%                            SSSSS   UUU   N   N                              %
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
%   D e c o d e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DecodeImage unpacks the packed image pixels into
%  runlength-encoded pixel packets.
%
%  The format of the DecodeImage method is:
%
%      Image *ReadSUNImage(const ImageInfo *image_info)
%        pixels)
%
%  A description of each parameter follows:
%
%    o status:  Method DecodeImage returns True if all the pixels are
%      uncompressed without error, otherwise False.
%
%    o compressed_pixels:  The address of a byte (8 bits) array of compressed
%      pixel data.
%
%    o pixels:  The address of a byte (8 bits) array of pixel data created by
%      the uncompression process.  The number of bytes in this array
%      must be at least equal to the number columns times the number of rows
%      of the source pixels.
%
%    o number_columns:  An integer value that is the number of columns or
%      width in pixels of your source image.
%
%    o number_rows:  An integer value that is the number of rows or
%      heigth in pixels of your source image.
%
%
*/
static unsigned int DecodeImage(const unsigned char *compressed_pixels,
  const unsigned int number_columns,const unsigned int number_rows,
  unsigned char *pixels)
{
  register const unsigned char
    *p;

  register int
    count;

  register unsigned char
    *q;

  unsigned char
    byte;

  assert(compressed_pixels != (unsigned char *) NULL);
  assert(pixels != (unsigned char *) NULL);
  p=compressed_pixels;
  q=pixels;
  while ((q-pixels) <= (int) (number_columns*number_rows))
  {
    byte=(*p++);
    if (byte != 128)
      *q++=byte;
    else
      {
        /*
          Runlength-encoded packet: <count><byte>
        */
        count=(*p++);
        if (count > 0)
          byte=(*p++);
        while (count >= 0)
        {
          *q++=byte;
          count--;
        }
     }
  }
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d S U N I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadSUNImage reads a SUN image file and returns it.  It allocates
%  the memory necessary for the new Image structure and returns a pointer to
%  the new image.
%
%  The format of the ReadSUNImage method is:
%
%      image=ReadSUNImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadSUNImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadSUNImage(const ImageInfo *image_info)
{
#define RMT_EQUAL_RGB  1
#define RMT_NONE  0
#define RMT_RAW  2
#define RT_STANDARD  1
#define RT_ENCODED  2
#define RT_FORMAT_RGB  3

  typedef struct _SUNHeader
  {
    unsigned long
      magic,
      width,
      height,
      depth,
      length,
      type,
      maptype,
      maplength;
  } SUNHeader;

  Image
    *image;

  register int
    bit,
    i,
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  SUNHeader
    sun_header;

  unsigned char
    *sun_data,
    *sun_pixels;

  unsigned int
    bytes_per_line,
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
    Read SUN raster header.
  */
  sun_header.magic=MSBFirstReadLong(image);
  do
  {
    /*
      Verify SUN identifier.
    */
    if (sun_header.magic != 0x59a66a95)
      ReaderExit(CorruptImageWarning,"Not a SUN raster image",image);
    sun_header.width=MSBFirstReadLong(image);
    sun_header.height=MSBFirstReadLong(image);
    sun_header.depth=MSBFirstReadLong(image);
    sun_header.length=MSBFirstReadLong(image);
    sun_header.type=MSBFirstReadLong(image);
    sun_header.maptype=MSBFirstReadLong(image);
    sun_header.maplength=MSBFirstReadLong(image);
    image->columns=(unsigned int) sun_header.width;
    image->rows=(unsigned int) sun_header.height;
    if (sun_header.depth < 24)
      {
        image->class=PseudoClass;
        image->colors=sun_header.maplength;
        if (sun_header.maptype == RMT_NONE)
          image->colors=1 << sun_header.depth;
        if (sun_header.maptype == RMT_EQUAL_RGB)
          image->colors=(unsigned int) sun_header.maplength/3;
      }
    if (image_info->ping)
      {
        CloseBlob(image);
        return(image);
      }
    switch (sun_header.maptype)
    {
      case RMT_NONE:
      {
        if (sun_header.depth < 24)
          {
            /*
              Create linear color ramp.
            */
            image->colormap=(ColorPacket *)
              AllocateMemory(image->colors*sizeof(ColorPacket));
            if (image->colormap == (ColorPacket *) NULL)
              ReaderExit(ResourceLimitWarning,"Memory allocation failed",
                image);
            for (i=0; i < (int) image->colors; i++)
            {
              image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
            }
          }
        break;
      }
      case RMT_EQUAL_RGB:
      {
        unsigned char
          *sun_colormap;

        /*
          Read SUN raster colormap.
        */
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
        sun_colormap=(unsigned char *)
          AllocateMemory(image->colors*sizeof(unsigned char));
        if ((image->colormap == (ColorPacket *) NULL) ||
            (sun_colormap == (unsigned char *) NULL))
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        (void) ReadBlob(image,image->colors,(char *) sun_colormap);
        for (i=0; i < (int) image->colors; i++)
          image->colormap[i].red=UpScale(sun_colormap[i]);
        (void) ReadBlob(image,image->colors,(char *) sun_colormap);
        for (i=0; i < (int) image->colors; i++)
          image->colormap[i].green=UpScale(sun_colormap[i]);
        (void) ReadBlob(image,image->colors,(char *) sun_colormap);
        for (i=0; i < (int) image->colors; i++)
          image->colormap[i].blue=UpScale(sun_colormap[i]);
        FreeMemory((char *) sun_colormap);
        break;
      }
      case RMT_RAW:
      {
        unsigned char
          *sun_colormap;

        /*
          Read SUN raster colormap.
        */
        sun_colormap=(unsigned char *)
          AllocateMemory(sun_header.maplength*sizeof(unsigned char));
        if (sun_colormap == (unsigned char *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        (void) ReadBlob(image,(unsigned int) sun_header.maplength,
          (char *) sun_colormap);
        FreeMemory((char *) sun_colormap);
        break;
      }
      default:
        ReaderExit(CorruptImageWarning,"Colormap type is not supported",
          image);
    }
    sun_data=(unsigned char *)
      AllocateMemory(sun_header.length*sizeof(unsigned char));
    if (sun_data == (unsigned char *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    status=ReadBlob(image,(unsigned int) sun_header.length,(char *) sun_data);
    if ((status == False) && (sun_header.type != RT_ENCODED))
      ReaderExit(CorruptImageWarning,"Unable to read image data",image);
    sun_pixels=sun_data;
    if (sun_header.type == RT_ENCODED)
      {
        unsigned int
          height;

        /*
          Read run-length encoded raster pixels.
        */
        height=(unsigned int) sun_header.height;
        bytes_per_line=(2*sun_header.width*sun_header.depth+15)/16;
        sun_pixels=(unsigned char *)
          AllocateMemory(bytes_per_line*height*sizeof(unsigned char));
        if (sun_pixels == (unsigned char *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        (void) DecodeImage(sun_data,bytes_per_line,height,sun_pixels);
        FreeMemory((char *) sun_data);
      }
    /*
      Initialize image structure.
    */
    image->matte=(sun_header.depth == 32);
    image->columns=(unsigned int) sun_header.width;
    image->rows=(unsigned int) sun_header.height;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    /*
      Convert SUN raster image to runlength-encoded packets.
    */
    p=sun_pixels;
    q=image->pixels;
    if (sun_header.depth == 1)
      for (y=0; y < (int) image->rows; y++)
      {
        /*
          Convert bitmap scanline to runlength-encoded color packets.
        */
        for (x=0; x < (int) (image->columns >> 3); x++)
        {
          for (bit=7; bit >= 0; bit--)
          {
            q->index=((*p) & (0x01 << bit) ? 0x00 : 0x01);
            q->length=0;
            q++;
          }
          p++;
        }
        if ((image->columns % 8) != 0)
          {
            for (bit=7; bit >= (int) (8-(image->columns % 8)); bit--)
            {
              q->index=((*p) & (0x01 << bit) ? 0x00 : 0x01);
              q->length=0;
              q++;
            }
            p++;
          }
        if ((((image->columns/8)+(image->columns % 8 ? 1 : 0)) % 2) != 0)
          p++;
        if (image->previous == (Image *) NULL)
          if (QuantumTick(y,image->rows))
            ProgressMonitor(LoadImageText,y,image->rows);
      }
    else
      if (image->class == PseudoClass)
        for (y=0; y < (int) image->rows; y++)
        {
          /*
            Convert PseudoColor scanline to runlength-encoded color packets.
          */
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=(*p++);
            q->length=0;
            q++;
          }
          if ((image->columns % 2) != 0)
            p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
      else
        for (y=0; y < (int) image->rows; y++)
        {
          /*
            Convert DirectColor scanline to runlength-encoded color packets.
          */
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=0;
            if (image->matte)
              q->index=UpScale(*p++);
            if (sun_header.type == RT_STANDARD)
              {
                q->blue=UpScale(*p++);
                q->green=UpScale(*p++);
                q->red=UpScale(*p++);
              }
            else
              {
                q->red=UpScale(*p++);
                q->green=UpScale(*p++);
                q->blue=UpScale(*p++);
              }
            if (image->colors != 0)
              {
                q->red=image->colormap[q->red].red;
                q->green=image->colormap[q->green].green;
                q->blue=image->colormap[q->blue].blue;
              }
            q->length=0;
            q++;
          }
          if (((image->columns % 2) != 0) && (image->matte == False))
            p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
    FreeMemory((char *) sun_pixels);
    if (image->class == PseudoClass)
      SyncImage(image);
    CondenseImage(image);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    sun_header.magic=MSBFirstReadLong(image);
    if (sun_header.magic == 0x59a66a95)
      {
        /*
          Allocate next image structure.
        */
        AllocateNextImage(image_info,image);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        image=image->next;
        ProgressMonitor(LoadImagesText,(unsigned int) TellBlob(image),
          (unsigned int) image->filesize);
      }
  } while (sun_header.magic == 0x59a66a95);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e S U N I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteSUNImage writes an image in the SUN rasterfile format.
%
%  The format of the WriteSUNImage method is:
%
%      unsigned int WriteSUNImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteSUNImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteSUNImage(const ImageInfo *image_info,Image *image)
{
#define RMT_EQUAL_RGB  1
#define RMT_NONE  0
#define RMT_RAW  2
#define RT_STANDARD  1
#define RT_FORMAT_RGB  3

  typedef struct _SUNHeader
  {
    unsigned long
      magic,
      width,
      height,
      depth,
      length,
      type,
      maptype,
      maplength;
  } SUNHeader;

  int
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  SUNHeader
    sun_header;

  unsigned int
    scene,
    status;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  scene=0;
  do
  {
    /*
      Initialize SUN raster file header.
    */
    TransformRGBImage(image,RGBColorspace);
    sun_header.magic=0x59a66a95;
    sun_header.width=image->columns;
    sun_header.height=image->rows;
    sun_header.type=(image->class == DirectClass ? RT_FORMAT_RGB : RT_STANDARD);
    sun_header.maptype=RMT_NONE;
    sun_header.maplength=0;
    if (!IsPseudoClass(image) && !IsGrayImage(image))
      {
        /*
          Full color SUN raster.
        */
        sun_header.depth=(image->matte ? 32 : 24);
        sun_header.length=image->columns*image->rows*(image->matte ? 4 : 3);
        sun_header.length+=image->columns & 0x01 ? image->rows : 0;
      }
    else
      if (IsMonochromeImage(image))
        {
          /*
            Monochrome SUN raster.
          */
          sun_header.depth=1;
          sun_header.length=((image->columns+7) >> 3)*image->rows;
          sun_header.length+=((image->columns/8)+(image->columns % 8 ? 1 : 0)) %
            2 ? image->rows : 0;
        }
      else
        {
          /*
            Colormapped SUN raster.
          */
          sun_header.depth=8;
          sun_header.length=image->columns*image->rows;
          sun_header.length+=image->columns & 0x01 ? image->rows : 0;
          sun_header.maptype=RMT_EQUAL_RGB;
          sun_header.maplength=image->colors*3;
        }
    /*
      Write SUN header.
    */
    MSBFirstWriteLong(image,sun_header.magic);
    MSBFirstWriteLong(image,sun_header.width);
    MSBFirstWriteLong(image,sun_header.height);
    MSBFirstWriteLong(image,sun_header.depth);
    MSBFirstWriteLong(image,sun_header.length);
    MSBFirstWriteLong(image,sun_header.type);
    MSBFirstWriteLong(image,sun_header.maptype);
    MSBFirstWriteLong(image,sun_header.maplength);
    /*
      Convert MIFF to SUN raster pixels.
    */
    p=image->pixels;
    x=0;
    y=0;
    if (!IsPseudoClass(image) && !IsGrayImage(image))
      {
        register unsigned char
          *q;

        unsigned char
          *pixels;

        unsigned short
          value;

        /*
          Allocate memory for pixels.
        */
        pixels=(unsigned char *)
          AllocateMemory(image->columns*sizeof(RunlengthPacket));
        if (pixels == (unsigned char *) NULL)
          WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
        /*
          Convert DirectClass packet to SUN RGB pixel.
        */
        q=pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            if (image->matte)
              *q++=DownScale(p->index);
            *q++=DownScale(p->red);
            *q++=DownScale(p->green);
            *q++=DownScale(p->blue);
            x++;
            if (x == (int) image->columns)
              {
                if (image->columns & 0x01)
                  WriteQuantum(0,q);  /* pad scanline */
                (void) WriteBlob(image,q-pixels,(char *) pixels);
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(y,image->rows))
                    ProgressMonitor(SaveImageText,y,image->rows);
                q=pixels;
                x=0;
                y++;
              }
          }
          p++;
        }
        FreeMemory((char *) pixels);
      }
    else
      if (IsMonochromeImage(image))
        {
          register unsigned char
            bit,
            byte,
            polarity;

          /*
            Convert PseudoClass image to a SUN monochrome image.
          */
          polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
          if (image->colors == 2)
            polarity=
              Intensity(image->colormap[0]) > Intensity(image->colormap[1]);
          bit=0;
          byte=0;
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              byte<<=1;
              if (p->index == polarity)
                byte|=0x01;
              bit++;
              if (bit == 8)
                {
                  (void) WriteByte(image,byte);
                  bit=0;
                  byte=0;
                }
              x++;
              if (x == (int) image->columns)
                {
                  /*
                    Advance to the next scanline.
                  */
                  if (bit != 0)
                    (void) WriteByte(image,byte << (8-bit));
                  if ((((image->columns/8)+
                      (image->columns % 8 ? 1 : 0)) % 2) != 0)
                    (void) WriteByte(image,0);  /* pad scanline */
                  if (image->previous == (Image *) NULL)
                    if (QuantumTick(y,image->rows))
                      ProgressMonitor(SaveImageText,y,image->rows);
                  bit=0;
                  byte=0;
                  x=0;
               }
            }
            p++;
          }
        }
      else
        {
          /*
            Dump colormap to file.
          */
          for (i=0; i < (int) image->colors; i++)
            (void) WriteByte(image,DownScale(image->colormap[i].red));
          for (i=0; i < (int) image->colors; i++)
            (void) WriteByte(image,DownScale(image->colormap[i].green));
          for (i=0; i < (int) image->colors; i++)
            (void) WriteByte(image,DownScale(image->colormap[i].blue));
          /*
            Convert PseudoClass packet to SUN colormapped pixel.
          */
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              (void) WriteByte(image,p->index);
              x++;
              if (x == (int) image->columns)
                {
                  if (image->columns & 0x01)
                    (void) WriteByte(image,0);  /* pad scanline */
                  if (image->previous == (Image *) NULL)
                    if (QuantumTick(y,image->rows))
                      ProgressMonitor(SaveImageText,y,image->rows);
                  x=0;
                }
            }
            p++;
          }
        }
    if (image->next == (Image *) NULL)
      break;
    image->next->file=image->file;
    image=image->next;
    ProgressMonitor(SaveImagesText,scene++,GetNumberScenes(image));
  } while (image_info->adjoin);
  if (image_info->adjoin)
    while (image->previous != (Image *) NULL)
      image=image->previous;
  CloseBlob(image);
  return(True);
}
