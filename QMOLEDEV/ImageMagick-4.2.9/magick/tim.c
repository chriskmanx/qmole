/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            TTTTT  IIIII  M   M                              %
%                              T      I    MM MM                              %
%                              T      I    M M M                              %
%                              T      I    M   M                              %
%                              T    IIIII  M   M                              %
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
%  R e a d T I M I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadTIMImage reads a PSX TIM image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  Contributed by os@scee.sony.co.uk.
%
%  The format of the ReadTIMImage method is:
%
%      Image *ReadTIMImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadTIMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadTIMImage(const ImageInfo *image_info)
{
#define ScaleColor5to8(x)  ((x) << 3)

  typedef struct _TIMHeader
  {
    unsigned long
      id,
      flag;
  } TIMHeader;

  TIMHeader
    tim_header;

  Image
    *image;

  register int
    i,
    x,
    y;

  int
    bits_per_pixel,
    has_clut,
    pixel_mode;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *tim_data,
    *tim_pixels;

  unsigned short
    word;

  unsigned int
    bytes_per_line,
    height,
    image_size,
    status,
    width;

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
    Determine if this is a TIM file.
  */
  tim_header.id=LSBFirstReadLong(image);
  do
  {
    /*
      Verify TIM identifier.
    */
    if (tim_header.id != 0x00000010)
      ReaderExit(CorruptImageWarning,"Not a TIM image file",image);
    tim_header.flag=LSBFirstReadLong(image);
    has_clut=!!(tim_header.flag & (1 << 3));
    pixel_mode=tim_header.flag & 0x07;
    switch (pixel_mode)
    {
      case 0: bits_per_pixel=4; break;
      case 1: bits_per_pixel=8; break;
      case 2: bits_per_pixel=16; break;
      case 3: bits_per_pixel=24; break;
      default: bits_per_pixel=4; break;
    }
    if (has_clut)
      {
        unsigned char
          *tim_colormap;

        /*
          Read TIM raster colormap.
        */
        (void)LSBFirstReadLong(image);
        (void)LSBFirstReadShort(image);
        (void)LSBFirstReadShort(image);
        width=LSBFirstReadShort(image);
        height=LSBFirstReadShort(image);
        image->class=PseudoClass;
        image->colors=(unsigned int) pixel_mode == 1 ? 256 : 16;
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
        tim_colormap=(unsigned char *)
          AllocateMemory(image->colors*2*sizeof(unsigned char));
        if ((image->colormap == (ColorPacket *) NULL) ||
            (tim_colormap == (unsigned char *) NULL))
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        (void) ReadBlob(image,2*image->colors,(char *) tim_colormap);
        p=tim_colormap;
        for (i=0; i < (int) image->colors; i++)
        {
          word=(*p++);
          word=word | (*p++ << 8);
          image->colormap[i].blue=UpScale(ScaleColor5to8((word >> 10) & 0x1f));
          image->colormap[i].green=UpScale(ScaleColor5to8((word >> 5) & 0x1f));
          image->colormap[i].red=UpScale(ScaleColor5to8(word & 0x1f));
        }
        free((char *) tim_colormap);
      }
    /*
      Read image data.
    */
    (void) LSBFirstReadLong(image);
    (void) LSBFirstReadShort(image);
    (void) LSBFirstReadShort(image);
    width=LSBFirstReadShort(image);
    height=LSBFirstReadShort(image);
    image_size=2*width*height;
    bytes_per_line=width*2;
    width=(width*16)/bits_per_pixel;
    tim_data=(unsigned char *) AllocateMemory(image_size*sizeof(unsigned char));
    if (tim_data == (unsigned char *) NULL)
      ReaderExit(ResourceLimitWarning,"Unable to allocate memory",image);
    (void) ReadBlob(image,image_size,(char *) tim_data);
    tim_pixels=tim_data;
    /*
      Initialize image structure.
    */
    image->columns=width;
    image->rows=height;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Unable to allocate memory",image);
    /*
      Convert TIM raster image to runlength-encoded packets.
    */
    switch (bits_per_pixel)
    {
      case 4:
      {
        /*
          Convert PseudoColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=tim_pixels+y*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < ((int) image->columns-1); x+=2)
          {
            q->index=(*p) & 0xf;
            q->length=0;
            q++;
            q->index=(*p >> 4) & 0xf;
            q->length=0;
            p++;
            q++;
          }
          if ((image->columns % 2) != 0)
            {
              q->index=(*p >> 4) & 0xf;
              q->length=0;
              q++;
              p++;
            }
          if (QuantumTick(y,image->rows))
            ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      case 8:
      {
        /*
          Convert PseudoColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=tim_pixels+y*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=(*p++);
            q->length=0;
            q++;
          }
          if (QuantumTick(y,image->rows))
            ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      case 16:
      {
        /*
          Convert DirectColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=tim_pixels+y*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=0;
            word=(*p++);
            word=word | (*p++ << 8);
            q->blue=UpScale(ScaleColor5to8((word >> 10) & 0x1f));
            q->green=UpScale(ScaleColor5to8((word >> 5) & 0x1f));
            q->red=UpScale(ScaleColor5to8(word & 0x1f));
            q->length=0;
            q++;
          }
          if (QuantumTick(y,image->rows))
            ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      case 24:
      {
        /*
          Convert DirectColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=tim_pixels+y*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=0;
            q->red=UpScale(*p++);
            q->green=UpScale(*p++);
            q->blue=UpScale(*p++);
            q->length=0;
            q++;
          }
          if (QuantumTick(y,image->rows))
            ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      default:
        ReaderExit(CorruptImageWarning,"Not a TIM image file",image);
    }
    free((char *) tim_pixels);
    if (image->class == PseudoClass)
      SyncImage(image);
    CondenseImage(image);
    /*
      Proceed to next image.
    */
    tim_header.id=LSBFirstReadLong(image);
    if (tim_header.id == 0x00000010)
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
  } while (tim_header.id == 0x00000010);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseBlob(image);
  return(image);
}
