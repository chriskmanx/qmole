/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            M   M   AAA   PPPP                               %
%                            MM MM  A   A  P   P                              %
%                            M M M  AAAAA  PPPP                               %
%                            M   M  A   A  P                                  %
%                            M   M  A   A  P                                  %
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
%   R e a d M A P I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadMAPImage reads an image of raw RGB colormap and colormap index
%  bytes and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadMAPImage method is:
%
%      Image *ReadMAPImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadMAPImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadMAPImage(const ImageInfo *image_info)
{
  Image
    *image;

  register int
    i;

  register unsigned char
    *p;

  unsigned char
    *colormap;

  unsigned int
    packet_size,
    status;

  unsigned short
    value;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  if ((image->columns == 0) || (image->rows == 0))
    ReaderExit(OptionWarning,"must specify image size",image);
  /*
    Open image file.
  */
  status=OpenBlob(image_info,image,ReadBinaryType);
  if (status == False)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  /*
    Initialize image structure.
  */
  image->class=PseudoClass;
  image->compression=NoCompression;
  image->colors=image->offset ? image->offset : 256;
  image->packets=image->columns*image->rows;
  packet_size=3*(image->depth >> 3);
  colormap=(unsigned char *)
    AllocateMemory(packet_size*image->colors*sizeof(unsigned char));
  image->colormap=(ColorPacket *)
    AllocateMemory(image->colors*sizeof(ColorPacket));
  image->packed_pixels=(unsigned char *)
    AllocateMemory(image->packets*packet_size*sizeof(unsigned char));
  if ((colormap == (unsigned char *) NULL) ||
      (image->colormap == (ColorPacket *) NULL))
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Read image colormap.
  */
  (void) ReadBlob(image,packet_size*image->colors,(char *) colormap);
  p=colormap;
  for (i=0; i < (int) image->colors; i++)
  {
    ReadQuantum(image->colormap[i].red,p);
    ReadQuantum(image->colormap[i].green,p);
    ReadQuantum(image->colormap[i].blue,p);
  }
  FreeMemory((char *) colormap);
  /*
    Convert raster image to runlength-encoded packets.
  */
  packet_size=1;
  if (image->colors > 256)
    packet_size++;
  if (image->packed_pixels != (unsigned char *) NULL)
    FreeMemory((char *) image->packed_pixels);
  image->packed_pixels=(unsigned char *)
    AllocateMemory(image->packets*packet_size);
  if (image->packed_pixels == (unsigned char *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  (void) ReadBlob(image,packet_size*image->packets,
    (char *) image->packed_pixels);
  status=RunlengthDecodeImage(image);
  if (status == False)
    {
      DestroyImages(image);
      return((Image *) NULL);
    }
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e M A P I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteMAPImage writes an image to a file as red, green, and blue
%  colormap bytes followed by the colormap indexes.
%
%  The format of the WriteMAPImage method is:
%
%      unsigned int WriteMAPImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteMAPImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteMAPImage(const ImageInfo *image_info,Image *image)
{
  register int
    i;

  register unsigned char
    *q;

  unsigned char
    *colormap;

  unsigned int
    packets,
    packet_size,
    status;

  unsigned short
    value;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  TransformRGBImage(image,RGBColorspace);
  /*
    Allocate colormap.
  */
  if (!IsPseudoClass(image))
    {
      QuantizeInfo
        quantize_info;

      /*
        Demote DirectClass to PseudoClass.
      */
      GetQuantizeInfo(&quantize_info);
      quantize_info.number_colors=MaxColormapSize;
      quantize_info.dither=image_info->dither;
      (void) QuantizeImage(&quantize_info,image);
      SyncImage(image);
    }
  packet_size=3*(image->depth >> 3);
  colormap=(unsigned char *)
    AllocateMemory(packet_size*image->colors*sizeof(unsigned char));
  if (colormap == (unsigned char *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Write colormap to file.
  */
  q=colormap;
  for (i=0; i < (int) image->colors; i++)
  {
    WriteQuantum(image->colormap[i].red,q);
    WriteQuantum(image->colormap[i].green,q);
    WriteQuantum(image->colormap[i].blue,q);
  }
  (void) WriteBlob(image,packet_size*(int) image->colors,(char *) colormap);
  FreeMemory((char *) colormap);
  /*
    Write image pixels to file.
  */
  image->compression=NoCompression;
  packets=RunlengthEncodeImage(image);
  (void) WriteBlob(image,image->packet_size*packets,
    (char *) image->packed_pixels);
  CloseBlob(image);
  return(True);
}
