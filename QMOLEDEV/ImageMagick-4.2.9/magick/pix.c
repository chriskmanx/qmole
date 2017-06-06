/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            PPPP   IIIII  X   X                              %
%                            P   P    I     X X                               %
%                            PPPP     I      X                                %
%                            P        I     X X                               %
%                            P      IIIII  X   X                              %
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
%   R e a d P I X I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadPIXImage reads a Alias/Wavefront RLE image file and returns it.
%  It allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadPIXImage method is:
%
%      Image *ReadPIXImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadPIXImage returns a pointer to the image after
%      reading. A null image is returned if there is a memory shortage or if
%      the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadPIXImage(const ImageInfo *image_info)
{
  Image
    *image;

  register long
    packets;

  register RunlengthPacket
    *q;

  unsigned int
    bits_per_pixel,
    status;

  unsigned long
    height,
    number_pixels,
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
    Read PIX image.
  */
  width=MSBFirstReadShort(image);
  height=MSBFirstReadShort(image);
  (void) MSBFirstReadShort(image);  /* x-offset */
  (void) MSBFirstReadShort(image);  /* y-offset */
  bits_per_pixel=MSBFirstReadShort(image);
  if ((width == (unsigned long) ~0) || (height == (unsigned long) ~0) ||
      ((bits_per_pixel != 8) && (bits_per_pixel != 24)))
    ReaderExit(CorruptImageWarning,"Not a PIX image file",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->columns=width;
    image->rows=height;
    if (bits_per_pixel == 8)
      {
        register int
          i;

        /*
          Create linear colormap.
        */
        image->class=PseudoClass;
        image->colors=MaxRGB+1;
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        for (i=0; i < (int) image->colors; i++)
        {
          image->colormap[i].red=(Quantum) UpScale(i);
          image->colormap[i].green=(Quantum) UpScale(i);
          image->colormap[i].blue=(Quantum) UpScale(i);
        }
      }
    if (image_info->ping)
      {
        CloseBlob(image);
        return(image);
      }
    packets=0;
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->columns*image->rows*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    /*
      Convert PIX raster image to runlength-encoded packets.
    */
    number_pixels=0;
    q=image->pixels;
    do
    {
      q->length=ReadByte(image);
      if (q->length == 0)
        continue;
      if (bits_per_pixel == 8)
        q->index=UpScale(ReadByte(image));
      else
        {
          q->blue=UpScale(ReadByte(image));
          q->green=UpScale(ReadByte(image));
          q->red=UpScale(ReadByte(image));
          q->index=0;
        }
      number_pixels+=q->length;
      q++;
      packets++;
      if (image->previous == (Image *) NULL)
        ProgressMonitor(LoadImageText,number_pixels,image->columns*image->rows);
    } while (number_pixels < (image->columns*image->rows));
    SetRunlengthPackets(image,packets);
    if (image->class == PseudoClass)
      SyncImage(image);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    width=MSBFirstReadLong(image);
    height=MSBFirstReadLong(image);
    (void) MSBFirstReadShort(image);
    (void) MSBFirstReadShort(image);
    bits_per_pixel=MSBFirstReadShort(image);
    status=(width != (unsigned long) ~0) && (height == (unsigned long) ~0) &&
      ((bits_per_pixel == 8) || (bits_per_pixel == 24));
    if (status == True)
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
  } while (status == True);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseBlob(image);
  return(image);
}
