/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%              SSSSS  TTTTT  EEEEE   GGGG   AAA   N   N   OOO                 %
%              SS       T    E      G      A   A  NN  N  O   O                %
%               SSS     T    EEE    G  GG  AAAAA  N N N  O   O                %
%                 SS    T    E      G   G  A   A  N  NN  O   O                %
%              SSSSS    T    EEEEE   GGG   A   A  N   N   OOO                 %
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
%   R e a d S T E G A N O I m a g e                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadSTEGANOImage reads a steganographic image hidden within another
%  image type.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadSTEGANOImage method is:
%
%      Image *ReadSTEGANOImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadSTEGANOImage returns a pointer to the image
%      after reading.  A null image is returned if there is a memory shortage
%      of if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadSTEGANOImage(const ImageInfo *image_info)
{
#define UnembedBit(byte) \
{ \
  q->index|=(byte & 0x01) << shift; \
  q++; \
  if (q >= (image->pixels+image->packets-1)) \
    { \
      q=image->pixels; \
      shift--; \
      if (shift < 0) \
        break; \
    } \
}

  ImageInfo
    *local_info;

  int
    shift;

  register int
    i;

  register RunlengthPacket
    *p,
    *q;

  Image
    *cloned_image,
    *image,
    *stegano_image;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  if ((image->columns == 0) || (image->rows == 0))
    ReaderExit(OptionWarning,"must specify image size",image);
  /*
    Initialize Image structure.
  */
  local_info=CloneImageInfo(image_info);
  if (local_info == (ImageInfo *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  *local_info->magick='\0';
  stegano_image=ReadImage(local_info);
  DestroyImageInfo(local_info);
  if (stegano_image == (Image *) NULL)
    return((Image *) NULL);
  if (!UncondenseImage(stegano_image))
    return((Image *) NULL);
  stegano_image->orphan=True;
  cloned_image=CloneImage(stegano_image,image->columns,image->rows,False);
  stegano_image->orphan=False;
  DestroyImage(image);
  if (cloned_image == (Image *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",
      stegano_image);
  image=cloned_image;
  SetImage(image);
  image->class=PseudoClass;
  image->colors=1 << QuantumDepth;
  image->colormap=(ColorPacket *)
    AllocateMemory(image->colors*sizeof(ColorPacket));
  if (image->colormap == (ColorPacket *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  for (i=0; i < (int) image->colors; i++)
  {
    image->colormap[i].red=(Quantum)
      ((unsigned long) (MaxRGB*i)/(image->colors-1));
    image->colormap[i].green=(Quantum)
      ((unsigned long) (MaxRGB*i)/(image->colors-1));
    image->colormap[i].blue=(Quantum)
      ((unsigned long) (MaxRGB*i)/(image->colors-1));
  }
  /*
    Grab embedded watermark.
  */
  shift=image->depth-1;
  p=stegano_image->pixels+(stegano_image->offset % stegano_image->packets);
  q=image->pixels;
  for (i=0; i < (int) stegano_image->packets; i++)
  {
    if (stegano_image->class == PseudoClass)
      UnembedBit(p->index)
    else
      {
        UnembedBit(p->red);
        UnembedBit(p->green);
        UnembedBit(p->blue);
      }
    p++;
    if (p >= (stegano_image->pixels+stegano_image->packets-1))
      p=stegano_image->pixels;
    if (QuantumTick(i,stegano_image->packets))
      ProgressMonitor(LoadImageText,i,stegano_image->packets);
  }
  SyncImage(image);
  CondenseImage(image);
  DestroyImage(stegano_image);
  return(image);
}
