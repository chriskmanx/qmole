/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                     M   M   AAA   TTTTT  TTTTT  EEEEE                       %
%                     MM MM  A   A    T      T    E                           %
%                     M M M  AAAAA    T      T    EEE                         %
%                     M   M  A   A    T      T    E                           %
%                     M   M  A   A    T      T    EEEEE                       %
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
%   W r i t e M A T T E I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function WriteMATTEImage writes an image of matte bytes to a file.  It
%  consists of data from the matte component of the image [0..255].
%
%  The format of the WriteMATTEImage method is:
%
%      unsigned int WriteMATTEImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Function WriteMATTEImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteMATTEImage(const ImageInfo *image_info,Image *image)
{
  Image
    *matte_image;

  register int
    i;

  unsigned int
    status;

  if (!image->matte)
    WriterExit(ResourceLimitWarning,"Image does not have a matte channel",
      image);
  image->orphan=True;
  matte_image=CloneImage(image,image->columns,image->rows,True);
  image->orphan=False;
  if (matte_image == (Image *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  matte_image->class=PseudoClass;
  matte_image->colors=(Opaque-Transparent)+1;
  matte_image->colormap=(ColorPacket *)
    AllocateMemory(matte_image->colors*sizeof(ColorPacket));
  if (matte_image->colormap == (ColorPacket *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  for (i=Transparent; i <= Opaque; i++)
  {
    matte_image->colormap[i].red=(Quantum) i;
    matte_image->colormap[i].green=(Quantum) i;
    matte_image->colormap[i].blue=(Quantum) i;
  }
  SyncImage(matte_image);
  status=WriteMIFFImage(image_info,matte_image);
  DestroyImage(matte_image);
  return(status);
}
