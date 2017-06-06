/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        IIIII  PPPP   TTTTT   CCCC                           %
%                          I    P   P    T    C                               %
%                          I    PPPP     T    C                               %
%                          I    P        T    C                               %
%                        IIIII  P        T     CCCC                           %
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
%   R e a d I P T C I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadIPTCImage reads an image file in the IPTC format and returns it.
%  It allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.  This method differs from the other decoder
%  methods in that only the iptc profile information is useful in the
%  returned image.
%
%  The format of the ReadIPTCImage method is:
%
%      Image *ReadIPTCImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadIPTCImage returns a pointer to the image after
%      reading. A null image is returned if there is a memory shortage or if
%      the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadIPTCImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    c;

  register unsigned char
    *q;

  unsigned int
    length,
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
    Read IPTC image.
  */
  length=MaxTextExtent;
  image->iptc_profile.info=(unsigned char *)
    AllocateMemory((length+2)*sizeof(unsigned char));
  for (q=image->iptc_profile.info; ; q++)
  {
    c=ReadByte(image);
    if (c == EOF)
      break;
    if ((q-image->iptc_profile.info+1) >= (int) length)
      {
        image->iptc_profile.length=q-image->iptc_profile.info;
        length<<=1;
        image->iptc_profile.info=(unsigned char *) ReallocateMemory((char *)
          image->iptc_profile.info,(length+2)*sizeof(unsigned char));
        if (image->iptc_profile.info == (unsigned char *) NULL)
          break;
        q=image->iptc_profile.info+image->iptc_profile.length;
      }
    *q=(unsigned char) c;
  }
  image->iptc_profile.length=0;
  if (image->iptc_profile.info != (unsigned char *) NULL)
    image->iptc_profile.length=q-image->iptc_profile.info;
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e I P T C I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteIPTCImage writes an image in the IPTC format.
%
%  The format of the WriteIPTCImage method is:
%
%      unsigned int WriteIPTCImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteIPTCImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteIPTCImage(const ImageInfo *image_info,Image *image)
{
  unsigned int
    status;

  if (image->iptc_profile.length == 0)
    WriterExit(FileOpenWarning,"No IPTC profile available",image);
  /*
    Open image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  (void) WriteBlob(image,(int) image->iptc_profile.length,
    (char *) image->iptc_profile.info);
  CloseBlob(image);
  return(True);
}
