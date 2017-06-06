/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            PPPP   W   W  PPPP                               %
%                            P   P  W   W  P   P                              %
%                            PPPP   W   W  PPPP                               %
%                            P      W W W  P                                  %
%                            P       W W   P                                  %
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
%   R e a d L O G O I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadPWPImage reads a Seattle Film Works multi-image file and returns
%  it.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.
%
%  The format of the ReadPWPImage method is:
%
%      Image *ReadPWPImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadPWPImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadPWPImage(const ImageInfo *image_info)
{
  FILE
    *file;

  Image
    *image,
    *next_image,
    *pwp_image;

  ImageInfo
    *local_info;

  int
    c;

  long
    filesize;

  MonitorHandler
    handler;

  register Image
    *p;

  register int
    i;

  unsigned char
    magick[MaxTextExtent];

  unsigned int
    status;

  /*
    Allocate image structure.
  */
  pwp_image=AllocateImage(image_info);
  if (pwp_image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  status=OpenBlob(image_info,pwp_image,ReadBinaryType);
  if (pwp_image->file == (FILE *) NULL)
    ReaderExit(FileOpenWarning,"Unable to open file",pwp_image);
  status=ReadBlob(pwp_image,5,(char *) magick);
  if ((status == False) || (strncmp((char *) magick,"SFW95",5) != 0))
    ReaderExit(CorruptImageWarning,"Not a PWP image file",pwp_image);
  local_info=CloneImageInfo(image_info);
  TemporaryFilename(local_info->filename);
  image=(Image *) NULL;
  for ( ; ; )
  {
    for (c=ReadByte(pwp_image); c != EOF; c=ReadByte(pwp_image))
    {
      for (i=0; i < 17; i++)
        magick[i]=magick[i+1];
      magick[17]=(unsigned char) c;
      if (strncmp((char *) (magick+12),"SFW94A",6) == 0)
        break;
    }
    if (c == EOF)
      break;
    if (strncmp((char *) (magick+12),"SFW94A",6) != 0)
      ReaderExit(CorruptImageWarning,"Not a PWP image file",pwp_image);
    /*
      Dump SFW image to a temporary file.
    */
    file=fopen(local_info->filename,WriteBinaryType);
    if (file == (FILE *) NULL)
      ReaderExit(FileOpenWarning,"Unable to write file",image);
    (void) fwrite("SFW94A",1,6,file);
    filesize=65535L*magick[2]+256L*magick[1]+magick[0];
    for (i=0; i < filesize; i++)
    {
      c=ReadByte(pwp_image);
      (void) fputc(c,file);
    }
    (void) fclose(file);
    handler=SetMonitorHandler((MonitorHandler) NULL);
    next_image=ReadSFWImage(local_info);
    (void) SetMonitorHandler(handler);
    if (next_image == (Image *) NULL)
      break;
    FormatString(next_image->filename,"slide_%02d.sfw",next_image->scene);
    if (image == (Image *) NULL)
      image=next_image;
    else
      {
        /*
          Link image into image list.
        */
        for (p=image; p->next != (Image *) NULL; p=p->next);
        next_image->previous=p;
        next_image->scene=p->scene+1;
        p->next=next_image;
      }
    if (image_info->subrange != 0)
      if (next_image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    ProgressMonitor(LoadImagesText,(unsigned int) TellBlob(pwp_image),
      (unsigned int) pwp_image->filesize);
  }
  (void) remove(local_info->filename);
  DestroyImageInfo(local_info);
  CloseBlob(pwp_image);
  DestroyImage(pwp_image);
  return(image);
}
