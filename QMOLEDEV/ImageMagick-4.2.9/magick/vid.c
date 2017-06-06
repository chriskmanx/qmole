/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            V   V  IIIII  DDDD                               %
%                            V   V    I    D   D                              %
%                            V   V    I    D   D                              %
%                             V V     I    D   D                              %
%                              V    IIIII  DDDD                               %
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
%   R e a d V I D I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadVIDImage reads one of more images and creates a Visual Image
%  Directory file.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadVIDImage method is:
%
%      Image *ReadVIDImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadVIDImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadVIDImage(const ImageInfo *image_info)
{
#define ClientName  "montage"

  char
    *commands[5],
    **filelist,
    **list;

  Image
    *image,
    *montage_image,
    *next_image;

  ImageInfo
    *local_info;

  int
    number_files;

  MonitorHandler
    handler;

  MontageInfo
    montage_info;

  register int
    i;

  unsigned int
    status;

  /*
    Expand the filename.
  */
  list=(char **) AllocateMemory(sizeof(char *));
  if (list == (char **) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return((Image *) NULL);
    }
  list[0]=(char *) AllocateMemory(Extent(image_info->filename)+1);
  if (list[0] == (char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return((Image *) NULL);
    }
  (void) strcpy(list[0],image_info->filename);
  number_files=1;
  filelist=list;
  status=ExpandFilenames(&number_files,&filelist);
  if ((status == False) || (number_files == 0))
    {
      MagickWarning(CorruptImageWarning,"unable to read VID image",
        image_info->filename);
      return((Image *) NULL);
    }
  /*
    Read each image and convert them to a tile.
  */
  image=(Image *) NULL;
  local_info=CloneImageInfo(image_info);
  if (local_info == (ImageInfo *) NULL)
    return((Image *) NULL);
  (void) CloneString(&local_info->size,DefaultTileGeometry);
  commands[0]=SetClientName((char *) NULL);
  commands[1]="-label";
  commands[2]=(char *) DefaultTileLabel;
  commands[3]="-geometry";
  commands[4]=(char *) DefaultTileGeometry;
  for (i=0; i < number_files; i++)
  {
    handler=SetMonitorHandler((MonitorHandler) NULL);
    (void) strcpy(local_info->filename,filelist[i]);
    *local_info->magick='\0';
    next_image=ReadImage(local_info);
    FreeMemory((char *) filelist[i]);
    if (next_image != (Image *) NULL)
      {
        MogrifyImages(local_info,5,commands,&next_image);
        if (image == (Image *) NULL)
          image=next_image;
        else
          {
            image->next=next_image;
            image->next->previous=image;
            image=image->next;
          }
      }
    (void) SetMonitorHandler(handler);
    ProgressMonitor(LoadImageText,i,number_files);
  }
  DestroyImageInfo(local_info);
  FreeMemory((char *) filelist);
  if (image == (Image *) NULL)
    {
      MagickWarning(CorruptImageWarning,"unable to read VID image",
        image_info->filename);
      return((Image *) NULL);
    }
  while (image->previous != (Image *) NULL)
    image=image->previous;
  /*
    Create the visual image directory.
  */
  GetMontageInfo(&montage_info);
  (void) strcpy(montage_info.filename,image_info->filename);
  (void) CloneString(&montage_info.font,image_info->font);
  montage_info.pointsize=image_info->pointsize;
  montage_image=MontageImages(image,&montage_info);
  DestroyMontageInfo(&montage_info);
  if (montage_image == (Image *) NULL)
    {
      MagickWarning(CorruptImageWarning,"unable to read VID image",
        image_info->filename);
      return((Image *) NULL);
    }
  DestroyImages(image);
  FreeMemory((char *) list[0]);
  FreeMemory((char *) list);
  return(montage_image);
}
