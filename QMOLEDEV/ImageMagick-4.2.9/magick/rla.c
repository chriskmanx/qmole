/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            RRRR   L       AAA                               %
%                            R   R  L      A   A                              %
%                            RRRR   L      AAAAA                              %
%                            R R    L      A   A                              %
%                            R  R   LLLLL  A   A                              %
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
%   R e a d R L A I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadRLAImage reads a run-length encoded Wavefront RLA image file
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  Note:  This module was contributed by Lester Vecsey (master@internexus.net).
%
%  The format of the ReadRLAImage method is:
%
%      Image *ReadRLAImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadRLAImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadRLAImage(const ImageInfo *image_info)
{
  typedef struct _WindowFrame
  {
    short
      left,
      right,
      bottom,
      top;
  } WindowFrame;

  typedef struct _RLAHeader
  {
    WindowFrame
      window,
      active_window;

    short
      frame,
      storage_type,
      number_channels,
      number_matte_channels,
      number_auxiliary_channels,
      revision;

    char
      gamma[16],
      red_primary[24],
      green_primary[24],
      blue_primary[24],
      white_point[24];

    long
      job_number;

    char
      name[128],
      description[128],
      program[64],
      machine[32],
      user[32],
      date[20],
      aspect[24],
      aspect_ratio[8],
      chan[32];

    short
      field;

    char
      time[12],
      filter[32];

    short
      bits_per_channel,
      matte_type,
      matte_bits,
      auxiliary_type,
      auxiliary_bits;

    char
      auxiliary[32],
      space[36];

    long
      next;
  } RLAHeader;

  Image
    *image;

  int
    channel,
    length,
    runlength,
    y;

  long
    *scanlines;

  register int
    i;

  register RunlengthPacket
    *q;

  RLAHeader
    rla_header;

  unsigned char
    byte;

  unsigned int
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
  rla_header.window.left=MSBFirstReadShort(image);
  rla_header.window.right=MSBFirstReadShort(image);
  rla_header.window.bottom=MSBFirstReadShort(image);
  rla_header.window.top=MSBFirstReadShort(image);
  rla_header.active_window.left=MSBFirstReadShort(image);
  rla_header.active_window.right=MSBFirstReadShort(image);
  rla_header.active_window.bottom=MSBFirstReadShort(image);
  rla_header.active_window.top=MSBFirstReadShort(image);
  rla_header.frame=MSBFirstReadShort(image);
  rla_header.storage_type=MSBFirstReadShort(image);
  rla_header.number_channels=MSBFirstReadShort(image);
  rla_header.number_matte_channels=MSBFirstReadShort(image);
  if (rla_header.number_channels == 0)
    rla_header.number_channels=3;
  rla_header.number_channels+=rla_header.number_matte_channels;
  rla_header.number_auxiliary_channels=MSBFirstReadShort(image);
  rla_header.revision=MSBFirstReadShort(image);
  (void) ReadBlob(image,16,(char *) rla_header.gamma);
  (void) ReadBlob(image,24,(char *) rla_header.red_primary);
  (void) ReadBlob(image,24,(char *) rla_header.green_primary);
  (void) ReadBlob(image,24,(char *) rla_header.blue_primary);
  (void) ReadBlob(image,24,(char *) rla_header.white_point);
  rla_header.job_number=(long) MSBFirstReadLong(image);
  (void) ReadBlob(image,128,(char *) rla_header.name);
  (void) ReadBlob(image,128,(char *) rla_header.description);
  (void) ReadBlob(image,64,(char *) rla_header.program);
  (void) ReadBlob(image,32,(char *) rla_header.machine);
  (void) ReadBlob(image,32,(char *) rla_header.user);
  (void) ReadBlob(image,20,(char *) rla_header.date);
  (void) ReadBlob(image,24,(char *) rla_header.aspect);
  (void) ReadBlob(image,8,(char *) rla_header.aspect_ratio);
  (void) ReadBlob(image,32,(char *) rla_header.chan);
  rla_header.field=MSBFirstReadShort(image);
  (void) ReadBlob(image,12,(char *) rla_header.time);
  (void) ReadBlob(image,32,(char *) rla_header.filter);
  rla_header.bits_per_channel=MSBFirstReadShort(image);
  rla_header.matte_type=MSBFirstReadShort(image);
  rla_header.matte_bits=MSBFirstReadShort(image);
  rla_header.auxiliary_type=MSBFirstReadShort(image);
  rla_header.auxiliary_bits=MSBFirstReadShort(image);
  (void) ReadBlob(image,32,(char *) rla_header.auxiliary);
  (void) ReadBlob(image,36,(char *) rla_header.space);
  rla_header.next=(long) MSBFirstReadLong(image);
  /*
    Initialize image structure.
  */
  image->matte=rla_header.number_matte_channels != 0;
  image->depth=QuantumDepth;
  image->columns=rla_header.active_window.right-rla_header.active_window.left;
  image->rows=rla_header.active_window.top-rla_header.active_window.bottom;
  image->packets=image->columns*image->rows;
  if (image_info->ping)
    {
      CloseBlob(image);
      return(image);
    }
  scanlines=(long *) AllocateMemory(image->rows*sizeof(long));
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  SetImage(image);
  if (*rla_header.description != '\0')
    {
      /*
        RLA image comment.
      */
      image->comments=(char *)
        AllocateMemory((Extent(rla_header.description)+1)*sizeof(char));
      if (image->comments == (char *) NULL)
        ReaderExit(ResourceLimitWarning,"Memory allocation failed",image)
      (void) strcpy(image->comments,rla_header.description);
    }
  /*
    Read offsets to each scanline data.
  */
  for (i=0; i < (int) image->rows; i++)
    scanlines[i]=(long) MSBFirstReadLong(image);
  /*
    Read image data.
  */
  q=image->pixels;
  for (y=0; y < (int) image->rows; y++)
  {
    (void) SeekBlob(image,scanlines[image->rows-y-1],SEEK_SET);
    for (channel=0; channel < (int) rla_header.number_channels; channel++)
    {
      length=MSBFirstReadShort(image);
      q=image->pixels+(y*image->columns);
      while (length > 0)
      {
        byte=ReadByte(image);
        runlength=byte;
        if (byte > 127)
          runlength=byte-256;
        length--;
        if (length == 0)
          break;
        if (runlength < 0)
          {
            while (runlength < 0)
            {
              byte=ReadByte(image);
              length--;
              switch (channel)
              {
                case 0:
                {
                  q->red=UpScale(byte);
                  q->index=0;
                  q->length=0;
                  break;
                }
                case 1:
                {
                  q->green=UpScale(byte);
                  break;
                }
                case 2:
                {
                  q->blue=UpScale(byte);
                  break;
                }
                case 3:
                default:
                {
                  q->index=UpScale(byte);
                  break;
                }
              }
              q++;
              if ((q-image->pixels) >= (int) image->packets)
                break;
              runlength++;
            }
            continue;
          }
        byte=ReadByte(image);
        length--;
        runlength++;
        do
        {
          switch (channel)
          {
            case 0:
            {
              q->red=UpScale(byte);
              q->index=0;
              q->length=0;
              break;
            }
            case 1:
            {
              q->green=UpScale(byte);
              break;
            }
            case 2:
            {
              q->blue=UpScale(byte);
              break;
            }
            case 3:
            default:
            {
              q->index=UpScale(byte);
              break;
            }
          }
          q++;
          if ((q-image->pixels) >= (int) image->packets)
            break;
          runlength--;
        }
        while (runlength > 0);
      }
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
    if ((q-image->pixels) >= (int) image->packets)
      break;
  }
  for ( ;  (q-image->pixels) < (int) image->packets; q++)
    q->length=0;
  CondenseImage(image);
  CloseBlob(image);
  return(image);
}
