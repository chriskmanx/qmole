/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%       H   H  IIIII  SSSSS  TTTTT   OOO    GGGG  RRRR    AAA   M   M         %
%       H   H    I    SS       T    O   O  G      R   R  A   A  MM MM         %
%       HHHHH    I     SSS     T    O   O  G  GG  RRRR   AAAAA  M M M         %
%       H   H    I       SS    T    O   O  G   G  R R    A   A  M   M         %
%       H   H  IIIII  SSSSS    T     OOO    GGG   R  R   A   A  M   M         %
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
%   R e a d H I S T O G R A M I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadHISTOGRAMImage reads a HISTOGRAM image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadHISTOGRAMImage method is:
%
%      Image *ReadHISTOGRAMImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadHISTOGRAMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadHISTOGRAMImage(const ImageInfo *image_info)
{
  Image
    *image;

  image=ReadMIFFImage(image_info);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e H I S T O G R A M I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteHISTOGRAMImage writes an image to a file in HISTOGRAM format.
%  The image shows a histogram of the color (or gray) values in the image.  The
%  image consists of three overlaid histograms:  a red one for the red channel,
%  a green one for the green channel, and a blue one for the blue channel.  The
%  image comment contains a list of unique pixel values and the number of times
%  each occurs in the image.
%
%  This method is strongly based on a similar one written by
%  muquit@warm.semcor.com which in turn is based on ppmhistmap of netpbm.
%
%  The format of the WriteHISTOGRAMImage method is:
%
%      unsigned int WriteHISTOGRAMImage(const ImageInfo *image_info,
%        Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteHISTOGRAMImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteHISTOGRAMImage(const ImageInfo *image_info,
  Image *image)
{
#define HistogramDensity  "256x200"

  char
    filename[MaxTextExtent];

  double
    scale;

  FILE
    *file;

  Image
    *histogram_image;

  int
    *blue,
    *green,
    maximum,
    *red,
    sans_offset;

  register RunlengthPacket
    *p,
    *q;

  register int
    i,
    j;

  unsigned int
    height,
    status,
    width;

  /*
    Allocate histogram image.
  */
  width=image->columns;
  height=image->rows;
  if (image_info->density != (char *) NULL)
    (void) ParseGeometry(image_info->density,&sans_offset,&sans_offset,
      &width,&height);
  else
    (void) ParseGeometry(HistogramDensity,&sans_offset,&sans_offset,
      &width,&height);
  image->orphan=True;
  histogram_image=CloneImage(image,width,height,False);
  image->orphan=False;
  if (histogram_image == (Image *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  histogram_image->class=DirectClass;
  /*
    Allocate histogram count arrays.
  */
  red=(int *) AllocateMemory (histogram_image->columns*sizeof(int));
  green=(int *) AllocateMemory (histogram_image->columns*sizeof(int));
  blue=(int *) AllocateMemory (histogram_image->columns*sizeof(int));
  if ((red == (int *) NULL) || (green == (int *) NULL) ||
      (blue == (int *) NULL))
    {
      DestroyImage(histogram_image);
      WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  /*
    Initialize histogram count arrays.
  */
  for (i=0; i < (int) histogram_image->columns; i++)
  {
    red[i]=0;
    green[i]=0;
    blue[i]=0;
  }
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    red[DownScale(p->red)]+=(p->length+1);
    green[DownScale(p->green)]+=(p->length+1);
    blue[DownScale(p->blue)]+=(p->length+1);
    p++;
  }
  maximum=0;
  for (i=0; i < (int) histogram_image->columns; i++)
  {
    if (maximum < red[i])
      maximum=red[i];
    if (maximum < green[i])
      maximum=green[i];
    if (maximum < blue[i])
      maximum=blue[i];
  }
  for (i=0; i < (int) histogram_image->columns; i++)
  {
    if (red[i] > maximum)
      red[i]=maximum;
    if (green[i] > maximum)
      green[i]=maximum;
    if (blue[i] > maximum)
      blue[i]=maximum;
  }
  /*
    Initialize histogram image.
  */
  q=histogram_image->pixels;
  for (i=0; i < (int) histogram_image->packets; i++)
  {
    q->red=0;
    q->green=0;
    q->blue=0;
    q->index=0;
    q->length=0;
    q++;
  }
  scale=(double) histogram_image->rows/maximum;
  q=histogram_image->pixels;
  for (i=0; i < (int) histogram_image->columns; i++)
  {
    j=histogram_image->rows-(int) (scale*red[i]);
    while (j < (int) histogram_image->rows)
    {
      q=histogram_image->pixels+(j*histogram_image->columns+i);
      q->red=MaxRGB;
      j++;
    }
    j=histogram_image->rows-(int) (scale*green[i]);
    while (j < (int) histogram_image->rows)
    {
      q=histogram_image->pixels+(j*histogram_image->columns+i);
      q->green=MaxRGB;
      j++;
    }
    j=histogram_image->rows-(int) (scale*blue[i]);
    while (j < (int) histogram_image->rows)
    {
      q=histogram_image->pixels+(j*histogram_image->columns+i);
      q->blue=MaxRGB;
      j++;
    }
    if (QuantumTick(i,histogram_image->columns))
      ProgressMonitor(SaveImageText,i,histogram_image->columns);
  }
  FreeMemory ((char *) blue);
  FreeMemory ((char *) green);
  FreeMemory ((char *) red);
  TemporaryFilename(filename);
  file=fopen(filename,WriteBinaryType);
  if (file != (FILE *) NULL)
    {
      char
        command[MaxTextExtent];

      /*
        Add a histogram as an image comment.
      */
      if (image->comments != (char *) NULL)
        (void) fprintf(file,"%s\n",image->comments);
      (void) GetNumberColors(image,file);
      (void) fclose(file);
      FormatString(command,"@%.1024s",filename);
      CommentImage(histogram_image,command);
      (void) remove(filename);
    }
  /*
    Write HISTOGRAM image as MIFF.
  */
  status=WriteMIFFImage(image_info,histogram_image);
  DestroyImage(histogram_image);
  return(status);
}
