/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%        GGGG  RRRR    AAA   DDDD    AAA   TTTTT  IIIII   OOO   N   N         %
%       G      R   R  A   A  D   D  A   A    T      I    O   O  NN  N         %
%       G  GG  RRRR   AAAAA  D   D  AAAAA    T      I    O   O  N N N         %
%       G   G  R R    A   A  D   D  A   A    T      I    O   O  N  NN         %
%        GGG   R  R   A   A  DDDD   A   A    T    IIIII   OOO   N   N         %
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
%   R e a d G R A D A T I O N I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadGRADATIONImage creates a gradation image and initializes it to
%  the X server color range as specified by the filename.  It allocates the
%  memory necessary for the new Image structure and returns a pointer to the
%  new image.
%
%  The format of the ReadGRADATIONImage method is:
%
%      Image *ReadGRADATIONImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadGRADATIONImage returns a pointer to the image after
%      creating it. A null image is returned if there is a memory shortage
%      or if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadGRADATIONImage(const ImageInfo *image_info)
{
  char
    colorname[MaxTextExtent];

  ColorPacket
    color;

  double
    brightness,
    brightness_step,
    hue,
    hue_step,
    saturation,
    saturation_step;

  Image
    *image;

  int
    x,
    y;

  register RunlengthPacket
    *q;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Initialize Image structure.
  */
  (void) strcpy(image->filename,image_info->filename);
  if (image->columns == 0)
    image->columns=512;
  if (image->rows == 0)
    image->rows=512;
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  SetImage(image);
  /*
    Determine (Hue, Saturation, Brightness) gradient.
  */
  (void) strcpy(colorname,image_info->filename);
  (void) sscanf(image_info->filename,"%[^-]",colorname);
  (void) QueryColorDatabase(colorname,&color);
  TransformHSL((Quantum) XDownScale(color.red),
    (Quantum) XDownScale(color.green),(Quantum) XDownScale(color.blue),
    &hue,&saturation,&brightness);
  (void) strcpy(colorname,"white");
  if (Intensity(color) > 32767)
    (void) strcpy(colorname,"black");
  (void) sscanf(image_info->filename,"%*[^-]-%s",colorname);
  (void) QueryColorDatabase(colorname,&color);
  TransformHSL((Quantum) XDownScale(color.red),
    (Quantum) XDownScale(color.green),(Quantum) XDownScale(color.blue),
    &hue_step,&saturation_step,&brightness_step);
  hue_step=(hue_step-hue)/(double) image->packets;
  saturation_step=(saturation_step-saturation)/(double) image->packets;
  brightness_step=(brightness_step-brightness)/(double) image->packets;
  /*
    Initialize image pixels.
  */
  q=image->pixels;
  for (y=0; y < (int) image->rows; y++)
  {
    for (x=0; x < (int) image->columns; x++)
    {
      HSLTransform(hue,saturation,brightness,&q->red,&q->green,&q->blue);
      q->index=0;
      q->length=0;
      q++;
      hue+=hue_step;
      saturation+=saturation_step;
      brightness+=brightness_step;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
  }
  CondenseImage(image);
  return(image);
}
