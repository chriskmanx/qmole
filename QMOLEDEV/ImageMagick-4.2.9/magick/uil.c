/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            U   U  IIIII  L                                  %
%                            U   U    I    L                                  %
%                            U   U    I    L                                  %
%                            U   U    I    L                                  %
%                             UUU   IIIII  LLLLL                              %
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
%   R e a d U I L I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadUILImage reads a X-Motif UIL table and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadUILImage method is:
%
%      Image *ReadUILImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadUILImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadUILImage(const ImageInfo *image_info)
{
  MagickWarning(MissingDelegateWarning,"Cannot read UIL images",
    image_info->filename);
  return((Image *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e U I L I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure WriteUILImage writes an image to a file in the X-Motif UIL table
%  format.
%
%  The format of the WriteUILImage method is:
%
%      unsigned int WriteUILImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteUILImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteUILImage(const ImageInfo *image_info,Image *image)
{
#define MaxCixels  92

  static const char
    Cixel[MaxCixels+1] = " .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjk"
                         "lzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`'][{}|";

  char
    buffer[MaxTextExtent],
    name[MaxTextExtent],
    symbol[MaxTextExtent];

  double
    min_distance;

  int
    j,
    k,
    y;

  long
    mean;

  register double
    distance_squared;

  register int
    distance,
    i,
    runlength,
    x;

  register RunlengthPacket
    *p;

  register const ColorlistInfo
    *q;

  unsigned int
    characters_per_pixel,
    colors,
    status,
    transparent;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  TransformRGBImage(image,RGBColorspace);
  transparent=False;
  if (image->class == PseudoClass)
    colors=image->colors;
  else
    {
      QuantizeInfo
        quantize_info;

      unsigned char
        *matte_image;

      /*
        Convert DirectClass to PseudoClass image.
      */
      matte_image=(unsigned char *) NULL;
      if (image->matte)
        {
          /*
            Map all the transparent pixels.
          */
          if (!UncondenseImage(image))
            return(False);
          matte_image=(unsigned char *)
            AllocateMemory(image->packets*sizeof(unsigned char));
          if (matte_image == (unsigned char *) NULL)
            WriterExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          p=image->pixels;
          for (i=0; i < (int) image->packets; i++)
          {
            matte_image[i]=p->index == Transparent;
            if (matte_image[i])
              transparent=True;
            p++;
          }
        }
      GetQuantizeInfo(&quantize_info);
      quantize_info.dither=image_info->dither;
      (void) QuantizeImage(&quantize_info,image);
      SyncImage(image);
      colors=image->colors;
      if (transparent)
        {
          if (!UncondenseImage(image))
            return(False);
          colors++;
          p=image->pixels;
          for (i=0; i < (int) image->packets; i++)
          {
            if (matte_image[i])
              p->index=image->colors;
            p++;
          }
        }
      if (matte_image != (unsigned char *) NULL)
        FreeMemory((char *) matte_image);
    }
  /*
    Compute the character per pixel.
  */
  characters_per_pixel=1;
  for (k=MaxCixels; (int) colors > k; k*=MaxCixels)
    characters_per_pixel++;
  /*
    UIL header.
  */
  (void) strcpy(buffer,"/* UIL */\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"value\n  %.1024s_ct : color_table(\n",
    BaseFilename(image->filename));
  (void) WriteBlob(image,strlen(buffer),buffer);
  for (i=0; i < (int) colors; i++)
  {
    ColorPacket
      *p;

    /*
      Define UIL color.
    */
    min_distance=0;
    p=image->colormap+i;
    for (q=XPMColorlist; q->name != (char *) NULL; q++)
    {
      mean=(DownScale(p->red)+q->red)/2;
      distance=DownScale(p->red)-(int) q->red;
      distance_squared=(2.0*256.0+mean)*distance*distance/256.0;
      distance=DownScale(p->green)-(int) q->green;
      distance_squared+=4.0*distance*distance;
      distance=DownScale(p->blue)-(int) q->blue;
      distance_squared+=(3.0*256.0-1.0-mean)*distance*distance/256.0;
      if ((q == XPMColorlist) || (distance_squared <= min_distance))
        {
          min_distance=distance_squared;
          (void) strcpy(name,q->name);
        }
    }
    if (transparent)
      if (i == (int) (colors-1))
        (void) strcpy(name,"None");
    /*
      Write UIL color.
    */
    k=i % MaxCixels;
    symbol[0]=Cixel[k];
    for (j=1; j < (int) characters_per_pixel; j++)
    {
      k=((i-k)/MaxCixels) % MaxCixels;
      symbol[j]=Cixel[k];
    }
    symbol[j]='\0';
    if (Latin1Compare(name,"None") == 0)
      (void) sprintf(buffer,"    background color = '%.1024s'",symbol);
    else
      (void) sprintf(buffer,"    color('%.1024s',%.1024s) = '%.1024s'",
        name,Intensity(*p) < ((MaxRGB+1)/2) ? "background" : "foreground",
        symbol);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"%.1024s",(i == (int) (colors-1) ? ");\n" : ",\n"));
    (void) WriteBlob(image,strlen(buffer),buffer);
  }
  /*
    Define UIL pixels.
  */
  (void) sprintf(buffer,
    "  %.1024s_icon : icon(color_table = %.1024s_ct,\n",
    BaseFilename(image->filename),BaseFilename(image->filename));
  (void) WriteBlob(image,strlen(buffer),buffer);
  p=image->pixels;
  runlength=p->length+1;
  for (y=0; y < (int) image->rows; y++)
  {
    (void) strcpy(buffer,"    \"");
    (void) WriteBlob(image,strlen(buffer),buffer);
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      k=p->index % MaxCixels;
      symbol[0]=Cixel[k];
      for (j=1; j < (int) characters_per_pixel; j++)
      {
        k=(((int) p->index-k)/MaxCixels) % MaxCixels;
        symbol[j]=Cixel[k];
      }
      symbol[j]='\0';
      (void) sprintf(buffer,"%.1024s",symbol);
      (void) WriteBlob(image,strlen(buffer),buffer);
    }
    (void) sprintf(buffer,"\"%.1024s\n",
      (y == (int) (image->rows-1) ? ");" : ","));
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (QuantumTick(y,image->rows))
      ProgressMonitor(SaveImageText,y,image->rows);
  }
  CloseBlob(image);
  return(True);
}
