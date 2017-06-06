/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                      SSSSS  H   H  EEEEE   AAA    RRRR                      %
%                      SS     H   H  E      A   A   R   R                     %
%                       SSS   HHHHH  EEE    AAAAA   RRRR                      %
%                         SS  H   H  E      A   A   R R                       %
%                      SSSSS  H   H  EEEEE  A   A   R  R                      %
%                                                                             %
%                                                                             %
%            Methods to Shear or Rotate an Image by an Arbitrary Angle        %
%                                                                             %
%                                                                             %
%                               Software Design                               %
%                                 John Cristy                                 %
%                                  July 1992                                  %
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
%  Method RotateImage, XShearImage, and YShearImage is based on the paper
%  "A Fast Algorithm for General Raster Rotatation" by Alan W. Paeth,
%  Graphics Interface '86 (Vancouver).  RotateImage is adapted from a similar
%  method based on the Paeth paper written by Michael Halle of the Spatial
%  Imaging Group, MIT Media Lab.
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
+   C r o p S h e a r I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CropShearImage crops the sheared image as determined by the
%  bounding box as defined by width and height and shearing angles.
%
%  The format of the CropShearImage method is:
%
%      Image *RotateImage(const Image *image,const double degrees,
%        const unsigned int crop,const unsigned int sharpen)
%
%  A description of each parameter follows.
%
%    o image: The address of a structure of type Image.
%
%    o x_shear, y_shear, width, height: Defines a region of the image to crop.
%
%    o crop: A value other than zero crops the corners of the rotated
%      image and retains the original image size.
%
%
*/
static void CropShearImage(Image **image,const double x_shear,
  const double y_shear,const unsigned int width,const unsigned int height,
  const unsigned int crop)
{
  typedef struct Point
  {
    double
      x,
      y;
  } Point;

  char
    geometry[MaxTextExtent];

  double
    x_max,
    x_min,
    y_max,
    y_min;

  Point
    corners[4];

  RectangleInfo
    crop_info;

  register int
    i;

  /*
    Calculate the rotated image size.
  */
  crop_info.width=width;
  crop_info.height=height;
  corners[0].x=(-((int) crop_info.width)/2.0);
  corners[0].y=(-((int) crop_info.height)/2.0);
  corners[1].x=((int) crop_info.width)/2.0;
  corners[1].y=(-((int) crop_info.height)/2.0);
  corners[2].x=(-((int) crop_info.width)/2.0);
  corners[2].y=((int) crop_info.height)/2.0;
  corners[3].x=((int) crop_info.width)/2.0;
  corners[3].y=((int) crop_info.height)/2.0;
  for (i=0; i < 4; i++)
  {
    corners[i].x+=x_shear*corners[i].y;
    corners[i].y+=y_shear*corners[i].x;
    corners[i].x+=x_shear*corners[i].y;
    corners[i].x+=((*image)->columns-1)/2.0;
    corners[i].y+=((*image)->rows-3)/2.0;
  }
  x_min=corners[0].x;
  y_min=corners[0].y;
  x_max=corners[0].x;
  y_max=corners[0].y;
  for (i=1; i < 4; i++)
  {
    if (x_min > corners[i].x)
      x_min=corners[i].x;
    if (y_min > corners[i].y)
      y_min=corners[i].y;
    if (x_max < corners[i].x)
      x_max=corners[i].x;
    if (y_max < corners[i].y)
      y_max=corners[i].y;
  }
  x_min=floor((double) x_min);
  x_max=ceil((double) x_max);
  y_min=floor((double) y_min);
  y_max=ceil((double) y_max);
  if (!crop)
    {
      /*
        Do not crop sheared image.
      */
      crop_info.width=(unsigned int) (x_max-x_min)-1;
      crop_info.height=(unsigned int) (y_max-y_min)-1;
    }
  crop_info.x=(int) x_min+(((int) (x_max-x_min)-crop_info.width) >> 1)+1;
  crop_info.y=(int) y_min+(((int) (y_max-y_min)-crop_info.height) >> 1)+2;
  /*
    Crop image and return.
  */
  FormatString(geometry,"%ux%u%+d%+d",crop_info.width,crop_info.height,
    crop_info.x,crop_info.y);
  TransformImage(image,geometry,(char *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   I n t e g r a l R o t a t e I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method IntegralRotateImage rotates the image an integral of 90 degrees.
%  It allocates the memory necessary for the new Image structure and returns
%  a pointer to the rotated image.
%
%  The format of the IntegralRotateImage method is:
%
%      rotated_image=IntegralRotateImage(image,rotations)
%
%  A description of each parameter follows.
%
%    o rotated_image: Method IntegralRotateImage returns a pointer to the
%      rotated image.  A null image is returned if there is a a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o rotations: Specifies the number of 90 degree rotations.
%
%
*/
static Image *IntegralRotateImage(const Image *image,unsigned int rotations)
{
#define RotateImageText  "  Rotating image...  "

  Image
    *rotated_image;

  int
    y;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q;

  /*
    Initialize rotated image attributes.
  */
  rotations%=4;
  if ((rotations == 1) || (rotations == 3))
    rotated_image=CloneImage(image,image->rows,image->columns,False);
  else
    rotated_image=CloneImage(image,image->columns,image->rows,False);
  if (rotated_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to rotate image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Expand runlength packets into a rectangular array of pixels.
  */
  p=image->pixels;
  runlength=p->length+1;
  switch (rotations)
  {
    case 0:
    {
      /*
        Rotate 0 degrees.
      */
      q=rotated_image->pixels;
      for (y=0; y < (int) image->rows; y++)
      {
        for (x=0; x < (int) image->columns; x++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          *q=(*p);
          q->length=0;
          q++;
        }
        if (QuantumTick(y,image->rows))
          ProgressMonitor(RotateImageText,y,image->rows);
      }
      break;
    }
    case 1:
    {
      /*
        Rotate 90 degrees.
      */
      for (x=0; x < (int) rotated_image->columns; x++)
      {
        q=rotated_image->pixels+(rotated_image->columns-x)-1;
        for (y=0; y < (int) rotated_image->rows; y++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          *q=(*p);
          q->length=0;
          q+=rotated_image->columns;
        }
        if (QuantumTick(x,rotated_image->columns))
          ProgressMonitor(RotateImageText,x,rotated_image->columns);
      }
      break;
    }
    case 2:
    {
      /*
        Rotate 180 degrees.
      */
      q=rotated_image->pixels+(rotated_image->columns*rotated_image->rows)-1;
      for (y=image->rows-1; y >= 0; y--)
      {
        for (x=0; x < (int) image->columns; x++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          *q=(*p);
          q->length=0;
          q--;
        }
        if (QuantumTick(y,image->rows))
          ProgressMonitor(RotateImageText,image->rows-y,image->rows);
      }
      break;
    }
    case 3:
    {
      /*
        Rotate 270 degrees.
      */
      for (x=rotated_image->columns-1; x >= 0; x--)
      {
        q=rotated_image->pixels+(rotated_image->columns*rotated_image->rows)-
          x-1;
        for (y=0; y < (int) rotated_image->rows; y++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          *q=(*p);
          q->length=0;
          q-=rotated_image->columns;
        }
        if (QuantumTick(x,rotated_image->columns))
          ProgressMonitor(RotateImageText,rotated_image->columns-x,
            rotated_image->columns);
      }
      break;
    }
  }
  return(rotated_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X S h e a r I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure XShearImage shears the image in the X direction with a shear angle
%  of 'degrees'.  Positive angles shear counter-clockwise (right-hand rule),
%  and negative angles shear clockwise.  Angles are measured relative to a
%  vertical Y-axis.  X shears will widen an image creating 'empty' triangles
%  on the left and right sides of the source image.
%
%  The format of the XShearImage method is:
%
%      XShearImage(image,degrees,width,height,x_offset,y_offset,range_limit)
%
%  A description of each parameter follows.
%
%    o image: The address of a structure of type Image.
%
%    o degrees: A double representing the shearing angle along the X axis.
%
%    o width, height, x_offset, y_offset: Defines a region of the image
%      to shear.
%
*/
static void XShearImage(Image *image,const double degrees,
  const unsigned int width,const unsigned int height,const int x_offset,
  int y_offset,register Quantum *range_limit)
{
#define XShearImageText  "  X Shear image...  "

  double
    displacement;

  enum {LEFT,RIGHT}
    direction;

  int
    step,
    y;

  long
    blue,
    fractional_step,
    green,
    index,
    red;

  register RunlengthPacket
    *p,
    *q;

  register int
    i;

  RunlengthPacket
    last_pixel;

  y_offset--;
  for (y=0; y < (int) height; y++)
  {
    y_offset++;
    displacement=degrees*(((double) y)-(height-1)/2.0);
    if (displacement == 0.0)
      continue;
    if (displacement > 0.0)
      direction=RIGHT;
    else
      {
        displacement*=(-1.0);
        direction=LEFT;
      }
    step=(int) floor(displacement);
    fractional_step=UpShifted(displacement-(double) step);
    if (fractional_step == 0)
      {
        /*
          No fractional displacement-- just copy.
        */
        switch (direction)
        {
          case LEFT:
          {
            /*
              Transfer pixels left-to-right.
            */
            p=image->pixels+image->columns*y_offset+x_offset;
            q=p-step;
            for (i=0; i < (int) width; i++)
            {
              *q=(*p);
              q++;
              p++;
            }
            /*
              Set old row to background color.
            */
            for (i=0; i < (int) step; i++)
            {
              q->red=image->background_color.red;
              q->green=image->background_color.green;
              q->blue=image->background_color.blue;
              q->index=image->background_color.index;
              q++;
            }
            break;
          }
          case RIGHT:
          {
            /*
              Transfer pixels right-to-left.
            */
            p=image->pixels+image->columns*y_offset+x_offset+width;
            q=p+step;
            for (i=0; i < (int) width; i++)
            {
              p--;
              q--;
              *q=(*p);
            }
            /*
              Set old row to background color.
            */
            for (i=0; i < (int) step; i++)
            {
              q--;
              q->red=image->background_color.red;
              q->green=image->background_color.green;
              q->blue=image->background_color.blue;
              q->index=image->background_color.index;
            }
            break;
          }
        }
        continue;
      }
    /*
      Fractional displacement.
    */
    step++;
    last_pixel.red=image->background_color.red;
    last_pixel.green=image->background_color.green;
    last_pixel.blue=image->background_color.blue;
    last_pixel.index=image->background_color.index;
    switch (direction)
    {
      case LEFT:
      {
        /*
          Transfer pixels left-to-right.
        */
        p=image->pixels+image->columns*y_offset+x_offset;
        q=p-step;
        for (i=0; i < (int) width; i++)
        {
          red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+p->red*
            fractional_step);
          green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
            p->green*fractional_step);
          blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+p->blue*
            fractional_step);
          index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
            p->index*fractional_step);
          last_pixel=(*p);
          p++;
          q->red=range_limit[red];
          q->green=range_limit[green];
          q->blue=range_limit[blue];
          if (index < 0)
            q->index=0;
          else
            if (index > MaxColormapSize)
              q->index=MaxColormapSize;
            else
              q->index=(unsigned short) index;
          q++;
        }
        /*
          Set old row to background color.
        */
        red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+
          image->background_color.red*fractional_step);
        green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
          image->background_color.green*fractional_step);
        blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+
          image->background_color.blue*fractional_step);
        index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
          image->background_color.index*fractional_step);
        q->red=range_limit[red];
        q->green=range_limit[green];
        q->blue=range_limit[blue];
        if (index < 0)
          q->index=0;
        else
          if (index > MaxColormapSize)
            q->index=MaxColormapSize;
          else
            q->index=(unsigned short) index;
        q++;
        for (i=0; i < step-1; i++)
        {
          q->red=image->background_color.red;
          q->green=image->background_color.green;
          q->blue=image->background_color.blue;
          q->index=image->background_color.index;
          q++;
        }
        break;
      }
      case RIGHT:
      {
        /*
          Transfer pixels right-to-left.
        */
        p=image->pixels+image->columns*y_offset+x_offset+width;
        q=p+step;
        for (i=0; i < (int) width; i++)
        {
          p--;
          red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+p->red*
            fractional_step);
          green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
            p->green*fractional_step);
          blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+p->blue*
            fractional_step);
          index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
            p->index*fractional_step);
          last_pixel=(*p);
          q--;
          q->red=range_limit[red];
          q->green=range_limit[green];
          q->blue=range_limit[blue];
          if (index < 0)
            q->index=0;
          else
            if (index > MaxColormapSize)
              q->index=MaxColormapSize;
            else
              q->index=(unsigned short) index;
        }
        /*
          Set old row to background color.
        */
        red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+
          image->background_color.red*fractional_step);
        green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
          image->background_color.green*fractional_step);
        blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+
          image->background_color.blue*fractional_step);
        index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
          image->background_color.index*fractional_step);
        q--;
        q->red=range_limit[red];
        q->green=range_limit[green];
        q->blue=range_limit[blue];
        if (index < 0)
          q->index=0;
        else
          if (index > MaxColormapSize)
            q->index=MaxColormapSize;
          else
            q->index=(unsigned short) index;
        for (i=0; i < step-1; i++)
        {
          q--;
          q->red=image->background_color.red;
          q->green=image->background_color.green;
          q->blue=image->background_color.blue;
          q->index=image->background_color.index;
        }
        break;
      }
    }
    if (QuantumTick(y,height))
      ProgressMonitor(XShearImageText,y,height);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   Y S h e a r I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure YShearImage shears the image in the Y direction with a shear
%  angle of 'degrees'.  Positive angles shear counter-clockwise (right-hand
%  rule), and negative angles shear clockwise.  Angles are measured relative
%  to a horizontal X-axis.  Y shears will increase the height of an image
%  creating 'empty' triangles on the top and bottom of the source image.
%
%  The format of the YShearImage method is:
%
%      YShearImage(image,degrees,width,height,x_offset,y_offset,range_limit)
%
%  A description of each parameter follows.
%
%    o image: The address of a structure of type Image.
%
%    o degrees: A double representing the shearing angle along the Y axis.
%
%    o width, height, x_offset, y_offset: Defines a region of the image
%      to shear.
%
%
*/
static void YShearImage(Image *image,const double degrees,
  const unsigned int width,const unsigned int height,int x_offset,
  const int y_offset,register Quantum *range_limit)
{
#define YShearImageText  "  Y Shear image...  "

  double
    displacement;

  enum {UP,DOWN}
    direction;

  int
    step,
    y;

  long
    blue,
    fractional_step,
    green,
    index,
    red;

  register RunlengthPacket
    *p,
    *q;

  register int
    i;

  RunlengthPacket
    last_pixel;

  x_offset--;
  for (y=0; y < (int) width; y++)
  {
    x_offset++;
    displacement=degrees*(((double) y)-(width-1)/2.0);
    if (displacement == 0.0)
      continue;
    if (displacement > 0.0)
      direction=DOWN;
    else
      {
        displacement*=(-1.0);
        direction=UP;
      }
    step=(int) floor(displacement);
    fractional_step=UpShifted(displacement-(double) step);
    if (fractional_step == 0)
      {
        /*
          No fractional displacement-- just copy the pixels.
        */
        switch (direction)
        {
          case UP:
          {
            /*
              Transfer pixels top-to-bottom.
            */
            p=image->pixels+image->columns*y_offset+x_offset;
            q=p-step*image->columns;
            for (i=0; i < (int) height; i++)
            {
              *q=(*p);
              q+=image->columns;
              p+=image->columns;
            }
            /*
              Set old column to background color.
            */
            for (i=0; i < (int) step; i++)
            {
              q->red=image->background_color.red;
              q->green=image->background_color.green;
              q->blue=image->background_color.blue;
              q->index=image->background_color.index;
              q+=image->columns;
            }
            break;
          }
          case DOWN:
          {
            /*
              Transfer pixels bottom-to-top.
            */
            p=image->pixels+image->columns*(y_offset+height)+x_offset;
            q=p+step*image->columns;
            for (i=0; i < (int) height; i++)
            {
              q-=image->columns;
              p-=image->columns;
              *q=(*p);
            }
            /*
              Set old column to background color.
            */
            for (i=0; i < (int) step; i++)
            {
              q-=image->columns;
              q->red=image->background_color.red;
              q->green=image->background_color.green;
              q->blue=image->background_color.blue;
              q->index=image->background_color.index;
            }
            break;
          }
        }
        continue;
      }
    /*
      Fractional displacment.
    */
    step++;
    last_pixel.red=image->background_color.red;
    last_pixel.green=image->background_color.green;
    last_pixel.blue=image->background_color.blue;
    last_pixel.index=image->background_color.index;
    switch (direction)
    {
      case UP:
      {
        /*
          Transfer pixels top-to-bottom.
        */
        p=image->pixels+image->columns*y_offset+x_offset;
        q=p-step*image->columns;
        for (i=0; i < (int) height; i++)
        {
          red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+p->red*
            fractional_step);
          green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
            p->green*fractional_step);
          blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+p->blue*
            fractional_step);
          index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
            p->index*fractional_step);
          last_pixel=(*p);
          p+=image->columns;
          q->red=range_limit[red];
          q->green=range_limit[green];
          q->blue=range_limit[blue];
          if (index < 0)
            q->index=0;
          else
            if (index > MaxColormapSize)
              q->index=MaxColormapSize;
            else
              q->index=(unsigned short) index;
          q+=image->columns;
        }
        /*
          Set old column to background color.
        */
        red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+
          image->background_color.red*fractional_step);
        green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
          image->background_color.green*fractional_step);
        blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+
          image->background_color.blue*fractional_step);
        index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
          image->background_color.index*fractional_step);
        q->red=range_limit[red];
        q->green=range_limit[green];
        q->blue=range_limit[blue];
        if (index < 0)
          q->index=0;
        else
          if (index > MaxColormapSize)
            q->index=MaxColormapSize;
          else
            q->index=(unsigned short) index;
        q+=image->columns;
        for (i=0; i < step-1; i++)
        {
          q->red=image->background_color.red;
          q->green=image->background_color.green;
          q->blue=image->background_color.blue;
          q->index=image->background_color.index;
          q+=image->columns;
        }
        break;
      }
      case DOWN:
      {
        /*
          Transfer pixels bottom-to-top.
        */
        p=image->pixels+image->columns*(y_offset+height)+x_offset;
        q=p+step*image->columns;
        for (i=0; i < (int) height; i++)
        {
          p-=image->columns;
          red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+p->red*
            fractional_step);
          green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
            p->green*fractional_step);
          blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+p->blue*
            fractional_step);
          index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
            p->index*fractional_step);
          last_pixel=(*p);
          q-=image->columns;
          q->red=range_limit[red];
          q->green=range_limit[green];
          q->blue=range_limit[blue];
          if (index < 0)
            q->index=0;
          else
            if (index > MaxColormapSize)
              q->index=MaxColormapSize;
            else
              q->index=(unsigned short) index;
        }
        /*
          Set old column to background color.
        */
        red=DownShift(last_pixel.red*(UpShift(1)-fractional_step)+
          image->background_color.red*fractional_step);
        green=DownShift(last_pixel.green*(UpShift(1)-fractional_step)+
          image->background_color.green*fractional_step);
        blue=DownShift(last_pixel.blue*(UpShift(1)-fractional_step)+
          image->background_color.blue*fractional_step);
        index=DownShift(last_pixel.index*(UpShift(1)-fractional_step)+
          image->background_color.index*fractional_step);
        q-=image->columns;
        q->red=range_limit[red];
        q->green=range_limit[green];
        q->blue=range_limit[blue];
        if (index < 0)
          q->index=0;
        else
          if (index > MaxColormapSize)
            q->index=MaxColormapSize;
          else
            q->index=(unsigned short) index;
        for (i=0; i < step-1; i++)
        {
          q-=image->columns;
          q->red=image->background_color.red;
          q->green=image->background_color.green;
          q->blue=image->background_color.blue;
          q->index=image->background_color.index;
        }
        break;
      }
    }
    if (QuantumTick(y,width))
      ProgressMonitor(YShearImageText,y,width);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R o t a t e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method RotateImage creates a new image that is a rotated copy of an
%  existing one.  Positive angles rotate counter-clockwise (right-hand rule),
%  while negative angles rotate clockwise.  Rotated images are usually larger
%  than the originals and have 'empty' triangular corners.  X axis.  Empty
%  triangles left over from shearing the image are filled with the color
%  defined by the pixel at location (0,0).  RotateImage allocates the memory
%  necessary for the new Image structure and returns a pointer to the new
%  image.
%
%  Method RotateImage is based on the paper "A Fast Algorithm for General
%  Raster Rotatation" by Alan W. Paeth.  RotateImage is adapted from a similar
%  method based on the Paeth paper written by Michael Halle of the Spatial
%  Imaging Group, MIT Media Lab.
%
%  The format of the RotateImage method is:
%
%      RotateImage(image,degrees,crop,sharpen)
%
%  A description of each parameter follows.
%
%    o status: Method RotateImage returns a pointer to the image after
%      rotating.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o degrees: Specifies the number of degrees to rotate the image.
%
%    o crop: A value other than zero crops the corners of the rotated
%      image and retains the original image size.
%
%    o sharpen: A value other than zero sharpens the image after it is
%      rotated.
%
%
*/
Export Image *RotateImage(const Image *image,const double degrees,
  const unsigned int crop,const unsigned int sharpen)
{
  double
    angle;

  Image
    *integral_image,
    *rotated_image,
    *sharpened_image;

  int
    x_offset,
    y_offset;

  PointInfo
    shear;

  Quantum
    *range_limit,
    *range_table;

  RectangleInfo
    border_info;

  register int
    i;

  unsigned int
    height,
    rotations,
    width,
    y_width;

  /*
    Adjust rotation angle.
  */
  assert(image != (Image *) NULL);
  angle=degrees;
  while (angle < -45.0)
    angle+=360.0;
  for (rotations=0; angle > 45.0; rotations++)
    angle-=90.0;
  rotations%=4;
  /*
    Calculate shear equations.
  */
  shear.x=(-tan(DegreesToRadians(angle)/2.0));
  shear.y=sin(DegreesToRadians(angle));
  integral_image=IntegralRotateImage(image,rotations);
  if ((shear.x == 0.0) || (shear.y == 0.0))
    return(integral_image);
  /*
    Initialize range table.
  */
  range_table=(Quantum *) AllocateMemory(3*(MaxRGB+1)*sizeof(Quantum));
  if (range_table == (Quantum *) NULL)
    {
      DestroyImage(integral_image);
      MagickWarning(ResourceLimitWarning,"Unable to rotate image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  range_limit=range_table+(MaxRGB+1);
  /*
    Compute image size.
  */
  width=image->columns;
  height=image->rows;
  if ((rotations == 1) || (rotations == 3))
    {
      width=image->rows;
      height=image->columns;
    }
  y_width=width+(int) ceil(fabs(shear.x)*(double) (height-1));
  x_offset=(width+
    ((int) ceil(fabs(shear.y)*(double) (height-1)) << 1)-width) >> 1;
  y_offset=(height+(int) ceil(fabs(shear.y)*(double) (y_width-1))-height) >> 1;
  /*
    Surround image with border of background color.
  */
  border_info.width=x_offset;
  border_info.height=y_offset+1;
  if (integral_image->matte)
    {
      integral_image->border_color.index=Transparent;
      integral_image->background_color.index=Transparent;
    }
  rotated_image=BorderImage(integral_image,&border_info);
  DestroyImage(integral_image);
  if (rotated_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to rotate image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  rotated_image->class=DirectClass;
  /*
    Perform a fractional rotation.  First, shear the image rows.
  */
  XShearImage(rotated_image,shear.x,width,height,x_offset,
    ((int) (rotated_image->rows-height) >> 1),range_limit);
  /*
    Shear the image columns.
  */
  YShearImage(rotated_image,shear.y,y_width,height,
    ((int) (rotated_image->columns-y_width) >> 1),y_offset+1,range_limit);
  /*
    Shear the image rows again.
  */
  XShearImage(rotated_image,shear.x,y_width,rotated_image->rows-2,
    ((int) (rotated_image->columns-y_width) >> 1),1,range_limit);
  FreeMemory((char *) range_table);
  /*
    Crop image.
  */
  CropShearImage(&rotated_image,shear.x,shear.y,width,height,crop);
  if (sharpen)
    {
      /*
        Sharpen image.
      */
      sharpened_image=SharpenImage(rotated_image,60.0);
      if (sharpened_image != (Image *) NULL)
        {
          DestroyImage(rotated_image);
          rotated_image=sharpened_image;
        }
    }
  return(rotated_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S h e a r I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ShearImage creates a new image that is a sheared copy of an
%  existing one.  Shearing slides one edge of an image along the X or Y
%  axis, creating a parallelogram.  An X direction shear slides an edge
%  along the X axis, while a Y direction shear slides an edge along the Y
%  axis.  The amount of the shear is controlled by a shear angle.  For X
%  direction shears, x_shear is measured relative to the Y axis, and
%  similarly, for Y direction shears y_shear is measured relative to the
%  X axis.  Empty triangles left over from shearing the image are filled
%  with the color defined by the pixel at location (0,0).  ShearImage
%  allocates the memory necessary for the new Image structure and returns
%  a pointer to the new image.
%
%  Method ShearImage is based on the paper "A Fast Algorithm for General
%  Raster Rotatation" by Alan W. Paeth.
%
%  The format of the ShearImage method is:
%
%      Image *ShearImage(const Image *image,const double x_shear,
%        const double y_shear,const unsigned int crop)
%
%  A description of each parameter follows.
%
%    o status: Method ShearImage returns a pointer to the image after
%      rotating.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o x_shear, y_shear: Specifies the number of degrees to shear the image.
%
%    o crop: A value other than zero crops the corners of the rotated
%      image and retains the original image size.
%
%
*/
Export Image *ShearImage(const Image *image,const double x_shear,
  const double y_shear,const unsigned int crop)
{
  Image
    *sharpened_image,
    *sheared_image;

  int
    x_offset,
    y_offset;

  PointInfo
    shear;

  Quantum
    *range_limit,
    *range_table;

  RectangleInfo
    border_info;

  register int
    i;

  unsigned int
    y_width;

  assert(image != (Image *) NULL);
  if ((x_shear == 180.0) || (y_shear == 180.0))
    {
      MagickWarning(OptionWarning,"Unable to shear image",
        "angle is discontinuous");
      return((Image *) NULL);
    }
  /*
    Initialize shear angle.
  */
  shear.x=(-tan(DegreesToRadians(x_shear)/2.0));
  shear.y=sin(DegreesToRadians(y_shear));
  /*
    Initialize range table.
  */
  range_table=(Quantum *) AllocateMemory(3*(MaxRGB+1)*sizeof(Quantum));
  if (range_table == (Quantum *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to shear image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  range_limit=range_table+(MaxRGB+1);
  /*
    Compute image size.
  */
  y_width=image->columns+(int) ceil(fabs(shear.x)*(double) (image->rows-1));
  x_offset=(image->columns+((int) ceil(fabs(shear.x)*(double)
    (image->rows-1)) << 1)-image->columns) >> 1;
  y_offset=(image->rows+(int) ceil(fabs(shear.y)*(double) (y_width-1))-
    image->rows) >> 1;
  /*
    Surround image with border of background color.
  */
  border_info.width=x_offset;
  border_info.height=y_offset+1;
  if (image->matte)
    {
      ((Image *) image)->border_color.index=Transparent;
      ((Image *) image)->background_color.index=Transparent;
    }
  sheared_image=BorderImage(image,&border_info);
  if (sheared_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to shear image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  sheared_image->class=DirectClass;
  /*
    Shear the image rows.
  */
  XShearImage(sheared_image,shear.x,image->columns,image->rows,x_offset,
    ((int) (sheared_image->rows-image->rows) >> 1),range_limit);
  /*
    Shear the image columns.
  */
  YShearImage(sheared_image,shear.y,y_width,image->rows,
    ((int) (sheared_image->columns-y_width) >> 1),y_offset+1,range_limit);
  FreeMemory((char *) range_table);
  /*
    Crop image.
  */
  CropShearImage(&sheared_image,shear.x,shear.y,image->columns,image->rows,
    crop);
  /*
    Sharpen image.
  */
  sharpened_image=SharpenImage(sheared_image,60.0);
  if (sharpened_image != (Image *) NULL)
    {
      DestroyImage(sheared_image);
      sheared_image=sharpened_image;
    }
  return(sheared_image);
}
