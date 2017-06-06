/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%          DDDD   EEEEE   CCCC   OOO   RRRR    AAA   TTTTT  EEEEE             %
%          D   D  E      C      O   O  R   R  A   A    T    E                 %
%          D   D  EEE    C      O   O  RRRR   AAAAA    T    EEE               %
%          D   D  E      C      O   O  R R    A   A    T    E                 %
%          DDDD   EEEEE   CCCC   OOO   R  R   A   A    T    EEEEE             %
%                                                                             %
%                                                                             %
%                   ImageMagick Image Decoration Methods                      %
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
%   B o r d e r I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method BorderImage takes an image and puts a border around it of a
%  particular color.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the BorderImage method is:
%
%      Image *BorderImage(const Image *image,const RectangleInfo *border_info)
%
%  A description of each parameter follows:
%
%    o bordered_image: Method BorderImage returns a pointer to the bordered
%      image.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o border_info: Specifies a pointer to a structure of type Rectangle which
%      defines the border region.
%
*/
Export Image *BorderImage(const Image *image,const RectangleInfo *border_info)
{
  ColorPacket
    matte_color;

  Image
    *bordered_image;

  FrameInfo
    frame_info;

  assert(image != (Image *) NULL);
  assert(border_info != (RectangleInfo *) NULL);
  frame_info.width=image->columns+(border_info->width << 1);
  frame_info.height=image->rows+(border_info->height << 1);
  frame_info.x=border_info->width;
  frame_info.y=border_info->height;
  frame_info.inner_bevel=0;
  frame_info.outer_bevel=0;
  matte_color=image->matte_color;
  ((Image *) image)->matte_color=image->border_color;
  bordered_image=FrameImage(image,&frame_info);
  bordered_image->matte_color=matte_color;
  ((Image *) image)->matte_color=matte_color;
  return(bordered_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   F r a m e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method FrameImage takes an image and puts a frame around it of a
%  particular color.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the FrameImage method is:
%
%      Image *FrameImage(const Image *image,const FrameInfo *frame_info)
%
%  A description of each parameter follows:
%
%    o framed_image: Method FrameImage returns a pointer to the framed
%      image.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o frame_info: Specifies a pointer to a FrameInfo structure which
%      defines the framed region.
%
%
*/
Export Image *FrameImage(const Image *image,const FrameInfo *frame_info)
{
#define FrameImageText  "  Adding frame to image...  "

  Image
    *framed_image;

  int
    height,
    width,
    y;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q;

  RunlengthPacket
    accentuate,
    highlight,
    matte,
    shadow,
    trough;

  unsigned int
    bevel_width;

  /*
    Check frame geometry.
  */
  assert(image != (Image *) NULL);
  assert(frame_info != (FrameInfo *) NULL);
  if ((frame_info->outer_bevel < 0) || (frame_info->inner_bevel < 0))
    {
      MagickWarning(OptionWarning,"Unable to frame image",
        "bevel width is negative");
      return((Image *) NULL);
    }
  bevel_width=frame_info->outer_bevel+frame_info->inner_bevel;
  width=(int) frame_info->width-frame_info->x-bevel_width;
  height=(int) frame_info->height-frame_info->y-bevel_width;
  if ((width < (int) image->columns) || (height < (int) image->rows))
    {
      MagickWarning(OptionWarning,"Unable to frame image",
        "frame is less than image size");
      return((Image *) NULL);
    }
  /*
    Initialize framed image attributes.
  */
  framed_image=CloneImage(image,frame_info->width,frame_info->height,False);
  if (framed_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to frame image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  framed_image->class=DirectClass;
  /*
    Initialize 3D effects color.
  */
  matte.red=image->matte_color.red;
  matte.green=image->matte_color.green;
  matte.blue=image->matte_color.blue;
  matte.index=image->matte_color.index;
  matte.length=0;
  accentuate.red=(Quantum) ((unsigned long)
    (matte.red*AccentuateModulate+(MaxRGB-AccentuateModulate)*MaxRGB)/MaxRGB);
  accentuate.green=(Quantum) ((unsigned long)
    (matte.green*AccentuateModulate+(MaxRGB-AccentuateModulate)*MaxRGB)/MaxRGB);
  accentuate.blue=(Quantum) ((unsigned long)
    (matte.blue*AccentuateModulate+(MaxRGB-AccentuateModulate)*MaxRGB)/MaxRGB);
  accentuate.index=(unsigned short) ((unsigned long)
    (matte.index*AccentuateModulate+(MaxRGB-AccentuateModulate)*MaxRGB)/MaxRGB);
  accentuate.length=0;
  highlight.red=(Quantum) ((unsigned long)
    (matte.red*HighlightModulate+(MaxRGB-HighlightModulate)*MaxRGB)/MaxRGB);
  highlight.green=(Quantum) ((unsigned long)
    (matte.green*HighlightModulate+(MaxRGB-HighlightModulate)*MaxRGB)/MaxRGB);
  highlight.blue=(Quantum) ((unsigned long)
    (matte.blue*HighlightModulate+(MaxRGB-HighlightModulate)*MaxRGB)/MaxRGB);
  highlight.index=(unsigned short) ((unsigned long)
    (matte.index*HighlightModulate+(MaxRGB-HighlightModulate)*MaxRGB)/MaxRGB);
  highlight.length=0;
  shadow.red=(Quantum) ((unsigned long) (matte.red*ShadowModulate)/MaxRGB);
  shadow.green=(Quantum) ((unsigned long) (matte.green*ShadowModulate)/MaxRGB);
  shadow.blue=(Quantum) ((unsigned long) (matte.blue*ShadowModulate)/MaxRGB);
  shadow.index=(unsigned short)
    ((unsigned long) (matte.index*ShadowModulate)/MaxRGB);
  shadow.length=0;
  trough.red=(Quantum) ((unsigned long) (matte.red*TroughModulate)/MaxRGB);
  trough.green=(Quantum) ((unsigned long) (matte.green*TroughModulate)/MaxRGB);
  trough.blue=(Quantum) ((unsigned long) (matte.blue*TroughModulate)/MaxRGB);
  trough.index=(unsigned short)
    ((unsigned long) (matte.index*TroughModulate)/MaxRGB);
  trough.length=0;
  /*
    Put an ornamental border around the image.
  */
  q=framed_image->pixels;
  for (y=0; y < frame_info->outer_bevel; y++)
  {
    for (x=0; x < (int) (framed_image->columns-y); x++)
      if (x < y)
        *q++=highlight;
      else
        *q++=accentuate;
    for ( ; x < (int) framed_image->columns; x++)
      *q++=shadow;
  }
  for (y=0; y < (int) (frame_info->y-bevel_width); y++)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (framed_image->columns-2*frame_info->outer_bevel); x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  for (y=0; y < frame_info->inner_bevel; y++)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (frame_info->x-bevel_width); x++)
      *q++=matte;
    for (x=0; x < (int) (image->columns+(frame_info->inner_bevel << 1)-y); x++)
      if (x < y)
        *q++=shadow;
      else
        *q++=trough;
    for ( ; x < (int) (image->columns+(frame_info->inner_bevel << 1)); x++)
      *q++=highlight;
    width=frame_info->width-frame_info->x-image->columns-bevel_width;
    for (x=0; x < width; x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  p=image->pixels;
  runlength=p->length+1;
  for (y=0; y < (int) image->rows; y++)
  {
    /*
      Initialize scanline with border color.
    */
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (frame_info->x-bevel_width); x++)
      *q++=matte;
    for (x=0; x < frame_info->inner_bevel; x++)
      *q++=shadow;
    /*
      Transfer scanline.
    */
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
    for (x=0; x < frame_info->inner_bevel; x++)
      *q++=highlight;
    width=frame_info->width-frame_info->x-image->columns-bevel_width;
    for (x=0; x < width; x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
    if (QuantumTick(y,image->rows))
      ProgressMonitor(FrameImageText,y,image->rows);
  }
  for (y=frame_info->inner_bevel-1; y >= 0; y--)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (frame_info->x-bevel_width); x++)
      *q++=matte;
    for (x=0; x < y; x++)
      *q++=shadow;
    for ( ; x < (int) (image->columns+(frame_info->inner_bevel << 1)); x++)
      if (x >= (int) (image->columns+(frame_info->inner_bevel << 1)-y))
        *q++=highlight;
      else
        *q++=accentuate;
    width=frame_info->width-frame_info->x-image->columns-bevel_width;
    for (x=0; x < (int) width; x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  height=frame_info->height-frame_info->y-image->rows-bevel_width;
  for (y=0; y < height; y++)
  {
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=highlight;
    for (x=0; x < (int) (framed_image->columns-2*frame_info->outer_bevel); x++)
      *q++=matte;
    for (x=0; x < frame_info->outer_bevel; x++)
      *q++=shadow;
  }
  for (y=frame_info->outer_bevel-1; y >= 0; y--)
  {
    for (x=0; x < y; x++)
      *q++=highlight;
    for ( ; x < (int) framed_image->columns; x++)
      if (x >= (int) (framed_image->columns-y))
        *q++=shadow;
      else
        *q++=trough;
  }
  return(framed_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R a i s e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method RaiseImage lightens and darkens the edges of an image to give a
%  3-D raised or lower effect.
%
%  The format of the RaiseImage method is:
%
%      void RaiseImage(Image *image,const RectangleInfo *raise_info,
%        const int raised)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o raise_info: Specifies a pointer to a XRectangle which defines the
%      raised region.
%
%    o raised: A value other than zero causes the image to have a 3-D raised
%      effect, otherwise it has a lowered effect.
%
%
*/
Export void RaiseImage(Image *image,const RectangleInfo *raise_info,
  const int raised)
{
#define AccentuateFactor  UpScale(135)
#define HighlightFactor  UpScale(190)
#define ShadowFactor  UpScale(190)
#define RaiseImageText  "  Raising image...  "
#define TroughFactor  UpScale(135)

  Quantum
    foreground,
    background;

  register int
    x,
    y;

  register RunlengthPacket
    *p;

  unsigned int
    height;

  assert(image != (Image *) NULL);
  assert(raise_info != (RectangleInfo *) NULL);
  if ((image->columns <= (raise_info->width << 1)) ||
      (image->rows <= (raise_info->height << 1)))
    {
      MagickWarning(OptionWarning,"Unable to raise image",
        "image size must exceed bevel width");
      return;
    }
  if (!UncondenseImage(image))
    return;
  foreground=MaxRGB;
  background=0;
  if (!raised)
    {
      foreground=0;
      background=MaxRGB;
    }
  image->class=DirectClass;
  p=image->pixels;
  for (y=0; y < (int) raise_info->height; y++)
  {
    for (x=0; x < y; x++)
    {
      p->red=(Quantum) ((unsigned long)
        (p->red*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long)
        (p->green*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
        (p->blue*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p++;
    }
    for (x=0; x < (int) (image->columns-(y << 1)); x++)
    {
      p->red=(Quantum) ((unsigned long)
        (p->red*AccentuateFactor+foreground*(MaxRGB-AccentuateFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long) (p->green*
        AccentuateFactor+foreground*(MaxRGB-AccentuateFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
        (p->blue*AccentuateFactor+foreground*(MaxRGB-AccentuateFactor))/MaxRGB);
      p++;
    }
    for (x=0; x < y; x++)
    {
      p->red=(Quantum) ((unsigned long)
        (p->red*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long)
        (p->green*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
        (p->blue*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p++;
    }
  }
  height=image->rows-(raise_info->height << 1);
  for (y=0; y < (int) height; y++)
  {
    for (x=0; x < (int) raise_info->width; x++)
    {
      p->red=(Quantum) ((unsigned long)
	(p->red*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long)
	(p->green*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
	(p->blue*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p++;
    }
    for (x=0; x < (int) (image->columns-(raise_info->width << 1)); x++)
      p++;
    for (x=0; x < (int) raise_info->width; x++)
    {
      p->red=(Quantum) ((unsigned long)
        (p->red*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long)
        (p->green*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
        (p->blue*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p++;
    }
    if (QuantumTick(y,height))
      ProgressMonitor(RaiseImageText,y,height);
  }
  for (y=0; y < (int) raise_info->height; y++)
  {
    for (x=0; x < (int) (raise_info->width-y); x++)
    {
      p->red=(Quantum) ((unsigned long)
	(p->red*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long)
	(p->green*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
	(p->blue*HighlightFactor+foreground*(MaxRGB-HighlightFactor))/MaxRGB);
      p++;
    }
    for (x=0; x < (int) (image->columns-((raise_info->width-y) << 1)); x++)
    {
      p->red=(Quantum) ((unsigned long)
        (p->red*TroughFactor+background*(MaxRGB-TroughFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long)
        (p->green*TroughFactor+background*(MaxRGB-TroughFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
        (p->blue*TroughFactor+background*(MaxRGB-TroughFactor))/MaxRGB);
      p++;
    }
    for (x=0; x < (int) (raise_info->width-y); x++)
    {
      p->red=(Quantum) ((unsigned long)
        (p->red*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p->green=(Quantum) ((unsigned long)
        (p->green*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p->blue=(Quantum) ((unsigned long)
        (p->blue*ShadowFactor+background*(MaxRGB-ShadowFactor))/MaxRGB);
      p++;
    }
  }
  return;
}
