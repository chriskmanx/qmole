/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                EEEEE  FFFFF  FFFFF  EEEEE  CCCC  TTTTT  SSSSS               %
%                E      F      F      E     C        T    SS                  %
%                EEE    FFF    FFF    EEE   C        T     SSS                %
%                E      F      F      E     C        T       SS               %
%                EEEEE  F      F      EEEEE  CCCC    T    SSSSS               %
%                                                                             %
%                                                                             %
%                      ImageMagick Image Effects Methods                      %
%                                                                             %
%                                                                             %
%                               Software Design                               %
%                                 John Cristy                                 %
%                                 October 1996                                %
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
%     A d d N o i s e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method AddNoiseImage creates a new image that is a copy of an existing
%  one with noise added.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the AddNoiseImage method is:
%
%      Image *AddNoiseImage(const Image *image,const NoiseType noise_type)
%
%  A description of each parameter follows:
%
%    o noisy_image: Method AddNoiseImage returns a pointer to the image after
%      the noise is minified.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o noise_type:  The type of noise: Gaussian, multiplicative Gaussian,
%      impulse, laplacian, or Poisson.
%
%
*/
Export Image *AddNoiseImage(const Image *image,const NoiseType noise_type)
{
#define AddNoiseImageText  "  Adding noise to the image...  "

  Image
    *noisy_image;

  int
    y;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q;

  /*
    Initialize noisy image attributes.
  */
  assert(image != (Image *) NULL);
  noisy_image=CloneImage(image,image->columns,image->rows,False);
  if (noisy_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reduce noise",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  noisy_image->class=DirectClass;
  /*
    Add noise in each row.
  */
  p=image->pixels;
  runlength=p->length+1;
  q=noisy_image->pixels;
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
      q->red=GenerateNoise(p->red,noise_type);
      q->green=GenerateNoise(p->green,noise_type);
      q->blue=GenerateNoise(p->blue,noise_type);
      q->length=0;
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(AddNoiseImageText,y,image->rows);
  }
  return(noisy_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     B l u r I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method BlurImage creates a new image that is a copy of an existing
%  one with the pixels blurred.  It allocates the memory necessary for the
%  new Image structure and returns a pointer to the new image.
%
%  BlurImage convolves the pixel neighborhood with this blurring mask:
%
%     1  2  1
%     2  W  2
%     1  2  1
%
%  The scan only processes pixels that have a full set of neighbors.  Pixels
%  in the top, bottom, left, and right pairs of rows and columns are omitted
%  from the scan.
%
%  The format of the BlurImage method is:
%
%      Image *BlurImage(const Image *image,const double factor)
%
%  A description of each parameter follows:
%
%    o blurred_image: Method BlurImage returns a pointer to the image
%      after it is blurred.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o factor:  An double value reflecting the percent weight to give to the
%      center pixel of the neighborhood.
%
%
*/
Export Image *BlurImage(const Image *image,const double factor)
{
#define Blur(weight) \
  total_red+=(weight)*(int) (s->red); \
  total_green+=(weight)*(int) (s->green); \
  total_blue+=(weight)*(int) (s->blue); \
  s++;
#define BlurImageText  "  Blurring image...  "

  Image
    *blurred_image;

  int
    y;

  register int
    runlength,
    x;

  long
    total_blue,
    total_green,
    total_red,
    weight;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2;

  RunlengthPacket
    *scanline;

  unsigned int
    quantum;

  assert(image != (Image *) NULL);
  if ((image->columns < 3) || (image->rows < 3))
    return((Image *) NULL);
  /*
    Initialize blurred image attributes.
  */
  blurred_image=CloneImage(image,image->columns,image->rows,False);
  if (blurred_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to blur image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  blurred_image->class=DirectClass;
  /*
    Allocate scan line buffer for 3 rows of the image.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(3*image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to blur image",
        "Memory allocation failed");
      DestroyImage(blurred_image);
      return((Image *) NULL);
    }
  /*
    Read the first two rows of the image.
  */
  p=image->pixels;
  runlength=p->length+1;
  s=scanline;
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Dump first scanline of pixels.
  */
  q=blurred_image->pixels;
  s1=scanline;
  for (x=0; x < (int) image->columns; x++)
  {
    *q=(*s1++);
    q->length=0;
    q++;
  }
  /*
    Blur each row.
  */
  weight=(long) ((100.0-factor)/2);
  quantum=(unsigned int) Max(weight+12,1);
  for (y=1; y < (int) (image->rows-1); y++)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y-1) % 3);
    s1=scanline+image->columns*(y % 3);
    s2=scanline+image->columns*((y+1) % 3);
    /*
      Read another scan line.
    */
    s=s2;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Blur this row of pixels.
    */
    *q=(*s1);
    q->length=0;
    q++;
    for (x=1; x < (int) (image->columns-1); x++)
    {
      /*
        Compute weighted average of target pixel color components.
      */
      total_red=0;
      total_green=0;
      total_blue=0;
      s=s0;
      Blur(1);  Blur(2); Blur(1);
      s=s1;
      Blur(2); Blur(weight); Blur(2);
      s=s2;
      Blur(1);  Blur(2); Blur(1);
      q->red=(Quantum) ((total_red+(quantum >> 1))/quantum);
      q->green=(Quantum) ((total_green+(quantum >> 1))/quantum);
      q->blue=(Quantum) ((total_blue+(quantum >> 1))/quantum);
      q->index=s1->index;
      q->length=0;
      q++;
      s0++;
      s1++;
      s2++;
    }
    /*
      Transfer last pixel of the scanline.
    */
    *q=(*s1);
    q->length=0;
    q++;
    if (QuantumTick(y,image->rows))
      ProgressMonitor(BlurImageText,y,image->rows);
  }
  /*
    Dump last scanline of pixels.
  */
  s1=scanline+image->columns*(y % 3);
  for (x=0; x < (int) image->columns; x++)
  {
    *q=(*s1++);
    q->length=0;
    q++;
  }
  FreeMemory((char *) scanline);
  return(blurred_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     C o l o r i z e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ColorizeImage colorizes an image with the pen color.  The amount
%  of the coloring is controlled with the opacity levels.
%
%  The format of the ColorizeImage method is:
%
%      void ColorizeImage(Image *image,const char *opacity,
%        const char *pen_color)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o opaque_color,
%      pen_color: A character string that contain an X11 color string.
%
%
*/
Export void ColorizeImage(Image *image,const char *opacity,
  const char *pen_color)
{
#define ColorizeImageText  "  Colorizing the image...  "

  ColorPacket
    target,
    target_color;

  int
    blue_opacity,
    count,
    green_opacity,
    red_opacity;

  register int
    i;

  register RunlengthPacket
    *p;

  unsigned int
    status;

  /*
    Determine RGB values of the pen color.
  */
  assert(image != (Image *) NULL);
  if (opacity == (char *) NULL)
    return;
  status=QueryColorDatabase(pen_color,&target_color);
  if (status == False)
    return;
  target.red=XDownScale(target_color.red);
  target.green=XDownScale(target_color.green);
  target.blue=XDownScale(target_color.blue);
  red_opacity=100;
  green_opacity=100;
  blue_opacity=100;
  count=sscanf(opacity,"%d/%d/%d",&red_opacity,&green_opacity,&blue_opacity);
  if (count == 1)
    {
      if (red_opacity == 0)
        return;
      green_opacity=red_opacity;
      blue_opacity=red_opacity;
    }
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Colorize DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        p->red=(Quantum) ((unsigned long)
          (p->red*(100-red_opacity)+target.red*red_opacity)/100);
        p->green=(Quantum) ((unsigned long)
          (p->green*(100-green_opacity)+target.green*green_opacity)/100);
        p->blue=(Quantum) ((unsigned long)
          (p->blue*(100-blue_opacity)+target.blue*blue_opacity)/100);
        p++;
        if (QuantumTick(i,image->packets))
          ProgressMonitor(ColorizeImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Colorize PseudoClass image.
      */
      for (i=0; i < (int) image->colors; i++)
      {
        image->colormap[i].red=(Quantum) ((unsigned long)
          (image->colormap[i].red*(100-red_opacity)+target.red*
          red_opacity)/100);
        image->colormap[i].green=(Quantum) ((unsigned long)
          (image->colormap[i].green*(100-green_opacity)+target.green*
          green_opacity)/100);
        image->colormap[i].blue=(Quantum) ((unsigned long)
          (image->colormap[i].blue*(100-blue_opacity)+target.blue*
          blue_opacity)/100);
      }
      SyncImage(image);
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     D e s p e c k l e I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DespeckleImage creates a new image that is a copy of an existing
%  one with the speckle noise minified.  It uses the eight hull algorithm
%  described in Applied Optics, Vol. 24, No. 10, 15 May 1985, "Geometric filter
%  for Speckle Reduction", by Thomas R Crimmins.  Each pixel in the image is
%  replaced by one of its eight of its surrounding pixels using a polarity and
%  negative hull function.  DespeckleImage allocates the memory necessary for
%  the new Image structure and returns a pointer to the new image.
%
%  The format of the DespeckleImage method is:
%
%      Image *DespeckleImage(Image *image)
%
%  A description of each parameter follows:
%
%    o despeckled_image: Method DespeckleImage returns a pointer to the image
%      after it is despeckled.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export Image *DespeckleImage(Image *image)
{
#define DespeckleImageText  "  Despeckling image...  "

  Image
    *despeckled_image;

  int
    x;

  Quantum
    *blue_channel,
    *buffer,
    *green_channel,
    *matte_channel,
    *red_channel;

  register int
    i,
    j;

  register RunlengthPacket
    *p,
    *q;

  static const int
    X[4]= {0, 1, 1,-1},
    Y[4]= {1, 0, 1, 1};

  unsigned int
    packets;

  /*
    Allocate despeckled image.
  */
  assert(image != (Image *) NULL);
  despeckled_image=CloneImage(image,image->columns,image->rows,False);
  if (despeckled_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to despeckle image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  despeckled_image->class=DirectClass;
  /*
    Allocate image buffers.
  */
  packets=(image->columns+2)*(image->rows+2);
  red_channel=(Quantum *) AllocateMemory(packets*sizeof(Quantum));
  green_channel=(Quantum *) AllocateMemory(packets*sizeof(Quantum));
  blue_channel=(Quantum *) AllocateMemory(packets*sizeof(Quantum));
  matte_channel=(Quantum *) AllocateMemory(packets*sizeof(Quantum));
  buffer=(Quantum *) AllocateMemory(packets*sizeof(Quantum));
  if ((red_channel == (Quantum *) NULL) ||
      (green_channel == (Quantum *) NULL) ||
      (blue_channel == (Quantum *) NULL) ||
      (matte_channel == (Quantum *) NULL) ||
      (buffer == (Quantum *) NULL) || !UncondenseImage(image))
    {
      MagickWarning(ResourceLimitWarning,"Unable to despeckle image",
        "Memory allocation failed");
      DestroyImage(despeckled_image);
      return((Image *) NULL);
    }
  /*
    Zero image buffers.
  */
  for (i=0; i < (int) packets; i++)
  {
    red_channel[i]=0;
    green_channel[i]=0;
    blue_channel[i]=0;
    matte_channel[i]=0;
    buffer[i]=0;
  }
  /*
    Copy image pixels to color component buffers
  */
  x=image->columns+2;
  p=image->pixels;
  for (j=0; j < (int) image->rows; j++)
  {
    x++;
    for (i=0; i < (int) image->columns; i++)
    {
      red_channel[x]=p->red;
      green_channel[x]=p->green;
      blue_channel[x]=p->blue;
      matte_channel[x]=p->index;
      x++;
      p++;
    }
    x++;
  }
  /*
    Reduce speckle in red channel.
  */
  for (i=0; i < 4; i++)
  {
    ProgressMonitor(DespeckleImageText,i,12);
    Hull(X[i],Y[i],1,image->columns,image->rows,red_channel,buffer);
    Hull(-X[i],-Y[i],1,image->columns,image->rows,red_channel,buffer);
    Hull(-X[i],-Y[i],-1,image->columns,image->rows,red_channel,buffer);
    Hull(X[i],Y[i],-1,image->columns,image->rows,red_channel,buffer);
  }
  /*
    Reduce speckle in green channel.
  */
  for (i=0; i < (int) packets; i++)
    buffer[i]=0;
  for (i=0; i < 4; i++)
  {
    ProgressMonitor(DespeckleImageText,i+4,12);
    Hull(X[i],Y[i],1,image->columns,image->rows,green_channel,buffer);
    Hull(-X[i],-Y[i],1,image->columns,image->rows,green_channel,buffer);
    Hull(-X[i],-Y[i],-1,image->columns,image->rows,green_channel,buffer);
    Hull(X[i],Y[i],-1,image->columns,image->rows,green_channel,buffer);
  }
  /*
    Reduce speckle in blue channel.
  */
  for (i=0; i < (int) packets; i++)
    buffer[i]=0;
  for (i=0; i < 4; i++)
  {
    ProgressMonitor(DespeckleImageText,i+8,12);
    Hull(X[i],Y[i],1,image->columns,image->rows,blue_channel,buffer);
    Hull(-X[i],-Y[i],1,image->columns,image->rows,blue_channel,buffer);
    Hull(-X[i],-Y[i],-1,image->columns,image->rows,blue_channel,buffer);
    Hull(X[i],Y[i],-1,image->columns,image->rows,blue_channel,buffer);
  }
  /*
    Copy color component buffers to despeckled image.
  */
  x=image->columns+2;
  q=despeckled_image->pixels;
  for (j=0; j < (int) image->rows; j++)
  {
    x++;
    for (i=0; i < (int) image->columns; i++)
    {
      q->red=red_channel[x];
      q->green=green_channel[x];
      q->blue=blue_channel[x];
      q->index=matte_channel[x];
      q->length=0;
      q++;
      x++;
    }
    x++;
  }
  /*
    Free memory.
  */
  FreeMemory((char *) buffer);
  FreeMemory((char *) matte_channel);
  FreeMemory((char *) blue_channel);
  FreeMemory((char *) green_channel);
  FreeMemory((char *) red_channel);
  return(despeckled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     E d g e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method EdgeImage creates a new image that is a copy of an existing
%  one with the edges highlighted.  It allocates the memory necessary for the
%  new Image structure and returns a pointer to the new image.
%
%  EdgeImage convolves the pixel neighborhood with this edge detection mask:
%
%    -1 -1 -1
%    -1  W -1
%    -1 -1 -1
%
%  The scan only processes pixels that have a full set of neighbors.  Pixels
%  in the top, bottom, left, and right pairs of rows and columns are omitted
%  from the scan.
%
%  The format of the EdgeImage method is:
%
%      Image *EdgeImage(const Image *image,const double factor)
%
%  A description of each parameter follows:
%
%    o edged_image: Method EdgeImage returns a pointer to the image
%      after it is edged.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o factor:  An double value reflecting the percent weight to give to the
%      center pixel of the neighborhood.
%
%
*/
Export Image *EdgeImage(const Image *image,const double factor)
{
#define Edge(weight) \
  total_red+=(long) ((weight)*(int) (s->red)); \
  total_green+=(long) ((weight)*(int) (s->green)); \
  total_blue+=(long) ((weight)*(int) (s->blue)); \
  total_index+=(long) ((weight)*(int) (s->index)); \
  s++;
#define EdgeImageText  "  Detecting image edges...  "

  double
    weight;

  Image
    *edged_image;

  int
    y;

  register int
    runlength,
    x;

  long
    total_blue,
    total_green,
    total_index,
    total_red;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2;

  RunlengthPacket
    *scanline;

  assert(image != (Image *) NULL);
  if ((image->columns < 3) || (image->rows < 3))
    return((Image *) NULL);
  /*
    Initialize edged image attributes.
  */
  edged_image=CloneImage(image,image->columns,image->rows,False);
  if (edged_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to detect edges",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  edged_image->class=DirectClass;
  /*
    Allocate scan line buffer for 3 rows of the image.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(3*image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to detect edges",
        "Memory allocation failed");
      DestroyImage(edged_image);
      return((Image *) NULL);
    }
  /*
    Read the first two rows of the image.
  */
  p=image->pixels;
  runlength=p->length+1;
  s=scanline;
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Dump first scanline of pixels.
  */
  q=edged_image->pixels;
  for (x=0; x < (int) image->columns; x++)
  {
    q->red=0;
    q->green=0;
    q->blue=0;
    q->index=0;
    q->length=0;
    q++;
  }
  /*
    Edge detect each row.
  */
  weight=factor/8.0+0.5;
  for (y=1; y < (int) (image->rows-1); y++)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y-1) % 3);
    s1=scanline+image->columns*(y % 3);
    s2=scanline+image->columns*((y+1) % 3);
    /*
      Read another scan line.
    */
    s=s2;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Edge detect this row of pixels.
    */
    *q=(*(q-1));
    q++;
    for (x=1; x < (int) (image->columns-1); x++)
    {
      /*
        Compute weighted average of target pixel color components.
      */
      total_red=0;
      total_green=0;
      total_blue=0;
      total_index=0;
      s=s1+1;
      s=s0;
      Edge(-weight/8); Edge(-weight/8) Edge(-weight/8);
      s=s1;
      Edge(-weight/8); Edge(weight); Edge(-weight/8);
      s=s2;
      Edge(-weight/8); Edge(-weight/8); Edge(-weight/8);
      q->red=(Quantum)
        ((total_red < 0) ? 0 : (total_red > MaxRGB) ? MaxRGB : total_red);
      q->green=(Quantum)
        ((total_green < 0) ? 0 : (total_green > MaxRGB) ? MaxRGB : total_green);
      q->blue=(Quantum)
        ((total_blue < 0) ? 0 : (total_blue > MaxRGB) ? MaxRGB : total_blue);
      q->index=(unsigned short) ((total_index < Transparent) ? Transparent :
        (total_index > Opaque) ? Opaque : total_index);
      q->length=0;
      q++;
      s0++;
      s1++;
      s2++;
    }
    *q=(*(q-1));
    q++;
    if (QuantumTick(y,image->rows))
      ProgressMonitor(EdgeImageText,y,image->rows-1);
  }
  /*
    Dump last scanline of pixels.
  */
  for (x=0; x < (int) image->columns; x++)
  {
    q->red=0;
    q->green=0;
    q->blue=0;
    q->index=0;
    q->length=0;
    q->length=0;
    q++;
  }
  FreeMemory((char *) scanline);
  /*
    Normalize image.
  */
  NormalizeImage(edged_image);
  return(edged_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     E m b o s s I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method EmbossImage creates a new image that is a copy of an existing
%  one with the edge highlighted.  It allocates the memory necessary for the
%  new Image structure and returns a pointer to the new image.
%
%  EmbossImage convolves the pixel neighborhood with this edge detection mask:
%
%    -1 -2  0
%    -2  0  2
%     0  2  1
%
%  The scan only processes pixels that have a full set of neighbors.  Pixels
%  in the top, bottom, left, and right pairs of rows and columns are omitted
%  from the scan.
%
%  The format of the EmbossImage method is:
%
%      Image *EmbossImage(const Image *image)
%
%  A description of each parameter follows:
%
%    o embossed_image: Method EmbossImage returns a pointer to the image
%      after it is embossed.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export Image *EmbossImage(const Image *image)
{
#define EmbossImageText  "  Embossing image...  "
#define Emboss(weight) \
  total_red+=(weight)*(int) (s->red); \
  total_green+=(weight)*(int) (s->green); \
  total_blue+=(weight)*(int) (s->blue); \
  s++;

  Image
    *embossed_image;

  int
    y;

  long
    total_blue,
    total_green,
    total_red;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2;

  RunlengthPacket
    *scanline;

  assert(image != (Image *) NULL);
  if ((image->columns < 3) || (image->rows < 3))
    return((Image *) NULL);
  /*
    Initialize embossed image attributes.
  */
  embossed_image=CloneImage(image,image->columns,image->rows,False);
  if (embossed_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to enhance image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  embossed_image->class=DirectClass;
  /*
    Allocate scan line buffer for 3 rows of the image.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(3*image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to enhance image",
        "Memory allocation failed");
      DestroyImage(embossed_image);
      return((Image *) NULL);
    }
  /*
    Read the first two rows of the image.
  */
  p=image->pixels;
  runlength=p->length+1;
  s=scanline;
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Dump first scanline of pixels.
  */
  q=embossed_image->pixels;
  for (x=0; x < (int) image->columns; x++)
  {
    q->red=0;
    q->green=0;
    q->blue=0;
    q->index=0;
    q->length=0;
    q++;
  }
  /*
    Emboss each row.
  */
  for (y=1; y < (int) (image->rows-1); y++)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y-1) % 3);
    s1=scanline+image->columns*(y % 3);
    s2=scanline+image->columns*((y+1) % 3);
    /*
      Read another scan line.
    */
    s=s2;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Emboss this row of pixels.
    */
    *q=(*(q-1));
    q++;
    for (x=1; x < (int) (image->columns-1); x++)
    {
      /*
        Compute weighted average of target pixel color components.
      */
      total_red=0;
      total_green=0;
      total_blue=0;
      s=s1+1;
      s=s0;
      Emboss(-1); Emboss(-2); Emboss( 0);
      s=s1;
      Emboss(-2); Emboss( 0); Emboss( 2);
      s=s2;
      Emboss( 0); Emboss( 2); Emboss( 1);
      total_red+=(MaxRGB+1) >> 1;
      q->red=(Quantum)
        ((total_red < 0) ? 0 : (total_red > MaxRGB) ? MaxRGB : total_red);
      total_green+=(MaxRGB+1) >> 1;
      q->green=(Quantum)
        ((total_green < 0) ? 0 : (total_green > MaxRGB) ? MaxRGB : total_green);
      total_blue+=(MaxRGB+1) >> 1;
      q->blue=(Quantum)
        ((total_blue < 0) ? 0 : (total_blue > MaxRGB) ? MaxRGB : total_blue);
      q->index=s1->index;
      q->length=0;
      q++;
      s0++;
      s1++;
      s2++;
    }
    *q=(*(q-1));
    q++;
    if (QuantumTick(y,image->rows))
      ProgressMonitor(EmbossImageText,y,image->rows-1);
  }
  /*
    Dump last scanline of pixels.
  */
  for (x=0; x < (int) image->columns; x++)
  {
    q->red=0;
    q->green=0;
    q->blue=0;
    q->index=0;
    q->length=0;
    q++;
  }
  FreeMemory((char *) scanline);
  /*
    Convert image to grayscale and normalize.
  */
  embossed_image->class=DirectClass;
  (void) IsGrayImage(embossed_image);
  NormalizeImage(embossed_image);
  return(embossed_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     E n h a n c e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method EnhanceImage creates a new image that is a copy of an existing
%  one with the noise minified.  It allocates the memory necessary for the new
%  Image structure and returns a pointer to the new image.
%
%  EnhanceImage does a weighted average of pixels in a 5x5 cell around each
%  target pixel.  Only pixels in the 5x5 cell that are within a RGB distance
%  threshold of the target pixel are averaged.
%
%  Weights assume that the importance of neighboring pixels is negately
%  proportional to the square of their distance from the target pixel.
%
%  The scan only processes pixels that have a full set of neighbors.  Pixels
%  in the top, bottom, left, and right pairs of rows and columns are omitted
%  from the scan.
%
%  The format of the EnhanceImage method is:
%
%      Image *EnhanceImage(const Image *image)
%
%  A description of each parameter follows:
%
%    o enhanced_image: Method EnhanceImage returns a pointer to the image
%      after it is enhanced.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export Image *EnhanceImage(const Image *image)
{
#define Enhance(weight) \
  mean=(int) (s->red+red)/2; \
  distance=s->red-(int) red; \
  distance_squared= \
    (2.0*(MaxRGB+1)+mean)*squares[distance]/(double) (MaxRGB+1); \
  distance=s->green-(int) green; \
  distance_squared+=4.0*squares[distance]; \
  distance=s->blue-(int) blue; \
  distance_squared+= \
    (3.0*(MaxRGB+1)-1.0-mean)*squares[distance]/(double) (MaxRGB+1); \
  if (distance_squared < Threshold) \
    { \
      total_red+=(weight)*(s->red); \
      total_green+=(weight)*(s->green); \
      total_blue+=(weight)*(s->blue); \
      total_weight+=(weight); \
    } \
  s++;
#define EnhanceImageText  "  Enhancing image...  "
#define Threshold  2500

  double
    distance_squared;

  Image
    *enhanced_image;

  int
    distance,
    y;

  long
    mean;

  Quantum
    blue,
    green,
    red;

  register int
    i,
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2,
    *s3,
    *s4;

  register unsigned int
    *squares;

  RunlengthPacket
    *scanline;

  unsigned long
    total_blue,
    total_green,
    total_red,
    total_weight;

  assert(image != (Image *) NULL);
  if ((image->columns < 5) || (image->rows < 5))
    return((Image *) NULL);
  /*
    Initialize enhanced image attributes.
  */
  enhanced_image=CloneImage(image,image->columns,image->rows,False);
  if (enhanced_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to enhance image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  enhanced_image->class=DirectClass;
  /*
    Allocate scan line buffer for 5 rows of the image.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(5*image->columns*sizeof(RunlengthPacket));
  squares=(unsigned int *)
    AllocateMemory((MaxRGB+MaxRGB+1)*sizeof(unsigned int));
  if ((scanline == (RunlengthPacket *) NULL) ||
      (squares == (unsigned int *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Unable to enhance image",
        "Memory allocation failed");
      DestroyImage(enhanced_image);
      return((Image *) NULL);
    }
  squares+=MaxRGB;
  for (i=(-MaxRGB); i <= MaxRGB; i++)
    squares[i]=i*i;
  /*
    Read the first 4 rows of the image.
  */
  p=image->pixels;
  runlength=p->length+1;
  s=scanline;
  for (x=0; x < (int) (image->columns*4); x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Dump first 2 scanlines of image.
  */
  q=enhanced_image->pixels;
  s=scanline;
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    *q=(*s);
    q->length=0;
    q++;
    s++;
  }
  /*
    Enhance each row.
  */
  for (y=2; y < (int) (image->rows-2); y++)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y-2) % 5);
    s1=scanline+image->columns*((y-1) % 5);
    s2=scanline+image->columns*(y % 5);
    s3=scanline+image->columns*((y+1) % 5);
    s4=scanline+image->columns*((y+2) % 5);
    /*
      Read another scan line.
    */
    s=s4;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Transfer first 2 pixels of the scanline.
    */
    s=s2;
    for (x=0; x < 2; x++)
    {
      *q=(*s);
      q->length=0;
      q++;
      s++;
    }
    for (x=2; x < (int) (image->columns-2); x++)
    {
      /*
        Compute weighted average of target pixel color components.
      */
      total_red=0;
      total_green=0;
      total_blue=0;
      total_weight=0;
      s=s2+2;
      red=s->red;
      green=s->green;
      blue=s->blue;
      s=s0;
      Enhance(5);  Enhance(8);  Enhance(10); Enhance(8);  Enhance(5);
      s=s1;
      Enhance(8);  Enhance(20); Enhance(40); Enhance(20); Enhance(8);
      s=s2;
      Enhance(10); Enhance(40); Enhance(80); Enhance(40); Enhance(10);
      s=s3;
      Enhance(8);  Enhance(20); Enhance(40); Enhance(20); Enhance(8);
      s=s4;
      Enhance(5);  Enhance(8);  Enhance(10); Enhance(8);  Enhance(5);
      q->red=(Quantum) ((total_red+(total_weight >> 1)-1)/total_weight);
      q->green= (Quantum) ((total_green+(total_weight >> 1)-1)/total_weight);
      q->blue=(Quantum) ((total_blue+(total_weight >> 1)-1)/total_weight);
      q->index=s2->index;
      q->length=0;
      q++;
      s0++;
      s1++;
      s2++;
      s3++;
      s4++;
    }
    /*
      Transfer last 2 pixels of the scanline.
    */
    s=s2;
    for (x=0; x < 2; x++)
    {
      *q=(*s);
      q->length=0;
      q++;
      s++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(EnhanceImageText,y,image->rows-2);
  }
  /*
    Dump last 2 scanlines of pixels.
  */
  s=scanline+image->columns*(y % 5);
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    *q=(*s);
    q->length=0;
    q++;
    s++;
  }
  squares-=MaxRGB;
  FreeMemory((char *) squares);
  FreeMemory((char *) scanline);
  return(enhanced_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     I m p l o d e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ImplodeImage creates a new image that is a copy of an existing
%  one with the image pixels "imploded" by the specified percentage.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ImplodeImage method is:
%
%      Image *ImplodeImage(Image *image,const double factor)
%
%  A description of each parameter follows:
%
%    o imploded_image: Method ImplodeImage returns a pointer to the image
%      after it is imploded.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o factor:  A double value that defines the extent of the implosion.
%
%
*/
Export Image *ImplodeImage(Image *image,const double factor)
{
#define ImplodeImageText  "  Imploding image...  "

  ColorPacket
    interpolated_color;

  double
    amount,
    distance,
    radius,
    x_center,
    x_distance,
    x_scale,
    y_center,
    y_distance,
    y_scale,
    y_squared;

  Image
    *imploded_image;

  register RunlengthPacket
    *p,
    *q;

  register unsigned int
    x,
    y;

  assert(image != (Image *) NULL);
  if (!UncondenseImage(image))
    return((Image *) NULL);
  image->background_color.index=Transparent;
  /*
    Initialize imploded image attributes.
  */
  if (!image->matte)
    MatteImage(image);
  imploded_image=CloneImage(image,image->columns,image->rows,False);
  if (imploded_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to implode image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Compute scaling factor.
  */
  x_scale=1.0;
  y_scale=1.0;
  x_center=(double) image->columns/2.0;
  y_center=(double) image->rows/2.0;
  radius=x_center;
  if (image->columns > image->rows)
    y_scale=image->columns/image->rows;
  else
    if (image->columns < image->rows)
      {
        x_scale=image->rows/image->columns;
        radius=y_center;
      }
  amount=factor/10.0;
  if (amount >= 0)
    amount/=10.0;
  /*
    Implode each row.
  */
  p=image->pixels;
  q=imploded_image->pixels;
  for (y=0; y < image->rows; y++)
  {
    y_distance=y_scale*((double) y-y_center);
    y_squared=y_distance*y_distance;
    for (x=0; x < image->columns; x++)
    {
      /*
        Determine if the pixel is within an ellipse.
      */
      x_distance=x_scale*((double) x-x_center);
      distance=x_distance*x_distance+y_squared;
      if (distance >= radius*radius)
        *q=(*p);
      else
        {
          double
            factor;

          /*
            Implode the pixel.
          */
          factor=1.0;
          if (distance > 0.0)
            factor=pow(sin(0.5*M_PI*sqrt(distance)/radius),-amount);
          interpolated_color=InterpolateColor(image,
            factor*x_distance/x_scale+x_center,
            factor*y_distance/y_scale+y_center);
          q->red=interpolated_color.red;
          q->green=interpolated_color.green;
          q->blue=interpolated_color.blue;
          q->index=interpolated_color.index;
          q->length=0;
        }
      p++;
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(ImplodeImageText,y,image->rows);
  }
  return(imploded_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     M o r p h I m a g e s                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MorphImages morphs a set of images.  Both the image pixels and size
%  are linearly interpolated to give the appearance of a meta-morphosis from
%  one image to the next.
%
%  The format of the MorphImage method is:
%
%      Image *MorphImages(Image *images,const unsigned int number_frames)
%
%  A description of each parameter follows:
%
%    o morphed_image: Method MorphImages returns an image sequence that
%      has linearly interpolated pixels and size between two input images.
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o number_frames:  This unsigned integer reflects the number of in-between
%      images to generate.  The more in-between frames, the smoother
%      the morph.
%
%
*/
Export Image *MorphImages(Image *images,const unsigned int number_frames)
{
#define MorphImageText  "  Morphing image sequence...  "

  double
    alpha,
    beta;

  Image
    *image,
    *morphed_image,
    *morphed_images;

  MonitorHandler
    handler;

  register int
    i,
    j;

  register RunlengthPacket
    *p,
    *q;

  unsigned int
    scene;

  assert(images != (Image *) NULL);
  if (images->next == (Image *) NULL)
    {
      MagickWarning(OptionWarning,"Unable to morph image",
        "image sequence required");
      return((Image *) NULL);
    }
  /*
    Clone first frame in sequence.
  */
  ((Image *) images)->orphan=True;
  morphed_images=CloneImage(images,images->columns,images->rows,True);
  ((Image *) images)->orphan=False;
  if (morphed_images == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to morph image sequence",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Morph image.
  */
  scene=0;
  for (image=images; image->next != (Image *) NULL; image=image->next)
  {
    handler=SetMonitorHandler((MonitorHandler) NULL);
    for (i=0; i < (int) number_frames; i++)
    {
      beta=(double) (i+1.0)/(number_frames+1.0);
      alpha=1.0-beta;
      ((Image *) image)->orphan=True;
      morphed_images->next=ZoomImage(image,
        (unsigned int) (alpha*image->columns+beta*image->next->columns+0.5),
        (unsigned int) (alpha*image->rows+beta*image->next->rows+0.5));
      ((Image *) image)->orphan=False;
      if (morphed_images->next == (Image *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to morph image sequence",
            "Memory allocation failed");
          break;
        }
      morphed_images->next->previous=morphed_images;
      morphed_images=morphed_images->next;
      image->next->orphan=True;
      morphed_image=ZoomImage(image->next,morphed_images->columns,
        morphed_images->rows);
      image->next->orphan=False;
      if (morphed_image == (Image *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to morph image sequence",
            "Memory allocation failed");
          break;
        }
      if (!UncondenseImage(morphed_images) || !UncondenseImage(morphed_image))
        {
          MagickWarning(ResourceLimitWarning,"Unable to morph image sequence",
            "Memory allocation failed");
          break;
        }
      morphed_images->class=DirectClass;
      p=morphed_image->pixels;
      q=morphed_images->pixels;
      for (j=0; j < (int) morphed_images->packets; j++)
      {
        q->red=(Quantum) (alpha*q->red+beta*p->red+0.5);
        q->green=(Quantum) (alpha*q->green+beta*p->green+0.5);
        q->blue=(Quantum) (alpha*q->blue+beta*p->blue+0.5);
        q->index=(unsigned short) (alpha*q->index+beta*p->index+0.5);
        p++;
        q++;
      }
      DestroyImage(morphed_image);
      CondenseImage(morphed_images);
    }
    /*
      Clone last frame in sequence.
    */
    image->next->orphan=True;
    morphed_images->next=
      CloneImage(image->next,image->next->columns,image->next->rows,True);
    image->next->orphan=False;
    if (morphed_images->next == (Image *) NULL)
      {
        MagickWarning(ResourceLimitWarning,"Unable to morph image sequence",
          "Memory allocation failed");
        break;
      }
    morphed_images->next->previous=morphed_images;
    morphed_images=morphed_images->next;
    (void) SetMonitorHandler(handler);
    ProgressMonitor(MorphImageText,scene,GetNumberScenes(images));
    scene++;
  }
  while (morphed_images->previous != (Image *) NULL)
    morphed_images=morphed_images->previous;
  if (image->next != (Image *) NULL)
    {
      DestroyImages(morphed_images);
      return((Image *) NULL);
    }
  return(morphed_images);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     O i l P a i n t I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method OilPaintImage creates a new image that is a copy of an existing
%  one with each pixel component replaced with the color of greatest frequency
%  in a circular neighborhood.
%
%  The format of the OilPaintImage method is:
%
%      Image *OilPaintImage(Image *image,const unsigned int radius)
%
%  A description of each parameter follows:
%
%    o painted_image: Method OilPaintImage returns a pointer to the image
%      after it is `painted'.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o radius: An unsigned int that is the radius of the circular
%      neighborhood.
%
%
*/
Export Image *OilPaintImage(Image *image,const unsigned int radius)
{
#define OilPaintImageText  "  Oil painting image...  "

  Image
    *painted_image;

  int
    count,
    k;

  register int
    i,
    j;

  register RunlengthPacket
    *p,
    *q,
    *s;

  register unsigned int
    x;

  unsigned int
    *histogram,
    y;

  assert(image != (Image *) NULL);
  if ((image->columns < (radius << 1)) || (image->rows < (radius << 1)))
    {
      MagickWarning(ResourceLimitWarning,"Unable to oil paint",
        "image smaller than radius");
      return((Image *) NULL);
    }
  if (!UncondenseImage(image))
    return((Image *) NULL);
  /*
    Initialize painted image attributes.
  */
  painted_image=CloneImage(image,image->columns,image->rows,True);
  if (painted_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to oil paint",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate histogram and scanline.
  */
  histogram=(unsigned int *) AllocateMemory((MaxRGB+1)*sizeof(unsigned int));
  if (histogram == (unsigned int *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to oil paint",
        "Memory allocation failed");
      DestroyImage(painted_image);
      return((Image *) NULL);
    }
  /*
    Paint each row of the image.
  */
  k=0;
  p=image->pixels;
  q=painted_image->pixels;
  for (y=0; y < image->rows; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      /*
        Determine most frequent color.
      */
      count=0;
      for (i=0; i < (int) (MaxRGB+1); i++)
        histogram[i]=0;
      for (i=0; i < (int) radius; i++)
      {
        s=p-(radius-i)*image->columns-1-i;
        for (j=0; j < (2*i+1); j++)
        {
          if (s >= image->pixels)
            k=Intensity(*s);
          histogram[k]++;
          if ((int) histogram[k] > count)
            {
              if (s >= image->pixels)
                *q=(*s);
              count=histogram[k];
            }
          s++;
        }
        s=p+(radius-i)*image->columns-1-i;
        for (j=0; j < (2*i+1); j++)
        {
          if (s < (image->pixels+image->packets-1))
            k=Intensity(*s);
          histogram[k]++;
          if ((int) histogram[k] > count)
            {
              if (s < (image->pixels+image->packets-1))
                *q=(*s);
              count=histogram[k];
            }
          s++;
        }
      }
      s=p-radius;
      for (j=0; j < (int) (radius+radius+1); j++)
      {
        if (s >= image->pixels)
          k=Intensity(*s);
        histogram[k]++;
        if ((int) histogram[k] > count)
          {
            if (s >= image->pixels)
              *q=(*s);
            count=histogram[k];
          }
        s++;
      }
      q++;
      p++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(OilPaintImageText,y,image->rows);
  }
  FreeMemory((char *) histogram);
  return(painted_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     P l a s m a I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method PlasmaImage initializes an image with plasma fractal values.  The
%  image must be initialized with a base color and the random number generator
%  seeded before this method is called.
%
%  The format of the PlasmaImage method is:
%
%      unsigned int PlasmaImage(Image *image,const SegmentInfo *segment_info,
%        int attenuate,int depth)
%
%  A description of each parameter follows:
%
%    o status: Method PlasmaImage returns True when the fractal process
%      is complete.  Otherwise False is returned.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o segment_info:  specifies a structure of type SegmentInfo that defines
%      the boundaries of the area where the plasma fractals are applied.
%
%    o attenuate:  specifies the plasma attenuation factor.
%
%    o depth: this integer values define the plasma recursion depth.
%
%
*/

static Quantum PlasmaPixel(const double pixel,const double noise)
{
  double
    value;

  value=pixel+(noise/2.0)-((int) noise ? (rand() % (int) noise) : 0.0);
  if (value < 0.0)
    return(0);
  if (value > MaxRGB)
    return(MaxRGB);
  return((Quantum) (value+0.5));
}

Export unsigned int PlasmaImage(Image *image,const SegmentInfo *segment_info,
  int attenuate,int depth)
{
  double
    plasma,
    x_mid,
    y_mid;

  register RunlengthPacket
    *p,
    *q,
    *r;

  assert(image != (Image *) NULL);
  if (image->packets != (image->columns*image->rows))
    if (!UncondenseImage(image))
      return(True);
  if (depth != 0)
    {
      SegmentInfo
        local_info;

      /*
        Divide the area into quadrants and recurse.
      */
      depth--;
      attenuate++;
      x_mid=(segment_info->x1+segment_info->x2)/2;
      y_mid=(segment_info->y1+segment_info->y2)/2;
      local_info=(*segment_info);
      local_info.x2=x_mid;
      local_info.y2=y_mid;
      (void) PlasmaImage(image,&local_info,attenuate,depth);
      local_info=(*segment_info);
      local_info.y1=y_mid;
      local_info.x2=x_mid;
      (void) PlasmaImage(image,&local_info,attenuate,depth);
      local_info=(*segment_info);
      local_info.x1=x_mid;
      local_info.y2=y_mid;
      (void) PlasmaImage(image,&local_info,attenuate,depth);
      local_info=(*segment_info);
      local_info.x1=x_mid;
      local_info.y1=y_mid;
      return(PlasmaImage(image,&local_info,attenuate,depth));
    }
  x_mid=(segment_info->x1+segment_info->x2)/2;
  y_mid=(segment_info->y1+segment_info->y2)/2;
  if ((segment_info->x1 == x_mid) && (segment_info->x2 == x_mid) &&
      (segment_info->y1 == y_mid) && (segment_info->y2 == y_mid))
    return(False);
  /*
    Average pixels and apply plasma.
  */
  plasma=(MaxRGB+1)/(2.0*(double) attenuate);
  if ((segment_info->x1 != x_mid) || (segment_info->x2 != x_mid))
    {
      /*
        Left pixel.
      */
      p=PixelOffset(image,segment_info->x1,segment_info->y1);
      q=PixelOffset(image,segment_info->x1,segment_info->y2);
      r=PixelOffset(image,segment_info->x1,y_mid);
      r->red=PlasmaPixel((int) (p->red+q->red)/2,plasma);
      r->green=PlasmaPixel((int) (p->green+q->green)/2,plasma);
      r->blue=PlasmaPixel((int) (p->blue+q->blue)/2,plasma);
      if (segment_info->x1 != segment_info->x2)
        {
          /*
            Right pixel.
          */
          p=PixelOffset(image,segment_info->x2,segment_info->y1);
          q=PixelOffset(image,segment_info->x2,segment_info->y2);
          r=PixelOffset(image,segment_info->x2,y_mid);
          r->red=PlasmaPixel((int) (p->red+q->red)/2,plasma);
          r->green=PlasmaPixel((int) (p->green+q->green)/2,plasma);
          r->blue=PlasmaPixel((int) (p->blue+q->blue)/2,plasma);
        }
    }
  if ((segment_info->y1 != y_mid) || (segment_info->y2 != y_mid))
    {
      if ((segment_info->x1 != x_mid) || (segment_info->y2 != y_mid))
        {
          /*
            Bottom pixel.
          */
          p=PixelOffset(image,segment_info->x1,segment_info->y2);
          q=PixelOffset(image,segment_info->x2,segment_info->y2);
          r=PixelOffset(image,x_mid,segment_info->y2);
          r->red=PlasmaPixel((int) (p->red+q->red)/2,plasma);
          r->green=PlasmaPixel((int) (p->green+q->green)/2,plasma);
          r->blue=PlasmaPixel((int) (p->blue+q->blue)/2,plasma);
        }
      if (segment_info->y1 != segment_info->y2)
        {
          /*
            Top pixel.
          */
          p=PixelOffset(image,segment_info->x1,segment_info->y1);
          q=PixelOffset(image,segment_info->x2,segment_info->y1);
          r=PixelOffset(image,x_mid,segment_info->y1);
          r->red=PlasmaPixel((int) (p->red+q->red)/2,plasma);
          r->green=PlasmaPixel((int) (p->green+q->green)/2,plasma);
          r->blue=PlasmaPixel((int) (p->blue+q->blue)/2,plasma);
        }
    }
  if ((segment_info->x1 != segment_info->x2) ||
      (segment_info->y1 != segment_info->y2))
    {
      /*
        Middle pixel.
      */
      p=PixelOffset(image,segment_info->x1,segment_info->y1);
      q=PixelOffset(image,segment_info->x2,segment_info->y2);
      r=PixelOffset(image,x_mid,y_mid);
      r->red=PlasmaPixel((int) (p->red+q->red)/2,plasma);
      r->green=PlasmaPixel((int) (p->green+q->green)/2,plasma);
      r->blue=PlasmaPixel((int) (p->blue+q->blue)/2,plasma);
      p=PixelOffset(image,segment_info->x1,segment_info->y2);
      q=PixelOffset(image,segment_info->x2,segment_info->y1);
      r->red=PlasmaPixel((int) (p->red+q->red)/2,plasma);
      r->green=PlasmaPixel((int) (p->green+q->green)/2,plasma);
      r->blue=PlasmaPixel((int) (p->blue+q->blue)/2,plasma);
    }
  if (((segment_info->x2-segment_info->x1) < 3.0) &&
      ((segment_info->y2-segment_info->y1) < 3.0))
    return(True);
  return(False);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     R e d u c e N o i s e I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReduceNoiseImage creates a new image that is a copy of an existing
%  one with the noise minified with a noise peak elimination filter.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The principal function of noise peak elimination filter is to smooth the
%  objects within an image without losing edge information and without
%  creating undesired structures.  The central idea of the algorithm is to
%  replace a pixel with its next neighbor in value within a 3 x 3 window,
%  if this pixel has been found to be noise.  A pixel is defined as noise
%  if and only if this pixel is a maximum or minimum within the 3 x 3
%  window.
%
%  The format of the ReduceNoiseImage method is:
%
%      Image *ReduceNoiseImage(const Image *image)
%
%  A description of each parameter follows:
%
%    o noisy_image: Method ReduceNoiseImage returns a pointer to the image
%      after the noise is minified.  A null image is returned if there is a
%      memory shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/

static int ReduceNoiseCompare(const void *x,const void *y)
{
  ColorPacket
    *color_1,
    *color_2;

  color_1=(ColorPacket *) x;
  color_2=(ColorPacket *) y;
  return((int) Intensity(*color_1)-(int) Intensity(*color_2));
}

Export Image *ReduceNoiseImage(const Image *image)
{
#define ReduceNoiseImageText  "  Reducing the image noise...  "

  Image
    *noisy_image;

  int
    y;

  register int
    i,
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2;

  RunlengthPacket
    pixel,
    *scanline,
    window[9];

  assert(image != (Image *) NULL);
  if ((image->columns < 3) || (image->rows < 3))
    return((Image *) NULL);
  /*
    Initialize noisy image attributes.
  */
  noisy_image=CloneImage(image,image->columns,image->rows,False);
  if (noisy_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reduce noise",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate scanline buffer for 3 rows of the image.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(3*image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reduce noise",
        "Memory allocation failed");
      DestroyImage(noisy_image);
      return((Image *) NULL);
    }
  /*
    Preload the first 2 rows of the image.
  */
  p=image->pixels;
  runlength=p->length+1;
  s=scanline;
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Dump first scanline of image.
  */
  q=noisy_image->pixels;
  s=scanline;
  for (x=0; x < (int) image->columns; x++)
  {
    *q=(*s);
    q->length=0;
    q++;
    s++;
  }
  /*
    Reduce noise in each row.
  */
  for (y=1; y < (int) (image->rows-1); y++)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y-1) % 3);
    s1=scanline+image->columns*(y % 3);
    s2=scanline+image->columns*((y+1) % 3);
    /*
      Read another scan line.
    */
    s=s2;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Transfer first pixel of the scanline.
    */
    s=s1;
    *q=(*s);
    q->length=0;
    q++;
    for (x=1; x < (int) (image->columns-1); x++)
    {
      /*
        Sort window pixels by increasing intensity.
      */
      s=s0;
      window[0]=(*s++);
      window[1]=(*s++);
      window[2]=(*s++);
      s=s1;
      window[3]=(*s++);
      window[4]=(*s++);
      window[5]=(*s++);
      s=s2;
      window[6]=(*s++);
      window[7]=(*s++);
      window[8]=(*s++);
      pixel=window[4];
      qsort((void *) window,9,sizeof(RunlengthPacket),
        (int (*)(const void *, const void *)) ReduceNoiseCompare);
      if (Intensity(pixel) == Intensity(window[0]))
        {
          /*
            Pixel is minimum noise; replace with next neighbor in value.
          */
          for (i=1; i < 8; i++)
            if (Intensity(window[i]) != Intensity(window[0]))
              break;
          pixel=window[i];
        }
      else
        if (Intensity(pixel) == Intensity(window[8]))
          {
            /*
              Pixel is maximum noise; replace with next neighbor in value.
            */
            for (i=7; i > 0; i--)
              if (Intensity(window[i]) != Intensity(window[8]))
                break;
            pixel=window[i];
          }
      *q=pixel;
      q->length=0;
      q++;
      s0++;
      s1++;
      s2++;
    }
    /*
      Transfer last pixel of the scanline.
    */
    s=s1;
    *q=(*s);
    q->length=0;
    q++;
    if (QuantumTick(y,image->rows))
      ProgressMonitor(ReduceNoiseImageText,y,image->rows-1);
  }
  /*
    Dump last scanline of pixels.
  */
  s=scanline+image->columns*(y % 3);
  for (x=0; x < (int) image->columns; x++)
  {
    *q=(*s);
    q->length=0;
    q++;
    s++;
  }
  FreeMemory((char *) scanline);
  return(noisy_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     S h a d e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ShadeImage creates a new image that is a copy of an existing
%  one with the image pixels shaded using a distance light source.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ShadeImage method is:
%
%      Image *ShadeImage(Image *image,const unsigned int color_shading,
%        double azimuth,double elevation)
%
%  A description of each parameter follows:
%
%    o shaded_image: Method ShadeImage returns a pointer to the image
%      after it is shaded.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o color_shading: A value other than zero shades the red, green, and blue
%      components of the image.
%
%    o azimuth, elevation:  A double value that indicates the light source
%      direction.
%
%
*/
Export Image *ShadeImage(Image *image,const unsigned int color_shading,
  double azimuth,double elevation)
{
#define ShadeImageText  "  Shading image...  "

   typedef struct _VectorPacket
   {
     int
       x,
       y,
       z;
   } VectorPacket;

  double
    distance,
    normal_distance;

  Image
    *shaded_image;

  int
    y;

  long
    shade;

  register int
    i,
    x;

  register RunlengthPacket
    *p,
    *q,
    *s0,
    *s1,
    *s2;

  VectorPacket
    light,
    normal;

  assert(image != (Image *) NULL);
  if (!UncondenseImage(image))
    return((Image *) NULL);
  /*
    Initialize shaded image attributes.
  */
  shaded_image=CloneImage(image,image->columns,image->rows,False);
  if (shaded_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to shade image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  shaded_image->class=DirectClass;
  if (!color_shading)
    {
      /*
        Initialize shaded image colormap.
      */
      shaded_image->class=PseudoClass;
      shaded_image->colors=MaxRGB+1;
      if (shaded_image->colormap != (ColorPacket *) NULL)
        FreeMemory((char *) shaded_image->colormap);
      shaded_image->colormap=(ColorPacket *)
        AllocateMemory(shaded_image->colors*sizeof(ColorPacket));
      if (shaded_image->colormap == (ColorPacket *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to shade image",
            "Memory allocation failed");
          DestroyImage(shaded_image);
          return((Image *) NULL);
        }
      for (i=0; i < (int) shaded_image->colors; i++)
      {
        shaded_image->colormap[i].red=(Quantum) i;
        shaded_image->colormap[i].green=(Quantum) i;
        shaded_image->colormap[i].blue=(Quantum) i;
      }
    }
  /*
    Compute the light vector.
  */
  azimuth=DegreesToRadians(azimuth);
  elevation=DegreesToRadians(elevation);
  light.x=(int) (MaxRGB*cos(azimuth)*cos(elevation));
  light.y=(int) (MaxRGB*sin(azimuth)*cos(elevation));
  light.z=(int) (MaxRGB*sin(elevation));
  normal.z=(int) ((6.0*MaxRGB)/3.0);  /* constant Z of surface normal */
  /*
    Shade image.
  */
  p=image->pixels;
  q=shaded_image->pixels;
  for (y=0; y < (int) image->rows; y++)
  {
    for (x=0; x < (int) image->columns; x++)
    {
      s0=p-image->columns;
      s1=p;
      s2=p+image->columns;
      while (s0 <= image->pixels)
      {
        s0+=image->columns;
        s1+=image->columns;
        s2+=image->columns;
      }
      while (s2 >= (image->pixels+image->packets-1))
      {
        s0-=image->columns;
        s1-=image->columns;
        s2-=image->columns;
      }
      /*
        Determine the surface normal and compute shading.
      */
      normal.x=Intensity(*(s0-1))+Intensity(*(s1-1))+Intensity(*(s2-1))-
        Intensity(*(s0+1))-Intensity(*(s1+1))-Intensity(*(s2+1));
      normal.y=Intensity(*(s2-1))+Intensity(*s2)+Intensity(*(s2+1))-
        Intensity(*(s0-1))-Intensity(*s0)-Intensity(*(s0+1));
      if ((normal.x == 0) && (normal.y == 0))
        shade=light.z;
      else
        {
          shade=0;
          distance=(double)
            (normal.x*light.x+normal.y*light.y+normal.z*light.z);
          if (distance > 0.0)
            {
              normal_distance=
                normal.x*normal.x+normal.y*normal.y+normal.z*normal.z;
              if (AbsoluteValue(normal_distance) > 0.0000001)
                shade=(long int) (distance/sqrt(normal_distance));
            }
        }
      if (color_shading)
        {
          q->red=(Quantum) (((long) p->red*shade) >> QuantumDepth);
          q->green=(Quantum) (((long) p->green*shade) >> QuantumDepth);
          q->blue=(Quantum) (((long) p->blue*shade) >> QuantumDepth);
        }
      q->index=p->index;
      if (!color_shading)
        q->index=(unsigned short) shade;
      q->length=0;
      p++;
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(ShadeImageText,y,image->rows);
  }
  if (!color_shading)
    SyncImage(shaded_image);
  return(shaded_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     S h a r p e n I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SharpenImage creates a new image that is a copy of an existing
%  one with the pixels sharpened.  It allocates the memory necessary for the
%  new Image structure and returns a pointer to the new image.
%
%  SharpenImage convolves the pixel neighborhood with this sharpening mask:
%
%    -1 -2 -1
%    -2  W -2
%    -1 -2 -1
%
%  The scan only processes pixels that have a full set of neighbors.  Pixels
%  in the top, bottom, left, and right pairs of rows and columns are omitted
%  from the scan.
%
%  The format of the SharpenImage method is:
%
%      Image *SharpenImage(const Image *image,const double factor)
%
%  A description of each parameter follows:
%
%    o sharpened_image: Method SharpenImage returns a pointer to the image
%      after it is sharpened.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o factor:  An double value reflecting the percent weight to give to the
%      center pixel of the neighborhood.
%
%
*/
Export Image *SharpenImage(const Image *image,const double factor)
{
#define Sharpen(weight) \
  total_red+=(weight)*(int) (s->red); \
  total_green+=(weight)*(int) (s->green); \
  total_blue+=(weight)*(int) (s->blue); \
  total_index+=(weight)*(int) (s->index); \
  s++;
#define SharpenImageText  "  Sharpening image...  "

  Image
    *sharpened_image;

  int
    y;

  register int
    runlength,
    x;

  long
    total_blue,
    total_green,
    total_index,
    total_red,
    weight;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2;

  RunlengthPacket
    *scanline;

  unsigned int
    quantum;

  assert(image != (Image *) NULL);
  if ((image->columns < 3) || (image->rows < 3))
    return((Image *) NULL);
  /*
    Initialize sharpened image attributes.
  */
  sharpened_image=CloneImage(image,image->columns,image->rows,False);
  if (sharpened_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to enhance image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  sharpened_image->class=DirectClass;
  /*
    Allocate scan line buffer for 3 rows of the image.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(3*(image->columns+1)*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to enhance image",
        "Memory allocation failed");
      DestroyImage(sharpened_image);
      return((Image *) NULL);
    }
  /*
    Read the first two rows of the image.
  */
  p=image->pixels;
  runlength=p->length+1;
  for (x=0; x < (int) (3*(image->columns+1)); x++)
    scanline[x]=(*p);
  s=scanline;
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Dump first scanline of image.
  */
  q=sharpened_image->pixels;
  s=scanline;
  for (x=0; x < (int) image->columns; x++)
  {
    *q=(*s++);
    q->length=0;
    q++;
  }
  /*
    Convolve each row.
  */
  weight=(long) ((100.0-factor)/2+13);
  quantum=(unsigned int) Max(weight-12,1);
  for (y=1; y < (int) (image->rows-1); y++)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y-1) % 3);
    s1=scanline+image->columns*(y % 3);
    s2=scanline+image->columns*((y+1) % 3);
    /*
      Read another scan line.
    */
    s=s2;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Transfer first pixel of the scanline.
    */
    *q=(*s1);
    q->length=0;
    q++;
    for (x=1; x < (int) (image->columns-1); x++)
    {
      /*
        Compute weighted average of target pixel color components.
      */
      total_red=0;
      total_green=0;
      total_blue=0;
      total_index=0;
      s=s0;
      Sharpen(-1); Sharpen(-2); Sharpen(-1);
      s=s1;
      Sharpen(-2); Sharpen(weight); Sharpen(-2);
      s=s2;
      Sharpen(-1); Sharpen(-2); Sharpen(-1);
      if (total_red < 0)
        q->red=0;
      else
        if (total_red > (int) (MaxRGB*quantum))
          q->red=MaxRGB;
        else
          q->red=(Quantum) ((total_red+(quantum >> 1))/quantum);
      if (total_green < 0)
        q->green=0;
      else
        if (total_green > (int) (MaxRGB*quantum))
          q->green=MaxRGB;
        else
          q->green=(Quantum) ((total_green+(quantum >> 1))/quantum);
      if (total_blue < 0)
        q->blue=0;
      else
        if (total_blue > (int) (MaxRGB*quantum))
          q->blue=MaxRGB;
        else
          q->blue=(Quantum) ((total_blue+(quantum >> 1))/quantum);
      if (total_index < 0)
        q->index=0;
      else
        if (total_index > (int) (MaxRGB*quantum))
          q->index=MaxRGB;
        else
          q->index=(unsigned short) ((total_index+(quantum >> 1))/quantum);
      q->length=0;
      q++;
      s0++;
      s1++;
      s2++;
    }
    /*
      Transfer last pixel of the scanline.
    */
    s1++;
    *q=(*s1);
    q->length=0;
    q++;
    if (QuantumTick(y,image->rows))
      ProgressMonitor(SharpenImageText,y,image->rows-1);
  }
  /*
    Dump last scanline of pixels.
  */
  s=scanline+image->columns*(y % 3);
  for (x=0; x < (int) image->columns; x++)
  {
    *q=(*s++);
    q->length=0;
    q++;
  }
  FreeMemory((char *) scanline);
  return(sharpened_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     S o l a r i z e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SolarizeImage produces a 'solarization' effect seen when exposing
%  a photographic film to light during the development process.
%
%  The format of the SolarizeImage method is:
%
%      void SolarizeImage(Image *image,const double factor)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o factor:  An double value that defines the extent of the solarization.
%
%
*/
Export void SolarizeImage(Image *image,const double factor)
{
#define SolarizeImageText  "  Solarizing the image colors...  "

  register int
    i;

  register RunlengthPacket
    *p;

  unsigned int
    threshold;

  assert(image != (Image *) NULL);
  threshold=(unsigned int) (factor*(MaxRGB+1)/100.0);
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Solarize DirectClass packets.
      */
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        p->red=p->red > threshold ? MaxRGB-p->red : p->red;
        p->green=p->green > threshold ? MaxRGB-p->green : p->green;
        p->blue=p->blue > threshold ? MaxRGB-p->blue : p->blue;
        p++;
        if (QuantumTick(i,image->packets))
          ProgressMonitor(SolarizeImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Solarize PseudoClass packets.
      */
      for (i=0; i < (int) image->colors; i++)
      {
        image->colormap[i].red=image->colormap[i].red > threshold ?
          MaxRGB-image->colormap[i].red : image->colormap[i].red;
        image->colormap[i].green=image->colormap[i].green > threshold ?
          MaxRGB-image->colormap[i].green : image->colormap[i].green;
        image->colormap[i].blue=image->colormap[i].blue > threshold ?
          MaxRGB-image->colormap[i].blue : image->colormap[i].blue;
      }
      SyncImage(image);
      break;
    }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     S p r e a d I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SpreadImage creates a new image that is a copy of an existing
%  one with the image pixels randomly displaced.  It allocates the memory
%  necessary for the new Image structure and returns a pointer to the new
%  image.
%
%  The format of the SpreadImage method is:
%
%      Image *SpreadImage(Image *image,const unsigned int amount)
%
%  A description of each parameter follows:
%
%    o spread_image: Method SpreadImage returns a pointer to the image
%      after it is spread.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o amount:  An unsigned value constraining the "vicinity" for choosing
%      a random pixel to swap.
%
%
*/
Export Image *SpreadImage(Image *image,const unsigned int amount)
{
#define SpreadImageText  "  Spreading image...  "

  Image
    *spread_image;

  int
    quantum;

  long
    x_distance,
    y_distance;

  register RunlengthPacket
    *p,
    *q;

  register unsigned int
    x;

  unsigned int
    y;

  assert(image != (Image *) NULL);
  if ((image->columns < 3) || (image->rows < 3))
    return((Image *) NULL);
  if (!UncondenseImage(image))
    return((Image *) NULL);
  /*
    Initialize spread image attributes.
  */
  spread_image=CloneImage(image,image->columns,image->rows,True);
  if (spread_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to enhance image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  spread_image->class=DirectClass;
  /*
    Convolve each row.
  */
  quantum=(amount+1) >> 1;
  q=spread_image->pixels;
  for (y=0; y < image->rows; y++)
  {
    for (x=0; x < image->columns; x++)
    {
      x_distance=(rand() & (amount+1))-quantum;
      y_distance=(rand() & (amount+1))-quantum;
      p=image->pixels+(y+y_distance)*image->columns+(x+x_distance);
      if ((p > image->pixels) && (p < (image->pixels+image->packets)))
        *q=(*p);
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(SpreadImageText,y,image->rows);
  }
  return(spread_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S t e g a n o I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SteganoImage hides a digital watermark within the image.
%
%  The format of the SteganoImage method is:
%
%      Image *SteganoImage(Image *image,Image *watermark)
%
%  A description of each parameter follows:
%
%    o stegano_image: Method SteganoImage returns a pointer to the
%      steganographic image with the watermark hidden.  A null image is
%      returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o watermark: The address of a structure of type Image.
%
%
*/
Export Image *SteganoImage(Image *image,Image *watermark)
{
#define EmbedBit(byte) \
{ \
  byte&=(~0x01); \
  byte|=(Intensity(*r) >> shift) & 0x01; \
  r++; \
  if (r >= (watermark->pixels+watermark->packets-1)) \
    { \
      r=watermark->pixels; \
      shift--; \
      if (shift < 0) \
        break; \
    } \
}
#define SteganoImageText  "  Hiding image...  "

  Image
    *stegano_image;

  int
    shift;

  register int
    i;

  register RunlengthPacket
    *p,
    *q,
    *r;

  assert(image != (Image *) NULL);
  assert(watermark != (Image *) NULL);
  if (!UncondenseImage(image))
    return((Image *) NULL);
  if (!UncondenseImage(watermark))
    return((Image *) NULL);
  /*
    Initialize steganographic image attributes.
  */
  stegano_image=CloneImage(image,image->columns,image->rows,True);
  if (stegano_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,
        "Unable to create steganographic image","Memory allocation failed");
      return((Image *) NULL);
    }
  if (stegano_image->class == PseudoClass)
    {
      if (stegano_image->colors > ((MaxRGB+1) >> 1))
        stegano_image->class=DirectClass;
      else
        {
          /*
            Shift colormap to make room for information hiding.
          */
          stegano_image->colors<<=1;
          stegano_image->colormap=(ColorPacket *) ReallocateMemory((char *)
            stegano_image->colormap,stegano_image->colors*sizeof(ColorPacket));
          if (stegano_image->colormap == (ColorPacket *) NULL)
            {
              MagickWarning(ResourceLimitWarning,
                "Unable to create steganographic image",
                "Memory allocation failed");
              DestroyImage(stegano_image);
              return((Image *) NULL);
            }
          for (i=stegano_image->colors-1; i >= 0; i--)
            stegano_image->colormap[i]=stegano_image->colormap[i >> 1];
          q=stegano_image->pixels;
          for (i=0; i < (int) stegano_image->packets; i++)
          {
            q->index<<=1;
            q++;
          }
        }
    }
  /*
    Hide watermark in low-order bits of image.
  */
  shift=QuantumDepth-1;
  p=image->pixels+(image->offset % image->packets);
  q=stegano_image->pixels+(stegano_image->offset % stegano_image->packets);
  r=watermark->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    if (stegano_image->class == PseudoClass)
      EmbedBit(q->index)
    else
      {
        EmbedBit(q->red);
        EmbedBit(q->green);
        EmbedBit(q->blue);
      }
    p++;
    q++;
    if (p >= (image->pixels+image->packets-1))
      {
        p=image->pixels;
        q=stegano_image->pixels;
      }
    if (QuantumTick(i,image->packets))
      ProgressMonitor(SteganoImageText,i,image->packets);
  }
  return(stegano_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S t e r e o I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method StereoImage combines two images and produces a single image that
%  is the composite of a left and right image of a stereo pair.  The left
%  image is converted to gray scale and written to the red channel of the
%  stereo image.  The right image is converted to gray scale and written to the
%  blue channel of the stereo image.  View the composite image with red-blue
%  glasses to create a stereo effect.
%
%  The format of the StereoImage method is:
%
%      Image *StereoImage(Image *left_image,const Image *right_image)
%
%  A description of each parameter follows:
%
%    o stereo_image: Method StereoImage returns a pointer to the stereo
%      image.  A null image is returned if there is a memory shortage.
%
%    o left_image: The address of a structure of type Image.
%
%    o right_image: The address of a structure of type Image.
%
%
*/
Export Image *StereoImage(Image *left_image,const Image *right_image)
{
#define StereoImageText  "  Stereo image...  "

  Image
    *stereo_image;

  int
    y;

  register int
    left_runlength,
    right_runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *r;

  assert(left_image != (Image *) NULL);
  assert(right_image != (Image *) NULL);
  if ((left_image->columns != right_image->columns) ||
      (left_image->rows != right_image->rows))
    {
      MagickWarning(ResourceLimitWarning,"Unable to create stereo image",
        "left and right image sizes differ");
      return((Image *) NULL);
    }
  /*
    Initialize stereo image attributes.
  */
  stereo_image=
    CloneImage(left_image,left_image->columns,left_image->rows,False);
  if (stereo_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to create stereo image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  stereo_image->class=DirectClass;
  /*
    Copy left image to red channel and right image to blue channel.
  */
  p=left_image->pixels;
  left_runlength=p->length+1;
  q=right_image->pixels;
  right_runlength=q->length+1;
  r=stereo_image->pixels;
  for (y=0; y < (int) stereo_image->rows; y++)
  {
    for (x=0; x < (int) stereo_image->columns; x++)
    {
      if (left_runlength != 0)
        left_runlength--;
      else
        {
          p++;
          left_runlength=p->length;
        }
      if (right_runlength != 0)
        right_runlength--;
      else
        {
          q++;
          right_runlength=q->length;
        }
      r->red=Intensity(*p);
      r->green=0;
      r->blue=Intensity(*q);
      r->index=0;
      r->length=0;
      r++;
    }
    if (QuantumTick(y,stereo_image->rows))
      ProgressMonitor(StereoImageText,y,stereo_image->rows);
  }
  return(stereo_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     S w i r l I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SwirlImage creates a new image that is a copy of an existing
%  one with the image pixels "swirled" at a specified angle.  It allocates the
%  memory necessary for the new Image structure and returns a pointer to the
%  new image.
%
%  The format of the SwirlImage method is:
%
%      Image *SwirlImage(Image *image,double degrees)
%
%  A description of each parameter follows:
%
%    o swirled_image: Method SwirlImage returns a pointer to the image
%      after it is swirled.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o degrees:  An double value that defines the tightness of the swirling.
%
%
*/
Export Image *SwirlImage(Image *image,double degrees)
{
#define SwirlImageText  "  Swirling image...  "

  ColorPacket
    interpolated_color;

  double
    cosine,
    distance,
    factor,
    radius,
    sine,
    x_center,
    x_distance,
    x_scale,
    y_center,
    y_distance,
    y_scale,
    y_squared;

  Image
    *swirled_image;

  register RunlengthPacket
    *p,
    *q;

  register unsigned int
    x,
    y;

  assert(image != (Image *) NULL);
  if (!UncondenseImage(image))
    return((Image *) NULL);
  /*
    Initialize swirled image attributes.
  */
  if (!image->matte)
    MatteImage(image);
  swirled_image=CloneImage(image,image->columns,image->rows,False);
  if (swirled_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to swirl image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  swirled_image->class=DirectClass;
  swirled_image->matte=True;
  /*
    Compute scaling factor.
  */
  x_center=(double) image->columns/2.0;
  y_center=(double) image->rows/2.0;
  radius=Max(x_center,y_center);
  x_scale=1.0;
  y_scale=1.0;
  if (image->columns > image->rows)
    y_scale=image->columns/image->rows;
  else
    if (image->columns < image->rows)
      x_scale=image->rows/image->columns;
  degrees=DegreesToRadians(degrees);
  /*
    Swirl each row.
  */
  p=image->pixels;
  q=swirled_image->pixels;
  for (y=0; y < image->rows; y++)
  {
    y_distance=y_scale*((double) y-y_center);
    y_squared=y_distance*y_distance;
    for (x=0; x < image->columns; x++)
    {
      /*
        Determine if the pixel is within an ellipse.
      */
      x_distance=x_scale*((double) x-x_center);
      distance=x_distance*x_distance+y_squared;
      if (distance >= (radius*radius))
        *q=(*p);
      else
        {
          /*
            Swirl the pixel.
          */
          factor=1.0-sqrt(distance)/radius;
          factor*=degrees*factor;
          sine=sin(factor);
          cosine=cos(factor);
          interpolated_color=InterpolateColor(image,
            (cosine*x_distance-sine*y_distance)/x_scale+x_center,
            (sine*x_distance+cosine*y_distance)/y_scale+y_center);
          q->red=interpolated_color.red;
          q->green=interpolated_color.green;
          q->blue=interpolated_color.blue;
          q->index=interpolated_color.index;
          q->length=0;
        }
      p++;
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(SwirlImageText,y,image->rows);
  }
  return(swirled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T h r e s h o l d I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ThresholdImage thresholds the reference image.
%
%  The format of the ThresholdImage method is:
%
%      void ThresholdImage(Image *image,const double threshold)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o threshold: A double indicating the threshold value.
%
%
*/
Export void ThresholdImage(Image *image,const double threshold)
{
#define ThresholdImageText  "  Threshold the image...  "

  ColorPacket
    *colormap;

  register int
    i;

  register RunlengthPacket
    *p;

  /*
    Threshold image.
  */
  assert(image != (Image *) NULL);
  colormap=(ColorPacket *) AllocateMemory(2*sizeof(ColorPacket));
  if (colormap == (ColorPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to allocate image",
        "Memory allocation failed");
      return;
    }
  if (image->colormap != (ColorPacket *) NULL)
    FreeMemory((char *) image->colormap);
  image->class=PseudoClass;
  image->colors=2;
  image->colormap=colormap;
  image->colormap[0].red=0;
  image->colormap[0].green=0;
  image->colormap[0].blue=0;
  image->colormap[1].red=MaxRGB;
  image->colormap[1].green=MaxRGB;
  image->colormap[1].blue=MaxRGB;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    p->index=Intensity(*p) < threshold ? 0 : 1;
    p++;
    if (QuantumTick(i,image->packets))
      ProgressMonitor(ThresholdImageText,i,image->packets);
  }
  SyncImage(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     W a v e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WaveImage creates a new image that is a copy of an existing
%  one with the image pixels altered along a sine wave.  It allocates the
%  memory necessary for the new Image structure and returns a pointer to
%  the new image.
%
%  The format of the WaveImage method is:
%
%      Image *WaveImage(Image *image,const double amplitude,
%        const double wavelength)
%
%  A description of each parameter follows:
%
%    o shaded_image: Method WaveImage returns a pointer to the image
%      after it is shaded.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o amplitude, frequency:  A double value that indicates the amplitude
%      and wavelength of the sine wave.
%
%
*/
Export Image *WaveImage(Image *image,const double amplitude,
  const double wavelength)
{
#define WaveImageText  "  Waving image...  "

  ColorPacket
    interpolated_color;

  double
    offset,
    *sine_map;

  Image
    *waved_image;

  register int
    x,
    y;

  register RunlengthPacket
    *q;

  assert(image != (Image *) NULL);
  if (!UncondenseImage(image))
    return((Image *) NULL);
  /*
    Initialize waved image attributes.
  */
  if (!image->matte)
    MatteImage(image);
  image->background_color.index=Transparent;
  waved_image=CloneImage(image,image->columns,image->rows+
    (int) (2*AbsoluteValue(amplitude)),False);
  if (waved_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to wave image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate sine map.
  */
  sine_map=(double *) AllocateMemory(image->columns*sizeof(double));
  if (sine_map == (double *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to wave image",
        "Memory allocation failed");
      DestroyImage(waved_image);
      return((Image *) NULL);
    }
  for (x=0; x < (int) image->columns; x++)
    sine_map[x]=
      AbsoluteValue(amplitude)+amplitude*sin((2*M_PI*x)/wavelength);
  /*
    Wave image.
  */
  q=waved_image->pixels;
  for (y=0; y < (int) waved_image->rows; y++)
  {
    for (x=0; x < (int) waved_image->columns; x++)
    {
      offset=y-sine_map[x];
      interpolated_color=InterpolateColor(image,x,offset);
      q->red=interpolated_color.red;
      q->green=interpolated_color.green;
      q->blue=interpolated_color.blue;
      q->index=interpolated_color.index;
      q->length=0;
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(WaveImageText,y,image->rows);
  }
  FreeMemory((char *) sine_map);
  return(waved_image);
}
