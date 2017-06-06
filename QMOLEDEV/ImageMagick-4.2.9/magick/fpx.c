/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            FFFFF  PPPP   X   X                              %
%                            F      P   P   X X                               %
%                            FFF    PPPP     X                                %
%                            F      P       X X                               %
%                            F      P      X   X                              %
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

#if defined(HasFPX)
#include "Fpxlib.h"
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d F P X I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadFPXImage reads a FlashPix image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.  This method was contributed by BillR@corbis.com.
%
%  The format of the ReadFPXImage method is:
%
%      Image *ReadFPXImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadFPXImage returns a pointer to the image after
%      reading. A null image is returned if there is a memory shortage or if
%      the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadFPXImage(const ImageInfo *image_info)
{
  FPXColorspace
    colorspace;

  FPXImageComponentDesc
    *alpha_component,
    *blue_component,
    *green_component,
    *red_component;

  FPXImageDesc
    fpx_info;

  FPXImageHandle
    *flashpix;

  FPXStatus
    fpx_status;

  FPXSummaryInformation
    summary_info;

  Image
    *image;

  int
    x,
    y;

  Quantum
    blue,
    green,
    red;

  register int
    i;

  register long
    packets;

  register RunlengthPacket
    *q;

  register unsigned char
    *a,
    *b,
    *g,
    *r;

  unsigned char
    *scanline;

  unsigned int
    status,
    subimage;

  unsigned long
    height,
    memory_limit,
    tile_width,
    tile_height,
    width;

  unsigned short
    index;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image.
  */
  status=OpenBlob(image_info,image,ReadBinaryType);
  if (status == False)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  if ((image->file == stdin) || image->pipe)
    {
      FILE
        *file;

      int
        c;

      /*
        Copy standard input or pipe to temporary file.
      */
      TemporaryFilename((char *) image_info->filename);
      file=fopen(image_info->filename,WriteBinaryType);
      if (file == (FILE *) NULL)
        ReaderExit(FileOpenWarning,"Unable to write file",image);
      c=ReadByte(image);
      while (c != EOF)
      {
        (void) putc(c,file);
        c=ReadByte(image);
      }
      (void) fclose(file);
      (void) strcpy(image->filename,image_info->filename);
      image->temporary=True;
    }
  CloseBlob(image);
  /*
    Initialize FPX toolkit.
  */
  memory_limit=20000000;
  fpx_status=FPX_SetToolkitMemoryLimit(&memory_limit);
  if (fpx_status != FPX_OK)
    ReaderExit(DelegateWarning,"Unable to initialize FPX library",image);
  tile_width=64;
  tile_height=64;
  flashpix=(FPXImageHandle *) NULL;
  {
#if defined(macintosh)
    FSSpec
      fsspec;

    FilenameToFSSpec(image->filename,&fsspec);
    fpx_status=FPX_OpenImageByFilename((const FSSpec &) fsspec,
#else
    fpx_status=FPX_OpenImageByFilename(image->filename,
#endif
      NULL,&width,&height,&tile_width,&tile_height,&colorspace,&flashpix);
  }
  if (fpx_status == FPX_LOW_MEMORY_ERROR)
    {
      FPX_ClearSystem();
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  if (fpx_status != FPX_OK)
    {
      FPX_ClearSystem();
      ReaderExit(FileOpenWarning,"Unable to open FPX file",image);
    }
  if (image_info->view == (char *) NULL)
    {
      float
        aspect_ratio;

      /*
        Get the aspect ratio.
      */
      aspect_ratio=(float) width/height;
      fpx_status=FPX_GetImageResultAspectRatio(flashpix,&aspect_ratio);
      if (fpx_status != FPX_OK)
        MagickWarning(DelegateWarning,"Unable to read aspect ratio",
          image_info->filename);
      if (width != (unsigned long) ((aspect_ratio*height)+0.5))
        Swap(width,height);
    }
  fpx_status=FPX_GetSummaryInformation(flashpix,&summary_info);
  if (fpx_status != FPX_OK)
    {
      FPX_ClearSystem();
      ReaderExit(FileOpenWarning,"Unable to read summary info",image);
    }
  if (summary_info.title_valid)
    if ((summary_info.title.length != 0) &&
        (summary_info.title.ptr != (unsigned char *) NULL))
      {
        /*
          Note image label.
        */
        image->label=(char *) AllocateMemory((unsigned int)
          (summary_info.title.length+1)*sizeof(char));
        if (image->label == (char *) NULL)
          MagickWarning(DelegateWarning,"Memory allocation failed",
            image_info->filename);
        else
          {
            (void) strncpy(image->label,(char *) summary_info.title.ptr,
              summary_info.title.length);
            image->label[summary_info.title.length]='\0';
          }
      }
  if (summary_info.comments_valid)
    if ((summary_info.comments.length != 0) &&
        (summary_info.comments.ptr != (unsigned char *) NULL))
      {
        /*
          Note image comment.
        */
        image->comments=(char *) AllocateMemory((unsigned int)
          (summary_info.comments.length+1)*sizeof(char));
        if (image->comments == (char *) NULL)
          MagickWarning(DelegateWarning,"Memory allocation failed",
            image_info->filename);
        else
          {
            (void) strncpy(image->comments,(char *) summary_info.comments.ptr,
              summary_info.comments.length);
            image->comments[summary_info.comments.length]='\0';
          }
      }
  /*
    Determine resolution by subimage specification.
  */
  for (i=1; ; i++)
    if (((width >> i) < tile_width) ||
        ((height >> i) < tile_height))
      break;
  subimage=i;
  if (image_info->subrange != 0)
    while (subimage > image_info->subimage)
    {
      width>>=1;
      height>>=1;
      subimage--;
    }
  if (image_info->size != (char *) NULL)
    while ((width > image->columns) || (height > image->rows))
    {
      width>>=1;
      height>>=1;
      subimage--;
    }
  image->columns=width;
  image->rows=height;
  if ((colorspace.numberOfComponents % 2) == 0)
    image->matte=True;
  if (colorspace.numberOfComponents == 1)
    {
      /*
        Create linear colormap.
      */
      image->class=PseudoClass;
      image->colors=MaxRGB+1;
      image->colormap=(ColorPacket *)
        AllocateMemory(image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        {
          FPX_ClearSystem();
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",
            image);
        }
      for (i=0; i < (int) image->colors; i++)
      {
        image->colormap[i].red=(Quantum) i;
        image->colormap[i].green=(Quantum) i;
        image->colormap[i].blue=(Quantum) i;
      }
    }
  if (image_info->ping)
    {
      CloseBlob(image);
      return(image);
    }
  /*
    Allocate memory for the image and pixel buffer.
  */
  packets=0;
  scanline=(unsigned char *) AllocateMemory(colorspace.numberOfComponents*
    image->columns*(tile_height+1)*sizeof(unsigned char));
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->columns*image->rows*sizeof(RunlengthPacket));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (scanline == (unsigned char *) NULL))
    {
      FPX_ClearSystem();
      (void) FPX_CloseImage(flashpix);
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  /*
    Initialize FlashPix image description.
  */
  fpx_info.numberOfComponents=colorspace.numberOfComponents;
  for (i=0; i < 4; i++)
  {
    fpx_info.components[i].myColorType.myDataType=DATA_TYPE_UNSIGNED_BYTE;
    fpx_info.components[i].horzSubSampFactor=1;
    fpx_info.components[i].vertSubSampFactor=1;
    fpx_info.components[i].columnStride=
      fpx_info.numberOfComponents*sizeof(unsigned char);
    fpx_info.components[i].lineStride=
      image->columns*fpx_info.components[i].columnStride;
    fpx_info.components[i].theData=scanline+i;
  }
  fpx_info.components[0].myColorType.myColor=
    fpx_info.numberOfComponents > 2 ? NIFRGB_R : MONOCHROME;
  red_component=(&fpx_info.components[0]);
  fpx_info.components[1].myColorType.myColor=
    fpx_info.numberOfComponents > 2 ? NIFRGB_G : ALPHA;
  green_component=(&fpx_info.components[1]);
  fpx_info.components[2].myColorType.myColor=NIFRGB_B;
  blue_component=(&fpx_info.components[2]);
  fpx_info.components[3].myColorType.myColor=ALPHA;
  alpha_component=(&fpx_info.components[fpx_info.numberOfComponents-1]);
  FPX_SetResampleMethod(FPX_LINEAR_INTERPOLATION);
  /*
    Initialize image pixels.
  */
  red=0;
  green=0;
  blue=0;
  index=0;
  q=image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; y < (int) image->rows; y++)
  {
    if ((y % tile_height) == 0)
      {
        /*
          Read FPX image tile (with or without viewing transform)..
        */
        if (image_info->view != (char *) NULL)
          fpx_status=FPX_ReadImageRectangle(flashpix,0,y,image->columns,y+
            tile_height-1,subimage,&fpx_info);
        else
          fpx_status=FPX_ReadImageTransformRectangle(flashpix,0.0F,(float) y/
            image->rows,(float) image->columns/image->rows,(float) (y+
            tile_height-1)/image->rows,(long) image->columns,(long) tile_height,
            &fpx_info);
        if (fpx_status == FPX_LOW_MEMORY_ERROR)
          {
            FreeMemory((char *) scanline);
            (void) FPX_CloseImage(flashpix);
            FPX_ClearSystem();
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          }
      }
    /*
      Transfer a FPX scanline.
    */
    r=red_component->theData+(y % tile_height)*red_component->lineStride;
    g=green_component->theData+(y % tile_height)*green_component->lineStride;
    b=blue_component->theData+(y % tile_height)*blue_component->lineStride;
    a=alpha_component->theData+(y % tile_height)*alpha_component->lineStride;
    for (x=0; x < (int) image->columns; x++)
    {
      if (fpx_info.numberOfComponents > 2)
        {
          red=UpScale(*r);
          green=UpScale(*g);
          blue=UpScale(*b);
        }
      else
        {
          index=UpScale(*r);
          red=index;
          green=index;
          blue=index;
        }
      if (image->matte)
        index=UpScale(*a);
      if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
          (index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (packets != 0)
            q++;
          packets++;
          q->red=red;
          q->green=green;
          q->blue=blue;
          q->index=index;
          q->length=0;
        }
      r+=red_component->columnStride;
      g+=green_component->columnStride;
      b+=blue_component->columnStride;
      a+=alpha_component->columnStride;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
  }
  SetRunlengthPackets(image,packets);
  FreeMemory((char *) scanline);
  (void) FPX_CloseImage(flashpix);
  FPX_ClearSystem();
  if (image->temporary)
    {
      (void) remove(image->filename);
      image->temporary=False;
    }
  return(image);
}
#else
Export Image *ReadFPXImage(const ImageInfo *image_info)
{
  MagickWarning(MissingDelegateWarning,"FPX library is not available",
    image_info->filename);
  return((Image *) NULL);
}
#endif

#if defined(HasFPX)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e F P X I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteFPXImage writes an image in the FlashPix image format.  This
%  method was contributed by BillR@corbis.com.
%
%  The format of the WriteFPXImage method is:
%
%      unsigned int WriteFPXImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o fpx_status: Method WriteFPXImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/

static void ColorTwistMultiply(FPXColorTwistMatrix first,
  FPXColorTwistMatrix second,FPXColorTwistMatrix *color_twist)
{
  /*
    Matrix multiply.
  */
  assert(color_twist != (FPXColorTwistMatrix *) NULL);
  color_twist->byy=(first.byy*second.byy)+(first.byc1*second.bc1y)+(first.byc2*
    second.bc2y)+(first.dummy1_zero*second.dummy4_zero);
  color_twist->byc1=(first.byy*second.byc1)+(first.byc1*second.bc1c1)+
    (first.byc2*second.bc2c1)+(first.dummy1_zero*second.dummy5_zero);
  color_twist->byc2=(first.byy*second.byc2)+(first.byc1*second.bc1c2)+
    (first.byc2*second.bc2c2)+(first.dummy1_zero*second.dummy6_zero);
  color_twist->dummy1_zero=(first.byy*second.dummy1_zero)+(first.byc1*
    second.dummy2_zero)+(first.byc2*second.dummy3_zero)+(first.dummy1_zero*
    second.dummy7_one);
  color_twist->bc1y=(first.bc1y*second.byy)+(first.bc1c1*second.bc1y)+
    (first.bc1c2*second.bc2y)+(first.dummy2_zero*second.dummy4_zero);
  color_twist->bc1c1=(first.bc1y*second.byc1)+(first.bc1c1*second.bc1c1)+
    (first.bc1c2*second.bc2c1)+(first.dummy2_zero*second.dummy5_zero);
  color_twist->bc1c2=(first.bc1y*second.byc2)+(first.bc1c1*second.bc1c2)+
    (first.bc1c2*second.bc2c2)+(first.dummy2_zero*second.dummy6_zero);
  color_twist->dummy2_zero=(first.bc1y*second.dummy1_zero)+(first.bc1c1*
    second.dummy2_zero)+(first.bc1c2*second.dummy3_zero)+(first.dummy2_zero*
    second.dummy7_one);
  color_twist->bc2y=(first.bc2y*second.byy)+(first.bc2c1*second.bc1y)+
    (first.bc2c2*second.bc2y)+(first.dummy3_zero*second.dummy4_zero);
  color_twist->bc2c1=(first.bc2y*second.byc1)+(first.bc2c1*second.bc1c1)+
    (first.bc2c2*second.bc2c1)+(first.dummy3_zero*second.dummy5_zero);
  color_twist->bc2c2=(first.bc2y*second.byc2)+(first.bc2c1*second.bc1c2)+
    (first.bc2c2*second.bc2c2)+(first.dummy3_zero*second.dummy6_zero);
  color_twist->dummy3_zero=(first.bc2y*second.dummy1_zero)+(first.bc2c1*
    second.dummy2_zero)+(first.bc2c2*second.dummy3_zero)+(first.dummy3_zero*
    second.dummy7_one);
  color_twist->dummy4_zero=(first.dummy4_zero*second.byy)+(first.dummy5_zero*
    second.bc1y)+(first.dummy6_zero*second.bc2y)+(first.dummy7_one*
    second.dummy4_zero);
  color_twist->dummy5_zero=(first.dummy4_zero*second.byc1)+(first.dummy5_zero*
    second.bc1c1)+(first.dummy6_zero*second.bc2c1)+(first.dummy7_one*
    second.dummy5_zero);
  color_twist->dummy6_zero=(first.dummy4_zero*second.byc2)+(first.dummy5_zero*
    second.bc1c2)+(first.dummy6_zero*second.bc2c2)+(first.dummy7_one*
    second.dummy6_zero);
  color_twist->dummy7_one=(first.dummy4_zero*second.dummy1_zero)+
    (first.dummy5_zero*second.dummy2_zero)+(first.dummy6_zero*
    second.dummy3_zero)+(first.dummy7_one*second.dummy7_one);
}

static void SetBrightness(double brightness,FPXColorTwistMatrix *color_twist)
{
  FPXColorTwistMatrix
    effect,
    result;

  /*
    Set image brightness in color twist matrix.
  */
  assert(color_twist != (FPXColorTwistMatrix *) NULL);
  brightness=sqrt((double) brightness);
  effect.byy=brightness;
  effect.byc1=0.0;
  effect.byc2=0.0;
  effect.dummy1_zero=0.0;
  effect.bc1y=0.0;
  effect.bc1c1=brightness;
  effect.bc1c2=0.0;
  effect.dummy2_zero=0.0;
  effect.bc2y=0.0;
  effect.bc2c1=0.0;
  effect.bc2c2=brightness;
  effect.dummy3_zero=0.0;
  effect.dummy4_zero=0.0;
  effect.dummy5_zero=0.0;
  effect.dummy6_zero=0.0;
  effect.dummy7_one=1.0;
  ColorTwistMultiply(*color_twist,effect,&result);
  *color_twist=result;
}

static void SetColorBalance(double red,double green,double blue,
  FPXColorTwistMatrix *color_twist)
{
  FPXColorTwistMatrix
    blue_effect,
    green_effect,
    result,
    rgb_effect,
    rg_effect,
    red_effect;

  /*
    Set image color balance in color twist matrix.
  */
  assert(color_twist != (FPXColorTwistMatrix *) NULL);
  red=sqrt((double) red)-1.0;
  green=sqrt((double) green)-1.0;
  blue=sqrt((double) blue)-1.0;
  red_effect.byy=1.0;
  red_effect.byc1=0.0;
  red_effect.byc2=0.299*red;
  red_effect.dummy1_zero=0.0;
  red_effect.bc1y=(-0.299)*red;
  red_effect.bc1c1=1.0-0.299*red;
  red_effect.bc1c2=(-0.299)*red;
  red_effect.dummy2_zero=0.0;
  red_effect.bc2y=0.701*red;
  red_effect.bc2c1=0.0;
  red_effect.bc2c2=1.0+0.402*red;
  red_effect.dummy3_zero=0.0;
  red_effect.dummy4_zero=0.0;
  red_effect.dummy5_zero=0.0;
  red_effect.dummy6_zero=0.0;
  red_effect.dummy7_one=1.0;
  green_effect.byy=1.0;
  green_effect.byc1=(-0.114)*green;
  green_effect.byc2=(-0.299)*green;
  green_effect.dummy1_zero=0.0;
  green_effect.bc1y=(-0.587)*green;
  green_effect.bc1c1=1.0-0.473*green;
  green_effect.bc1c2=0.299*green;
  green_effect.dummy2_zero=0.0;
  green_effect.bc2y=(-0.587)*green;
  green_effect.bc2c1=0.114*green;
  green_effect.bc2c2=1.0-0.288*green;
  green_effect.dummy3_zero=0.0;
  green_effect.dummy4_zero=0.0;
  green_effect.dummy5_zero=0.0;
  green_effect.dummy6_zero=0.0;
  green_effect.dummy7_one=1.0;
  blue_effect.byy=1.0;
  blue_effect.byc1=0.114*blue;
  blue_effect.byc2=0.0;
  blue_effect.dummy1_zero=0.0;
  blue_effect.bc1y=0.886*blue;
  blue_effect.bc1c1=1.0+0.772*blue;
  blue_effect.bc1c2=0.0;
  blue_effect.dummy2_zero=0.0;
  blue_effect.bc2y=(-0.114)*blue;
  blue_effect.bc2c1=(-0.114)*blue;
  blue_effect.bc2c2=1.0-0.114*blue;
  blue_effect.dummy3_zero=0.0;
  blue_effect.dummy4_zero=0.0;
  blue_effect.dummy5_zero=0.0;
  blue_effect.dummy6_zero=0.0;
  blue_effect.dummy7_one=1.0;
  ColorTwistMultiply(red_effect,green_effect,&rg_effect);
  ColorTwistMultiply(rg_effect,blue_effect,&rgb_effect);
  ColorTwistMultiply(*color_twist,rgb_effect,&result);
  *color_twist=result;
}

static void SetSaturation(double saturation,FPXColorTwistMatrix *color_twist)
{
  FPXColorTwistMatrix
    effect,
    result;

  /*
    Set image saturation in color twist matrix.
  */
  assert(color_twist != (FPXColorTwistMatrix *) NULL);
  effect.byy=1.0;
  effect.byc1=0.0;
  effect.byc2=0.0;
  effect.dummy1_zero=0.0;
  effect.bc1y=0.0;
  effect.bc1c1=saturation;
  effect.bc1c2=0.0;
  effect.dummy2_zero=0.0;
  effect.bc2y=0.0;
  effect.bc2c1=0.0;
  effect.bc2c2=saturation;
  effect.dummy3_zero=0.0;
  effect.dummy4_zero=0.0;
  effect.dummy5_zero=0.0;
  effect.dummy6_zero=0.0;
  effect.dummy7_one=1.0;
  ColorTwistMultiply(*color_twist,effect,&result);
  *color_twist=result;
}

Export unsigned int WriteFPXImage(const ImageInfo *image_info,Image *image)
{
  FPXBackground
    background_color;

  FPXColorspace
    colorspace =
    {
      TRUE, 4,
      NIFRGB_R, DATA_TYPE_UNSIGNED_BYTE,
      NIFRGB_G, DATA_TYPE_UNSIGNED_BYTE,
      NIFRGB_B, DATA_TYPE_UNSIGNED_BYTE,
      ALPHA, DATA_TYPE_UNSIGNED_BYTE
    };

  FPXCompressionOption
    compression;

  FPXImageDesc
    fpx_info;

  FPXImageHandle
    *flashpix;

  FPXStatus
    fpx_status;

  FPXSummaryInformation
    summary_info;

  Image
    fpx_image;

  int
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  unsigned char
    *pixels;

  unsigned int
    status;

  unsigned long
    memory_limit,
    tile_height,
    tile_width;

  unsigned short
    value;

  /*
    Open input file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  if ((image->file != stdout) && !image->pipe)
    (void) remove(image->filename);
  else
    {
      /*
        Write standard output or pipe to temporary file.
      */
      fpx_image=(*image);
      TemporaryFilename(image->filename);
      image->temporary=True;
    }
  CloseBlob(image);
  TransformRGBImage(image,RGBColorspace);
  /*
    Initialize FPX toolkit.
  */
  memory_limit=20000000;
  fpx_status=FPX_SetToolkitMemoryLimit(&memory_limit);
  if (fpx_status != FPX_OK)
    WriterExit(ResourceLimitWarning,"Unable to initialize FPX library",
      image);
  tile_width=64;
  tile_height=64;
  colorspace.numberOfComponents=3;
  if (image->matte)
    colorspace.numberOfComponents=4;
  if (IsGrayImage(image))
    {
      colorspace.numberOfComponents=1;
      colorspace.theComponents[0].myColor=MONOCHROME;
    }
  background_color.color1_value=0;
  background_color.color2_value=0;
  background_color.color3_value=0;
  background_color.color4_value=0;
  compression=NONE;
  if (image_info->compression == JPEGCompression)
    compression=JPEG_UNSPECIFIED;
  {
#if defined(macintosh)
    FSSpec
      fsspec;

    FilenameToFSSpec(image->filename,&fsspec);
    fpx_status=FPX_CreateImageByFilename((const FSSpec &) fsspec,
#else
    fpx_status=FPX_CreateImageByFilename(image->filename,
#endif
    image->columns,image->rows,tile_width,tile_height,colorspace,
    background_color,compression,&flashpix);
  }
  if (fpx_status != FPX_OK)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  if (image_info->compression == JPEGCompression)
    {
      /*
        Initialize the compression by quality for the entire image.
      */
      fpx_status=
        FPX_SetJPEGCompression(flashpix,(unsigned short) (image_info->quality));
      if (fpx_status != FPX_OK)
        MagickWarning(DelegateWarning,"Unable to set JPEG level",(char *) NULL);
    }
  /*
    Set image summary info.
  */
  summary_info.subject_valid=False;
  summary_info.author_valid=False;
  summary_info.keywords_valid=False;
  summary_info.OLEtemplate_valid=False;
  summary_info.last_author_valid=False;
  summary_info.rev_number_valid=False;
  summary_info.edit_time_valid=False;
  summary_info.last_printed_valid=False;
  summary_info.create_dtm_valid=False;
  summary_info.last_save_dtm_valid=False;
  summary_info.page_count_valid=False;
  summary_info.word_count_valid=False;
  summary_info.char_count_valid=False;
  summary_info.thumbnail_valid=False;
  summary_info.appname_valid=False;
  summary_info.security_valid=False;
  if (image->label != (char *) NULL)
    {
      /*
        Note image label.
      */
      summary_info.title_valid=True;
      summary_info.title.length=strlen(image->label);
      summary_info.title.ptr=(unsigned char *)
        AllocateMemory((strlen(image->label)+1)*sizeof(unsigned char));
      if (summary_info.title.ptr != (unsigned char *) NULL)
        (void) strcpy((char *) summary_info.title.ptr,image->label);
      else
        MagickWarning(DelegateWarning,"Unable to set image title",
          (char *) NULL);
    }
  if (image->comments != (char *) NULL)
    {
      /*
        Note image comment.
      */
      summary_info.comments_valid=True;
      summary_info.comments.length=strlen(image->comments);
      summary_info.comments.ptr=(unsigned char *)
        AllocateMemory((strlen(image->comments)+1)*sizeof(unsigned char));
      if (summary_info.comments.ptr != (unsigned char *) NULL)
        (void) strcpy((char *) summary_info.comments.ptr,image->comments);
      else
        MagickWarning(DelegateWarning,"Unable to set image comments",
          (char *) NULL);
    }
  fpx_status=FPX_SetSummaryInformation(flashpix,&summary_info);
  if (fpx_status != FPX_OK)
    MagickWarning(DelegateWarning,"Unable to set summary info",(char *) NULL);
  /*
    Allocate pixels.
  */
  pixels=(unsigned char *) AllocateMemory(colorspace.numberOfComponents*
    image->columns*sizeof(unsigned char));
  if (pixels == (unsigned char *) NULL)
    {
      (void) FPX_CloseImage(flashpix);
      FPX_ClearSystem();
      WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  /*
    Initialize FlashPix image description.
  */
  fpx_info.numberOfComponents=colorspace.numberOfComponents;
  for (i=0; i < fpx_info.numberOfComponents; i++)
  {
    fpx_info.components[i].myColorType.myDataType=DATA_TYPE_UNSIGNED_BYTE;
    fpx_info.components[i].horzSubSampFactor=1;
    fpx_info.components[i].vertSubSampFactor=1;
    fpx_info.components[i].columnStride=
      fpx_info.numberOfComponents*sizeof(unsigned char);
    fpx_info.components[i].lineStride=
      image->columns*fpx_info.components[i].columnStride;
    fpx_info.components[i].theData=pixels+i;
  }
  fpx_info.components[0].myColorType.myColor=
    fpx_info.numberOfComponents != 1 ? NIFRGB_R : MONOCHROME;
  fpx_info.components[1].myColorType.myColor=NIFRGB_G;
  fpx_info.components[2].myColorType.myColor=NIFRGB_B;
  fpx_info.components[3].myColorType.myColor=ALPHA;
  /*
    Write image scanlines.
  */
  x=0;
  y=0;
  p=image->pixels;
  q=pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    for (j=0; j <= (int) p->length; j++)
    {
      if (fpx_info.numberOfComponents == 1)
        WriteQuantum(Intensity(*p),q)
      else
        {
          WriteQuantum(p->red,q);
          WriteQuantum(p->green,q);
          WriteQuantum(p->blue,q);
          if (image->matte)
            WriteQuantum(p->index,q);
        }
      x++;
      if (x == (int) image->columns)
        {
          fpx_status=FPX_WriteImageLine(flashpix,&fpx_info);
          if (fpx_status != FPX_OK)
            break;
          if (QuantumTick(y,image->rows))
            ProgressMonitor(SaveImageText,y,image->rows);
          q=pixels;
          x=0;
          y++;
        }
    }
    p++;
  }
  if (image_info->view != (char *) NULL)
    {
      FPXAffineMatrix
        affine;

      FPXColorTwistMatrix
        color_twist;

      FPXContrastAdjustment
        contrast;

      FPXFilteringValue
        sharpen;

      FPXResultAspectRatio
        aspect_ratio;

      FPXROI
        view_rect;

      unsigned int
        affine_valid,
        aspect_ratio_valid,
        color_twist_valid,
        contrast_valid,
        sharpen_valid,
        view_rect_valid;

      /*
        Initialize default viewing parameters.
      */
      contrast=1.0;
      contrast_valid=False;
      color_twist.byy=1.0;
      color_twist.byc1=0.0;
      color_twist.byc2=0.0;
      color_twist.dummy1_zero=0.0;
      color_twist.bc1y=0.0;
      color_twist.bc1c1=1.0;
      color_twist.bc1c2=0.0;
      color_twist.dummy2_zero=0.0;
      color_twist.bc2y=0.0;
      color_twist.bc2c1=0.0;
      color_twist.bc2c2=1.0;
      color_twist.dummy3_zero=0.0;
      color_twist.dummy4_zero=0.0;
      color_twist.dummy5_zero=0.0;
      color_twist.dummy6_zero=0.0;
      color_twist.dummy7_one=1.0;
      color_twist_valid=False;
      sharpen=0.0;
      sharpen_valid=False;
      aspect_ratio=(double) image->columns/image->rows;
      aspect_ratio_valid=False;
      view_rect.left=0.1;
      view_rect.width=aspect_ratio-0.2;
      view_rect.top=0.1;
      view_rect.height=1.0-0.2;
      view_rect_valid=False;
      affine.a11=1.0;
      affine.a12=0.0;
      affine.a13=0.0;
      affine.a14=0.0;
      affine.a21=0.0;
      affine.a22=1.0;
      affine.a23=0.0;
      affine.a24=0.0;
      affine.a31=0.0;
      affine.a32=0.0;
      affine.a33=1.0;
      affine.a34=0.0;
      affine.a41=0.0;
      affine.a42=0.0;
      affine.a43=0.0;
      affine.a44=1.0;
      affine_valid=False;
      if (0)
        {
          /*
            Color color_twist.
          */
          SetBrightness(0.5,&color_twist);
          SetSaturation(0.5,&color_twist);
          SetColorBalance(0.5,1.0,1.0,&color_twist);
          color_twist_valid=True;
        }
      if (affine_valid)
        {
          fpx_status=FPX_SetImageAffineMatrix(flashpix,&affine);
          if (fpx_status != FPX_OK)
            MagickWarning(DelegateWarning,"Unable to set affine matrix",
              (char *) NULL);
        }
      if (aspect_ratio_valid)
        {
          fpx_status=FPX_SetImageResultAspectRatio(flashpix,&aspect_ratio);
          if (fpx_status != FPX_OK)
            MagickWarning(DelegateWarning,"Unable to set aspect ratio",
              (char *) NULL);
        }
      if (color_twist_valid)
        {
          fpx_status=FPX_SetImageColorTwistMatrix(flashpix,&color_twist);
          if (fpx_status != FPX_OK)
            MagickWarning(DelegateWarning,"Unable to set color color twist",
              (char *) NULL);
        }
      if (contrast_valid)
        {
          fpx_status=FPX_SetImageContrastAdjustment(flashpix,&contrast);
          if (fpx_status != FPX_OK)
            MagickWarning(DelegateWarning,"Unable to set contrast",
              (char *) NULL);
        }
      if (sharpen_valid)
        {
          fpx_status=FPX_SetImageFilteringValue(flashpix,&sharpen);
          if (fpx_status != FPX_OK)
            MagickWarning(DelegateWarning,"Unable to set filtering value",
              (char *) NULL);
        }
      if (view_rect_valid)
        {
          fpx_status=FPX_SetImageROI(flashpix, &view_rect);
          if (fpx_status != FPX_OK)
            MagickWarning(DelegateWarning,"Unable to set region of interest",
              (char *) NULL);
        }
    }
  (void) FPX_CloseImage(flashpix);
  FPX_ClearSystem();
  FreeMemory((char *) pixels);
  if (image->temporary)
    {
      FILE
        *file;

      int
        c;

      /*
        Copy temporary file to standard output or pipe.
      */
      file=fopen(image->filename,ReadBinaryType);
      if (file == (FILE *) NULL)
        WriterExit(FileOpenWarning,"Unable to open file",image);
      for (c=fgetc(file); c != EOF; c=fgetc(file))
        (void) putc(c,fpx_image.file);
      (void) fclose(file);
      (void) remove(image->filename);
      image->temporary=False;
      CloseBlob(&fpx_image);
    }
  return(True);
}
#else
Export unsigned int WriteFPXImage(const ImageInfo *image_info,Image *image)
{
  MagickWarning(MissingDelegateWarning,"FPX library is not available",
    image->filename);
  return(False);
}
#endif
