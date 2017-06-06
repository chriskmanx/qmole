/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            Y   Y  U   U  V   V                              %
%                             Y Y   U   U  V   V                              %
%                              Y    U   U  V   V                              %
%                              Y    U   U   V V                               %
%                              Y     UUU     V                                %
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
%   R e a d Y U V I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadYUVImage reads an image with digital YUV (CCIR 601 4:1:1) bytes
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the ReadYUVImage method is:
%
%      Image *ReadYUVImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadYUVImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadYUVImage(const ImageInfo *image_info)
{
  Image
    *image,
    *zoomed_image;

  int
    count,
    y;

  register int
    i,
    x;

  register RunlengthPacket
    *q,
    *r;

  register unsigned char
    *p;

  unsigned char
    *scanline;

  unsigned int
    status;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  if ((image->columns == 0) || (image->rows == 0))
    ReaderExit(OptionWarning,"must specify image size",image);
  if (image_info->interlace != PartitionInterlace)
    {
      /*
        Open image file.
      */
      status=OpenBlob(image_info,image,ReadBinaryType);
      if (status == False)
        ReaderExit(FileOpenWarning,"Unable to open file",image);
      for (i=0; i < image->offset; i++)
        (void) ReadByte(image);
    }
  /*
    Allocate memory for a scanline.
  */
  scanline=(unsigned char *)
    AllocateMemory(image->columns*sizeof(unsigned char));
  if (scanline == (unsigned char *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->columns>>=1;
    image->rows>>=1;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      AllocateMemory((image->packets << 2)*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    SetImage(image);
    /*
      Convert raster image to runlength-encoded packets.
    */
    if (image_info->interlace == PartitionInterlace)
      {
        AppendImageFormat("Y",image->filename);
        status=OpenBlob(image_info,image,ReadBinaryType);
        if (status == False)
          ReaderExit(FileOpenWarning,"Unable to open file",image);
      }
    i=0;
    q=image->pixels;
    for (y=0; y < (int) (image->rows << 1); y++)
    {
      if ((y > 0) || (image->previous == (Image *) NULL))
        (void) ReadBlob(image,image->columns << 1,(char *) scanline);
      p=scanline;
      for (x=0; x < (int) (image->columns << 1); x++)
      {
        q->red=UpScale(*p++);
        q->index=0;
        q->length=0;
        q++;
      }
      if (image->previous == (Image *) NULL)
        ProgressMonitor(LoadImageText,i,3);
      i++;
    }
    if (image_info->interlace == PartitionInterlace)
      {
        CloseBlob(image);
        AppendImageFormat("U",image->filename);
        status=OpenBlob(image_info,image,ReadBinaryType);
        if (status == False)
          ReaderExit(FileOpenWarning,"Unable to open file",image);
      }
    q=image->pixels;
    for (y=0; y < (int) image->rows; y++)
    {
      (void) ReadBlob(image,image->columns,(char *) scanline);
      p=scanline;
      for (x=0; x < (int) image->columns; x++)
      {
        q->green=UpScale(*p++);
        q++;
      }
      if (image->previous == (Image *) NULL)
        ProgressMonitor(LoadImageText,i,3);
      i++;
    }
    if (image_info->interlace == PartitionInterlace)
      {
        CloseBlob(image);
        AppendImageFormat("V",image->filename);
        status=OpenBlob(image_info,image,ReadBinaryType);
        if (status == False)
          ReaderExit(FileOpenWarning,"Unable to open file",image);
      }
    q=image->pixels;
    for (y=0; y < (int) image->rows; y++)
    {
      (void) ReadBlob(image,image->columns,(char *) scanline);
      p=scanline;
      for (x=0; x < (int) image->columns; x++)
      {
        q->blue=UpScale(*p++);
        q++;
      }
      if (image->previous == (Image *) NULL)
        ProgressMonitor(LoadImageText,i,3);
      i++;
    }
    /*
      Scale image.
    */
    image->orphan=True;
    zoomed_image=MagnifyImage(image);
    image->orphan=False;
    if (zoomed_image == (Image *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    image->columns<<=1;
    image->rows<<=1;
    image->packets=image->columns*image->rows;
    q=image->pixels;
    r=zoomed_image->pixels;
    for (i=0; i < (int) image->packets; i++)
    {
      q->green=r->green;
      q->blue=r->blue;
      q++;
      r++;
    }
    DestroyImage(zoomed_image);
    TransformRGBImage(image,YCbCrColorspace);
    CondenseImage(image);
    if (image_info->interlace == PartitionInterlace)
      (void) strcpy(image->filename,image_info->filename);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    count=ReadBlob(image,image->columns,(char *) scanline);
    if (count > 0)
      {
        /*
          Allocate next image structure.
        */
        AllocateNextImage(image_info,image);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        image=image->next;
        ProgressMonitor(LoadImagesText,(unsigned int) TellBlob(image),
          (unsigned int) image->filesize);
      }
  } while (count > 0);
  FreeMemory((char *) scanline);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e Y U V I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteYUVImage writes an image to a file in the digital YUV
%  (CCIR 601 4:1:1) format.
%
%  The format of the WriteYUVImage method is:
%
%      unsigned int WriteYUVImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteYUVImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteYUVImage(const ImageInfo *image_info,Image *image)
{
  FilterType
    filter;

  Image
    *downsampled_image,
    *yuv_image;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  unsigned int
    scene,
    status;

  if (image_info->interlace != PartitionInterlace)
    {
      /*
        Open output image file.
      */
      status=OpenBlob(image_info,image,WriteBinaryType);
      if (status == False)
        WriterExit(FileOpenWarning,"Unable to open file",image);
    }
  if (image_info->interlace == PartitionInterlace)
    {
      AppendImageFormat("Y",image->filename);
      status=OpenBlob(image_info,image,WriteBinaryType);
      if (status == False)
        WriterExit(FileOpenWarning,"Unable to open file",image);
    }
  scene=0;
  do
  {
    /*
      Zoom image to an even width and height.
    */
    TransformRGBImage(image,RGBColorspace);
    filter=image->filter;
    image->filter=PointFilter;
    image->orphan=True;
    yuv_image=ZoomImage(image,image->columns+(image->columns & 0x01 ? 1 : 0),
      image->rows+(image->rows & 0x01 ? 1 : 0));
    image->orphan=False;
    if (yuv_image == (Image *) NULL)
      WriterExit(ResourceLimitWarning,"Unable to zoom image",image);
    /*
      Initialize Y channel.
    */
    if (image->previous == (Image *) NULL)
      ProgressMonitor(SaveImageText,100,400);
    RGBTransformImage(yuv_image,YCbCrColorspace);
    p=yuv_image->pixels;
    for (i=0; i < (int) yuv_image->packets; i++)
    {
      for (j=0; j <= ((int) p->length); j++)
        (void) WriteByte(image,DownScale(p->red));
      p++;
    }
    DestroyImage(yuv_image);
    /*
      Downsample image.
    */
    image->orphan=True;
    downsampled_image=ZoomImage(image,
      (image->columns+(image->columns & 0x01 ? 1 : 0)) >> 1,
      (image->rows+(image->rows & 0x01 ? 1 : 0)) >> 1);
    image->orphan=False;
    image->filter=filter;
    if (downsampled_image == (Image *) NULL)
      WriterExit(ResourceLimitWarning,"Unable to zoom image",image);
    /*
      Initialize U channel.
    */
    if (image->previous == (Image *) NULL)
      ProgressMonitor(SaveImageText,200,400);
    if (image_info->interlace == PartitionInterlace)
      {
        CloseBlob(image);
        AppendImageFormat("U",image->filename);
        status=OpenBlob(image_info,image,WriteBinaryType);
        if (status == False)
          WriterExit(FileOpenWarning,"Unable to open file",image);
      }
    RGBTransformImage(downsampled_image,YCbCrColorspace);
    p=downsampled_image->pixels;
    for (i=0; i < (int) downsampled_image->packets; i++)
    {
      for (j=0; j <= ((int) p->length); j++)
        (void) WriteByte(image,DownScale(p->green));
      p++;
    }
    /*
      Initialize V channel.
    */
    if (image->previous == (Image *) NULL)
      ProgressMonitor(SaveImageText,300,400);
    if (image_info->interlace == PartitionInterlace)
      {
        CloseBlob(image);
        AppendImageFormat("V",image->filename);
        status=OpenBlob(image_info,image,WriteBinaryType);
        if (status == False)
          WriterExit(FileOpenWarning,"Unable to open file",image);
      }
    p=downsampled_image->pixels;
    for (i=0; i < (int) downsampled_image->packets; i++)
    {
      for (j=0; j <= ((int) p->length); j++)
        (void) WriteByte(image,DownScale(p->blue));
      p++;
    }
    DestroyImage(downsampled_image);
    if (image_info->interlace == PartitionInterlace)
      (void) strcpy(image->filename,image_info->filename);
    if (image->previous == (Image *) NULL)
      ProgressMonitor(SaveImageText,400,400);
    if (image->next == (Image *) NULL)
      break;
    image->next->file=image->file;
    image=image->next;
    ProgressMonitor(SaveImagesText,scene++,GetNumberScenes(image));
  } while (image_info->adjoin);
  if (image_info->adjoin)
    while (image->previous != (Image *) NULL)
      image=image->previous;
  CloseBlob(image);
  return(True);
}
