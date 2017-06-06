/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            RRRR    GGGG  BBBB                               %
%                            R   R  G      B   B                              %
%                            RRRR   G  GG  BBBB                               %
%                            R R    G   G  B   B                              %
%                            R  R    GGG   BBBB                               %
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
%   R e a d R G B I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadRGBImage reads an image of raw red, green, and blue bytes and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadRGBImage method is:
%
%      Image *ReadRGBImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadRGBImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadRGBImage(const ImageInfo *image_info)
{
  Image
    *image;

  int
    count,
    y;

  register int
    i,
    x;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *scanline;

  unsigned int
    packet_size,
    status;

  unsigned short
    value;

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
  packet_size=3*(image->depth >> 3);
  if (Latin1Compare(image_info->magick,"RGBA") == 0)
    {
      image->matte=True;
      packet_size=4*(image->depth >> 3);
    }
  scanline=(unsigned char *)
    AllocateMemory(packet_size*image->tile_info.width*sizeof(unsigned char));
  if (scanline == (unsigned char *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  if (image_info->subrange != 0)
    while (image->scene < image_info->subimage)
    {
      /*
        Skip to next image.
      */
      image->scene++;
      for (y=0; y < (int) image->rows; y++)
        (void) ReadBlob(image,packet_size*image->tile_info.width,
          (char *) scanline);
    }
  do
  {
    /*
      Initialize image structure.
    */
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    SetImage(image);
    /*
      Convert raster image to runlength-encoded packets.
    */
    switch (image_info->interlace)
    {
      case NoInterlace:
      default:
      {
        /*
          No interlacing:  RGBRGBRGBRGBRGBRGB...
        */
        for (y=0; y < image->tile_info.y; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        q=image->pixels;
        for (y=0; y < (int) image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadBlob(image,packet_size*image->tile_info.width,
              (char *) scanline);
          p=scanline+packet_size*image->tile_info.x;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(q->red,p);
            ReadQuantum(q->green,p);
            ReadQuantum(q->blue,p);
            q->index=0;
            if (image->matte)
              ReadQuantum(q->index,p);
            q->length=0;
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        count=image->tile_info.height-image->rows-image->tile_info.y;
        for (y=0; y < count; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        break;
      }
      case LineInterlace:
      {
        /*
          Line interlacing:  RRR...GGG...BBB...RRR...GGG...BBB...
        */
        packet_size=image->depth >> 3;
        for (y=0; y < image->tile_info.y; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        for (y=0; y < (int) image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadBlob(image,packet_size*image->tile_info.width,
              (char *) scanline);
          p=scanline+packet_size*image->tile_info.x;
          q=image->pixels+y*image->columns;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(q->red,p);
            q->index=0;
            q->length=0;
            q++;
          }
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
          p=scanline+packet_size*image->tile_info.x;
          q=image->pixels+y*image->columns;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(q->green,p);
            q++;
          }
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
          p=scanline+packet_size*image->tile_info.x;
          q=image->pixels+y*image->columns;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(q->blue,p);
            q++;
          }
          if (image->matte)
            {
              (void) ReadBlob(image,packet_size*image->tile_info.width,
                (char *) scanline);
              p=scanline+packet_size*image->tile_info.x;
              q=image->pixels+y*image->columns;
              for (x=0; x < (int) image->columns; x++)
              {
                ReadQuantum(q->index,p);
                q++;
              }
            }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        count=image->tile_info.height-image->rows-image->tile_info.y;
        for (y=0; y < count; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        break;
      }
      case PlaneInterlace:
      case PartitionInterlace:
      {
        unsigned int
          span;

        /*
          Plane interlacing:  RRRRRR...GGGGGG...BBBBBB...
        */
        if (image_info->interlace == PartitionInterlace)
          {
            AppendImageFormat("R",image->filename);
            status=OpenBlob(image_info,image,ReadBinaryType);
            if (status == False)
              ReaderExit(FileOpenWarning,"Unable to open file",image);
          }
        packet_size=image->depth >> 3;
        for (y=0; y < image->tile_info.y; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        i=0;
        span=image->rows*(image->matte ? 4 : 3);
        q=image->pixels;
        for (y=0; y < (int) image->rows; y++)
        {
          if ((y > 0) || (image->previous == (Image *) NULL))
            (void) ReadBlob(image,packet_size*image->tile_info.width,
              (char *) scanline);
          p=scanline+packet_size*image->tile_info.x;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(q->red,p);
            q->index=0;
            q->length=0;
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,span))
              ProgressMonitor(LoadImageText,i,span);
          i++;
        }
        count=image->tile_info.height-image->rows-image->tile_info.y;
        for (y=0; y < count; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        if (image_info->interlace == PartitionInterlace)
          {
            CloseBlob(image);
            AppendImageFormat("G",image->filename);
            status=OpenBlob(image_info,image,ReadBinaryType);
            if (status == False)
              ReaderExit(FileOpenWarning,"Unable to open file",image);
          }
        q=image->pixels;
        for (y=0; y < image->tile_info.y; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        for (y=0; y < (int) image->rows; y++)
        {
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
          p=scanline+packet_size*image->tile_info.x;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(q->green,p);
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,span))
              ProgressMonitor(LoadImageText,i,span);
          i++;
        }
        count=image->tile_info.height-image->rows-image->tile_info.y;
        for (y=0; y < count; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        if (image_info->interlace == PartitionInterlace)
          {
            CloseBlob(image);
            AppendImageFormat("B",image->filename);
            status=OpenBlob(image_info,image,ReadBinaryType);
            if (status == False)
              ReaderExit(FileOpenWarning,"Unable to open file",image);
          }
        q=image->pixels;
        for (y=0; y < image->tile_info.y; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        for (y=0; y < (int) image->rows; y++)
        {
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
          p=scanline+packet_size*image->tile_info.x;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(q->blue,p);
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,span))
              ProgressMonitor(LoadImageText,i,span);
          i++;
        }
        count=image->tile_info.height-image->rows-image->tile_info.y;
        for (y=0; y < count; y++)
          (void) ReadBlob(image,packet_size*image->tile_info.width,
            (char *) scanline);
        if (image->matte)
          {
            /*
              Read matte channel.
            */
            if (image_info->interlace == PartitionInterlace)
              {
                CloseBlob(image);
                AppendImageFormat("A",image->filename);
                status=OpenBlob(image_info,image,ReadBinaryType);
                if (status == False)
                  ReaderExit(FileOpenWarning,"Unable to open file",image);
              }
            q=image->pixels;
            for (y=0; y < image->tile_info.y; y++)
              (void) ReadBlob(image,packet_size*image->tile_info.width,
                (char *) scanline);
            for (y=0; y < (int) image->rows; y++)
            {
              (void) ReadBlob(image,packet_size*image->tile_info.width,
                (char *) scanline);
              p=scanline+packet_size*image->tile_info.x;
              for (x=0; x < (int) image->columns; x++)
              {
                ReadQuantum(q->index,p);
                q++;
              }
              if (image->previous == (Image *) NULL)
                if (QuantumTick(i,span))
                  ProgressMonitor(LoadImageText,i,span);
              i++;
            }
            count=image->tile_info.height-image->rows-image->tile_info.y;
            for (y=0; y < count; y++)
              (void) ReadBlob(image,packet_size*image->tile_info.width,
                (char *) scanline);
          }
        if (image_info->interlace == PartitionInterlace)
          (void) strcpy(image->filename,image_info->filename);
        break;
      }
    }
    CondenseImage(image);
    if (EOFBlob(image))
      MagickWarning(CorruptImageWarning,"not enough pixels",image->filename);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    count=ReadBlob(image,packet_size*image->tile_info.width,(char *) scanline);
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
%   W r i t e R G B I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteRGBImage writes an image to a file in red, green, and
%  blue rasterfile format.
%
%  The format of the WriteRGBImage method is:
%
%      unsigned int WriteRGBImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteRGBImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteRGBImage(const ImageInfo *image_info,Image *image)
{
  int
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  unsigned int
    scene,
    status;

  image->depth=QuantumDepth;
  if (image_info->interlace != PartitionInterlace)
    {
      /*
        Open output image file.
      */
      status=OpenBlob(image_info,image,WriteBinaryType);
      if (status == False)
        WriterExit(FileOpenWarning,"Unable to open file",image);
    }
  scene=0;
  do
  {
    /*
      Convert MIFF to RGB raster pixels.
    */
    TransformRGBImage(image,RGBColorspace);
    switch (image_info->interlace)
    {
      case NoInterlace:
      default:
      {
        register unsigned char
          *q;

        unsigned char
          *pixels;

        unsigned short
          value;

        /*
          Allocate memory for pixels.
        */
        pixels=(unsigned char *)
          AllocateMemory(image->columns*sizeof(RunlengthPacket));
        if (pixels == (unsigned char *) NULL)
          WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
        /*
          No interlacing:  RGBRGBRGBRGBRGBRGB...
        */
        x=0;
        y=0;
        p=image->pixels;
        q=pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            WriteQuantum(p->red,q);
            WriteQuantum(p->green,q);
            WriteQuantum(p->blue,q);
            if (Latin1Compare(image_info->magick,"RGBA") == 0)
              WriteQuantum(image->matte ? p->index : Opaque,q);
            x++;
            if (x == (int) image->columns)
              {
                (void) WriteBlob(image,q-pixels,(char *) pixels);
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(y,image->rows))
                    ProgressMonitor(SaveImageText,y,image->rows);
                q=pixels;
                x=0;
                y++;
              }
          }
          p++;
        }
        FreeMemory((char *) pixels);
        break;
      }
      case LineInterlace:
      {
        /*
          Line interlacing:  RRR...GGG...BBB...RRR...GGG...BBB...
        */
        if (!UncondenseImage(image))
          return(False);
        for (y=0; y < (int) image->rows; y++)
        {
          p=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            WriteQuantumFile(p->red);
            p++;
          }
          p=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            WriteQuantumFile(p->green);
            p++;
          }
          p=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            WriteQuantumFile(p->blue);
            p++;
          }
          p=image->pixels+(y*image->columns);
          if (image->matte)
            for (x=0; x < (int) image->columns; x++)
            {
              WriteQuantumFile(p->index);
              p++;
            }
          if (QuantumTick(y,image->rows))
            ProgressMonitor(SaveImageText,y,image->rows);
        }
        break;
      }
      case PlaneInterlace:
      case PartitionInterlace:
      {
        /*
          Plane interlacing:  RRRRRR...GGGGGG...BBBBBB...
        */
        if (image_info->interlace == PartitionInterlace)
          {
            AppendImageFormat("R",image->filename);
            status=OpenBlob(image_info,image,WriteBinaryType);
            if (status == False)
              WriterExit(FileOpenWarning,"Unable to open file",image);
          }
        p=image->pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
            WriteQuantumFile(p->red);
          p++;
        }
        if (image_info->interlace == PartitionInterlace)
          {
            CloseBlob(image);
            AppendImageFormat("G",image->filename);
            status=OpenBlob(image_info,image,WriteBinaryType);
            if (status == False)
              WriterExit(FileOpenWarning,"Unable to open file",image);
          }
        ProgressMonitor(SaveImageText,100,400);
        p=image->pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
            WriteQuantumFile(p->green);
          p++;
        }
        if (image_info->interlace == PartitionInterlace)
          {
            CloseBlob(image);
            AppendImageFormat("B",image->filename);
            status=OpenBlob(image_info,image,WriteBinaryType);
            if (status == False)
              WriterExit(FileOpenWarning,"Unable to open file",image);
          }
        ProgressMonitor(SaveImageText,200,400);
        p=image->pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
            WriteQuantumFile(p->blue);
          p++;
        }
        if (image->matte)
          {
            ProgressMonitor(SaveImageText,300,400);
            p=image->pixels;
            if (image_info->interlace == PartitionInterlace)
              {
                CloseBlob(image);
                AppendImageFormat("A",image->filename);
                status=OpenBlob(image_info,image,WriteBinaryType);
                if (status == False)
                  WriterExit(FileOpenWarning,"Unable to open file",image);
              }
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
                WriteQuantumFile(p->index);
              p++;
            }
          }
        if (image_info->interlace == PartitionInterlace)
          (void) strcpy(image->filename,image_info->filename);
        ProgressMonitor(SaveImageText,400,400);
        break;
      }
    }
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
