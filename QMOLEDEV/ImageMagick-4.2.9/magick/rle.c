/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            RRRR   L      EEEEE                              %
%                            R   R  L      E                                  %
%                            RRRR   L      EEE                                %
%                            R R    L      E                                  %
%                            R  R   LLLLL  EEEEE                              %
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
%   R e a d R L E I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadRLEImage reads a run-length encoded Utah Raster Toolkit
%  image file and returns it.  It allocates the memory necessary for the new
%  Image structure and returns a pointer to the new image.
%
%  The format of the ReadRLEImage method is:
%
%      Image *ReadRLEImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadRLEImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadRLEImage(const ImageInfo *image_info)
{
#define SkipLinesOp  0x01
#define SetColorOp  0x02
#define SkipPixelsOp  0x03
#define ByteDataOp  0x05
#define RunDataOp  0x06
#define EOFOp  0x07

  char
    magick[12];

  Image
    *image;

  int
    opcode,
    operand,
    status,
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    background_color[256],
    *colormap,
    pixel,
    plane,
    *rle_pixels;

  unsigned int
    bits_per_pixel,
    flags,
    map_length,
    number_colormaps,
    number_planes;

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
  /*
    Determine if this is a RLE file.
  */
  status=ReadBlob(image,2,(char *) magick);
  if ((status == False) || (strncmp(magick,"\122\314",2) != 0))
    ReaderExit(CorruptImageWarning,"Not a RLE image file",image);
  do
  {
    /*
      Read image header.
    */
    (void) LSBFirstReadShort(image);
    (void) LSBFirstReadShort(image);
    image->columns=LSBFirstReadShort(image);
    image->rows=LSBFirstReadShort(image);
    if (image_info->ping)
      {
        CloseBlob(image);
        return(image);
      }
    image->packets=image->columns*image->rows;
    flags=ReadByte(image);
    image->matte=flags & 0x04;
    number_planes=ReadByte(image);
    bits_per_pixel=ReadByte(image);
    number_colormaps=ReadByte(image);
    map_length=1 << ReadByte(image);
    if ((number_planes == 0) || (number_planes == 2) || (bits_per_pixel != 8) ||
        (image->columns == 0))
      ReaderExit(CorruptImageWarning,"Unsupported RLE image file",image);
    if (flags & 0x02)
      {
        /*
          No background color-- initialize to black.
        */
        for (i=0; i < (int) number_planes; i++)
          background_color[i]=(unsigned char) 0;
        (void) ReadByte(image);
      }
    else
      {
        /*
          Initialize background color.
        */
        p=background_color;
        for (i=0; i < (int) number_planes; i++)
          *p++=(unsigned char) ReadByte(image);
      }
    if ((number_planes & 0x01) == 0)
      (void) ReadByte(image);
    colormap=(unsigned char *) NULL;
    if (number_colormaps != 0)
      {
        /*
          Read image colormaps.
        */
        colormap=(unsigned char *)
          AllocateMemory(number_colormaps*map_length*sizeof(unsigned char));
        if (colormap == (unsigned char *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        p=colormap;
        for (i=0; i < (int) number_colormaps; i++)
          for (j=0; j < (int) map_length; j++)
            *p++=XDownScale(LSBFirstReadShort(image));
      }
    if (flags & 0x08)
      {
        unsigned int
          length;

        /*
          Read image comment.
        */
        length=LSBFirstReadShort(image);
        image->comments=(char *) AllocateMemory(length*sizeof(char));
        if (image->comments == (char *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        (void) ReadBlob(image,length-1,(char *) image->comments);
        image->comments[length-1]='\0';
        if ((length & 0x01) == 0)
          (void) ReadByte(image);
      }
    /*
      Allocate RLE pixels.
    */
    if (image->matte)
      number_planes++;
    rle_pixels=(unsigned char *)
      AllocateMemory(image->packets*number_planes*sizeof(unsigned char));
    if (rle_pixels == (unsigned char *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    if ((flags & 0x01) && !(flags & 0x02))
      {
        /*
          Set background color.
        */
        p=rle_pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          if (!image->matte)
            for (j=0; j < (int) number_planes; j++)
              *p++=background_color[j];
          else
            {
              for (j=0; j < (int) (number_planes-1); j++)
                *p++=background_color[j];
              *p++=0;  /* initialize matte channel */
            }
        }
      }
    /*
      Read runlength-encoded image.
    */
    plane=0;
    x=0;
    y=0;
    opcode=ReadByte(image);
    do
    {
      switch (opcode & 0x3f)
      {
        case SkipLinesOp:
        {
          operand=ReadByte(image);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image);
          x=0;
          y+=operand;
          break;
        }
        case SetColorOp:
        {
          operand=ReadByte(image);
          plane=(unsigned char) operand;
          if (plane == 255)
            plane=number_planes-1;
          x=0;
          break;
        }
        case SkipPixelsOp:
        {
          operand=ReadByte(image);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image);
          x+=operand;
          break;
        }
        case ByteDataOp:
        {
          operand=ReadByte(image);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image);
          p=rle_pixels+((image->rows-y-1)*image->columns*number_planes)+
            x*number_planes+plane;
          operand++;
          for (i=0; i < operand; i++)
          {
            pixel=ReadByte(image);
            if ((y < (int) image->rows) && ((x+i) < (int) image->columns))
              *p=pixel;
            p+=number_planes;
          }
          if (operand & 0x01)
            (void) ReadByte(image);
          x+=operand;
          break;
        }
        case RunDataOp:
        {
          operand=ReadByte(image);
          if (opcode & 0x40)
            operand=LSBFirstReadShort(image);
          pixel=ReadByte(image);
          (void) ReadByte(image);
          operand++;
          p=rle_pixels+((image->rows-y-1)*image->columns*number_planes)+
            x*number_planes+plane;
          for (i=0; i < operand; i++)
          {
            if ((y < (int) image->rows) && ((x+i) < (int) image->columns))
              *p=pixel;
            p+=number_planes;
          }
          x+=operand;
          break;
        }
        default:
          break;
      }
      opcode=ReadByte(image);
    } while (((opcode & 0x3f) != EOFOp) && (opcode != EOF));
    if (number_colormaps != 0)
      {
        unsigned int
          mask;

        /*
          Apply colormap transformation to image.
        */
        mask=(map_length-1);
        p=rle_pixels;
        if (number_colormaps == 1)
          for (i=0; i < (int) image->packets; i++)
          {
            *p=(unsigned char) colormap[*p & mask];
            p++;
          }
        else
          if ((number_planes >= 3) && (number_colormaps >= 3))
            for (i=0; i < (int) image->packets; i++)
              for (j=0; j < (int) number_planes; j++)
              {
                *p=(unsigned char) colormap[j*map_length+(*p & mask)];
                p++;
              }
      }
    /*
      Initialize image structure.
    */
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    SetImage(image);
    q=image->pixels;
    if (number_planes >= 3)
      {
        /*
          Convert raster image to DirectClass runlength-encoded packets.
        */
        p=rle_pixels;
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            q->red=UpScale(*p++);
            q->green=UpScale(*p++);
            q->blue=UpScale(*p++);
            q->index=0;
            if (image->matte)
              q->index=UpScale(*p++);
            q->length=0;
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
      }
    else
      {
        /*
          Create colormap.
        */
        image->class=PseudoClass;
        if (number_colormaps == 0)
          map_length=256;
        image->colors=map_length;
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        p=colormap;
        if (number_colormaps == 0)
          for (i=0; i < (int) image->colors; i++)
          {
            /*
              Grayscale.
            */
            image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
            image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
            image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
          }
        else
          if (number_colormaps == 1)
            for (i=0; i < (int) image->colors; i++)
            {
              /*
                Pseudocolor.
              */
              image->colormap[i].red=(Quantum) UpScale(i);
              image->colormap[i].green=(Quantum) UpScale(i);
              image->colormap[i].blue=(Quantum) UpScale(i);
            }
          else
            for (i=0; i < (int) image->colors; i++)
            {
              image->colormap[i].red=UpScale(*p);
              image->colormap[i].green=UpScale(*(p+map_length));
              image->colormap[i].blue=UpScale(*(p+map_length*2));
              p++;
            }
        p=rle_pixels;
        if (!image->matte)
          {
            /*
              Convert raster image to PseudoClass runlength-encoded packets.
            */
            for (y=0; y < (int) image->rows; y++)
            {
              for (x=0; x < (int) image->columns; x++)
              {
                q->index=(unsigned short) (*p++);
                q->length=0;
                q++;
              }
              if (image->previous == (Image *) NULL)
                if (QuantumTick(y,image->rows))
                  ProgressMonitor(LoadImageText,y,image->rows);
            }
            SyncImage(image);
          }
        else
          {
            /*
              Image has a matte channel-- promote to DirectClass.
            */
            for (y=0; y < (int) image->rows; y++)
            {
              for (x=0; x < (int) image->columns; x++)
              {
                q->red=image->colormap[*p++].red;
                q->green=image->colormap[*p++].green;
                q->blue=image->colormap[*p++].blue;
                q->index=UpScale(*p++);
                q->length=0;
                q++;
              }
              if (image->previous == (Image *) NULL)
                if (QuantumTick(y,image->rows))
                  ProgressMonitor(LoadImageText,y,image->rows);
            }
            FreeMemory(image->colormap);
            image->colormap=(ColorPacket *) NULL;
            image->class=DirectClass;
            image->colors=0;
          }
      }
    if (number_colormaps != 0)
      FreeMemory((char *) colormap);
    FreeMemory((char *) rle_pixels);
    CondenseImage(image);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    (void) ReadByte(image);
    status=ReadBlob(image,2,(char *) magick);
    if ((status == True) && (strncmp(magick,"\122\314",2) == 0))
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
  } while ((status == True) && (strncmp(magick,"\122\314",2) == 0));
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseBlob(image);
  return(image);
}
