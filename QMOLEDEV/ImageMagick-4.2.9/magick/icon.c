/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        IIIII   CCCC   OOO   N   N                           %
%                          I    C      O   O  NN  N                           %
%                          I    C      O   O  N N N                           %
%                          I    C      O   O  N  NN                           %
%                        IIIII   CCCC   OOO   N   N                           %
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
%   R e a d I C O N I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadICONImage reads a Microsoft icon image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadICONImage method is:
%
%      Image *ReadICONImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadICONImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadICONImage(const ImageInfo *image_info)
{
#define MaxIcons  256

  typedef struct _IconEntry
  {
    unsigned char
      width,
      height,
      colors,
      reserved;

    short int
      planes,
      bits_per_pixel;

    unsigned int
      size,
      offset;
  } IconEntry;

  typedef struct _IconFile
  {
    short
      reserved,
      resource_type,
      count;

    IconEntry
      directory[MaxIcons];
  } IconFile;

  typedef struct _IconHeader
  {
    unsigned long
      size,
      width,
      height;

    unsigned short
      planes,
      bits_per_pixel;

    unsigned long
      compression,
      image_size,
      x_pixels,
      y_pixels,
      number_colors,
      colors_important;
  } IconHeader;

  IconFile
    icon_file;

  IconHeader
    icon_header;

  Image
    *image;

  register int
    bit,
    i,
    x,
    y;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *icon_colormap,
    *icon_pixels;

  unsigned int
    bytes_per_line,
    offset,
    status;

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
  icon_file.reserved=LSBFirstReadShort(image);
  icon_file.resource_type=LSBFirstReadShort(image);
  icon_file.count=LSBFirstReadShort(image);
  if ((icon_file.reserved != 0) || (icon_file.resource_type != 1) ||
      (icon_file.count > MaxIcons))
    ReaderExit(CorruptImageWarning,"Not a ICO image file",image);
  for (i=0; i < icon_file.count; i++)
  {
    icon_file.directory[i].width=ReadByte(image);
    icon_file.directory[i].height=ReadByte(image);
    icon_file.directory[i].colors=ReadByte(image);
    icon_file.directory[i].reserved=ReadByte(image);
    icon_file.directory[i].planes=LSBFirstReadShort(image);
    icon_file.directory[i].bits_per_pixel=LSBFirstReadShort(image);
    icon_file.directory[i].size=LSBFirstReadLong(image);
    icon_file.directory[i].offset=LSBFirstReadLong(image);
  }
  for (i=0; i < icon_file.count; i++)
  {
    /*
      Verify ICON identifier.
    */
    (void) SeekBlob(image,icon_file.directory[i].offset,SEEK_SET);
    icon_header.size=LSBFirstReadLong(image);
    icon_header.width=LSBFirstReadLong(image);
    icon_header.height=LSBFirstReadLong(image);
    icon_header.planes=LSBFirstReadShort(image);
    icon_header.bits_per_pixel=LSBFirstReadShort(image);
    icon_header.compression=LSBFirstReadLong(image);
    icon_header.image_size=LSBFirstReadLong(image);
    icon_header.x_pixels=LSBFirstReadLong(image);
    icon_header.y_pixels=LSBFirstReadLong(image);
    icon_header.number_colors=LSBFirstReadLong(image);
    icon_header.colors_important=LSBFirstReadLong(image);
    image->columns=(unsigned int) icon_header.width;
    image->rows=(unsigned int) icon_header.height;
    image->class=PseudoClass;
    image->colors=image->colors=1 << icon_header.bits_per_pixel;
    if (image_info->ping)
      {
        CloseBlob(image);
        return(image);
      }
    /*
      Allocate image colormap.
    */
    image->colormap=(ColorPacket *)
      AllocateMemory(image->colors*sizeof(ColorPacket));
    if (image->colormap == (ColorPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    /*
      Read ICON raster colormap.
    */
    icon_colormap=(unsigned char *)
      AllocateMemory(4*image->colors*sizeof(unsigned char));
    if (icon_colormap == (unsigned char *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    (void) ReadBlob(image,4*image->colors,(char *) icon_colormap);
    p=icon_colormap;
    for (x=0; x < (int) image->colors; x++)
    {
      image->colormap[x].blue=UpScale(*p++);
      image->colormap[x].green=UpScale(*p++);
      image->colormap[x].red=UpScale(*p++);
      p++;
    }
    FreeMemory((char *) icon_colormap);
    /*
      Read image data.
    */
    icon_pixels=(unsigned char *)
      AllocateMemory(icon_file.directory[i].size*sizeof(unsigned char));
    if (icon_pixels == (unsigned char *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    (void) ReadBlob(image,icon_file.directory[i].size,(char *) icon_pixels);
    /*
      Initialize image structure.
    */
    image->columns=icon_file.directory[i].width;
    image->rows=icon_file.directory[i].height;
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    SetImage(image);
    /*
      Convert ICON raster image to runlength-encoded packets.
    */
    bytes_per_line=((image->columns*icon_header.bits_per_pixel+31)/32)*4;
    switch (icon_header.bits_per_pixel)
    {
      case 1:
      {
        /*
          Convert bitmap scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=icon_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < ((int) image->columns-7); x+=8)
          {
            for (bit=0; bit < 8; bit++)
            {
              q->index=((*p) & (0x80 >> bit) ? 0x01 : 0x00);
              q->length=0;
              q++;
            }
            p++;
          }
          if ((image->columns % 8) != 0)
            {
              for (bit=0; bit < (int) (image->columns % 8); bit++)
              {
                q->index=((*p) & (0x80 >> bit) ? 0x01 : 0x00);
                q->length=0;
                q++;
              }
              p++;
            }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      case 4:
      {
        /*
          Convert PseudoColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=icon_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < ((int) image->columns-1); x+=2)
          {
            q->index=(*p >> 4) & 0xf;
            q->length=0;
            q++;
            q->index=(*p) & 0xf;
            q->length=0;
            p++;
            q++;
          }
          if ((image->columns % 2) != 0)
            {
              q->index=(*p >> 4) & 0xf;
              q->length=0;
              q++;
              p++;
            }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      case 8:
      {
        /*
          Convert PseudoColor scanline to runlength-encoded color packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=icon_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=(*p++);
            q->length=0;
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      case 16:
      {
        /*
          Convert PseudoColor scanline to runlength-encoded color packets.
        */
        if (icon_header.compression == 1)
          bytes_per_line=image->columns << 1;
        for (y=image->rows-1; y >= 0; y--)
        {
          p=icon_pixels+(image->rows-y-1)*bytes_per_line;
          q=image->pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=(*p++);
            q->index|=(*p++) << 8;
            q->length=0;
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        break;
      }
      default:
        ReaderExit(CorruptImageWarning,"Not a ICO image file",image);
    }
    SyncImage(image);
    /*
      Convert bitmap scanline to runlength-encoded color packets.
    */
    image->class=DirectClass;
    image->matte=True;
    offset=image->rows*bytes_per_line;
    bytes_per_line=((image->columns+31)/32)*4;
    for (y=image->rows-1; y >= 0; y--)
    {
      p=icon_pixels+offset+(image->rows-y-1)*bytes_per_line;
      q=image->pixels+(y*image->columns);
      for (x=0; x < ((int) image->columns-7); x+=8)
      {
        for (bit=0; bit < 8; bit++)
        {
          q->index=((*p) & (0x80 >> bit) ? Transparent : Opaque);
          q->length=0;
          q++;
        }
        p++;
      }
      if ((image->columns % 8) != 0)
        {
          for (bit=0; bit < (int) (image->columns % 8); bit++)
          {
            q->index=((*p) & (0x80 >> bit) ? Transparent : Opaque);
            q->length=0;
            q++;
          }
          p++;
        }
      if (image->previous == (Image *) NULL)
        if (QuantumTick(y,image->rows))
          ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
    }
    FreeMemory((char *) icon_pixels);
    CondenseImage(image);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    if (i < (icon_file.count-1))
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
  }
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseBlob(image);
  return(image);
}
