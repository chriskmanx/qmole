/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            H   H  DDDD   FFFFF                              %
%                            H   H  D   D  F                                  %
%                            HHHHH  D   D  FFF                                %
%                            H   H  D   D  F                                  %
%                            H   H  DDDD   F                                  %
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

#if defined(HasHDF)
#include "hdf.h"
#undef BSD
#undef LOCAL
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d H D F I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadHDFImage reads a Hierarchical Data Format image file and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadHDFImage method is:
%
%      Image *ReadHDFImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadHDFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadHDFImage(const ImageInfo *image_info)
{

  ClassType
    class;

  Image
    *image;

  int
    interlace,
    is_palette,
    status,
    y;

  int32
    height,
    length,
    width;

  register int
    i,
    x;

  register unsigned char
    *p;

  register RunlengthPacket
    *q;

  uint16
    reference;

  unsigned char
    *hdf_pixels;

  unsigned int
    packet_size;

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
  CloseBlob(image);
  /*
    Read HDF image.
  */
  class=DirectClass;
  status=DF24getdims(image->filename,&width,&height,&interlace);
  if (status == -1)
    {
      class=PseudoClass;
      status=DFR8getdims(image->filename,&width,&height,&is_palette);
    }
  if (status == -1)
    ReaderExit(CorruptImageWarning,
      "Image file or does not contain any image data",image);
  do
  {
    /*
      Initialize image structure.
    */
    image->class=class;
    image->columns=width;
    image->rows=height;
    if (image->class == PseudoClass)
      image->colors=256;
    if (image_info->ping)
      {
        CloseBlob(image);
        return(image);
      }
    image->packets=image->columns*image->rows;
    packet_size=1;
    if (image->class == DirectClass)
      packet_size=3;
    hdf_pixels=(unsigned char *)
      AllocateMemory(packet_size*image->packets*sizeof(unsigned char));
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->packets*sizeof(RunlengthPacket));
    if ((hdf_pixels == (unsigned char *) NULL) ||
        (image->pixels == (RunlengthPacket *) NULL))
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    SetImage(image);
    q=image->pixels;
    if (image->class == PseudoClass)
      {
        unsigned char
          *hdf_palette;

        /*
          Create colormap.
        */
        hdf_palette=(unsigned char *) AllocateMemory(768*sizeof(unsigned char));
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
        if ((hdf_palette == (unsigned char *) NULL) ||
            (image->colormap == (ColorPacket *) NULL))
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        (void) DFR8getimage(image->filename,hdf_pixels,(int) image->columns,
          (int) image->rows,hdf_palette);
        reference=DFR8lastref();
        /*
          Convert HDF raster image to PseudoClass runlength-encoded packets.
        */
        p=hdf_palette;
        if (is_palette)
          for (i=0; i < 256; i++)
          {
            image->colormap[i].red=UpScale(*p++);
            image->colormap[i].green=UpScale(*p++);
            image->colormap[i].blue=UpScale(*p++);
          }
        else
          for (i=0; i < (int) image->colors; i++)
          {
            image->colormap[i].red=(Quantum) UpScale(i);
            image->colormap[i].green=(Quantum) UpScale(i);
            image->colormap[i].blue=(Quantum) UpScale(i);
          }
        FreeMemory((char *) hdf_palette);
        p=hdf_pixels;
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            q->index=(*p++);
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
        int
          y;

        /*
          Convert HDF raster image to DirectClass runlength-encoded packets.
        */
        (void) DF24getimage(image->filename,(void *) hdf_pixels,image->columns,
          image->rows);
        reference=DF24lastref();
        p=hdf_pixels;
        image->interlace=interlace ? PlaneInterlace : NoInterlace;
        q=image->pixels;
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            q->red=UpScale(*p++);
            q->green=UpScale(*p++);
            q->blue=UpScale(*p++);
            q->index=0;
            q->length=0;
            q++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
      }
    length=DFANgetlablen(image->filename,DFTAG_RIG,reference);
    if (length > 0)
      {
        /*
          Read the image label.
        */
        length+=MaxTextExtent;
        image->label=(char *) AllocateMemory(length*sizeof(char));
        if (image->label != (char *) NULL)
          DFANgetlabel(image->filename,DFTAG_RIG,reference,image->label,length);
      }
    length=DFANgetdesclen(image->filename,DFTAG_RIG,reference);
    if (length > 0)
      {
        /*
          Read the image comments.
        */
        length+=MaxTextExtent;
        image->comments=(char *) AllocateMemory(length*sizeof(char));
        if (image->comments != (char *) NULL)
          DFANgetdesc(image->filename,DFTAG_RIG,reference,image->comments,
            length);
      }
    FreeMemory((char *) hdf_pixels);
    if (image->class == PseudoClass)
      SyncImage(image);
    CondenseImage(image);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    class=DirectClass;
    status=DF24getdims(image->filename,&width,&height,&interlace);
    if (status == -1)
      {
        class=PseudoClass;
        status=DFR8getdims(image->filename,&width,&height,&is_palette);
      }
    if (status != -1)
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
  } while (status != -1);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}
#else
Export Image *ReadHDFImage(const ImageInfo *image_info)
{
  MagickWarning(MissingDelegateWarning,"HDF library is not available",
    image_info->filename);
  return((Image *) NULL);
}
#endif

#if defined(HasHDF)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e H D F I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteHDFImage writes an image in the Hierarchial Data Format image
%  format.
%
%  The format of the WriteHDFImage method is:
%
%      unsigned int WriteHDFImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteHDFImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteHDFImage(const ImageInfo *image_info,Image *image)
{
  int
    status;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  uint16
    reference;

  unsigned char
    *hdf_pixels;

  unsigned int
    compression,
    packet_size,
    scene;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  CloseBlob(image);
  scene=0;
  do
  {
    /*
      Initialize raster file header.
    */
    TransformRGBImage(image,RGBColorspace);
    packet_size=1;
    if (image->class == DirectClass)
      packet_size=3;
    hdf_pixels=(unsigned char *) AllocateMemory(packet_size*image->columns*
      image->rows*sizeof(unsigned char));
    if (hdf_pixels == (unsigned char *) NULL)
      WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    p=image->pixels;
    if (!IsPseudoClass(image) && !IsGrayImage(image))
      {
        /*
          Convert DirectClass packet to HDF pixels.
        */
        q=hdf_pixels;
        switch (image_info->interlace)
        {
          case NoInterlace:
          default:
          {
            /*
              No interlacing:  RGBRGBRGBRGBRGBRGB...
            */
            DF24setil(DFIL_PIXEL);
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
              {
                *q++=DownScale(p->red);
                *q++=DownScale(p->green);
                *q++=DownScale(p->blue);
              }
              p++;
              if (image->previous == (Image *) NULL)
                if (QuantumTick(i,image->packets))
                  ProgressMonitor(SaveImageText,i,image->packets);
            }
            break;
          }
          case LineInterlace:
          {
            register int
              x,
              y;

            /*
              Line interlacing:  RRR...GGG...BBB...RRR...GGG...BBB...
            */
            if (!UncondenseImage(image))
              return(False);
            DF24setil(DFIL_LINE);
            for (y=0; y < (int) image->rows; y++)
            {
              p=image->pixels+(y*image->columns);
              for (x=0; x < (int) image->columns; x++)
              {
                *q++=DownScale(p->red);
                p++;
              }
              p=image->pixels+(y*image->columns);
              for (x=0; x < (int) image->columns; x++)
              {
                *q++=DownScale(p->green);
                p++;
              }
              p=image->pixels+(y*image->columns);
              for (x=0; x < (int) image->columns; x++)
              {
                *q++=DownScale(p->blue);
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
            DF24setil(DFIL_PLANE);
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
                *q++=DownScale(p->red);
              p++;
            }
            ProgressMonitor(SaveImageText,100,400);
            p=image->pixels;
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
                *q++=DownScale(p->green);
              p++;
            }
            ProgressMonitor(SaveImageText,250,400);
            p=image->pixels;
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
                *q++=DownScale(p->blue);
              p++;
            }
            ProgressMonitor(SaveImageText,400,400);
            break;
          }
        }
        if (scene == 0)
          status=DF24putimage(image->filename,(void *) hdf_pixels,
            image->columns,image->rows);
        else
          status=DF24addimage(image->filename,(void *) hdf_pixels,
            image->columns,image->rows);
        reference=DF24lastref();
      }
    else
      {
        /*
          Convert PseudoClass packet to HDF pixels.
        */
        q=hdf_pixels;
        if (IsGrayImage(image))
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
              *q++=DownScale(p->red);
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
        else
          {
            unsigned char
              *hdf_palette;

            hdf_palette=(unsigned char *)
              AllocateMemory(768*sizeof(unsigned char));
            if (hdf_palette == (unsigned char *) NULL)
              WriterExit(ResourceLimitWarning,"Memory allocation failed",
                image);
            q=hdf_palette;
            for (i=0; i < (int) image->colors; i++)
            {
              *q++=DownScale(image->colormap[i].red);
              *q++=DownScale(image->colormap[i].green);
              *q++=DownScale(image->colormap[i].blue);
            }
            (void) DFR8setpalette(hdf_palette);
            FreeMemory(hdf_palette);
            q=hdf_pixels;
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
                *q++=p->index;
              p++;
              if (image->previous == (Image *) NULL)
                if (QuantumTick(i,image->packets))
                  ProgressMonitor(SaveImageText,i,image->packets);
            }
          }
        compression=image_info->compression == NoCompression ? 0 : DFTAG_RLE;
        if (scene == 0)
          status=DFR8putimage(image->filename,(void *) hdf_pixels,
            image->columns,image->rows,compression);
        else
          status=DFR8addimage(image->filename,(void *) hdf_pixels,
            image->columns,image->rows,compression);
        reference=DFR8lastref();
      }
    if (image->label != (char *) NULL)
      (void) DFANputlabel(image->filename,DFTAG_RIG,reference,image->label);
    if (image->comments != (char *) NULL)
      (void) DFANputdesc(image->filename,DFTAG_RIG,reference,image->comments,
        Extent(image->comments)+1);
    FreeMemory(hdf_pixels);
    if (image->next == (Image *) NULL)
      break;
    image->next->file=image->file;
    image=image->next;
    ProgressMonitor(SaveImagesText,scene++,GetNumberScenes(image));
  } while (image_info->adjoin);
  if (image_info->adjoin)
    while (image->previous != (Image *) NULL)
      image=image->previous;
  return(status != -1);
}
#else
Export unsigned int WriteHDFImage(const ImageInfo *image_info,Image *image)
{
  MagickWarning(MissingDelegateWarning,"HDF library is not available",
    image->filename);
  return(False);
}
#endif
