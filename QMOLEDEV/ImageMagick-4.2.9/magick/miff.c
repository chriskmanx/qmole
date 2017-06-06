/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        M   M  IIIII  FFFFF  FFFFF                           %
%                        MM MM    I    F      F                               %
%                        M M M    I    FFF    FFF                             %
%                        M   M    I    F      F                               %
%                        M   M  IIIII  F      F                               %
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
#if defined(HasBZLIB)
#include "bzlib.h"
#endif
#if defined(HasZLIB)
#include "zlib.h"
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d M I F F I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadMIFFImage reads a MIFF image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadMIFFImage method is:
%
%      Image *ReadMIFFImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image: Method ReadMIFFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadMIFFImage(const ImageInfo *image_info)
{
  char
    id[MaxTextExtent],
    keyword[MaxTextExtent],
    value[MaxTextExtent];

  ColorPacket
    color;

  Image
    *image;

  register int
    c,
    i;

  register unsigned char
    *p;

  unsigned int
    length,
    packet_size,
    status;

  unsigned long
    count,
    max_packets;

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
  image->depth=8;
  /*
    Decode image header;  header terminates one character beyond a ':'.
  */
  c=ReadByte(image);
  if (c == EOF)
    {
      DestroyImage(image);
      return((Image *) NULL);
    }
  *id='\0';
  do
  {
    /*
      Decode image header;  header terminates one character beyond a ':'.
    */
    image->compression=NoCompression;
    while (isgraph(c) && (c != ':'))
    {
      register char
        *p;

      if (c == '{')
        {
          /*
            Read comment-- any text between { }.
          */
          if (image->comments != (char *) NULL)
            {
              length=Extent(image->comments);
              p=image->comments+length;
            }
          else
            {
              length=MaxTextExtent;
              image->comments=(char *) AllocateMemory(length*sizeof(char));
              p=image->comments;
            }
          for ( ; image->comments != (char *) NULL; p++)
          {
            c=ReadByte(image);
            if ((c == EOF) || (c == '}'))
              break;
            if ((p-image->comments+1) >= (int) length)
              {
                *p='\0';
                length<<=1;
                image->comments=(char *) ReallocateMemory((char *)
                  image->comments,length*sizeof(char));
                if (image->comments == (char *) NULL)
                  break;
                p=image->comments+Extent(image->comments);
              }
            *p=(unsigned char) c;
          }
          if (image->comments == (char *) NULL)
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          *p='\0';
          c=ReadByte(image);
        }
      else
        if (isalnum(c))
          {
            /*
              Determine a keyword and its value.
            */
            p=keyword;
            do
            {
              if ((p-keyword) < (MaxTextExtent-1))
                *p++=(char) c;
              c=ReadByte(image);
            } while (isalnum(c) || (c == '-'));
            *p='\0';
            while (isspace(c) || (c == '='))
              c=ReadByte(image);
            p=value;
            if (c != '"')
              while (!isspace(c) && (c != EOF))
              {
                if ((p-value) < (MaxTextExtent-1))
                  *p++=(char) c;
                c=ReadByte(image);
              }
            else
              {
                c=ReadByte(image);
                while ((c != '"') && (c != EOF))
                {
                  if ((p-value) < (MaxTextExtent-1))
                    *p++=(char) c;
                  c=ReadByte(image);
                }
              }
            *p='\0';
            /*
              Assign a value to the specified keyword.
            */
            if (Latin1Compare(keyword,"background-color") == 0)
              {
                (void) QueryColorDatabase(value,&color);
                image->background_color.red=XDownScale(color.red);
                image->background_color.green=XDownScale(color.green);
                image->background_color.blue=XDownScale(color.blue);
                image->background_color.index=0;
              }
            if (Latin1Compare(keyword,"blue-primary") == 0)
              (void) sscanf(value,"%lf,%lf",&image->chromaticity.blue_primary.x,
                &image->chromaticity.blue_primary.y);
            if (Latin1Compare(keyword,"border-color") == 0)
              {
                (void) QueryColorDatabase(value,&color);
                image->border_color.red=XDownScale(color.red);
                image->border_color.green=XDownScale(color.green);
                image->border_color.blue=XDownScale(color.blue);
                image->border_color.index=0;
              }
            if (Latin1Compare(keyword,"class") == 0)
              {
                if (Latin1Compare(value,"PseudoClass") == 0)
                  image->class=PseudoClass;
                else
                  if (Latin1Compare(value,"DirectClass") == 0)
                    image->class=DirectClass;
                  else
                    image->class=UndefinedClass;
              }
            if (Latin1Compare(keyword,"colors") == 0)
              image->colors=(unsigned int) atoi(value);
            if (Latin1Compare(keyword,"color-profile") == 0)
              image->color_profile.length=(unsigned int) atoi(value);
            if (Latin1Compare(keyword,"colorspace") == 0)
              {
                if (Latin1Compare(value,"CMYK") == 0)
                  image->colorspace=CMYKColorspace;
                else
                  if (Latin1Compare(value,"RGB") == 0)
                    image->colorspace=RGBColorspace;
              }
            if (Latin1Compare(keyword,"compression") == 0)
              {
                if (Latin1Compare(value,"Zip") == 0)
                  image->compression=ZipCompression;
                else
                  if (Latin1Compare(value,"BZip") == 0)
                    image->compression=BZipCompression;
                  else
                    if (Latin1Compare(value,"RunlengthEncoded") == 0)
                      image->compression=RunlengthEncodedCompression;
                    else
                      image->compression=UndefinedCompression;
              }
            if (Latin1Compare(keyword,"columns") == 0)
              image->columns=(unsigned int) atoi(value);
            if (Latin1Compare(keyword,"delay") == 0)
              {
                if (image_info->delay == (char *) NULL)
                  image->delay=atoi(value);
              }
            if (Latin1Compare(keyword,"depth") == 0)
              image->depth=atoi(value) <= 8 ? 8 : 16;
            if (Latin1Compare(keyword,"dispose") == 0)
              {
              if (image_info->dispose == (char *) NULL)
                image->dispose=atoi(value);
              }
            if (Latin1Compare(keyword,"gamma") == 0)
              image->gamma=atof(value);
            if (Latin1Compare(keyword,"green-primary") == 0)
              (void) sscanf(value,"%lf,%lf",
                &image->chromaticity.green_primary.x,
                &image->chromaticity.green_primary.y);
            if (Latin1Compare(keyword,"id") == 0)
              (void) strcpy(id,value);
            if (Latin1Compare(keyword,"iterations") == 0)
              {
                if (image_info->iterations == (char *) NULL)
                  image->iterations=atoi(value);
              }
            if (Latin1Compare(keyword,"label") == 0)
              (void) CloneString(&image->label,value);
            if ((Latin1Compare(keyword,"matte") == 0) ||
                (Latin1Compare(keyword,"alpha") == 0))
              {
                if ((Latin1Compare(value,"True") == 0) ||
                    (Latin1Compare(value,"true") == 0))
                  image->matte=True;
                else
                  image->matte=False;
              }
            if (Latin1Compare(keyword,"matte-color") == 0)
              {
                (void) QueryColorDatabase(value,&color);
                image->matte_color.red=XDownScale(color.red);
                image->matte_color.green=XDownScale(color.green);
                image->matte_color.blue=XDownScale(color.blue);
                image->matte_color.index=0;
              }
            if (Latin1Compare(keyword,"montage") == 0)
              (void) CloneString(&image->montage,value);
            if (Latin1Compare(keyword,"page") == 0)
              {
                if (image_info->page == (char *) NULL)
                  image->page=PostscriptGeometry(value);
              }
            if (Latin1Compare(keyword,"packets") == 0)
              image->packets=(unsigned int) atoi(value);
            if (Latin1Compare(keyword,"red-primary") == 0)
              (void) sscanf(value,"%lf,%lf",&image->chromaticity.red_primary.x,
                &image->chromaticity.red_primary.y);
            if (Latin1Compare(keyword,"rendering-intent") == 0)
              {
                if (Latin1Compare(value,"saturation") == 0)
                  image->rendering_intent=SaturationIntent;
                else
                  if (Latin1Compare(value,"perceptual") == 0)
                    image->rendering_intent=PerceptualIntent;
                  else
                    if (Latin1Compare(value,"absolute") == 0)
                      image->rendering_intent=AbsoluteIntent;
                    else
                      if (Latin1Compare(value,"relative") == 0)
                        image->rendering_intent=RelativeIntent;
                      else
                        image->rendering_intent=UndefinedIntent;
              }
            if (Latin1Compare(keyword,"resolution") == 0)
              (void) sscanf(value,"%lfx%lf",&image->x_resolution,
                &image->y_resolution);
            if (Latin1Compare(keyword,"rows") == 0)
              image->rows=(unsigned int) atoi(value);
            if (Latin1Compare(keyword,"scene") == 0)
              image->scene=(unsigned int) atoi(value);
            if (Latin1Compare(keyword,"signature") == 0)
              (void) CloneString(&image->signature,value);
            if (Latin1Compare(keyword,"units") == 0)
              {
                if (Latin1Compare(value,"undefined") == 0)
                  image->units=UndefinedResolution;
                else
                  if (Latin1Compare(value,"pixels-per-inch") == 0)
                    image->units=PixelsPerInchResolution;
                  else
                    if (Latin1Compare(value,"pixels-per-centimeter") == 0)
                      image->units=PixelsPerCentimeterResolution;
              }
            if (Latin1Compare(keyword,"white-point") == 0)
              (void) sscanf(value,"%lf,%lf",&image->chromaticity.white_point.x,
                &image->chromaticity.white_point.y);
          }
        else
          c=ReadByte(image);
      while (isspace(c))
        c=ReadByte(image);
    }
    (void) ReadByte(image);
    /*
      Verify that required image information is defined.
    */
    if ((strcmp(id,"ImageMagick") != 0) || (image->class == UndefinedClass) ||
        (image->compression == UndefinedCompression) || (image->columns == 0) ||
        (image->rows == 0))
      ReaderExit(CorruptImageWarning,"Incorrect image header in file",image);
    if (image_info->ping)
      {
        CloseBlob(image);
        return(image);
      }
    if (image->montage != (char *) NULL)
      {
        register char
          *p;

        /*
          Image directory.
        */
        image->directory=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
        if (image->directory == (char *) NULL)
          ReaderExit(CorruptImageWarning,"Unable to read image data",image);
        p=image->directory;
        do
        {
          *p='\0';
          if (((Extent(image->directory)+1) % MaxTextExtent) == 0)
            {
              /*
                Allocate more memory for the image directory.
              */
              image->directory=(char *) ReallocateMemory((char *)
                image->directory,(Extent(image->directory)+MaxTextExtent+1)*
                sizeof(char));
              if (image->directory == (char *) NULL)
                ReaderExit(CorruptImageWarning,"Unable to read image data",
                  image);
              p=image->directory+Extent(image->directory);
            }
          c=ReadByte(image);
          *p++=(unsigned char) c;
        } while (c != '\0');
      }
    if (image->color_profile.length > 0)
      {
        /*
          Color profile.
        */
        image->color_profile.info=(unsigned char *)
          AllocateMemory(image->color_profile.length*sizeof(unsigned char));
        if (image->color_profile.info == (unsigned char *) NULL)
          ReaderExit(CorruptImageWarning,"Unable to read color profile",
            image);
        (void) ReadBlob(image,image->color_profile.length,
          (char *) image->color_profile.info);
      }
    if (image->class == PseudoClass)
      {
        unsigned int
          colors;

        unsigned short
          value;

        /*
          PseudoClass image cannot have matte data.
        */
        if (image->matte)
          ReaderExit(CorruptImageWarning,
            "Matte images must be DirectClass",image);
        /*
          Create image colormap.
        */
        colors=image->colors;
        if (colors == 0)
          colors=256;
        image->colormap=(ColorPacket *)
          AllocateMemory(colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        if (image->colors == 0)
          for (i=0; i < (int) colors; i++)
          {
            image->colormap[i].red=(Quantum) UpScale(i);
            image->colormap[i].green=(Quantum) UpScale(i);
            image->colormap[i].blue=(Quantum) UpScale(i);
            image->colors++;
          }
        else
          {
            unsigned char
              *colormap;

            /*
              Read image colormap from file.
            */
            packet_size=3*(image->depth >> 3);
            colormap=(unsigned char *)
              AllocateMemory(packet_size*image->colors*sizeof(unsigned char));
            if (colormap == (unsigned char *) NULL)
              ReaderExit(ResourceLimitWarning,"Memory allocation failed",
                image);
            (void) ReadBlob(image,packet_size*image->colors,
              (char *) colormap);
            p=colormap;
            for (i=0; i < (int) image->colors; i++)
            {
              ReadQuantum(image->colormap[i].red,p);
              ReadQuantum(image->colormap[i].green,p);
              ReadQuantum(image->colormap[i].blue,p);
            }
            FreeMemory((char *) colormap);
          }
      }
    /*
      Determine packed packet size.
    */
    if (image->class == PseudoClass)
      {
        image->packet_size=1;
        if (image->colors > 256)
          image->packet_size++;
      }
    else
      {
        image->packet_size=3*(image->depth >> 3);
        if (image->matte || (image->colorspace == CMYKColorspace))
          image->packet_size++;
      }
    if (image->compression == RunlengthEncodedCompression)
      image->packet_size++;
    packet_size=image->packet_size;
    if (image->compression == ZipCompression)
      packet_size=1;
    /*
      Allocate image pixels.
    */
    if (image->compression == NoCompression)
      image->packets=image->columns*image->rows;
    max_packets=image->packets;
    if (image->packets == 0)
      max_packets=image->columns*image->rows;
    image->packed_pixels=(unsigned char *) AllocateMemory((unsigned int)
      max_packets*packet_size*sizeof(unsigned char));
    if (image->packed_pixels == (unsigned char *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    /*
      Read image pixels from file.
    */
    if ((image->compression != RunlengthEncodedCompression) ||
        (image->packets != 0))
      (void) ReadBlob(image,(unsigned int) max_packets*packet_size,
        (char *) image->packed_pixels);
    else
      {
        /*
          Number of runlength packets is unspecified.
        */
        count=0;
        p=image->packed_pixels;
        do
        {
          (void) ReadBlob(image,packet_size,(char *) p);
          image->packets++;
          p+=(packet_size-1);
          count+=(*p+1);
          p++;
        }
        while (count < (image->columns*image->rows));
      }
    if (image->compression == BZipCompression)
      {
        unsigned char
          *compressed_pixels;

        /*
          Uncompress image pixels with BZip decoding.
        */
        compressed_pixels=image->packed_pixels;
        max_packets=image->columns*image->rows*image->packet_size;
        image->packed_pixels=(unsigned char *)
          AllocateMemory((max_packets+8)*sizeof(unsigned char));
        if (image->packed_pixels == (unsigned char *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        status=True;
#if defined(HasBZLIB)
        {
          unsigned int
            compressed_packets;

          compressed_packets=max_packets;
          status=bzBuffToBuffDecompress((char *) image->packed_pixels,
            &compressed_packets,(char *) compressed_pixels,image->packets,
            image_info->verbose,False);
          max_packets=compressed_packets;
        }
#endif
        image->packets=(unsigned int) (max_packets/image->packet_size);
        FreeMemory((char *) compressed_pixels);
        if (status)
          ReaderExit(DelegateWarning,"Unable to uncompress image",image);
      }
    if (image->compression == ZipCompression)
      {
        unsigned char
          *compressed_pixels;

        /*
          Uncompress image pixels with Zip decoding.
        */
        compressed_pixels=image->packed_pixels;
        max_packets=image->columns*image->rows*image->packet_size;
        image->packed_pixels=(unsigned char *)
          AllocateMemory((max_packets+8)*sizeof(unsigned char));
        if (image->packed_pixels == (unsigned char *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        status=True;
#if defined(HasZLIB)
        status=uncompress(image->packed_pixels,&max_packets,compressed_pixels,
          image->packets);
#endif
        image->packets=(unsigned int) (max_packets/image->packet_size);
        FreeMemory((char *) compressed_pixels);
        if (status)
          ReaderExit(DelegateWarning,"Unable to uncompress image",image);
      }
    /*
      Unpack the packed image pixels into runlength-encoded pixel packets.
    */
    status=RunlengthDecodeImage(image);
    if (status == False)
      {
        DestroyImages(image);
        return((Image *) NULL);
      }
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    do
    {
      c=ReadByte(image);
    } while (!isgraph(c) && (c != EOF));
    if (c != EOF)
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
  } while (c != EOF);
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
%   W r i t e M I F F I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteMIFFImage writes an image to a file.
%
%  The format of the WriteMIFFImage method is:
%
%      unsigned int WriteMIFFImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method WriteMIFFImage return True if the image is written.
%      False is returned if there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: A pointer to a Image structure.
%
%
*/
Export unsigned int WriteMIFFImage(const ImageInfo *image_info,Image *image)
{
  char
    buffer[MaxTextExtent],
    color[MaxTextExtent];

  CompressionType
    compression;

  register int
    i;

  unsigned int
    scene,
    status;

  unsigned long
    packets;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  (void) IsPseudoClass(image);
  CondenseImage(image);
  if (image->class == DirectClass)
    {
      if (image->packets >= ((3*image->columns*image->rows) >> 2))
        image->compression=NoCompression;
    }
  else
    if (image->packets >= ((image->columns*image->rows) >> 1))
      image->compression=NoCompression;
  compression=image_info->compression;
#if defined(HasZLIB)
  if (compression == UndefinedCompression)
    compression=ZipCompression;
#endif
#if defined(HasBZLIB)
  if (compression == UndefinedCompression)
    compression=BZipCompression;
#endif
  if (compression == UndefinedCompression)
    compression=RunlengthEncodedCompression;
  (void) strcpy((char *) image_info->magick,"MIFF");
  scene=0;
  do
  {
    /*
      Pack image pixels.
    */
    if (((image_info->colorspace != UndefinedColorspace) ||
         (image->colorspace != CMYKColorspace)) &&
         (image_info->colorspace != CMYKColorspace))
      TransformRGBImage(image,RGBColorspace);
    else
      if (image->colorspace != CMYKColorspace)
        RGBTransformImage(image,CMYKColorspace);
    image->compression=compression;
    packets=RunlengthEncodeImage(image);
    if ((image->compression != NoCompression) &&
        (image->compression != RunlengthEncodedCompression))
      {
        int
          status;

        unsigned char
          *compressed_pixels;

        unsigned long
          compressed_packets;

        /*
          Compress image pixels with Zip encoding.
        */
        compressed_packets=(long unsigned int)
          (1.001*(packets*image->packet_size)+12);
        compressed_pixels=(unsigned char *)
          AllocateMemory(compressed_packets*sizeof(unsigned char));
        if (compressed_pixels == (unsigned char *) NULL)
          WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
        status=True;
#if defined(HasBZLIB)
        if (compression == BZipCompression)
          {
            unsigned int
              max_packets;

            /*
              BZip compress the image pixels.
            */
            max_packets=compressed_packets;
            status=bzBuffToBuffCompress((char *) compressed_pixels,&max_packets,
              (char *) image->packed_pixels,packets*image->packet_size,
              Min(image_info->quality/10,9),image_info->verbose,
              (image_info->quality % 10)*(image_info->quality % 10)+5);
            compressed_packets=max_packets;
          }
#endif
#if defined(HasZLIB)
        if (status)
          {
            z_stream
              stream;

            /*
              BZlib compress the image pixels.
            */
            stream.next_in=image->packed_pixels;
            stream.avail_in=packets*image->packet_size;
            stream.next_out=compressed_pixels;
            stream.avail_out=compressed_packets;
            stream.zalloc=(alloc_func) NULL;
            stream.zfree=(free_func) NULL;
            stream.opaque=(voidpf) NULL;
            status=deflateInit(&stream,Min(image_info->quality/10,9));
            if (status == Z_OK)
              {
                status=deflate(&stream,Z_FINISH);
                if (status == Z_STREAM_END)
                  status=deflateEnd(&stream);
                else
                  (void) deflateEnd(&stream);
                compressed_packets=stream.total_out;
              }
            if (status == Z_OK)
              compression=ZipCompression;
          }
#endif
        if (status)
          {
            FreeMemory((char *) compressed_pixels);
            WriterExit(DelegateWarning,"Unable to compress image",image);
          }
        else
          {
            FreeMemory((char *) image->packed_pixels);
            image->packed_pixels=compressed_pixels;
            image->packet_size=1;
            packets=compressed_packets;
          }
      }
    /*
      Write header to file.
    */
    (void) strcpy(buffer,"id=ImageMagick\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (image->class == PseudoClass)
      (void) sprintf(buffer,"class=PseudoClass  colors=%u\n",image->colors);
    else
      if (image->matte)
        (void) strcpy(buffer,"class=DirectClass  matte=True\n");
      else
        if (image->colorspace == CMYKColorspace)
          (void) strcpy(buffer,"class=DirectClass  colorspace=CMYK\n");
        else
          (void) strcpy(buffer,"class=DirectClass\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (image->compression == RunlengthEncodedCompression)
      (void) sprintf(buffer,"compression=RunlengthEncoded  packets=%lu\n",
        packets);
    else
      if (image->compression == BZipCompression)
        (void) sprintf(buffer,"compression=BZip  packets=%lu\n",packets);
      else
        if (image->compression != NoCompression)
          (void) sprintf(buffer,"compression=Zip  packets=%lu\n",packets);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"columns=%u  rows=%u  depth=%u\n",image->columns,
      image->rows,image->depth);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if ((image->x_resolution != 0) && (image->y_resolution != 0))
      {
        char
          units[MaxTextExtent];

        /*
          Set image resolution.
        */
        (void) strcpy(units,"undefined");
        if (image->units == PixelsPerInchResolution)
          (void) strcpy(units,"pixels-per-inch");
        if (image->units == PixelsPerCentimeterResolution)
          (void) strcpy(units,"pixels-per-centimeter");
        (void) sprintf(buffer,"resolution=%gx%g  units=%.1024s\n",
          image->x_resolution,image->y_resolution,units);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    SignatureImage(image);
    if (image->signature != (char *) NULL)
      {
        (void) sprintf(buffer,"signature=%.1024s\n",image->signature);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->page != (char *) NULL)
      {
        (void) sprintf(buffer,"page=%.1024s\n",image->page);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    (void) QueryColorName(&image->background_color,color);
    (void) sprintf(buffer,"background-color=%.1024s  ",color);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) QueryColorName(&image->border_color,color);
    (void) sprintf(buffer,"border-color=%.1024s  ",color);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) QueryColorName(&image->matte_color,color);
    (void) sprintf(buffer,"matte-color=%.1024s\n",color);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if ((image->next != (Image *) NULL) || (image->previous != (Image *) NULL))
      {
        (void) sprintf(buffer,"scene=%u  iterations=%u  delay=%u  dispose=%u\n",
          image->scene,image->iterations,image->delay,image->dispose);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    else
      {
        if (image->scene != 0)
          {
            (void) sprintf(buffer,"scene=%u\n",image->scene);
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        if (image->iterations != 1)
          {
            (void) sprintf(buffer,"iterations=%u\n",image->iterations);
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        if (image->delay != 0)
          {
            (void) sprintf(buffer,"delay=%u\n",image->delay);
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        if (image->dispose != 0)
          {
            (void) sprintf(buffer,"dispose=%u\n",image->dispose);
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
      }
    if (image->rendering_intent != UndefinedIntent)
      {
        if (image->rendering_intent == SaturationIntent)
          (void) strcpy(buffer,"rendering-intent=saturation\n");
        else
          if (image->rendering_intent == PerceptualIntent)
            (void) strcpy(buffer,"rendering-intent=perceptual\n");
          else
            if (image->rendering_intent == AbsoluteIntent)
              (void) strcpy(buffer,"rendering-intent=absolute\n");
            else
              (void) strcpy(buffer,"rendering-intent=relative\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->gamma != 0.0)
      {
        (void) sprintf(buffer,"gamma=%g\n",image->gamma);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->chromaticity.white_point.x != 0.0)
      {
        /*
          Note chomaticity points.
        */
        (void) sprintf(buffer,
          "red-primary=%g,%g  green-primary=%g,%g  blue-primary=%g,%g\n",
          image->chromaticity.red_primary.x,image->chromaticity.red_primary.y,
          image->chromaticity.green_primary.x,
          image->chromaticity.green_primary.y,
          image->chromaticity.blue_primary.x,
          image->chromaticity.blue_primary.y);
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) sprintf(buffer,"white-point=%g,%g\n",
          image->chromaticity.white_point.x,image->chromaticity.white_point.y);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->color_profile.length > 0)
      {
        (void) sprintf(buffer,"color-profile=%u\n",image->color_profile.length);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->montage != (char *) NULL)
      {
        (void) sprintf(buffer,"montage=%.1024s\n",image->montage);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->label != (char *) NULL)
      {
        (void) sprintf(buffer,"label=\"%.1024s\"\n",image->label);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->comments != (char *) NULL)
      {
        (void) WriteByte(image,'{');
        (void) WriteByte(image,'\n');
        (void) WriteBlob(image,strlen(image->comments),image->comments);
        (void) WriteByte(image,'}');
        (void) WriteByte(image,'\n');
      }
    (void) strcpy(buffer,"\f\n:\032");
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (image->montage != (char *) NULL)
      {
        /*
          Write montage tile directory.
        */
        if (image->directory != (char *) NULL)
          (void) WriteBlob(image,strlen(image->directory),image->directory);
        (void) WriteByte(image,'\0');
      }
    if (image->color_profile.length > 0)
      (void) WriteBlob(image,(int) image->color_profile.length,
        (char *) image->color_profile.info);
    if (image->class == PseudoClass)
      {
        register unsigned char
          *q;

        unsigned char
          *colormap;

        unsigned int
          packet_size;

        unsigned short
          value;

        /*
          Allocate colormap.
        */
        packet_size=3*(image->depth >> 3);
        colormap=(unsigned char *)
          AllocateMemory(packet_size*image->colors*sizeof(unsigned char));
        if (colormap == (unsigned char *) NULL)
          WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
        q=colormap;
        for (i=0; i < (int) image->colors; i++)
        {
          WriteQuantum(image->colormap[i].red,q);
          WriteQuantum(image->colormap[i].green,q);
          WriteQuantum(image->colormap[i].blue,q);
        }
        /*
          Write colormap to file.
        */
        (void) WriteBlob(image,(int) image->colors*packet_size,
          (char *) colormap);
        FreeMemory((char *) colormap);
      }
    /*
      Write image pixels to file.
    */
    (void) WriteBlob(image,image->packet_size*packets,
      (char *) image->packed_pixels);
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
