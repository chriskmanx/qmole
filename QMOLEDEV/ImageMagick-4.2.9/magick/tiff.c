/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        TTTTT  IIIII  FFFFF  FFFFF                           %
%                          T      I    F      F                               %
%                          T      I    FFF    FFF                             %
%                          T      I    F      F                               %
%                          T    IIIII  F      F                               %
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

#if defined(HasTIFF)
#include "tiffio.h"
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d T I F F I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadTIFFImage reads a Tagged image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadTIFFImage method is:
%
%      Image *ReadTIFFImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadTIFFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#if defined(ICC_SUPPORT)
static unsigned int ReadColorProfile(char *text,long int length,
  Image *image)
{
  register unsigned char
    *p;

  if (length <= 0)
    return(False);
  p=(unsigned char *) text;
  if (image->color_profile.length != 0)
    {
      FreeMemory(image->color_profile.info);
      image->color_profile.length=0;
    }
  image->color_profile.info=(unsigned char *)
    AllocateMemory((unsigned int) length*sizeof(unsigned char));
  if (image->color_profile.info == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  image->color_profile.length=length;
  (void) memcpy(image->color_profile.info,p,length);
  return(True);
}
#endif

#if defined(IPTC_SUPPORT)
static unsigned int ReadNewsProfile(char *text,long int length,
  Image *image,int type)
{
  register unsigned char
    *p;

  if (length <= 0)
    return(False);
  p=(unsigned char *) text;
  if (image->iptc_profile.length != 0)
    {
      FreeMemory(image->iptc_profile.info);
      image->iptc_profile.length=0;
      image->iptc_profile.info=(char *) NULL;
    }
  if (type == TIFFTAG_RICHTIFFIPTC)
    {
      /*
        Handle TIFFTAG_RICHTIFFIPTC tag.
      */
      length*=4;
      image->iptc_profile.info=(unsigned char *)
        AllocateMemory((unsigned int) length*sizeof(unsigned char));
      if (image->iptc_profile.info == (unsigned char *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Memory allocation failed",
            (char *) NULL);
          return(False);
        }
      image->iptc_profile.length=length;
      (void) memcpy(image->iptc_profile.info,p,length);
      return(True);
    }
  /*
    Handle TIFFTAG_PHOTOSHOP tag.
  */
  while (length > 0)
  {
    if ((p[0] == '8') && (p[1] == 'B') && (p[2] == 'I') && (p[3] == 'M') &&
        (p[4] == 4) && (p[5] == 4))
      break;
    length-=2;
    p+=2;
  }
  if (length <= 0)
    return(False);
  if (image->iptc_profile.length != 0)
    {
      FreeMemory(image->iptc_profile.info);
      image->iptc_profile.length=0;
    }
  /*
    Eat OSType, IPTC ID code, and Pascal string length bytes.
  */
  p+=6;
  length=(*p++);
  if (length)
    p+=length;
  if ((length & 1) == 0)
    p++;  /* align to an even byte boundary */
  length=(p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
  image->iptc_profile.info=(unsigned char *)
    AllocateMemory((unsigned int) length*sizeof(unsigned char));
  if (image->iptc_profile.info == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  image->iptc_profile.length=length;
  (void) memcpy(image->iptc_profile.info,p+4,length);
  return(True);
}
#endif

static void TIFFWarningHandler(const char *module,const char *format,
  va_list warning)
{
  char
    message[MaxTextExtent];

  register char
    *p;

  p=message;
  if (module != (char *) NULL)
    {
      FormatString(p,"%.1024s: ",module);
      p+=Extent(message);
    }
  (void) vsprintf(p,format,warning);
  (void) strcat(p,".");
  MagickWarning(DelegateWarning,message,(char *) NULL);
}

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

Export Image *ReadTIFFImage(const ImageInfo *image_info)
{
  char
    *text;

  float
    *chromaticity,
    x_resolution,
    y_resolution;

  Image
    *image;

  int
    range,
    y;

  Quantum
    blue,
    green,
    red;

  register int
    i,
    x;

  register long
    packets;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  TIFF
    *tiff;

  uint16
    compress_tag,
    extra_samples,
    *sample_info;

  uint32
    length;

  unsigned char
    *scanline;

  unsigned int
    height,
    method,
    status,
    width;

  unsigned long
    max_packets;

  unsigned short
    bits_per_sample,
    index,
    interlace,
    max_sample_value,
    min_sample_value,
    pages,
    photometric,
    samples_per_pixel,
    sans,
    units,
    value;

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
  TIFFSetErrorHandler(TIFFWarningHandler);
  TIFFSetWarningHandler(TIFFWarningHandler);
  tiff=TIFFOpen(image->filename,ReadBinaryUnbufferedType);
  if (tiff == (TIFF *) NULL)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  if (image_info->subrange != 0)
    while (image->scene < image_info->subimage)
    {
      /*
        Skip to next image.
      */
      image->scene++;
      status=TIFFReadDirectory(tiff);
      if (status == False)
        ReaderExit(CorruptImageWarning,"Unable to read subimage",image);
    }
  do
  {
    if (image_info->verbose)
      TIFFPrintDirectory(tiff,stderr,False);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_COMPRESSION,&compress_tag);
    TIFFGetField(tiff,TIFFTAG_IMAGEWIDTH,&width);
    TIFFGetField(tiff,TIFFTAG_IMAGELENGTH,&height);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_PLANARCONFIG,&interlace);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_BITSPERSAMPLE,&bits_per_sample);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_MINSAMPLEVALUE,&min_sample_value);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_MAXSAMPLEVALUE,&max_sample_value);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_PHOTOMETRIC,&photometric);
    if (photometric == PHOTOMETRIC_SEPARATED)
      image->colorspace=CMYKColorspace;
    TIFFGetFieldDefaulted(tiff,TIFFTAG_SAMPLESPERPIXEL,&samples_per_pixel);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_RESOLUTIONUNIT,&units);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_XRESOLUTION,&x_resolution);
    TIFFGetFieldDefaulted(tiff,TIFFTAG_YRESOLUTION,&y_resolution);
    image->x_resolution=x_resolution;
    image->y_resolution=y_resolution;
    chromaticity=(float *) NULL;
    TIFFGetField(tiff,TIFFTAG_WHITEPOINT,&chromaticity);
    if (chromaticity != (float *) NULL)
      {
        image->chromaticity.white_point.x=chromaticity[0];
        image->chromaticity.white_point.y=chromaticity[1];
      }
    chromaticity=(float *) NULL;
    TIFFGetField(tiff,TIFFTAG_PRIMARYCHROMATICITIES,&chromaticity);
    if (chromaticity != (float *) NULL)
      {
        image->chromaticity.red_primary.x=chromaticity[0];
        image->chromaticity.red_primary.y=chromaticity[1];
        image->chromaticity.green_primary.x=chromaticity[2];
        image->chromaticity.green_primary.y=chromaticity[3];
        image->chromaticity.blue_primary.x=chromaticity[4];
        image->chromaticity.blue_primary.y=chromaticity[5];
      }
    length=0;
    text=(char *) NULL;
#if defined(ICC_SUPPORT)
    TIFFGetField(tiff,TIFFTAG_ICCPROFILE,&length,&text);
    ReadColorProfile(text,length,image);
#endif
#if defined(IPTC_SUPPORT)
    length=0;
    text=(char *) NULL;
    TIFFGetField(tiff,TIFFTAG_RICHTIFFIPTC,&length,&text);
    if (length > 0)
      {
        if (TIFFIsByteSwapped(tiff))
          TIFFSwabArrayOfLong((uint32 *) text,length);
        ReadNewsProfile(text,length,image,TIFFTAG_RICHTIFFIPTC);
      }
    else
      {
        TIFFGetField(tiff,TIFFTAG_PHOTOSHOP,&length,&text);
        if ( length > 0)
          ReadNewsProfile(text,length,image,TIFFTAG_PHOTOSHOP);
      }
#endif
    /*
      Allocate memory for the image and pixel buffer.
    */
    switch (compress_tag)
    {
      case COMPRESSION_NONE: image->compression=NoCompression; break;
      case COMPRESSION_CCITTFAX3: image->compression=FaxCompression; break;
      case COMPRESSION_CCITTFAX4: image->compression=Group4Compression; break;
      case COMPRESSION_JPEG: image->compression=JPEGCompression; break;
      case COMPRESSION_OJPEG: image->compression=JPEGCompression; break;
      case COMPRESSION_LZW: image->compression=LZWCompression; break;
      case COMPRESSION_DEFLATE: image->compression=ZipCompression; break;
      default: image->compression=RunlengthEncodedCompression; break;
    }
    image->columns=width;
    image->rows=height;
    range=max_sample_value-min_sample_value;
    if ((samples_per_pixel == 1) && !TIFFIsTiled(tiff))
      {
        image->class=PseudoClass;
        image->colors=1 << bits_per_sample;
        if (range <= (int) image->colors)
          image->colors=range+1;
        if (bits_per_sample > QuantumDepth)
          image->colors=MaxRGB+1;
      }
    if (image_info->ping)
      {
        TIFFClose(tiff);
        CloseBlob(image);
        return(image);
      }
    if (units == RESUNIT_INCH)
      image->units=PixelsPerInchResolution;
    if (units == RESUNIT_CENTIMETER)
      image->units=PixelsPerCentimeterResolution;
    image->depth=bits_per_sample;
    if (bits_per_sample < 8)
      image->depth=8;
    packets=0;
    max_packets=image->columns*image->rows;
    if (samples_per_pixel == 1)
      max_packets=Max((image->columns*image->rows+1) >> 1,1);
    if (bits_per_sample == 1)
      max_packets=Max((image->columns*image->rows+2) >> 2,1);
    image->pixels=(RunlengthPacket *)
      AllocateMemory(max_packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      {
        TIFFClose(tiff);
        ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
      }
    value=0;
    TIFFGetFieldDefaulted(tiff,TIFFTAG_PAGENUMBER,&value,&pages);
    image->scene=value;
    text=(char *) NULL;
    TIFFGetField(tiff,TIFFTAG_PAGENAME,&text);
    if (text != (char *) NULL)
      (void) CloneString(&image->label,text);
    text=(char *) NULL;
    TIFFGetField(tiff,TIFFTAG_IMAGEDESCRIPTION,&text);
    if (text != (char *) NULL)
      (void) CloneString(&image->comments,text);
    if (range < 0)
      range=max_sample_value;
    q=image->pixels;
    SetRunlengthEncoder(q);
    method=0;
    if (samples_per_pixel > 1)
      {
        method=2;
        if ((samples_per_pixel >= 3) && (photometric == PHOTOMETRIC_RGB) &&
            (interlace == PLANARCONFIG_CONTIG))
          method=1;
        if (image->colorspace == CMYKColorspace)
          method=1;
      }
    if (TIFFIsTiled(tiff))
      method=2;
    switch (method)
    {
      case 0:
      {
        Quantum
          *quantum_scanline;

        register Quantum
          *r;

        /*
          Convert TIFF image to PseudoClass MIFF image.
        */
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
        quantum_scanline=(Quantum *) AllocateMemory(width*sizeof(Quantum));
        scanline=(unsigned char *) AllocateMemory(2*TIFFScanlineSize(tiff)+4);
        if ((image->colormap == (ColorPacket *) NULL) ||
            (quantum_scanline == (Quantum *) NULL) ||
            (scanline == (unsigned char *) NULL))
          {
            TIFFClose(tiff);
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          }
        /*
          Create colormap.
        */
        switch (photometric)
        {
          case PHOTOMETRIC_MINISBLACK:
          {
            for (i=0; i < (int) image->colors; i++)
            {
              image->colormap[i].red=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].green=(MaxRGB*i)/(image->colors-1);
              image->colormap[i].blue=(MaxRGB*i)/(image->colors-1);
            }
            break;
          }
          case PHOTOMETRIC_MINISWHITE:
          {
            unsigned int
              colors;

            colors=image->colors;
            for (i=0; i < (int) image->colors; i++)
            {
              image->colormap[colors-i-1].red=(MaxRGB*i)/(image->colors-1);
              image->colormap[colors-i-1].green=(MaxRGB*i)/(image->colors-1);
              image->colormap[colors-i-1].blue=(MaxRGB*i)/(image->colors-1);
            }
            break;
          }
          case PHOTOMETRIC_PALETTE:
          {
            long
              range;

            unsigned short
              *blue_colormap,
              *green_colormap,
              *red_colormap;

            TIFFGetField(tiff,TIFFTAG_COLORMAP,&red_colormap,&green_colormap,
              &blue_colormap);
            range=256L;  /* might be old style 8-bit colormap */
            for (i=0; i < (int) image->colors; i++)
              if ((red_colormap[i] >= 256) || (green_colormap[i] >= 256) ||
                  (blue_colormap[i] >= 256))
                {
                  range=65535L;
                  break;
                }
            for (i=0; i < (int) image->colors; i++)
            {
              image->colormap[i].red=(Quantum)
                ((long) (MaxRGB*red_colormap[i])/range);
              image->colormap[i].green=(Quantum)
                ((long) (MaxRGB*green_colormap[i])/range);
              image->colormap[i].blue=(Quantum)
                ((long) (MaxRGB*blue_colormap[i])/range);
            }
            break;
          }
          default:
            break;
        }
        /*
          Convert image to PseudoClass runlength-encoded packets.
        */
        for (y=0; y < (int) image->rows; y++)
        {
          TIFFReadScanline(tiff,(char *) scanline,y,0);
          if (bits_per_sample == 16)
            {
              unsigned long
                lsb_first;

              /*
                Ensure the header byte-order is most-significant byte first.
              */
              lsb_first=1;
              if (*(char *) &lsb_first)
                MSBFirstOrderShort((char *) scanline,
                  (TIFFScanlineSize(tiff) << 1)+4);
            }
          p=scanline;
          r=quantum_scanline;
          switch (bits_per_sample)
          {
            case 1:
            {
              register int
                bit;

              for (x=0; x < ((int) width-7); x+=8)
              {
                for (bit=7; bit >= 0; bit--)
                  *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
                p++;
              }
              if ((width % 8) != 0)
                {
                  for (bit=7; bit >= (int) (8-(width % 8)); bit--)
                    *r++=((*p) & (0x01 << bit) ? 0x01 : 0x00);
                  p++;
                }
              break;
            }
            case 2:
            {
              for (x=0; x < ((int) width-3); x+=4)
              {
                *r++=(*p >> 6) & 0x3;
                *r++=(*p >> 4) & 0x3;
                *r++=(*p >> 2) & 0x3;
                *r++=(*p) & 0x3;
                p++;
              }
              if ((width % 4) != 0)
                {
                  for (i=3; i >= (int) (4-(width % 4)); i--)
                    *r++=(*p >> (i*2)) & 0x03;
                  p++;
                }
              break;
            }
            case 4:
            {
              for (x=0; x < ((int) width-1); x+=2)
              {
                *r++=(*p >> 4) & 0xf;
                *r++=(*p) & 0xf;
                p++;
              }
              if ((width % 2) != 0)
                *r++=(*p++ >> 4) & 0xf;
              break;
            }
            case 8:
            {
              for (x=0; x < (int) width; x++)
                *r++=(*p++);
              break;
            }
            case 16:
            {
              for (x=0; x < (int) image->columns; x++)
              {
                ReadQuantum(*r,p);
                r++;
              }
              break;
            }
            default:
              break;
          }
          /*
            Transfer image scanline.
          */
          r=quantum_scanline;
          for (x=0; x < (int) image->columns; x++)
          {
            index=(*r++);
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                if (packets == (int) max_packets)
                  {
                    max_packets<<=1;
                    image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
                      image->pixels,max_packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      {
                        FreeMemory((char *) scanline);
                        FreeMemory((char *) quantum_scanline);
                        TIFFClose(tiff);
                        ReaderExit(ResourceLimitWarning,
                          "Memory allocation failed",image);
                      }
                    q=image->pixels+packets-1;
                  }
                q->index=index;
                q->length=0;
              }
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        FreeMemory((char *) scanline);
        FreeMemory((char *) quantum_scanline);
        break;
      }
      case 1:
      {
        /*
          Convert TIFF image to DirectClass MIFF image.
        */
        scanline=(unsigned char *) AllocateMemory(2*TIFFScanlineSize(tiff)+4);
        if (scanline == (unsigned char *) NULL)
          {
            TIFFClose(tiff);
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          }
        TIFFGetFieldDefaulted(tiff,TIFFTAG_EXTRASAMPLES,&extra_samples,
          &sample_info);
        if (image->colorspace != CMYKColorspace)
          image->matte=extra_samples == 1;
        for (y=0; y < (int) image->rows; y++)
        {
          TIFFReadScanline(tiff,(char *) scanline,y,0);
          if (bits_per_sample == 16)
            {
              unsigned long
                lsb_first;

              /*
                Ensure the header byte-order is most-significant byte first.
              */
              lsb_first=1;
              if (*(char *) &lsb_first)
                MSBFirstOrderShort((char *) scanline,
                  (TIFFScanlineSize(tiff) << 1)+4);
            }
          if (bits_per_sample == 4)
            {
              register unsigned char
                *r;

              width=TIFFScanlineSize(tiff);
              p=scanline+width-1;
              r=scanline+(width << 1)-1;
              for (x=0; x < (int) width; x++)
              {
                *r--=((*p) & 0xf) << 4;
                *r--=((*p >> 4) & 0xf) << 4;
                p--;
              }
            }
          p=scanline;
          for (x=0; x < (int) image->columns; x++)
          {
            ReadQuantum(red,p);
            ReadQuantum(green,p);
            ReadQuantum(blue,p);
            index=0;
            if (samples_per_pixel > 3)
              ReadQuantum(index,p);
            for (i=4; i < samples_per_pixel; i++)
              ReadQuantum(sans,p);
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                (index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                if (packets == (int) max_packets)
                  {
                    max_packets<<=1;
                    image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
                      image->pixels,max_packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      {
                        TIFFClose(tiff);
                        FreeMemory((char *) scanline);
                        ReaderExit(ResourceLimitWarning,
                          "Memory allocation failed",image);
                      }
                    q=image->pixels+packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=index;
                q->length=0;
              }
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        FreeMemory((char *) scanline);
        break;
      }
      case 2:
      default:
      {
        register uint32
          *p,
          *pixels;

        /*
          Convert TIFF image to DirectClass MIFF image.
        */
        TIFFGetFieldDefaulted(tiff,TIFFTAG_EXTRASAMPLES,&extra_samples,
          &sample_info);
        image->matte=
          ((extra_samples == 1) && (sample_info[0] == EXTRASAMPLE_ASSOCALPHA));
        pixels=(uint32 *) AllocateMemory((image->columns*image->rows+
          image->columns)*sizeof(uint32));
        if (pixels == (uint32 *) NULL)
          {
            TIFFClose(tiff);
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          }
        status=TIFFReadRGBAImage(tiff,image->columns,image->rows,pixels,0);
        if (status == False)
          {
            FreeMemory((char *) pixels);
            TIFFClose(tiff);
            ReaderExit(CorruptImageWarning,"Unable to read image",image);
          }
        /*
          Convert image to DirectClass runlength-encoded packets.
        */
        for (y=image->rows-1; y >= 0; y--)
        {
          p=pixels+y*image->columns;
          for (x=0; x < (int) image->columns; x++)
          {
            red=UpScale(TIFFGetR(*p));
            green=UpScale(TIFFGetG(*p));
            blue=UpScale(TIFFGetB(*p));
            index=image->matte ? UpScale(TIFFGetA(*p)) : 0;
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                (index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                if (packets == (int) max_packets)
                  {
                    max_packets<<=1;
                    image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
                      image->pixels,max_packets*sizeof(RunlengthPacket));
                    if (image->pixels == (RunlengthPacket *) NULL)
                      {
                        FreeMemory((char *) pixels);
                        TIFFClose(tiff);
                        ReaderExit(ResourceLimitWarning,
                          "Memory allocation failed",image);
                      }
                    q=image->pixels+packets-1;
                  }
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=index;
                q->length=0;
              }
            p++;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,image->rows-y-1,image->rows);
        }
        FreeMemory((char *) pixels);
        break;
      }
    }
    SetRunlengthPackets(image,packets);
    if (image->class == PseudoClass)
      SyncImage(image);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    status=TIFFReadDirectory(tiff);
    if (status == True)
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
        ProgressMonitor(LoadImageText,image->scene-1,image->scene);
      }
  } while (status == True);
  TIFFClose(tiff);
  if (image->temporary)
    {
      (void) remove(image->filename);
      image->temporary=False;
    }
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}
#else
Export Image *ReadTIFFImage(const ImageInfo *image_info)
{
  MagickWarning(MissingDelegateWarning,"TIFF library is not available",
    image_info->filename);
  return((Image *) NULL);
}
#endif

#if defined(HasTIFF)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e T I F F I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteTIFFImage writes an image in the Tagged image file format.
%
%  The format of the WriteTIFFImage method is:
%
%      unsigned int WriteTIFFImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method WriteTIFFImage return True if the image is written.
%      False is returned is there is of a memory shortage or if the image
%      file cannot be opened for writing.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/

#if defined(IPTC_SUPPORT)
static void WriteNewsProfile(TIFF *tiff,int type,Image *image)
{
  register int
    i;

  unsigned char
    *profile;

  unsigned int
    length,
    roundup;

  if (type == TIFFTAG_RICHTIFFIPTC)
    {
      /*
        Handle TIFFTAG_RICHTIFFIPTC tag.
      */
      length=image->iptc_profile.length;
      roundup=4-(length & 0x03); /* round up for long word alignment */
      profile=(unsigned char *)
        AllocateMemory((length+roundup)*sizeof(unsigned char));
      if ((length == 0) || (profile == (unsigned char *) NULL))
        return;
      (void) memcpy((char *) profile,image->iptc_profile.info,length);
      for (i=0; i < roundup; i++)
        profile[length + i] = 0;
      length=(image->iptc_profile.length+roundup)/4;
      if (TIFFIsByteSwapped(tiff))
        TIFFSwabArrayOfLong((uint32 *) profile,length);
      TIFFSetField(tiff,type,(uint32) (length+roundup),(void *) profile);
      FreeMemory((char *) profile);
      return;
    }
  /*
    Handle TIFFTAG_PHOTOSHOP tag.
  */
  length=image->iptc_profile.length;
  roundup=(length & 0x01); /* round up for Photoshop */
  profile=(unsigned char *)
    AllocateMemory((length+roundup+12)*sizeof(unsigned char));
  if ((length == 0) || (profile == (unsigned char *) NULL))
  (void) memcpy((char *) profile,"8BIM\04\04\0\0",8);
  profile[8]=(length >> 24) & 0xff;
  profile[9]=(length >> 16) & 0xff;
  profile[10]=(length >> 8) & 0xff;
  profile[11]=length & 0xff;
  for (i=0; i < length; i++)
    profile[i+12]=image->iptc_profile.info[i];
  if (roundup)
    profile[length+roundup+11]=0;
  TIFFSetField(tiff,type,(uint32) length+roundup+12,(void *) profile);
  FreeMemory((char *) profile);
}
#endif

static int TIFFWritePixels(TIFF *tiff,tdata_t scanline,uint32 row,
  tsample_t sample,Image *image)
{
  int
    bytes_per_pixel,
    number_tiles,
    status,
    tile_width;

  register int
    i,
    j,
    k;

  static unsigned char
    *scanlines = (unsigned char *) NULL,
    *tile_pixels = (unsigned char *) NULL;

  if (!TIFFIsTiled(tiff))
    return(TIFFWriteScanline(tiff,scanline,row,sample));
  if (scanlines == (unsigned char *) NULL)
    scanlines=(unsigned char *)
      AllocateMemory(image->tile_info.height*TIFFScanlineSize(tiff));
  if (scanlines == (unsigned char *) NULL)
    return(-1);
  if (tile_pixels == (unsigned char *) NULL)
    tile_pixels=(unsigned char *)AllocateMemory(TIFFTileSize(tiff));
  if (tile_pixels == (unsigned char *) NULL)
    return(-1);
  /*
    Fill scanlines to tile height.
  */
  i=(row % image->tile_info.height)*TIFFScanlineSize(tiff);
  (void) memcpy((char *) scanlines+i,(char *) scanline,TIFFScanlineSize(tiff));
  if (((row % image->tile_info.height) != (image->tile_info.height-1)) &&
      (row != image->rows-1))
    return(0);
  /*
    Write tile to TIFF image.
  */
  status=0;
  bytes_per_pixel=
    TIFFTileSize(tiff)/(image->tile_info.height*image->tile_info.width);
  number_tiles=
    (image->columns+image->tile_info.width-1)/image->tile_info.height;
  for (i=0; i < number_tiles; i++)
  {
    tile_width=(i == (int) number_tiles-1) ?
      image->columns-(i*image->tile_info.width) : image->tile_info.width;
    for (j=0; j < (int) ((row % image->tile_info.height)+1); j++)
      for (k=0; k < tile_width; k++)
      {
        register int
          l;

        register unsigned char
          *p,
          *q;

        p=scanlines+(j*TIFFScanlineSize(tiff)+(i*image->tile_info.width+k)*
          bytes_per_pixel);
        q=tile_pixels+
          (j*(TIFFTileSize(tiff)/image->tile_info.height)+k*bytes_per_pixel);
        for (l=0; l < bytes_per_pixel; l++)
          *q++=(*p++);
      }
      status=TIFFWriteTile(tiff,tile_pixels,i*image->tile_info.width,(row/
        image->tile_info.height)*image->tile_info.height,0,sample);
      if (status < 0)
        break;
  }
  if (row == (image->rows-1))
    {
      /*
        Free memory resources.
      */
      FreeMemory((char *) scanlines);
      scanlines=(unsigned char *) NULL;
      FreeMemory((char *) tile_pixels);
      tile_pixels=(unsigned char *) NULL;
    }
  return(status);
}

Export unsigned int WriteTIFFImage(const ImageInfo *image_info,Image *image)
{
#if !defined(TIFFDefaultStripSize)
#define TIFFDefaultStripSize(tiff,request)  ((8*1024)/TIFFScanlineSize(tiff))
#endif

  Image
    encode_image;

  int
    y;

  register RunlengthPacket
    *p;

  register int
    i,
    j,
    x;

  register unsigned char
    *q;

  TIFF
    *tiff;

  uint16
    compress_tag,
    photometric;

  unsigned char
    *scanline;

  unsigned int
    scene,
    status;

  unsigned long
    strip_size;

  unsigned short
    value;

  if (Latin1Compare(image_info->magick,"PTIF") == 0)
    {
      Image
        *next_image,
        *pyramid_image;

      unsigned int
        height,
        width;

      /*
        Pyramid encode TIFF image.
      */
      pyramid_image=(Image *) NULL;
      if (image->tile_info.width == 0)
        image->tile_info.width=64;
      if (image->tile_info.height == 0)
        image->tile_info.height=64;
      width=image->columns;
      height=image->rows;
      do
      {
        image->orphan=True;
        next_image=ZoomImage(image,width,height);
        image->orphan=False;
        if (next_image == (Image *) NULL)
          WriterExit(FileOpenWarning,"Unable to pyramid encode image",image);
        if (pyramid_image == (Image *) NULL)
          pyramid_image=next_image;
        else
          {
            register Image
              *p;

            /*
              Link image into pyramid image list.
            */
            for (p=pyramid_image; p->next != (Image *) NULL; p=p->next);
            next_image->previous=p;
            p->next=next_image;
          }
        width=(width+1)/2;
        height=(height+1)/2;
        if ((width == 0) || (height == 0))
          break;
      } while (((2*width) >= image->tile_info.width) ||
               ((2*height) >= image->tile_info.height));
      image=pyramid_image;
    }
  /*
    Open TIFF file.
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
      encode_image=(*image);
      TemporaryFilename(image->filename);
      image->temporary=True;
    }
  CloseBlob(image);
  tiff=TIFFOpen(image->filename,WriteBinaryType);
  if (tiff == (TIFF *) NULL)
    return(False);
  scene=0;
  do
  {
    /*
      Initialize TIFF fields.
    */
    if (Latin1Compare(image_info->magick,"PTIF") == 0)
      if (image->previous != (Image *) NULL)
        TIFFSetField(tiff,TIFFTAG_SUBFILETYPE,FILETYPE_REDUCEDIMAGE);
    TIFFSetField(tiff,TIFFTAG_IMAGELENGTH,(uint32) image->rows);
    TIFFSetField(tiff,TIFFTAG_IMAGEWIDTH,(uint32) image->columns);
    TIFFSetField(tiff,TIFFTAG_BITSPERSAMPLE,8);
    if (image->depth > 8)
      TIFFSetField(tiff,TIFFTAG_BITSPERSAMPLE,16);
    compress_tag=COMPRESSION_NONE;
    if ((image_info->compression == FaxCompression) ||
        (image_info->compression == Group4Compression))
      if (!IsMonochromeImage(image))
        {
          QuantizeInfo
            quantize_info;

          GetQuantizeInfo(&quantize_info);
          quantize_info.number_colors=2;
          quantize_info.dither=image_info->dither;
          quantize_info.colorspace=GRAYColorspace;
          (void) QuantizeImage(&quantize_info,image);
          SyncImage(image);
        }
    switch (image->compression)
    {
      case FaxCompression:
      {
        if (IsMonochromeImage(image))
          compress_tag=COMPRESSION_CCITTFAX3;
        break;
      }
      case Group4Compression:
      {
        if (IsMonochromeImage(image))
          compress_tag=COMPRESSION_CCITTFAX4;
        break;
      }
      case JPEGCompression: compress_tag=COMPRESSION_JPEG; break;
      case LZWCompression: compress_tag=COMPRESSION_LZW; break;
      case RunlengthEncodedCompression:
        compress_tag=COMPRESSION_PACKBITS; break;
      case ZipCompression: compress_tag=COMPRESSION_DEFLATE; break;
      default: compress_tag=COMPRESSION_NONE; break;
    }
    if (((image_info->colorspace == UndefinedColorspace) &&
         (image->colorspace == CMYKColorspace)) ||
         (image_info->colorspace == CMYKColorspace))
      {
        photometric=PHOTOMETRIC_SEPARATED;
        TIFFSetField(tiff,TIFFTAG_SAMPLESPERPIXEL,4);
        TIFFSetField(tiff,TIFFTAG_INKSET,INKSET_CMYK);
      }
    else
      if ((Latin1Compare(image_info->magick,"TIFF24") == 0) ||
          (compress_tag == JPEGCompression) ||
          (!IsPseudoClass(image) && !IsGrayImage(image)))
        {
          /*
            Full color TIFF raster.
          */
          TransformRGBImage(image,RGBColorspace);
          photometric=PHOTOMETRIC_RGB;
          TIFFSetField(tiff,TIFFTAG_SAMPLESPERPIXEL,(image->matte ? 4 : 3));
          if (image->matte)
            {
              uint16
                extra_samples,
                sample_info[1];

              /*
                TIFF has a matte channel.
              */
              extra_samples=1;
              sample_info[0]=EXTRASAMPLE_ASSOCALPHA;
              TIFFSetField(tiff,TIFFTAG_EXTRASAMPLES,extra_samples,
                &sample_info);
            }
        }
      else
        {
          /*
            Colormapped TIFF raster.
          */
          TransformRGBImage(image,RGBColorspace);
          TIFFSetField(tiff,TIFFTAG_SAMPLESPERPIXEL,1);
          photometric=PHOTOMETRIC_PALETTE;
          if (image->colors <= 2)
            {
              if (IsMonochromeImage(image))
                photometric=PHOTOMETRIC_MINISWHITE;
              TIFFSetField(tiff,TIFFTAG_BITSPERSAMPLE,1);
              compress_tag=COMPRESSION_CCITTFAX4;
            }
          else
            if (IsGrayImage(image))
              photometric=PHOTOMETRIC_MINISBLACK;
        }
    switch (image_info->compression)
    {
      case NoCompression: compress_tag=COMPRESSION_NONE; break;
      case FaxCompression: compress_tag=COMPRESSION_CCITTFAX3; break;
      case Group4Compression: compress_tag=COMPRESSION_CCITTFAX4; break;
      case JPEGCompression: compress_tag=COMPRESSION_JPEG; break;
      case LZWCompression: compress_tag=COMPRESSION_LZW; break;
      case RunlengthEncodedCompression:
        compress_tag=COMPRESSION_PACKBITS; break;
      case ZipCompression: compress_tag=COMPRESSION_DEFLATE; break;
      default: break;
    }
    TIFFSetField(tiff,TIFFTAG_PHOTOMETRIC,photometric);
    TIFFSetField(tiff,TIFFTAG_COMPRESSION,compress_tag);
    TIFFSetField(tiff,TIFFTAG_FILLORDER,FILLORDER_MSB2LSB);
    TIFFSetField(tiff,TIFFTAG_ORIENTATION,ORIENTATION_TOPLEFT);
    TIFFSetField(tiff,TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
    if (photometric == PHOTOMETRIC_RGB)
      if ((image_info->interlace == PlaneInterlace) ||
          (image_info->interlace == PartitionInterlace))
        TIFFSetField(tiff,TIFFTAG_PLANARCONFIG,PLANARCONFIG_SEPARATE);
    strip_size=Max(TIFFDefaultStripSize(tiff,-1),1);
    if (compress_tag == COMPRESSION_JPEG)
      TIFFSetField(tiff,TIFFTAG_ROWSPERSTRIP,strip_size+(8-(strip_size % 8)));
    else
      if (compress_tag == COMPRESSION_CCITTFAX4)
        TIFFSetField(tiff,TIFFTAG_ROWSPERSTRIP,image->rows);
      else
        TIFFSetField(tiff,TIFFTAG_ROWSPERSTRIP,strip_size);
    if ((image->x_resolution != 0) && (image->y_resolution != 0))
      {
        unsigned short
          units;

        /*
          Set image resolution.
        */
        units=RESUNIT_NONE;
        if (image->units == PixelsPerInchResolution)
          units=RESUNIT_INCH;
        if (image->units == PixelsPerCentimeterResolution)
          units=RESUNIT_CENTIMETER;
        TIFFSetField(tiff,TIFFTAG_RESOLUTIONUNIT,(uint16) units);
        TIFFSetField(tiff,TIFFTAG_XRESOLUTION,image->x_resolution);
        TIFFSetField(tiff,TIFFTAG_YRESOLUTION,image->y_resolution);
      }
    if (image->chromaticity.white_point.x != 0.0)
      {
        float
          chromaticity[6];

        /*
          Set image chromaticity.
        */
        chromaticity[0]=image->chromaticity.red_primary.x;
        chromaticity[1]=image->chromaticity.red_primary.y;
        chromaticity[2]=image->chromaticity.green_primary.x;
        chromaticity[3]=image->chromaticity.green_primary.y;
        chromaticity[4]=image->chromaticity.blue_primary.x;
        chromaticity[5]=image->chromaticity.blue_primary.y;
        TIFFSetField(tiff,TIFFTAG_PRIMARYCHROMATICITIES,chromaticity);
        chromaticity[0]=image->chromaticity.white_point.x;
        chromaticity[1]=image->chromaticity.white_point.y;
        TIFFSetField(tiff,TIFFTAG_WHITEPOINT,chromaticity);
      }
#if defined(ICC_SUPPORT)
    if (image->color_profile.length > 0)
      TIFFSetField(tiff,TIFFTAG_ICCPROFILE,(uint32) image->color_profile.length,
        (void *) image->color_profile.info);
#endif
#if defined(IPTC_SUPPORT)
#if defined(PHOTOSHOP_SUPPORT)
    if (image->iptc_profile.length > 0)
      WriteNewsProfile(tiff,TIFFTAG_PHOTOSHOP,image);
#endif
    if (image->iptc_profile.length > 0)
      WriteNewsProfile(tiff,TIFFTAG_RICHTIFFIPTC,image);
#endif
    TIFFSetField(tiff,TIFFTAG_DOCUMENTNAME,image->filename);
    TIFFSetField(tiff,TIFFTAG_SOFTWARE,MagickVersion);
    if (GetNumberScenes(image) > 1)
      {
        TIFFSetField(tiff,TIFFTAG_SUBFILETYPE,FILETYPE_PAGE);
        TIFFSetField(tiff,TIFFTAG_PAGENUMBER,(unsigned short) image->scene,
          GetNumberScenes(image));
      }
    if (image->label != (char *) NULL)
      TIFFSetField(tiff,TIFFTAG_PAGENAME,image->label);
    if (image->comments != (char *) NULL)
      TIFFSetField(tiff,TIFFTAG_IMAGEDESCRIPTION,image->comments);
    /*
      Write image scanlines.
    */
    scanline=(unsigned char *) AllocateMemory(TIFFScanlineSize(tiff));
    if (scanline == (unsigned char *) NULL)
      WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    p=image->pixels;
    q=scanline;
    x=0;
    y=0;
    switch (photometric)
    {
      case PHOTOMETRIC_RGB:
      {
        /*
          RGB TIFF image.
        */
        switch (image_info->interlace)
        {
          case NoInterlace:
          default:
          {
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
              {
                /*
                  Convert DirectClass packets to contiguous RGB scanlines.
                */
                WriteQuantum(p->red,q);
                WriteQuantum(p->green,q);
                WriteQuantum(p->blue,q);
                if (image->matte)
                  WriteQuantum(p->index,q);
                x++;
                if (x == (int) image->columns)
                  {
                    if (TIFFWritePixels(tiff,(char *) scanline,y,0,image) < 0)
                      break;
                    if (image->previous == (Image *) NULL)
                      if (QuantumTick(y,image->rows))
                        ProgressMonitor(SaveImageText,y,image->rows);
                    q=scanline;
                    x=0;
                    y++;
                  }
              }
              p++;
            }
            break;
          }
          case PlaneInterlace:
          case PartitionInterlace:
          {
            /*
              Plane interlacing:  RRRRRR...GGGGGG...BBBBBB...
            */
            p=image->pixels;
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
              {
                WriteQuantum(p->red,q);
                x++;
                if (x == (int) image->columns)
                  {
                    if (TIFFWritePixels(tiff,(char *) scanline,y,0,image) < 0)
                      break;
                    q=scanline;
                    x=0;
                    y++;
                  }
              }
              p++;
            }
            ProgressMonitor(SaveImageText,100,400);
            p=image->pixels;
            y=0;
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
              {
                WriteQuantum(p->green,q);
                x++;
                if (x == (int) image->columns)
                  {
                    if (TIFFWritePixels(tiff,(char *) scanline,y,1,image) < 0)
                      break;
                    q=scanline;
                    x=0;
                    y++;
                  }
              }
              p++;
            }
            ProgressMonitor(SaveImageText,200,400);
            p=image->pixels;
            y=0;
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
              {
                WriteQuantum(p->blue,q);
                x++;
                if (x == (int) image->columns)
                  {
                    if (TIFFWritePixels(tiff,(char *) scanline,y,2,image) < 0)
                      break;
                    q=scanline;
                    x=0;
                    y++;
                  }
              }
              p++;
            }
            ProgressMonitor(SaveImageText,300,400);
            p=image->pixels;
            y=0;
            if (image->matte)
              for (i=0; i < (int) image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                {
                  WriteQuantum(p->index,q);
                  x++;
                  if (x == (int) image->columns)
                    {
                      if (TIFFWritePixels(tiff,(char *) scanline,y,3,image) < 0)
                        break;
                      q=scanline;
                      x=0;
                      y++;
                    }
                }
                p++;
              }
            ProgressMonitor(SaveImageText,400,400);
            break;
          }
        }
        break;
      }
      case PHOTOMETRIC_SEPARATED:
      {
        /*
          CMYK TIFF image.
        */
        if (image->colorspace != CMYKColorspace)
          RGBTransformImage(image,CMYKColorspace);
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            /*
              Convert DirectClass packets to contiguous RGB scanlines.
            */
            WriteQuantum(p->red,q);
            WriteQuantum(p->green,q);
            WriteQuantum(p->blue,q);
            WriteQuantum(p->index,q);
            x++;
            if (x == (int) image->columns)
              {
                if (TIFFWritePixels(tiff,(char *) scanline,y,0,image) < 0)
                  break;
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(y,image->rows))
                    ProgressMonitor(SaveImageText,y,image->rows);
                q=scanline;
                x=0;
                y++;
              }
          }
          p++;
        }
        break;
      }
      case PHOTOMETRIC_PALETTE:
      {
        unsigned short
          *blue,
          *green,
          *red;

        /*
          Colormapped TIFF image.
        */
        blue=(unsigned short *)
          AllocateMemory((1 << image->depth)*sizeof(unsigned short));
        green=(unsigned short *)
          AllocateMemory((1 << image->depth)*sizeof(unsigned short));
        red=(unsigned short *)
          AllocateMemory((1 << image->depth)*sizeof(unsigned short));
        if ((blue == (unsigned short *) NULL) ||
            (green == (unsigned short *) NULL) ||
            (red == (unsigned short *) NULL))
          WriterExit(ResourceLimitWarning,"Memory allocation failed",
            image);
        /*
          Initialize TIFF colormap.
        */
        for (i=0; i < (int) image->colors; i++)
        {
          red[i]=((unsigned long) (image->colormap[i].red*65535L)/MaxRGB);
          green[i]=((unsigned long) (image->colormap[i].green*65535L)/MaxRGB);
          blue[i]=((unsigned long) (image->colormap[i].blue*65535L)/MaxRGB);
        }
        for ( ; i < (1 << image->depth); i++)
        {
          red[i]=0;
          green[i]=0;
          blue[i]=0;
        }
        TIFFSetField(tiff,TIFFTAG_COLORMAP,red,green,blue);
        FreeMemory((char *) red);
        FreeMemory((char *) green);
        FreeMemory((char *) blue);
      }
      default:
      {
        register unsigned char
          bit,
          byte,
          polarity;

        if (image->colors > 2)
          {
            /*
              Convert PseudoClass packets to contiguous grayscale scanlines.
            */
            for (i=0; i < (int) image->packets; i++)
            {
              for (j=0; j <= ((int) p->length); j++)
              {
                if (photometric == PHOTOMETRIC_PALETTE)
                  WriteQuantum(p->index,q)
                else
                  WriteQuantum(Intensity(*p),q);
                x++;
                if (x == (int) image->columns)
                  {
                    if (TIFFWritePixels(tiff,(char *) scanline,y,0,image) < 0)
                      break;
                    if (image->previous == (Image *) NULL)
                      if (QuantumTick(y,image->rows))
                        ProgressMonitor(SaveImageText,y,image->rows);
                    q=scanline;
                    x=0;
                    y++;
                  }
              }
              p++;
            }
            break;
          }
        /*
          Convert PseudoClass packets to contiguous monochrome scanlines.
        */
        polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
        if (photometric == PHOTOMETRIC_PALETTE)
          polarity=1;
        else
          if (image->colors == 2)
            {
              polarity=
                Intensity(image->colormap[0]) > Intensity(image->colormap[1]);
              if (photometric == PHOTOMETRIC_MINISBLACK)
                polarity=!polarity;
            }
        bit=0;
        byte=0;
        x=0;
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            byte<<=1;
            if (p->index == polarity)
              byte|=0x01;
            bit++;
            if (bit == 8)
              {
                *q++=byte;
                bit=0;
                byte=0;
              }
            x++;
            if (x == (int) image->columns)
              {
                /*
                  Advance to the next scanline.
                */
                if (bit != 0)
                  *q++=byte << (8-bit);
                if (TIFFWritePixels(tiff,(char *) scanline,y,0,image) < 0)
                  break;
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(y,image->rows))
                    ProgressMonitor(SaveImageText,y,image->rows);
                q=scanline;
                bit=0;
                byte=0;
                x=0;
                y++;
             }
          }
          p++;
        }
        break;
      }
    }
    FreeMemory((char *) scanline);
    if (image_info->verbose == True)
      TIFFPrintDirectory(tiff,stderr,False);
    TIFFWriteDirectory(tiff);
    if (image->next == (Image *) NULL)
      break;
    image->next->file=image->file;
    image=image->next;
    ProgressMonitor(SaveImagesText,scene++,GetNumberScenes(image));
  } while (image_info->adjoin);
  if (image_info->adjoin)
    while (image->previous != (Image *) NULL)
      image=image->previous;
  (void) TIFFClose(tiff);
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
        (void) putc(c,encode_image.file);
      (void) fclose(file);
      (void) remove(image->filename);
      image->temporary=False;
      CloseBlob(&encode_image);
    }
  if (Latin1Compare(image_info->magick,"PTIF") == 0)
    DestroyImages(image);
  return(True);
}
#else
Export unsigned int WriteTIFFImage(const ImageInfo *image_info,Image *image)
{
  MagickWarning(MissingDelegateWarning,"TIFF library is not available",
    image->filename);
  return(False);
}
#endif
