/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            PPPP   N   N  M   M                              %
%                            P   P  NN  N  MM MM                              %
%                            PPPP   N N N  M M M                              %
%                            P      N  NN  M   M                              %
%                            P      N   N  M   M                              %
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
%   R e a d P N M I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadPNMImage reads a Portable Anymap image file and returns it.
%  It allocates the memory necessary for the new Image structure and returns
%  a pointer to the new image.
%
%  The format of the ReadPNMImage method is:
%
%      Image *ReadPNMImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadPNMImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static unsigned int PNMInteger(Image *image,const unsigned int base)
{
#define P7Comment  "END_OF_COMMENTS"

  int
    c;

  unsigned int
    value;

  /*
    Skip any leading whitespace.
  */
  do
  {
    c=ReadByte(image);
    if (c == EOF)
      return(0);
    if (c == '#')
      {
        register char
          *p,
          *q;

        unsigned int
          length;

        /*
          Read comment.
        */
        if (image->comments != (char *) NULL)
          {
            p=image->comments+Extent(image->comments);
            length=p-image->comments;
          }
        else
          {
            length=MaxTextExtent;
            image->comments=(char *) AllocateMemory(length*sizeof(char));
            p=image->comments;
          }
        q=p;
        if (image->comments != (char *) NULL)
          for ( ; (c != EOF) && (c != '\n'); p++)
          {
            if ((p-image->comments+sizeof(P7Comment)) >= length)
              {
                length<<=1;
                length+=MaxTextExtent;
                image->comments=(char *) ReallocateMemory((char *)
                  image->comments,length*sizeof(char));
                if (image->comments == (char *) NULL)
                  break;
                p=image->comments+Extent(image->comments);
              }
            c=ReadByte(image);
            *p=(char) c;
            *(p+1)='\0';
          }
        if (image->comments == (char *) NULL)
          {
            MagickWarning(ResourceLimitWarning,"Memory allocation failed",
              (char *) NULL);
            return(0);
          }
        if (Latin1Compare(q,P7Comment) == 0)
          *q='\0';
        continue;
      }
  } while (!isdigit(c));
  if (base == 2)
    return(c-'0');
  /*
    Evaluate number.
  */
  value=0;
  do
  {
    value*=10;
    value+=c-'0';
    c=ReadByte(image);
    if (c == EOF)
      return(0);
  }
  while (isdigit(c));
  return(value);
}

Export Image *ReadPNMImage(const ImageInfo *image_info)
{
#define MaxRawValue  255

  char
    format;

  Image
    *image;

  int
    y;

  MonitorHandler
    handler;

  Quantum
    *scale;

  register int
    i,
    x;

  register long
    packets;

  register RunlengthPacket
    *q;

  unsigned int
    max_value,
    status;

  unsigned short
    blue,
    green,
    index,
    red;

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
    Read PNM image.
  */
  status=ReadBlob(image,1,(char *) &format);
  do
  {
    /*
      Verify PNM identifier.
    */
    if ((status == False) || (format != 'P'))
      ReaderExit(CorruptImageWarning,"Not a PNM image file",image);
    /*
      Initialize image structure.
    */
    format=ReadByte(image);
    if (format == '7')
      (void) PNMInteger(image,10);
    image->columns=PNMInteger(image,10);
    image->rows=PNMInteger(image,10);
    if ((format == '1') || (format == '4'))
      max_value=1;  /* bitmap */
    else
      max_value=PNMInteger(image,10);
    if ((format != '3') && (format != '6'))
      {
        image->class=PseudoClass;
        image->colors=Min(max_value,MaxRGB)+1;
      }
    if (image_info->ping)
      {
        CloseBlob(image);
        return(image);
      }
    if ((image->columns*image->rows) == 0)
      ReaderExit(CorruptImageWarning,
        "Unable to read image: image dimensions are zero",image);
    scale=(Quantum *) NULL;
    if (image->class == PseudoClass)
      {
        /*
          Create colormap.
        */
        image->colormap=(ColorPacket *)
          AllocateMemory(image->colors*sizeof(ColorPacket));
        if (image->colormap == (ColorPacket *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        if (format != '7')
          for (i=0; i < (int) image->colors; i++)
          {
            image->colormap[i].red=(Quantum)
              ((long) (MaxRGB*i)/(image->colors-1));
            image->colormap[i].green=(Quantum)
              ((long) (MaxRGB*i)/(image->colors-1));
            image->colormap[i].blue=(Quantum)
              ((long) (MaxRGB*i)/(image->colors-1));
          }
        else
          {
            /*
              Initialize 332 colormap.
            */
            i=0;
            for (red=0; red < 8; red++)
              for (green=0; green < 8; green++)
                for (blue=0; blue < 4; blue++)
                {
                  image->colormap[i].red=(Quantum) ((long) (red*MaxRGB)/7);
                  image->colormap[i].green=(Quantum) ((long) (green*MaxRGB)/7);
                  image->colormap[i].blue=(Quantum) ((long) (blue*MaxRGB)/3);
                  i++;
                }
          }
      }
    else
      if (max_value != MaxRGB)
        {
          /*
            Compute pixel scaling table.
          */
          scale=(Quantum *) AllocateMemory((max_value+1)*sizeof(Quantum));
          if (scale == (Quantum *) NULL)
            ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
          for (i=0; i <= (int) max_value; i++)
            scale[i]=(Quantum) ((i*MaxRGB+(max_value >> 1))/max_value);
        }
    packets=0;
    image->pixels=(RunlengthPacket *)
      AllocateMemory(image->columns*image->rows*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    /*
      Convert PNM pixels to runlength-encoded MIFF packets.
    */
    q=image->pixels;
    SetRunlengthEncoder(q);
    switch (format)
    {
      case '1':
      {
        /*
          Convert PBM image to runlength-encoded packets.
        */
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            index=!PNMInteger(image,2);
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                q->index=index;
                q->length=0;
              }
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '2':
      {
        /*
          Convert PGM image to runlength-encoded packets.
        */
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            index=PNMInteger(image,10);
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                q->index=index;
                q->length=0;
              }
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '3':
      {
        /*
          Convert PNM image to runlength-encoded packets.
        */
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            red=PNMInteger(image,10);
            green=PNMInteger(image,10);
            blue=PNMInteger(image,10);
            if (scale != (Quantum *) NULL)
              {
                red=scale[red];
                green=scale[green];
                blue=scale[blue];
              }
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=0;
                q->length=0;
            }
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '4':
      {
        unsigned char
          bit,
          byte;

        /*
          Convert PBM raw image to runlength-encoded packets.
        */
        for (y=0; y < (int) image->rows; y++)
        {
          bit=0;
          byte=0;
          for (x=0; x < (int) image->columns; x++)
          {
            if (bit == 0)
              byte=ReadByte(image);
            index=(byte & 0x80) ? 0 : 1;
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                q->index=index;
                q->length=0;
              }
            bit++;
            if (bit == 8)
              bit=0;
            byte<<=1;
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '5':
      case '7':
      {
        /*
          Convert PGM raw image to runlength-encoded packets.
        */
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            if (max_value <= MaxRawValue)
              index=ReadByte(image);
            else
              index=LSBFirstReadShort(image);
            if (index > max_value)
              index=max_value;
            if ((index == q->index) && ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                q->index=index;
                q->length=0;
              }
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        break;
      }
      case '6':
      {
        /*
          Convert PNM raster image to runlength-encoded packets.
        */
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            if (max_value <= MaxRawValue)
              {
                red=ReadByte(image);
                green=ReadByte(image);
                blue=ReadByte(image);
              }
            else
              {
                red=LSBFirstReadShort(image);
                green=LSBFirstReadShort(image);
                blue=LSBFirstReadShort(image);
              }
            if (scale != (Quantum *) NULL)
              {
                red=scale[red];
                green=scale[green];
                blue=scale[blue];
              }
            if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
                ((int) q->length < MaxRunlength))
              q->length++;
            else
              {
                if (packets != 0)
                  q++;
                packets++;
                q->red=red;
                q->green=green;
                q->blue=blue;
                q->index=0;
                q->length=0;
              }
          }
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(LoadImageText,y,image->rows);
        }
        handler=SetMonitorHandler((MonitorHandler) NULL);
        (void) SetMonitorHandler(handler);
        break;
      }
      default:
        ReaderExit(CorruptImageWarning,"Not a PNM image file",image);
    }
    if (scale != (Quantum *) NULL)
      FreeMemory((char *) scale);
    if (EOFBlob(image))
      MagickWarning(CorruptImageWarning,"not enough pixels",image->filename);
    SetRunlengthPackets(image,packets);
    if (image->class == PseudoClass)
      SyncImage(image);
    /*
      Proceed to next image.
    */
    if (image_info->subrange != 0)
      if (image->scene >= (image_info->subimage+image_info->subrange-1))
        break;
    if ((format == '1') || (format == '2') || (format == '3'))
      do
      {
        /*
          Skip to end of line.
        */
        status=ReadBlob(image,1,&format);
        if (status == False)
          break;
      } while (format != '\n');
    status=ReadBlob(image,1,(char *) &format);
    if ((status == True) && (format == 'P'))
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
  } while ((status == True) && (format == 'P'));
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
%   W r i t e P N M I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure WritePNMImage writes an image to a file in the PNM rasterfile
%  format.
%
%  The format of the WritePNMImage method is:
%
%      unsigned int WritePNMImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WritePNMImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WritePNMImage(const ImageInfo *image_info,Image *image)
{
#define MaxRawValue  255

  char
    buffer[MaxTextExtent],
    *magick;

  int
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  unsigned char
    format;

  unsigned int
    scene,
    status;

  unsigned short
    index;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  scene=0;
  do
  {
    /*
      Promote/Demote image based on image type.
    */
    TransformRGBImage(image,RGBColorspace);
    (void) IsPseudoClass(image);
    if (Latin1Compare(image_info->magick,"PPM") == 0)
      image->class=DirectClass;
    magick=(char *) image_info->magick;
    if (((Latin1Compare(magick,"PGM") == 0) && !IsGrayImage(image)) ||
        ((Latin1Compare(magick,"PBM") == 0) && !IsMonochromeImage(image)))
      {
        QuantizeInfo
          quantize_info;

        GetQuantizeInfo(&quantize_info);
        quantize_info.number_colors=MaxRGB+1;
        if (Latin1Compare(image_info->magick,"PBM") == 0)
          quantize_info.number_colors=2;
        quantize_info.dither=image_info->dither;
        quantize_info.colorspace=GRAYColorspace;
        (void) QuantizeImage(&quantize_info,image);
        SyncImage(image);
      }
    /*
      Write PNM file header.
    */
    if (!IsPseudoClass(image) && !IsGrayImage(image))
      {
        /*
          Full color PNM image.
        */
        format='6';
        if ((image_info->compression == NoCompression) ||
            (MaxRGB > MaxRawValue))
          format='3';
      }
    else
      {
        /*
          Colormapped PNM image.
        */
        format='6';
        if ((image_info->compression == NoCompression) ||
            (MaxRGB > MaxRawValue))
          format='3';
        if ((Latin1Compare(magick,"PPM") != 0) && IsGrayImage(image))
          {
            /*
              Grayscale PNM image.
            */
            format='5';
            if ((image_info->compression == NoCompression) ||
                (MaxRGB > MaxRawValue))
              format='2';
            if (Latin1Compare(magick,"PGM") != 0)
              if (image->colors == 2)
                {
                  format='4';
                  if (image_info->compression == NoCompression)
                    format='1';
                }
          }
      }
    if (Latin1Compare(magick,"P7") == 0)
      {
        format='7';
        (void) strcpy(buffer,"P7 332\n");
      }
    else
      (void) sprintf(buffer,"P%c\n",format);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (image->comments != (char *) NULL)
      {
        register char
          *p;

        /*
          Write comments to file.
        */
        (void) WriteByte(image,'#');
        for (p=image->comments; *p != '\0'; p++)
        {
          (void) WriteByte(image,*p);
          if ((*p == '\n') && (*(p+1) != '\0'))
            (void) WriteByte(image,'#');
        }
        (void) WriteByte(image,'\n');
      }
    if (format != '7')
      {
        (void) sprintf(buffer,"%u %u\n",image->columns,image->rows);
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    /*
      Convert runlength encoded to PNM raster pixels.
    */
    x=0;
    y=0;
    p=image->pixels;
    switch (format)
    {
      case '1':
      {
        register unsigned char
          polarity;

        /*
          Convert image to a PBM image.
        */
        polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
        if (image->colors == 2)
          polarity=
            Intensity(image->colormap[0]) > Intensity(image->colormap[1]);
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            (void) sprintf(buffer,"%d ",(int) (p->index == polarity));
            (void) WriteBlob(image,strlen(buffer),buffer);
            x++;
            if (x == 36)
              {
                (void) WriteByte(image,'\n');
                x=0;
              }
          }
          p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
        }
        break;
      }
      case '2':
      {
        /*
          Convert image to a PGM image.
        */
        (void) sprintf(buffer,"%d\n",MaxRGB);
        (void) WriteBlob(image,strlen(buffer),buffer);
        for (i=0; i < (int) image->packets; i++)
        {
          index=Intensity(*p);
          for (j=0; j <= ((int) p->length); j++)
          {
            (void) sprintf(buffer,"%d ",index);
            (void) WriteBlob(image,strlen(buffer),buffer);
            x++;
            if (x == 12)
              {
                (void) WriteByte(image,'\n');
                x=0;
              }
          }
          p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
        }
        break;
      }
      case '3':
      {
        /*
          Convert image to a PNM image.
        */
        (void) sprintf(buffer,"%d\n",MaxRGB);
        (void) WriteBlob(image,strlen(buffer),buffer);
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            (void) sprintf(buffer,"%d %d %d ",p->red,p->green,p->blue);
            (void) WriteBlob(image,strlen(buffer),buffer);
            x++;
            if (x == 4)
              {
                (void) WriteByte(image,'\n');
                x=0;
              }
          }
          p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
        }
        break;
      }
      case '4':
      {
        register unsigned char
          bit,
          byte,
          polarity;

        /*
          Convert image to a PBM image.
        */
        polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
        if (image->colors == 2)
          polarity=
            Intensity(image->colormap[0]) > Intensity(image->colormap[1]);
        bit=0;
        byte=0;
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
                (void) WriteByte(image,byte);
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
                  (void) WriteByte(image,byte << (8-bit));
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(y,image->rows))
                    ProgressMonitor(SaveImageText,y,image->rows);
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
      case '5':
      {
        /*
          Convert image to a PGM image.
        */
        (void) sprintf(buffer,"%lu\n",DownScale(MaxRGB));
        (void) WriteBlob(image,strlen(buffer),buffer);
        for (i=0; i < (int) image->packets; i++)
        {
          index=DownScale(Intensity(*p));
          for (j=0; j <= ((int) p->length); j++)
            (void) WriteByte(image,index);
          p++;
          if (image->previous == (Image *) NULL)
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
        }
        break;
      }
      case '6':
      {
        register unsigned char
          *q;

        unsigned char
          *pixels;

        /*
          Allocate memory for pixels.
        */
        pixels=(unsigned char *)
          AllocateMemory(image->columns*sizeof(RunlengthPacket));
        if (pixels == (unsigned char *) NULL)
          WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
        /*
          Convert image to a PNM image.
        */
        (void) sprintf(buffer,"%lu\n",DownScale(MaxRGB));
        (void) WriteBlob(image,strlen(buffer),buffer);
        q=pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            *q++=DownScale(p->red);
            *q++=DownScale(p->green);
            *q++=DownScale(p->blue);
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
      case '7':
      {
        static const short int
          dither_red[2][16]=
          {
            {-16,  4, -1, 11,-14,  6, -3,  9,-15,  5, -2, 10,-13,  7, -4,  8},
            { 15, -5,  0,-12, 13, -7,  2,-10, 14, -6,  1,-11, 12, -8,  3, -9}
          },
          dither_green[2][16]=
          {
            { 11,-15,  7, -3,  8,-14,  4, -2, 10,-16,  6, -4,  9,-13,  5, -1},
            {-12, 14, -8,  2, -9, 13, -5,  1,-11, 15, -7,  3,-10, 12, -6,  0}
          },
          dither_blue[2][16]=
          {
            { -3,  9,-13,  7, -1, 11,-15,  5, -4,  8,-14,  6, -2, 10,-16,  4},
            {  2,-10, 12, -8,  0,-12, 14, -6,  3, -9, 13, -7,  1,-11, 15, -5}
          };

        int
          value;

        Quantum
          blue,
          green,
          pixel,
          red;

        unsigned short
          *blue_map[2][16],
          *green_map[2][16],
          *red_map[2][16];

        if (!UncondenseImage(image))
          break;
        /*
          Allocate and initialize dither maps.
        */
        for (i=0; i < 2; i++)
          for (j=0; j < 16; j++)
          {
            red_map[i][j]=(unsigned short *)
              AllocateMemory(256*sizeof(unsigned short));
            green_map[i][j]=(unsigned short *)
              AllocateMemory(256*sizeof(unsigned short));
            blue_map[i][j]=(unsigned short *)
              AllocateMemory(256*sizeof(unsigned short));
            if ((red_map[i][j] == (unsigned short *) NULL) ||
                (green_map[i][j] == (unsigned short *) NULL) ||
                (blue_map[i][j] == (unsigned short *) NULL))
              WriterExit(ResourceLimitWarning,"Memory allocation failed",
                image);
          }
        /*
          Initialize dither tables.
        */
        for (i=0; i < 2; i++)
          for (j=0; j < 16; j++)
            for (x=0; x < 256; x++)
            {
              value=x-16;
              if (x < 48)
                value=x/2+8;
              value+=dither_red[i][j];
              red_map[i][j][x]=(unsigned short)
                ((value < 0) ? 0 : (value > 255) ? 255 : value);
              value=x-16;
              if (x < 48)
                value=x/2+8;
              value+=dither_green[i][j];
              green_map[i][j][x]=(unsigned short)
                ((value < 0) ? 0 : (value > 255) ? 255 : value);
              value=x-32;
              if (x < 112)
                value=x/2+24;
              value+=(dither_blue[i][j] << 1);
              blue_map[i][j][x]=(unsigned short)
                ((value < 0) ? 0 : (value > 255) ? 255 : value);
            }
        /*
          Convert image to a P7 image.
        */
        (void) strcpy(buffer,"#END_OF_COMMENTS\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) sprintf(buffer,"%u %u 255\n",image->columns,image->rows);
        (void) WriteBlob(image,strlen(buffer),buffer);
        i=0;
        j=0;
        p=image->pixels;
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            red=DownScale(p->red);
            green=DownScale(p->green);
            blue=DownScale(p->blue);
            pixel=(Quantum) ((red_map[i][j][red] & 0xe0) |
              ((unsigned int) (green_map[i][j][green] & 0xe0) >> 3) |
              ((unsigned int) (blue_map[i][j][blue] & 0xc0) >> 6));
            (void) WriteByte(image,pixel);
            p++;
            j++;
            if (j == 16)
              j=0;
          }
          i++;
          if (i == 2)
            i=0;
          if (QuantumTick(y,image->rows))
            ProgressMonitor(SaveImageText,y,image->rows);
        }
        /*
          Free allocated memory.
        */
        for (i=0; i < 2; i++)
          for (j=0; j < 16; j++)
          {
            FreeMemory((char *) green_map[i][j]);
            FreeMemory((char *) blue_map[i][j]);
            FreeMemory((char *) red_map[i][j]);
          }
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
