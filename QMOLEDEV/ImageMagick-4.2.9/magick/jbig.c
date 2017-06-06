/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        JJJJJ  BBBB   IIIII   GGGG                           %
%                          J    B   B    I    G                               %
%                          J    BBBB     I    G  GG                           %
%                        J J    B   B    I    G   G                           %
%                        JJJ    BBBB   IIIII   GGG                            %
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

#if defined(HasJBIG)
#include "jbig.h"
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d J B I G I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadJBIGImage reads a JBIG image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadJBIGImage method is:
%
%      Image *ReadJBIGImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadJBIGImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadJBIGImage(const ImageInfo *image_info)
{
#define MaxBufferSize  8192

  Image
    *image;

  int
    status,
    y;

  long
    length,
    packets;

  register int
    x;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  size_t
    count;

  struct jbg_dec_state
    jbig_info;

  unsigned char
    bit,
    *buffer;

  unsigned int
    byte;

  unsigned long
    max_packets;

  unsigned short
    index;

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
    Initialize JBIG toolkit.
  */
  jbg_dec_init(&jbig_info);
  jbg_dec_maxsize(&jbig_info,(unsigned long) image->columns,
    (unsigned long) image->rows);
  image->columns=(unsigned int) jbg_dec_getwidth(&jbig_info);
  image->rows=(unsigned int) jbg_dec_getheight(&jbig_info);
  image->class=PseudoClass;
  image->colors=2;
  if (image_info->ping)
    {
      CloseBlob(image);
      return(image);
    }
  /*
    Read JBIG file.
  */
  buffer=(unsigned char *) AllocateMemory(MaxBufferSize*sizeof(unsigned char));
  if (buffer == (unsigned char *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  status=JBG_EAGAIN;
  do
  {
    length=(long) ReadBlob(image,MaxBufferSize,(char *) buffer);
    if (length == 0)
      break;
    p=buffer;
    count=0;
    while ((length > 0) && ((status == JBG_EAGAIN) || (status == JBG_EOK)))
    {
      status=jbg_dec_in(&jbig_info,p,length,&count);
      p+=count;
      length-=count;
    }
  } while ((status == JBG_EAGAIN) || (status == JBG_EOK));
  /*
    Create colormap.
  */
  image->class=PseudoClass;
  image->colors=2;
  image->colormap=(ColorPacket *)
    AllocateMemory(image->colors*sizeof(ColorPacket));
  if (image->colormap == (ColorPacket *) NULL)
    {
      FreeMemory((char *) buffer);
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  image->colormap[0].red=0;
  image->colormap[0].green=0;
  image->colormap[0].blue=0;
  image->colormap[1].red=MaxRGB;
  image->colormap[1].green=MaxRGB;
  image->colormap[1].blue=MaxRGB;
  image->x_resolution=300;
  image->y_resolution=300;
  /*
    Initialize image structure.
  */
  image->columns=(unsigned int) jbg_dec_getwidth(&jbig_info);
  image->rows=(unsigned int) jbg_dec_getheight(&jbig_info);
  packets=0;
  max_packets=Max((image->columns*image->rows+2) >> 2,1);
  image->pixels=(RunlengthPacket *)
    AllocateMemory(max_packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      FreeMemory((char *) buffer);
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  /*
    Convert X bitmap image to runlength-encoded packets.
  */
  byte=0;
  p=jbg_dec_getimage(&jbig_info,0);
  q=image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; y < (int) image->rows; y++)
  {
    bit=0;
    for (x=0; x < (int) image->columns; x++)
    {
      if (bit == 0)
        byte=(*p++);
      index=(byte & 0x80) ? 0 : 1;
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
                  jbg_dec_free(&jbig_info);
                  FreeMemory((char *) buffer);
                  ReaderExit(ResourceLimitWarning,
                    "Memory allocation failed",image);
                }
              q=image->pixels+packets-1;
            }
          q->index=index;
          q->length=0;
        }
      bit++;
      byte<<=1;
      if (bit == 8)
        bit=0;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
  }
  SetRunlengthPackets(image,packets);
  SyncImage(image);
  /*
    Free scale resource.
  */
  jbg_dec_free(&jbig_info);
  FreeMemory((char *) buffer);
  CloseBlob(image);
  return(image);
}
#else
Export Image *ReadJBIGImage(const ImageInfo *image_info)
{
  MagickWarning(MissingDelegateWarning,"JBIG library is not available",
    image_info->filename);
  return((Image *) NULL);
}
#endif

#if defined(HasJBIG)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e J B I G I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteJBIGImage writes an image in the JBIG encoded image format.
%
%  The format of the WriteJBIGImage method is:
%
%      unsigned int WriteJBIGImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteJBIGImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/

static void JBIGEncode(unsigned char *start,size_t length,void *file)
{
  (void) fwrite(start,length,1,(FILE *) file);
  return;
}

Export unsigned int WriteJBIGImage(const ImageInfo *image_info,Image *image)
{
  int
    sans_offset,
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  register unsigned char
    bit,
    *q;

  struct jbg_enc_state
    jbig_info;

  unsigned char
    *pixels,
    polarity;

  unsigned int
    byte,
    scene,
    status;

  unsigned long
    number_packets;

  /*
    Open image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  scene=0;
  do
  {
    /*
      Allocate pixel data.
    */
    TransformRGBImage(image,RGBColorspace);
    number_packets=((image->columns+7) >> 3)*image->rows;
    pixels=(unsigned char *)
      AllocateMemory(number_packets*sizeof(unsigned char));
    if (pixels == (unsigned char *) NULL)
      WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    /*
      Convert Runlength encoded pixels to a bitmap.
    */
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
    polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
    if (image->colors == 2)
      polarity=Intensity(image->colormap[0]) > Intensity(image->colormap[1]);
    bit=0;
    byte=0;
    x=0;
    y=0;
    p=image->pixels;
    q=pixels;
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
    /*
      Initialize JBIG info structure.
    */
    jbg_enc_init(&jbig_info,image->columns,image->rows,1,&pixels,
      (void (*)(unsigned char *,size_t,void *)) JBIGEncode,image->file);
    if (image_info->subimage != 0)
      jbg_enc_layers(&jbig_info,image_info->subimage);
    else
      {
        unsigned int
          x_resolution,
          y_resolution;

        x_resolution=640;
        y_resolution=480;
        if (image_info->density != (char *) NULL)
          (void) ParseGeometry(image_info->density,&sans_offset,&sans_offset,
            &x_resolution,&y_resolution);
        jbg_enc_lrlmax(&jbig_info,x_resolution,y_resolution);
      }
    jbg_enc_lrange(&jbig_info,-1,-1);
    jbg_enc_options(&jbig_info,JBG_ILEAVE | JBG_SMID,JBG_TPDON | JBG_TPBON |
      JBG_DPON,-1,-1,-1);
    /*
      Write JBIG image.
    */
    jbg_enc_out(&jbig_info);
    jbg_enc_free(&jbig_info);
    FreeMemory((char *) pixels);
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
#else
Export unsigned int WriteJBIGImage(const ImageInfo *image_info,Image *image)
{
  MagickWarning(MissingDelegateWarning,"JBIG library is not available",
    image->filename);
  return(False);
}
#endif
