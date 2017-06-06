/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            PPPP    CCCC  DDDD                               %
%                            P   P  C      D   D                              %
%                            PPPP   C      D   D                              %
%                            P      C      D   D                              %
%                            P       CCCC  DDDD                               %
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
%   P C D D e c o d e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method PCDDecodeImage recovers the Huffman encoded luminance and
%  chrominance deltas.
%
%  The format of the PCDDecodeImage method is:
%
%      Image *ReadPCDImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o status:  Method PCDDecodeImage returns True if all the deltas are
%      recovered without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%    o luma: The address of a character buffer that contains the
%      luminance information.
%
%    o chroma1: The address of a character buffer that contains the
%      chrominance information.
%
%    o chroma2: The address of a character buffer that contains the
%      chrominance information.
%
%
%
*/
static unsigned int PCDDecodeImage(Image *image,unsigned char *luma,
  unsigned char *chroma1,unsigned char *chroma2)
{
#define IsSync  ((accumulator & 0xffffff00) == 0xfffffe00)
#define PCDDecodeImageText  "  PCD decode image...  "
#define PCDGetBits(n) \
{  \
  accumulator=(accumulator << n) & 0xffffffff; \
  bits-=n; \
  while (bits <= 24) \
  { \
    if (p >= (buffer+0x800)) \
      { \
        (void) ReadBlob(image,0x800,(char *) buffer); \
        p=buffer; \
      } \
    accumulator|=((unsigned int) (*p) << (24-bits)); \
    bits+=8; \
    p++; \
  } \
  if (EOFBlob(image)) \
    break; \
}

  typedef struct PCDTable
  {
    unsigned int
      length,
      sequence;

    unsigned char
      key;

    unsigned int
      mask;
  } PCDTable;

  int
    count;

  PCDTable
    *pcd_table[3];

  register int
    i,
    j;

  register PCDTable
    *r;

  register Quantum
    *range_limit;

  register unsigned char
    *p,
    *q;

  unsigned char
    *buffer;

  unsigned int
    accumulator,
    bits,
    length,
    pcd_length[3],
    plane,
    row;

  /*
    Initialize Huffman tables.
  */
  assert(image != (const Image *) NULL);
  assert(luma != (unsigned char *) NULL);
  assert(chroma1 != (unsigned char *) NULL);
  assert(chroma2 != (unsigned char *) NULL);
  buffer=(unsigned char *) AllocateMemory(0x800*sizeof(unsigned char));
  if (buffer == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  accumulator=0;
  bits=32;
  p=buffer+0x800;
  for (i=0; i < (image->columns > 1536 ? 3 : 1); i++)
  {
    PCDGetBits(8);
    length=(accumulator & 0xff)+1;
    pcd_table[i]=(PCDTable *) AllocateMemory(length*sizeof(PCDTable));
    if (pcd_table[i] == (PCDTable *) NULL)
      {
        MagickWarning(ResourceLimitWarning,"Memory allocation failed",
          (char *) NULL);
        FreeMemory((char *) buffer);
        return(False);
      }
    r=pcd_table[i];
    for (j=0; j < (int) length; j++)
    {
      PCDGetBits(8);
      r->length=(accumulator & 0xff)+1;
      if (r->length > 16)
        {
          FreeMemory((char *) buffer);
          return(False);
        }
      PCDGetBits(16);
      r->sequence=(accumulator & 0xffff) << 16;
      PCDGetBits(8);
      r->key=accumulator & 0xff;
      r->mask=(~((((unsigned int) 1) << (32-r->length))-1));
      r++;
    }
    pcd_length[i]=length;
  }
  /*
    Initialize range limits.
  */
  range_limit=(Quantum *) AllocateMemory(3*(MaxRGB+1)*sizeof(Quantum));
  if (range_limit == (Quantum *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      FreeMemory((char *) buffer);
      return(False);
    }
  for (i=0; i <= MaxRGB; i++)
  {
    range_limit[i]=0;
    range_limit[i+(MaxRGB+1)]=(Quantum) i;
    range_limit[i+(MaxRGB+1)*2]=MaxRGB;
  }
  range_limit+=(MaxRGB+1);
  /*
    Search for Sync byte.
  */
  do { PCDGetBits(16) } while (0);
  do { PCDGetBits(16) } while (0);
  while ((accumulator & 0x00fff000) != 0x00fff000)
    PCDGetBits(8);
  while (!IsSync)
    PCDGetBits(1);
  /*
    Recover the Huffman encoded luminance and chrominance deltas.
  */
  count=0;
  length=0;
  plane=0;
  q=luma;
  for ( ; ; )
  {
    if (IsSync)
      {
        /*
          Determine plane and row number.
        */
        PCDGetBits(16);
        row=((accumulator >> 9) & 0x1fff);
        if (row == image->rows)
          break;
        PCDGetBits(8);
        plane=accumulator >> 30;
        PCDGetBits(16);
        switch (plane)
        {
          case 0:
          {
            q=luma+row*image->columns;
            count=image->columns;
            break;
          }
          case 2:
          {
            q=chroma1+(row >> 1)*image->columns;
            count=image->columns >> 1;
            plane--;
            break;
          }
          case 3:
          {
            q=chroma2+(row >> 1)*image->columns;
            count=image->columns >> 1;
            plane--;
            break;
          }
          default:
          {
            MagickWarning(CorruptImageWarning,"Corrupt PCD image",
              image->filename);
            return(False);
          }
        }
        length=pcd_length[plane];
        if (QuantumTick(row,image->rows))
          ProgressMonitor(PCDDecodeImageText,row,image->rows);
        continue;
      }
    /*
      Decode luminance or chrominance deltas.
    */
    r=pcd_table[plane];
    for (i=0; ((i < (int) length) && ((accumulator & r->mask) != r->sequence)); i++)
      r++;
    if (r == (PCDTable *) NULL)
      {
        MagickWarning(CorruptImageWarning,
          "Corrupt PCD image, skipping to sync byte",image->filename);
        while ((accumulator & 0x00fff000) != 0x00fff000)
          PCDGetBits(8);
        while (!IsSync)
          PCDGetBits(1);
        continue;
      }
    if (r->key < 128)
      *q=range_limit[(int) *q+(int) r->key];
    else
      *q=range_limit[(int) *q+(int) r->key-256];
    q++;
    PCDGetBits(r->length);
    count--;
  }
  /*
    Free memory.
  */
  for (i=0; i < (image->columns > 1536 ? 3 : 1); i++)
    FreeMemory((char *) pcd_table[i]);
  range_limit-=(MaxRGB+1);
  FreeMemory((char *) range_limit);
  FreeMemory((char *) buffer);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d P C D I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadPCDImage reads a Photo CD image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.  Much of the PCD decoder was derived from
%  the program hpcdtoppm(1) by Hadmut Danisch.
%
%  The format of the ReadPCDImage method is:
%
%      image=ReadPCDImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadPCDImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
static Image *OverviewImage(const ImageInfo *image_info,Image *image)
{
  char
    *commands[3];

  Image
    *montage_image;

  ImageInfo
    *local_info;

  MontageInfo
    montage_info;

  /*
    Create image tiles.
  */
  local_info=CloneImageInfo(image_info);
  if (local_info == (ImageInfo *) NULL)
    return((Image *) NULL);
  commands[0]=SetClientName((char *) NULL);
  commands[1]="-label";
  commands[2]=(char *) DefaultTileLabel;
  MogrifyImages(local_info,3,commands,&image);
  DestroyImageInfo(local_info);
  /*
    Create the PCD Overview image.
  */
  GetMontageInfo(&montage_info);
  (void) strcpy(montage_info.filename,image_info->filename);
  (void) CloneString(&montage_info.font,image_info->font);
  montage_info.pointsize=image_info->pointsize;
  montage_image=MontageImages(image,&montage_info);
  DestroyMontageInfo(&montage_info);
  if (montage_image == (Image *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  DestroyImage(image);
  return(montage_image);
}

Export Image *ReadPCDImage(const ImageInfo *image_info)
{
  Image
    *image;

  long int
    offset;

  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned char
    *c1,
    *c2,
    *y;

  unsigned char
    *chroma1,
    *chroma2,
    *header,
    *luma;

  unsigned int
    height,
    number_images,
    overview,
    rotate,
    status,
    subimage,
    width;

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
    Determine if this is a PCD file.
  */
  header=(unsigned char *) AllocateMemory(3*0x800*sizeof(unsigned char));
  if (header == (unsigned char *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  status=ReadBlob(image,3*0x800,(char *) header);
  overview=strncmp((char *) header,"PCD_OPA",7) == 0;
  if ((status == False) ||
      ((strncmp((char *) header+0x800,"PCD",3) != 0) && !overview))
    ReaderExit(CorruptImageWarning,"Not a PCD image file",image);
  rotate=header[0x0e02] & 0x03;
  number_images=(header[10] << 8) | header[11];
  FreeMemory((char *) header);
  /*
    Determine resolution by subimage specification.
  */
  if ((image->columns*image->rows) == 0)
    subimage=3;
  else
    {
      width=192;
      height=128;
      for (subimage=1; subimage < 6; subimage++)
      {
        if ((width >= image->columns) && (height >= image->rows))
          break;
        width<<=1;
        height<<=1;
      }
    }
  if (image_info->subrange != 0)
    subimage=Min(image_info->subimage,6);
  if (overview)
    subimage=1;
  /*
    Initialize image structure.
  */
  width=192;
  height=128;
  for (i=1; i < Min((int) subimage,3); i++)
  {
    width<<=1;
    height<<=1;
  }
  image->columns=width;
  image->rows=height;
  for ( ; i < (int) subimage; i++)
  {
    image->columns<<=1;
    image->rows<<=1;
  }
  if (image_info->ping)
    {
      if ((rotate == 1) || (rotate == 3))
        Swap(image->columns,image->rows);
      CloseBlob(image);
      return(image);
    }
  /*
    Allocate luma and chroma memory.
  */
  image->packets=image->columns*image->rows;
  chroma1=(unsigned char *)
    AllocateMemory((image->packets+1)*sizeof(unsigned char));
  chroma2=(unsigned char *)
    AllocateMemory((image->packets+1)*sizeof(unsigned char));
  luma=(unsigned char *)
    AllocateMemory((image->packets+1)*sizeof(unsigned char));
  if ((chroma1 == (unsigned char *) NULL) ||
      (chroma2 == (unsigned char *) NULL) || (luma == (unsigned char *) NULL))
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Advance to image data.
  */
  offset=93;
  if (overview)
    offset=2;
  else
    if (subimage == 2)
      offset=20;
    else
      if (subimage <= 1)
        offset=1;
  for (i=0; i < (offset*0x800); i++)
    (void) ReadByte(image);
  if (overview)
    {
      Image
        *overview_image;

      MonitorHandler
        handler;

      register int
        j;

      /*
        Read thumbnails from overview image.
      */
      for (j=1; j <= (int) number_images; j++)
      {
        handler=SetMonitorHandler((MonitorHandler) NULL);
        FormatString(image->filename,"images/img%04d.pcd",j);
        FormatString(image->magick_filename,"images/img%04d.pcd",j);
        image->scene=j;
        image->columns=width;
        image->rows=height;
        image->packets=image->columns*image->rows;
        image->pixels=(RunlengthPacket *)
          AllocateMemory(image->packets*sizeof(RunlengthPacket));
        if (image->pixels == (RunlengthPacket *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        SetImage(image);
        y=luma;
        c1=chroma1;
        c2=chroma2;
        for (i=0; i < (int) height; i+=2)
        {
          (void) ReadBlob(image,width,(char *) y);
          y+=image->columns;
          (void) ReadBlob(image,width,(char *) y);
          y+=image->columns;
          (void) ReadBlob(image,width >> 1,(char *) c1);
          c1+=image->columns;
          (void) ReadBlob(image,width >> 1,(char *) c2);
          c2+=image->columns;
        }
        Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma1);
        Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma2);
        /*
          Transfer luminance and chrominance channels.
        */
        p=image->pixels;
        y=luma;
        c1=chroma1;
        c2=chroma2;
        for (i=0; i < (int) image->packets; i++)
        {
          p->red=UpScale(*y++);
          p->green=UpScale(*c1++);
          p->blue=UpScale(*c2++);
          p->index=0;
          p->length=0;
          p++;
        }
        if (Latin1Compare(image_info->magick,"PCDS") == 0)
          TransformRGBImage(image,sRGBColorspace);
        else
          TransformRGBImage(image,YCCColorspace);
        if (j < (int) number_images)
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
          }
        (void) SetMonitorHandler(handler);
        ProgressMonitor(LoadImageText,j-1,number_images);
      }
      FreeMemory(chroma2);
      FreeMemory(chroma1);
      FreeMemory(luma);
      CondenseImage(image);
      while (image->previous != (Image *) NULL)
        image=image->previous;
      overview_image=OverviewImage(image_info,image);
      return(overview_image);
    }
  /*
    Allocate image pixels.
  */
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  SetImage(image);
  /*
    Read interleaved image.
  */
  y=luma;
  c1=chroma1;
  c2=chroma2;
  for (i=0; i < (int) height; i+=2)
  {
    (void) ReadBlob(image,width,(char *) y);
    y+=image->columns;
    (void) ReadBlob(image,width,(char *) y);
    y+=image->columns;
    (void) ReadBlob(image,width >> 1,(char *) c1);
    c1+=image->columns;
    (void) ReadBlob(image,width >> 1,(char *) c2);
    c2+=image->columns;
  }
  if (subimage >= 4)
    {
      /*
        Recover luminance deltas for 1536x1024 image.
      */
      Upsample(768,512,image->columns,luma);
      Upsample(384,256,image->columns,chroma1);
      Upsample(384,256,image->columns,chroma2);
      image->rows=1024;
      for (i=0; i < (4*0x800); i++)
        (void) ReadByte(image);
      status=PCDDecodeImage(image,luma,chroma1,chroma2);
      if ((subimage >= 5) && status)
        {
          /*
            Recover luminance deltas for 3072x2048 image.
          */
          Upsample(1536,1024,image->columns,luma);
          Upsample(768,512,image->columns,chroma1);
          Upsample(768,512,image->columns,chroma2);
          image->rows=2048;
          offset=TellBlob(image)/0x800+12;
          (void) SeekBlob(image,offset*0x800,SEEK_SET);
          status=PCDDecodeImage(image,luma,chroma1,chroma2);
          if ((subimage >= 6) && status)
            {
              /*
                Recover luminance deltas for 6144x4096 image (vaporware).
              */
              Upsample(3072,2048,image->columns,luma);
              Upsample(1536,1024,image->columns,chroma1);
              Upsample(1536,1024,image->columns,chroma2);
              image->rows=4096;
            }
        }
    }
  Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma1);
  Upsample(image->columns >> 1,image->rows >> 1,image->columns,chroma2);
  /*
    Transfer luminance and chrominance channels.
  */
  p=image->pixels;
  y=luma;
  c1=chroma1;
  c2=chroma2;
  for (i=0; i < (int) image->packets; i++)
  {
    p->red=UpScale(*y++);
    p->green=UpScale(*c1++);
    p->blue=UpScale(*c2++);
    p->index=0;
    p->length=0;
    p++;
    if (QuantumTick(i,image->columns*image->rows))
      ProgressMonitor(LoadImageText,i,image->columns*image->rows);
  }
  FreeMemory(chroma2);
  FreeMemory(chroma1);
  FreeMemory(luma);
  if (Latin1Compare(image_info->magick,"PCDS") == 0)
    TransformRGBImage(image,sRGBColorspace);
  else
    TransformRGBImage(image,YCCColorspace);
  if ((rotate == 1) || (rotate == 3))
    {
      double
        degrees;

      Image
        *rotated_image;

      /*
        Rotate image.
      */
      degrees=rotate == 1 ? -90.0 : 90.0;
      image->orphan=True;
      rotated_image=RotateImage(image,degrees,False,True);
      image->orphan=False;
      if (rotated_image != (Image *) NULL)
        {
          DestroyImage(image);
          image=rotated_image;
        }
    }
  /*
    Set CCIR 709 primaries with a D65 white point.
  */
  image->chromaticity.red_primary.x=0.64f;
  image->chromaticity.red_primary.y=0.33f;
  image->chromaticity.green_primary.x=0.30f;
  image->chromaticity.green_primary.y=0.60f;
  image->chromaticity.blue_primary.x=0.15f;
  image->chromaticity.blue_primary.y=0.06f;
  image->chromaticity.white_point.x=0.3127f;
  image->chromaticity.white_point.y=0.3290f;
  image->gamma=0.5;
  CondenseImage(image);
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e P C D I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WritePCDImage writes an image in the Photo CD encoded image
%  format.
%
%  The format of the WritePCDImage method is:
%
%      unsigned int WritePCDTile(const ImageInfo *image_info,Image *image,
%        char *geometry,char *tile_geometry)
%
%  A description of each parameter follows.
%
%    o status: Method WritePCDImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/

Export unsigned int WritePCDTile(const ImageInfo *image_info,Image *image,
  char *geometry,char *tile_geometry)
{
  Image
    *downsampled_image,
    *tile_image;

  int
    x,
    y;

  register int
    i;

  register RunlengthPacket
    *p,
    *q;

  unsigned int
    height,
    width;

  /*
    Scale image to tile size.
  */
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  (void) ParseImageGeometry(geometry,&x,&y,&width,&height);
  if ((width % 2) != 0)
    width--;
  if ((height % 2) != 0)
    height--;
  image->orphan=True;
  tile_image=ZoomImage(image,width,height);
  image->orphan=False;
  if (tile_image == (Image *) NULL)
    WriterExit(ResourceLimitWarning,"Unable to scale image",image);
  (void) sscanf(geometry,"%ux%u",&width,&height);
  if ((tile_image->columns != width) || (tile_image->rows != height))
    {
      Image
        *bordered_image;

      RectangleInfo
        border_info;

      /*
        Put a border around the image.
      */
      border_info.width=(width-tile_image->columns+1) >> 1;
      border_info.height=(height-tile_image->rows+1) >> 1;
      bordered_image=BorderImage(tile_image,&border_info);
      if (bordered_image == (Image *) NULL)
        WriterExit(ResourceLimitWarning,"Unable to border image",image);
      DestroyImage(tile_image);
      tile_image=bordered_image;
    }
  TransformImage(&tile_image,(char *) NULL,tile_geometry);
  RGBTransformImage(tile_image,YCCColorspace);
  downsampled_image=MinifyImage(tile_image);
  if (downsampled_image == (Image *) NULL)
    WriterExit(ResourceLimitWarning,"Unable to scale image",image);
  if (!UncondenseImage(tile_image))
    return(False);
  if (!UncondenseImage(downsampled_image))
    return(False);
  /*
    Write tile to PCD file.
  */
  p=tile_image->pixels;
  for (y=0; y < (int) tile_image->rows; y+=2)
  {
    for (x=0; x < (int) (tile_image->columns << 1); x++)
    {
      (void) WriteByte(image,DownScale(p->red));
      p++;
    }
    q=downsampled_image->pixels+(y >> 1)*downsampled_image->columns;
    for (x=0; x < (int) downsampled_image->columns; x++)
    {
      (void) WriteByte(image,DownScale(q->green));
      q++;
    }
    q=downsampled_image->pixels+(y >> 1)*downsampled_image->columns;
    for (x=0; x < (int) downsampled_image->columns; x++)
    {
      (void) WriteByte(image,DownScale(q->blue));
      q++;
    }
    if (QuantumTick(y,tile_image->rows))
      ProgressMonitor(SaveImageText,y,tile_image->rows);
  }
  for (i=0; i < 0x800; i++)
    (void) WriteByte(image,'\0');
  DestroyImage(downsampled_image);
  DestroyImage(tile_image);
  return(True);
}

Export unsigned int WritePCDImage(const ImageInfo *image_info,Image *image)
{
  Image
    *pcd_image;

  register int
    i;

  unsigned int
    status;

  pcd_image=image;
  if (image->columns < image->rows)
    {
      Image
        *rotated_image;

      /*
        Rotate portrait to landscape.
      */
      image->orphan=True;
      rotated_image=RotateImage(image,90.0,False,True);
      image->orphan=False;
      if (rotated_image == (Image *) NULL)
        WriterExit(ResourceLimitWarning,"Unable to rotate image",image);
      pcd_image=rotated_image;
    }
  /*
    Open output image file.
  */
  status=OpenBlob(image_info,pcd_image,WriteBinaryType);
  if (pcd_image->file == (FILE *) NULL)
    WriterExit(FileOpenWarning,"Unable to open file",pcd_image);
  TransformRGBImage(pcd_image,RGBColorspace);
  /*
    Write PCD image header.
  */
  for (i=0; i < 32; i++)
    (void) WriteByte(pcd_image,0xff);
  for (i=0; i < 4; i++)
    (void) WriteByte(pcd_image,0x0e);
  for (i=0; i < 8; i++)
    (void) WriteByte(pcd_image,'\0');
  for (i=0; i < 4; i++)
    (void) WriteByte(pcd_image,0x01);
  for (i=0; i < 4; i++)
    (void) WriteByte(pcd_image,0x05);
  for (i=0; i < 8; i++)
    (void) WriteByte(pcd_image,'\0');
  for (i=0; i < 4; i++)
    (void) WriteByte(pcd_image,0x0A);
  for (i=0; i < 36; i++)
    (void) WriteByte(pcd_image,'\0');
  for (i=0; i < 4; i++)
    (void) WriteByte(pcd_image,0x01);
  for (i=0; i < 1944; i++)
    (void) WriteByte(pcd_image,'\0');
  (void) WriteBlob(pcd_image,7,"PCD_IPI");
  (void) WriteByte(pcd_image,0x06);
  for (i=0; i < 1530; i++)
    (void) WriteByte(pcd_image,'\0');
  if (image->columns < image->rows)
    (void) WriteByte(pcd_image,'\1');
  else
    (void) WriteByte(pcd_image,'\0');
  for (i=0; i < 3*0x800-1539; i++)
    (void) WriteByte(pcd_image,'\0');
  /*
    Write PCD tiles.
  */
  status=WritePCDTile(image_info,pcd_image,"768x512>","192x128");
  status|=WritePCDTile(image_info,pcd_image,"768x512>","384x256");
  status|=WritePCDTile(image_info,pcd_image,"768x512>","768x512");
  CloseBlob(pcd_image);
  if (image->columns < image->rows)
    DestroyImage(pcd_image);
  return(status);
}
