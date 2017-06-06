/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            TTTTT  X   X  TTTTT                              %
%                              T     X X     T                                %
%                              T      X      T                                %
%                              T     X X     T                                %
%                              T    X   X    T                                %
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
%   R e a d T X T I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadTXTImage reads a text file and returns it as an image.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadTXTImage method is:
%
%      Image *ReadTXTImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadTXTImage returns a pointer to the image after
%      reading. A null image is returned if there is a memory shortage or if
%      the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadTXTImage(const ImageInfo *image_info)
{
  AnnotateInfo
    annotate_info;

  char
    filename[MaxTextExtent],
    geometry[MaxTextExtent],
    text[MaxTextExtent];

  ColorPacket
    color;

  double
    dx_resolution,
    dy_resolution;

  Image
    *image,
    *texture;

  int
    count,
    offset;

  RectangleInfo
    page_info;

  register char
    *p;

  register int
    i;

  register RunlengthPacket
    *q;

  unsigned int
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
  status=OpenBlob(image_info,image,"r");
  if (status == False)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  /*
    Set the page geometry.
  */
  dx_resolution=72.0;
  dy_resolution=72.0;
  if ((image->x_resolution == 0.0) || (image->y_resolution == 0.0))
    {
      char
        density[MaxTextExtent];

      (void) strcpy(density,PSDensityGeometry);
      count=sscanf(density,"%lfx%lf",&image->x_resolution,&image->y_resolution);
      if (count != 2)
        image->y_resolution=image->x_resolution;
    }
  page_info.width=612;
  page_info.height=792;
  page_info.x=0;
  page_info.y=0;
  (void) ParseImageGeometry("612x792+43+43",&page_info.x,&page_info.y,
    &page_info.width,&page_info.height);
  if (image_info->page != (char *) NULL)
    (void) ParseImageGeometry(image_info->page,&page_info.x,&page_info.y,
      &page_info.width,&page_info.height);
  /*
    Initialize Image structure.
  */
  image->columns=(unsigned int)
    (((page_info.width*image->x_resolution)/dx_resolution)+0.5);
  image->rows=(unsigned int)
    (((page_info.height*image->y_resolution)/dy_resolution)+0.5);
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  (void) QueryColorDatabase("#c0c0c0",&color);
  image->background_color.red=XDownScale(color.red);
  image->background_color.green=XDownScale(color.green);
  image->background_color.blue=XDownScale(color.blue);
  SetImage(image);
  texture=(Image *) NULL;
  if (image_info->texture != (char *) NULL)
    {
      ImageInfo
        *local_info;

      local_info=CloneImageInfo(image_info);
      if (local_info == (ImageInfo *) NULL)
        return((Image *) NULL);
      (void) strcpy(local_info->filename,image_info->texture);
      texture=ReadImage(local_info);
      if (texture != (Image *) NULL)
        TextureImage(image,texture);
      DestroyImageInfo(local_info);
    }
  /*
    Annotate the text image.
  */
  GetAnnotateInfo((ImageInfo *) image_info,&annotate_info);
  (void) strcpy(filename,image_info->filename);
  offset=0;
  for ( ; ; )
  {
    /*
      Annotate image with text.
    */
    p=GetStringBlob(image,text);
    if (p == (char *) NULL)
      break;
    if (Extent(text) > 0)
      text[Extent(text)-1]='\0';
    (void) CloneString(&annotate_info.text,text);
    FormatString(geometry,"%+d%+d",page_info.x,page_info.y+offset);
    (void) CloneString(&annotate_info.geometry,geometry);
    AnnotateImage(image,&annotate_info);
    offset+=annotate_info.bounds.height;
    if (image->previous == (Image *) NULL)
      if (QuantumTick(page_info.y+offset,image->rows))
        ProgressMonitor(LoadImageText,page_info.y+offset,image->rows);
    if (((2*page_info.y)+offset+annotate_info.bounds.height) < image->rows)
      continue;
    /*
      Page is full-- allocate next image structure.
    */
    image->orphan=True;
    image->next=CloneImage(image,image->columns,image->rows,False);
    image->orphan=False;
    if (image->next == (Image *) NULL)
      {
        DestroyAnnotateInfo(&annotate_info);
        MagickWarning(ResourceLimitWarning,"Unable to annotate image",
          "Memory allocation failed");
        break;
      }
    (void) strcpy(image->next->filename,filename);
    image->next->file=image->file;
    image->next->filesize=image->filesize;
    image->next->scene=image->scene+1;
    image->next->previous=image;
    (void) IsPseudoClass(image);
    image=image->next;
    ProgressMonitor(LoadImagesText,(unsigned int) TellBlob(image),
      (unsigned int) image->filesize);
    /*
      Initialize text image to background color.
    */
    q=image->pixels;
    for (i=0; i < (int) image->packets; i++)
    {
      q->red=XDownScale(color.red);
      q->green=XDownScale(color.green);
      q->blue=XDownScale(color.blue);
      q->index=0;
      q->length=0;
      q++;
    }
    if (texture != (Image *) NULL)
      {
        MonitorHandler
          handler;

        handler=SetMonitorHandler((MonitorHandler) NULL);
        TextureImage(image,texture);
        (void) SetMonitorHandler(handler);
      }
    offset=0;
  }
  if (texture != (Image *) NULL)
    DestroyImage(texture);
  DestroyAnnotateInfo(&annotate_info);
  (void) IsPseudoClass(image);
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
%   W r i t e T X T I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteTXTImage writes the pixel values as text numbers.
%
%  The format of the WriteTXTImage method is:
%
%      unsigned int WriteTXTImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteTXTImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteTXTImage(const ImageInfo *image_info,Image *image)
{
  char
    buffer[MaxTextExtent];

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
      Convert MIFF to TXT raster pixels.
    */
    TransformRGBImage(image,RGBColorspace);
    x=0;
    y=0;
    p=image->pixels;
    for (i=0; i < (int) image->packets; i++)
    {
      for (j=0; j <= ((int) p->length); j++)
      {
        if (image->matte)
          {
            (void) sprintf(buffer,"%d,%d: %d,%d,%d,%d\n",x,y,
              p->red,p->green,p->blue,p->index);
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        else
          {
            (void) sprintf(buffer,"%d,%d: %d,%d,%d  ",x,y,
              p->red,p->green,p->blue);
            (void) WriteBlob(image,strlen(buffer),buffer);
            (void) sprintf(buffer,HexColorFormat,p->red,p->green,p->blue);
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        (void) WriteByte(image,'\n');
        x++;
        if (x == (int) image->columns)
          {
            if (image->previous == (Image *) NULL)
              if (QuantumTick(y,image->rows))
                ProgressMonitor(SaveImageText,y,image->rows);
            x=0;
            y++;
          }
      }
      p++;
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
