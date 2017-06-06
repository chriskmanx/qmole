/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                     IIIII  M   M   AAA    GGGG  EEEEE                       %
%                       I    MM MM  A   A  G      E                           %
%                       I    M M M  AAAAA  G  GG  EEE                         %
%                       I    M   M  A   A  G   G  E                           %
%                     IIIII  M   M  A   A   GGGG  EEEEE                       %
%                                                                             %
%                                                                             %
%                          ImageMagick Image Methods                          %
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
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"
#if defined(HasX11)
#include "xwindows.h"
#endif

/*
  Constant declaration.
*/
const char
  *DefaultPointSize = "12",
  *DefaultTileFrame = "15x15+3+3",
  *DefaultTileLabel = "%f\n%wx%h\n%b",
  *DefaultTileGeometry = "106x106+4+3>",
  *LoadImageText = "  Loading image...  ",
  *LoadImagesText = "  Loading images...  ",
  *ReadBinaryType = "rb",
  *ReadBinaryUnbufferedType = "rbu",
  *SaveImageText = "  Saving image...  ",
  *SaveImagesText = "  Saving images...  ",
  *WriteBinaryType = "wb";

const char
  *BackgroundColor = "#bdbdbd",  /* gray */
  *BorderColor = "#bdbdbd",  /* gray */
  *ForegroundColor = "#000",  /* black */
  *MatteColor = "#bdbdbd";  /* gray */

const InterlaceType
  DefaultInterlace = NoInterlace;

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   A l l o c a t e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method AllocateImage allocates an Image structure and initializes each
%  field to a default value.
%
%  The format of the AllocateImage method is:
%
%      Image *AllocateImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o allocated_image: Method AllocateImage returns a pointer to an image
%      structure initialized to default values.  A null image is returned if
%      there is a memory shortage.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *AllocateImage(const ImageInfo *image_info)
{
  ColorPacket
    color;

  Image
    *allocated_image;

  int
    flags;

  /*
    Allocate image structure.
  */
  allocated_image=(Image *) AllocateMemory(sizeof(Image));
  if (allocated_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to allocate image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Initialize Image structure.
  */
  GetBlobInfo(&(allocated_image->blob));
  allocated_image->file=(FILE *) NULL;
  allocated_image->exempt=False;
  allocated_image->status=False;
  allocated_image->temporary=False;
  *allocated_image->filename='\0';
  allocated_image->filesize=0;
  allocated_image->pipe=False;
  (void) strcpy(allocated_image->magick,"MIFF");
  allocated_image->comments=(char *) NULL;
  allocated_image->label=(char *) NULL;
  allocated_image->class=DirectClass;
  allocated_image->matte=False;
  allocated_image->compression=RunlengthEncodedCompression;
  allocated_image->columns=0;
  allocated_image->rows=0;
  allocated_image->depth=QuantumDepth;
  allocated_image->tile_info.width=0;
  allocated_image->tile_info.height=0;
  allocated_image->tile_info.x=0;
  allocated_image->tile_info.y=0;
  allocated_image->offset=0;
  allocated_image->interlace=DefaultInterlace;
  allocated_image->scene=0;
  allocated_image->units=UndefinedResolution;
  allocated_image->x_resolution=0.0;
  allocated_image->y_resolution=0.0;
  allocated_image->montage=(char *) NULL;
  allocated_image->directory=(char *) NULL;
  allocated_image->colormap=(ColorPacket *) NULL;
  allocated_image->colors=0;
  allocated_image->colorspace=RGBColorspace;
  allocated_image->rendering_intent=UndefinedIntent;
  allocated_image->gamma=0.0;
  allocated_image->chromaticity.red_primary.x=0.0;
  allocated_image->chromaticity.red_primary.y=0.0;
  allocated_image->chromaticity.green_primary.x=0.0;
  allocated_image->chromaticity.green_primary.y=0.0;
  allocated_image->chromaticity.blue_primary.x=0.0;
  allocated_image->chromaticity.blue_primary.y=0.0;
  allocated_image->chromaticity.white_point.x=0.0;
  allocated_image->chromaticity.white_point.y=0.0;
  allocated_image->color_profile.length=0;
  allocated_image->color_profile.info=(unsigned char *) NULL;
  allocated_image->iptc_profile.length=0;
  allocated_image->iptc_profile.info=(unsigned char *) NULL;
  allocated_image->pixels=(RunlengthPacket *) NULL;
  allocated_image->packets=0;
  allocated_image->packet_size=0;
  allocated_image->packed_pixels=(unsigned char *) NULL;
  allocated_image->geometry=(char *) NULL;
  allocated_image->page=(char *) NULL;
  allocated_image->dispose=0;
  allocated_image->delay=0;
  allocated_image->iterations=1;
  allocated_image->fuzz=0;
  allocated_image->filter=LanczosFilter;
  allocated_image->blur=1.0;
  (void) QueryColorDatabase(BackgroundColor,&color);
  allocated_image->background_color.red=XDownScale(color.red);
  allocated_image->background_color.green=XDownScale(color.green);
  allocated_image->background_color.blue=XDownScale(color.blue);
  allocated_image->background_color.index=Opaque;
  (void) QueryColorDatabase(BorderColor,&color);
  allocated_image->border_color.red=XDownScale(color.red);
  allocated_image->border_color.green=XDownScale(color.green);
  allocated_image->border_color.blue=XDownScale(color.blue);
  allocated_image->border_color.index=Opaque;
  (void) QueryColorDatabase(MatteColor,&color);
  allocated_image->matte_color.red=XDownScale(color.red);
  allocated_image->matte_color.green=XDownScale(color.green);
  allocated_image->matte_color.blue=XDownScale(color.blue);
  allocated_image->matte_color.index=Opaque;
  allocated_image->total_colors=0;
  allocated_image->normalized_mean_error=0.0;
  allocated_image->normalized_maximum_error=0.0;
  allocated_image->mean_error_per_pixel=0;
  allocated_image->signature=(char *) NULL;
  *allocated_image->magick_filename='\0';
  allocated_image->magick_columns=0;
  allocated_image->magick_rows=0;
  allocated_image->magick_time=time((time_t *) NULL);
  allocated_image->tainted=False;
  allocated_image->orphan=False;
  allocated_image->previous=(Image *) NULL;
  allocated_image->list=(Image *) NULL;
  allocated_image->next=(Image *) NULL;
  allocated_image->restart_animation_here=False;
  if (image_info == (ImageInfo *) NULL)
    return(allocated_image);
  /*
    Transfer image info.
  */
  allocated_image->blob=image_info->blob;
  (void) strcpy(allocated_image->filename,image_info->filename);
  (void) strcpy(allocated_image->magick_filename,image_info->filename);
  (void) strcpy(allocated_image->magick,image_info->magick);
  if (image_info->size != (char *) NULL)
    {
      int
        y;

      (void) sscanf(image_info->size,"%ux%u",
        &allocated_image->columns,&allocated_image->rows);
      flags=ParseGeometry(image_info->size,&allocated_image->offset,&y,
        &allocated_image->columns,&allocated_image->rows);
      if ((flags & HeightValue) == 0)
        allocated_image->rows=allocated_image->columns;
      allocated_image->tile_info.width=allocated_image->columns;
      allocated_image->tile_info.height=allocated_image->rows;
    }
  if (image_info->tile != (char *) NULL)
    if (!IsSubimage(image_info->tile,False))
      {
        (void) sscanf(image_info->tile,"%ux%u",
          &allocated_image->columns,&allocated_image->rows);
        flags=ParseGeometry(image_info->tile,&allocated_image->tile_info.x,
          &allocated_image->tile_info.y,&allocated_image->columns,
          &allocated_image->rows);
        if ((flags & HeightValue) == 0)
          allocated_image->rows=allocated_image->columns;
      }
  allocated_image->compression=image_info->compression;
  allocated_image->interlace=image_info->interlace;
  allocated_image->units=image_info->units;
  if (image_info->density != (char *) NULL)
    {
      int
        count;

      count=sscanf(image_info->density,"%lfx%lf",&allocated_image->x_resolution,
        &allocated_image->y_resolution);
      if (count != 2)
        allocated_image->y_resolution=allocated_image->x_resolution;
    }
  if (image_info->page != (char *) NULL)
    allocated_image->page=PostscriptGeometry(image_info->page);
  if (image_info->dispose != (char *) NULL)
    allocated_image->dispose=atoi(image_info->dispose);
  if (image_info->delay != (char *) NULL)
    allocated_image->delay=atoi(image_info->delay);
  if (image_info->iterations != (char *) NULL)
    allocated_image->iterations=atoi(image_info->iterations);
  allocated_image->depth=image_info->depth;
  if (image_info->background_color != (char *) NULL)
    {
      (void) QueryColorDatabase(image_info->background_color,&color);
      allocated_image->background_color.red=XDownScale(color.red);
      allocated_image->background_color.green=XDownScale(color.green);
      allocated_image->background_color.blue=XDownScale(color.blue);
    }
  if (image_info->border_color != (char *) NULL)
    {
      (void) QueryColorDatabase(image_info->border_color,&color);
      allocated_image->border_color.red=XDownScale(color.red);
      allocated_image->border_color.green=XDownScale(color.green);
      allocated_image->border_color.blue=XDownScale(color.blue);
    }
  if (image_info->matte_color != (char *) NULL)
    {
      (void) QueryColorDatabase(image_info->matte_color,&color);
      allocated_image->matte_color.red=XDownScale(color.red);
      allocated_image->matte_color.green=XDownScale(color.green);
      allocated_image->matte_color.blue=XDownScale(color.blue);
    }
  return(allocated_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   A l l o c a t e N e x t I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method AllocateNextImage allocates an Image structure and initializes each
%  field to a default value.
%
%  The format of the AllocateNextImage method is:
%
%      void AllocateNextImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void AllocateNextImage(const ImageInfo *image_info,Image *image)
{
  /*
    Allocate image structure.
  */
  assert(image != (Image *) NULL);
  image->next=AllocateImage(image_info);
  if (image->next == (Image *) NULL)
    return;
  (void) strcpy(image->next->filename,image->filename);
  if (image_info != (ImageInfo *) NULL)
    (void) strcpy(image->next->filename,image_info->filename);
  image->next->file=image->file;
  image->next->filesize=image->filesize;
  image->next->scene=image->scene+1;
  image->next->previous=image;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   A n i m a t e I m a g e s                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method AnimateImages displays one or more images to an X window.
%
%  The format of the AnimateImages method is:
%
%      unsigned int AnimateImages(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method AnimateImages returns True if the images are displayed
%      in an X window, otherwise False is returned.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int AnimateImages(const ImageInfo *image_info,Image *image)
{
#if defined(HasX11)
  char
    *client_name;

  Display
    *display;

  XrmDatabase
    resource_database;

  XResourceInfo
    resource;

  display=XOpenDisplay((char *) NULL);
  if (display == (Display *) NULL)
    return(False);
  XSetErrorHandler(XError);
  client_name=SetClientName((char *) NULL);
  resource_database=XGetResourceDatabase(display,client_name);
  XGetResourceInfo(resource_database,client_name,&resource);
  *resource.image_info=(*image_info);
  resource.immutable=True;
  if (image_info->delay != (char *) NULL)
    resource.delay=atoi(image_info->delay);
  (void) XAnimateImages(display,&resource,&client_name,1,image);
  XCloseDisplay(display);
  return(True);
#else
  MagickWarning(MissingDelegateWarning,"X11 library is not available",
    image->filename);
  return(False);
#endif
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     A p p e n d I m a g e s                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method AppendImages appends a set of images.  All the input images must
%  have the same width or height.  Images of the same width are stacked
%  top-to-bottom.  Images of the same height are stacked left-to-right.
%  If stack is false, rectangular images are stacked left-to-right otherwise
%  top-to-bottom.
%
%  The format of the AppendImage method is:
%
%      Image *AppendImages(Image *images,const unsigned int stack)
%
%  A description of each parameter follows:
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o stack: An unsigned value other than stacks rectangular images
%      top-to-bottom otherwise left-to-right.
%
%
*/
Export Image *AppendImages(Image *images,const unsigned int stack)
{
#define AppendImageText  "  Appending image sequence...  "

  Image
    *appended_image,
    *image;

  register int
    i;

  register RunlengthPacket
    *p,
    *q;

  unsigned int
    height,
    max_packets,
    scene,
    width;

  /*
    Ensure the images have the same column width.
  */
  assert(images != (Image *) NULL);
  if (images->next == (Image *) NULL)
    {
      MagickWarning(OptionWarning,"Unable to append image",
        "image sequence required");
      return((Image *) NULL);
    }
  for (image=images->next; image != (Image *) NULL; image=image->next)
    if ((image->columns != images->columns) &&
        (image->rows != images->rows))
      {
        MagickWarning(OptionWarning,"Unable to append image",
          "image widths or heights differ");
        return((Image *) NULL);
      }
  width=images->columns;
  height=images->rows;
  max_packets=images->packets;
  for (image=images->next; image != (Image *) NULL; image=image->next)
  {
    width+=image->columns;
    height+=image->rows;
    max_packets+=image->packets;
  }
  /*
    Initialize append image attributes.
  */
  images->orphan=True;
  if ((images->columns != images->next->columns) || !stack)
    appended_image=CloneImage(images,width,images->rows,False);
  else
    appended_image=CloneImage(images,images->columns,images->rows,False);
  images->orphan=False;
  if (appended_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to append image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  scene=0;
  if ((images->columns != images->next->columns) || !stack)
    {
      register int
        x;

      /*
        Stack left-to-right.
      */
      x=0;
      for (image=images; image != (Image *) NULL; image=image->next)
      {
        if (image->class == DirectClass)
          appended_image->class=DirectClass;
        CompositeImage(appended_image,ReplaceCompositeOp,image,x,0);
        x+=image->columns;
        ProgressMonitor(AppendImageText,scene,GetNumberScenes(images));
        scene++;
      }
    }
  else
    {
      /*
        Stack top-to-bottom.
      */
      appended_image->rows=height;
      appended_image->packets=max_packets;
      appended_image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
        appended_image->pixels,appended_image->packets*sizeof(RunlengthPacket));
      if (appended_image->pixels == (RunlengthPacket *) NULL)
        {
          DestroyImage(appended_image);
          MagickWarning(ResourceLimitWarning,"Unable to append image",
            "Memory allocation failed");
          return((Image *) NULL);
        }
      q=appended_image->pixels;
      for (image=images; image != (Image *) NULL; image=image->next)
      {
        if (image->class == DirectClass)
          appended_image->class=DirectClass;
        p=image->pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          *q=(*p);
          p++;
          q++;
        }
        ProgressMonitor(AppendImageText,scene,GetNumberScenes(images));
        scene++;
      }
    }
  if (appended_image->class == PseudoClass)
    {
      unsigned int
        global_colormap;

      /*
        Determine if the sequence of images has the identical colormap.
      */
      global_colormap=True;
      for (image=images; image != (Image *) NULL; image=image->next)
      {
        if ((image->class == DirectClass) ||
            (image->colors != images->colors))
          {
            global_colormap=False;
            break;
          }
        for (i=0; i < (int) images->colors; i++)
          if (!ColorMatch(image->colormap[i],images->colormap[i],image->fuzz))
            {
              global_colormap=False;
              break;
            }
      }
      if (!global_colormap)
        appended_image->class=DirectClass;
    }
  return(appended_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     A v e r a g e I m a g e s                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method AverageImages averages a set of images.  All the input images must
%  be the same size in pixels.
%
%  The format of the AverageImage method is:
%
%      Image *AverageImages(const Image *images)
%
%  A description of each parameter follows:
%
%    o averaged_image: Method AverageImages returns the mean pixel value
%      for an image sequence.
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export Image *AverageImages(const Image *images)
{
#define AverageImageText  "  Average image sequence...  "

  typedef struct _SumPacket
  {
    double
      red,
      green,
      blue,
      index;
  } SumPacket;

  const Image
    *image;

  Image
    *averaged_image;

  int
    x;

  register int
    i,
    j;

  register RunlengthPacket
    *p,
    *q;

  SumPacket
    *sum;

  unsigned int
    number_scenes;

  assert(images != (Image *) NULL);
  if (images->next == (Image *) NULL)
    {
      MagickWarning(OptionWarning,"Unable to average image",
        "image sequence required");
      return((Image *) NULL);
    }
  /*
    Ensure the images are the same size.
  */
  for (image=images; image != (Image *) NULL; image=image->next)
  {
    if ((image->columns != images->columns) ||
        (image->rows != images->rows))
      {
        MagickWarning(OptionWarning,"Unable to average image",
          "images are not the same size");
        return((Image *) NULL);
      }
  }
  /*
    Allocate sum accumulation buffer.
  */
  sum=(SumPacket *)
    AllocateMemory(images->columns*images->rows*sizeof(SumPacket));
  if (sum == (SumPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to average image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  for (i=0; i < (int) (images->columns*images->rows); i++)
  {
    sum[i].red=0.0;
    sum[i].green=0.0;
    sum[i].blue=0.0;
    sum[i].index=0.0;
  }
  /*
    Initialize average image attributes.
  */
  ((Image *) images)->orphan=True;
  averaged_image=CloneImage(images,images->columns,images->rows,False);
  ((Image *) images)->orphan=False;
  if (averaged_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to average image",
        "Memory allocation failed");
      FreeMemory((char *) sum);
      return((Image *) NULL);
    }
  /*
    Compute sum over each pixel color component.
  */
  averaged_image->class=DirectClass;
  number_scenes=0;
  for (image=images; image != (Image *) NULL; image=image->next)
  {
    x=0;
    p=image->pixels;
    for (i=0; i < (int) image->packets; i++)
    {
      for (j=0; j <= (int) p->length; j++)
      {
        sum[x].red+=p->red;
        sum[x].green+=p->green;
        sum[x].blue+=p->blue;
        sum[x].index+=p->index;
        x++;
      }
      p++;
    }
    number_scenes++;
  }
  /*
    Average image pixels.
  */
  q=averaged_image->pixels;
  for (i=0; i < (int) averaged_image->packets; i++)
  {
    q->red=(Quantum) ((sum[i].red+number_scenes/2.0)/number_scenes);
    q->green=(Quantum) ((sum[i].green+number_scenes/2.0)/number_scenes);
    q->blue=(Quantum) ((sum[i].blue+number_scenes/2.0)/number_scenes);
    q->index=(Quantum) ((sum[i].index+number_scenes/2.0)/number_scenes);
    q->length=0;
    q++;
    if (QuantumTick(i,averaged_image->packets))
      ProgressMonitor(AverageImageText,i,averaged_image->packets);
  }
  FreeMemory((char *) sum);
  return(averaged_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C l o n e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CloneImage returns a copy of all fields of the input image.  The
%  the pixel memory is allocated but the pixel data is not copied.
%
%  The format of the CloneImage method is:
%
%      Image *CloneImage(const Image *image,const unsigned int columns,
%        const unsigned int rows,const unsigned int clone_pixels)
%
%  A description of each parameter follows:
%
%    o clone_image: Method CloneImage returns a pointer to the image after
%      copying.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the copied
%      image.
%
%    o rows: An integer that specifies the number of rows in the copied
%      image.
%
%    o clone_pixels: Specifies whether the pixel data is copied.  Must be
%      either True or False;
%
%
*/
Export Image *CloneImage(const Image *image,const unsigned int columns,
  const unsigned int rows,const unsigned int clone_pixels)
{
  Image
    *clone_image;

  register int
    i;

  /*
    Allocate image structure.
  */
  assert(image != (Image *) NULL);
  clone_image=(Image *) AllocateMemory(sizeof(Image));
  if (clone_image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Allocate the image pixels.
  */
  *clone_image=(*image);
  clone_image->columns=columns;
  clone_image->rows=rows;
  if (!clone_pixels)
    clone_image->packets=clone_image->columns*clone_image->rows;
  clone_image->pixels=(RunlengthPacket *) AllocateMemory((unsigned int)
    clone_image->packets*sizeof(RunlengthPacket));
  if (clone_image->pixels == (RunlengthPacket *) NULL)
    return((Image *) NULL);
  if (!clone_pixels)
    {
      clone_image->tainted=True;
      SetImage(clone_image);
    }
  else
    {
      register RunlengthPacket
        *p,
        *q;

      /*
        Copy image pixels.
      */
      p=image->pixels;
      q=clone_image->pixels;
      assert (p != (RunlengthPacket *)NULL);
      assert (q != (RunlengthPacket *)NULL);
      for (i=0; i < (int) image->packets; i++)
      {
        *q=(*p);
        p++;
        q++;
      }
    }
  clone_image->packed_pixels=(unsigned char *) NULL;
  clone_image->comments=(char *) NULL;
  if (image->comments != (char *) NULL)
    (void) CloneString(&clone_image->comments,image->comments);
  clone_image->label=(char *) NULL;
  if (image->label != (char *) NULL)
    (void) CloneString(&clone_image->label,image->label);
  clone_image->montage=(char *) NULL;
  if (clone_pixels)
    if (image->montage != (char *) NULL)
      (void) CloneString(&clone_image->montage,image->montage);
  clone_image->directory=(char *) NULL;
  if (clone_pixels)
    if (image->directory != (char *) NULL)
      (void) CloneString(&clone_image->directory,image->directory);
  clone_image->signature=(char *) NULL;
  if (clone_pixels)
    if (image->signature != (char *) NULL)
      (void) CloneString(&clone_image->signature,image->signature);
  clone_image->page=(char *) NULL;
  if (clone_pixels)
    if (image->page != (char *) NULL)
      (void) CloneString(&clone_image->page,image->page);
  if (image->colormap != (ColorPacket *) NULL)
    {
      /*
        Allocate and copy the image colormap.
      */
      clone_image->colormap=(ColorPacket *)
        AllocateMemory(image->colors*sizeof(ColorPacket));
      if (clone_image->colormap == (ColorPacket *) NULL)
        return((Image *) NULL);
      for (i=0; i < (int) image->colors; i++)
        clone_image->colormap[i]=image->colormap[i];
    }
  if (image->color_profile.length > 0)
    {
      /*
        Allocate and copy the image ICC profile.
      */
      clone_image->color_profile.info=(unsigned char *)
        AllocateMemory(image->color_profile.length*sizeof(unsigned char));
      if (clone_image->color_profile.info == (unsigned char *) NULL)
        return((Image *) NULL);
      for (i=0; i < (int) image->color_profile.length; i++)
        clone_image->color_profile.info[i]=image->color_profile.info[i];
    }
  if (image->iptc_profile.length > 0)
    {
      /*
        Allocate and copy the image IPTC profile.
      */
      clone_image->iptc_profile.info=(unsigned char *)
        AllocateMemory(image->iptc_profile.length*sizeof(unsigned char));
      if (clone_image->iptc_profile.info == (unsigned char *) NULL)
        return((Image *) NULL);
      for (i=0; i < (int) image->iptc_profile.length; i++)
        clone_image->iptc_profile.info[i]=image->iptc_profile.info[i];
    }
  if (image->orphan)
    {
      clone_image->file=(FILE *) NULL;
      clone_image->previous=(Image *) NULL;
      clone_image->next=(Image *) NULL;
    }
  else
    {
      /*
        Link image into image list.
      */
      if (clone_image->previous != (Image *) NULL)
        clone_image->previous->next=clone_image;
      if (clone_image->next != (Image *) NULL)
        clone_image->next->previous=clone_image;
    }
  clone_image->orphan=False;
  return(clone_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C l o n e I m a g e I n f o                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CloneImageInfo makes a duplicate of the given image info, or if
%  image info is NULL, a new one.
%
%  The format of the CloneImageInfo method is:
%
%      ImageInfo *CloneImageInfo(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o cloned_info: Method CloneImageInfo returns a duplicate of the given
%      image info, or if image info is NULL a new one.
%
%    o image_info: a structure of type info.
%
%
*/
Export ImageInfo *CloneImageInfo(const ImageInfo *image_info)
{
  ImageInfo
    *cloned_info;

  cloned_info=(ImageInfo *) AllocateMemory(sizeof(ImageInfo));
  if (cloned_info == (ImageInfo *) NULL)
    MagickError(ResourceLimitWarning,"Unable to clone image info",
      "Memory allocation failed");
  if (image_info == (ImageInfo *) NULL)
    {
      GetImageInfo(cloned_info);
      return(cloned_info);
    }
  *cloned_info=(*image_info);
  if (image_info->size != (char *) NULL)
    cloned_info->size=AllocateString(image_info->size);
  if (image_info->tile != (char *) NULL)
    cloned_info->tile=AllocateString(image_info->tile);
  if (image_info->page != (char *) NULL)
    cloned_info->page=AllocateString(image_info->page);
  if (image_info->server_name != (char *) NULL)
    cloned_info->server_name=AllocateString(image_info->server_name);
  if (image_info->box != (char *) NULL)
    cloned_info->box=AllocateString(image_info->box);
  if (image_info->font != (char *) NULL)
    cloned_info->font=AllocateString(image_info->font);
  if (image_info->pen != (char *) NULL)
    cloned_info->pen=AllocateString(image_info->pen);
  if (image_info->texture != (char *) NULL)
    cloned_info->texture=AllocateString(image_info->texture);
  if (image_info->density != (char *) NULL)
    cloned_info->density=AllocateString(image_info->density);
  if (image_info->background_color != (char *) NULL)
    cloned_info->background_color=AllocateString(image_info->background_color);
  if (image_info->border_color != (char *) NULL)
    cloned_info->border_color=AllocateString(image_info->border_color);
  if (image_info->matte_color != (char *) NULL)
    cloned_info->matte_color=AllocateString(image_info->matte_color);
  if (image_info->dispose != (char *) NULL)
    cloned_info->dispose=AllocateString(image_info->dispose);
  if (image_info->delay != (char *) NULL)
    cloned_info->delay=AllocateString(image_info->delay);
  if (image_info->iterations != (char *) NULL)
    cloned_info->iterations=AllocateString(image_info->iterations);
  if (image_info->view != (char *) NULL)
    cloned_info->view=AllocateString(image_info->view);
  return(cloned_info);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m m e n t I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CommentImage initializes an image comment.  Optionally the
%  comment can include the image filename, type, width, height, or scene
%  number by embedding special format characters.
%
%  The format of the CommentImage method is:
%
%      void CommentImage(Image *image,const char *comments)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o comments: The address of a character string containing the comment
%      format.
%
%
*/
Export void CommentImage(Image *image,const char *comments)
{
  if (image->comments != (char *) NULL)
    FreeMemory((char *) image->comments);
  image->comments=TranslateText((ImageInfo *) NULL,image,comments);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m p o s i t e I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CompositeImage returns the second image composited onto the
%  first at the specified offsets.
%
%  The format of the CompositeImage method is:
%
%      void CompositeImage(Image *image,const CompositeOperator compose,
%        Image *composite_image,const int x_offset,const int y_offset)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o compose: Specifies an image composite operator.
%
%    o composite_image: The address of a structure of type Image.
%
%    o x_offset: An integer that specifies the column offset of the composited
%      image.
%
%    o y_offset: An integer that specifies the row offset of the composited
%      image.
%
%
*/
Export void CompositeImage(Image *image,const CompositeOperator compose,
  Image *composite_image,const int x_offset,const int y_offset)
{
  int
    y;

  long
    blue,
    green,
    index,
    red;

  Quantum
    shade;

  register int
    i,
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q;

  /*
    Check composite geometry.
  */
  assert(image != (Image *) NULL);
  assert(composite_image != (Image *) NULL);
  if (((x_offset+(int) image->columns) < 0) ||
      ((y_offset+(int) image->rows) < 0) ||
      (x_offset > (int) image->columns) || (y_offset > (int) image->rows))
    {
      MagickWarning(ResourceLimitWarning,"Unable to composite image",
        "geometry does not contain image");
      return;
    }
  /*
    Image must be uncompressed.
  */
  if (!UncondenseImage(image))
    return;
  if (!UncondenseImage(composite_image))
    return;
  switch (compose)
  {
    case XorCompositeOp:
    case PlusCompositeOp:
    case MinusCompositeOp:
    case AddCompositeOp:
    case SubtractCompositeOp:
    case DifferenceCompositeOp:
    case BumpmapCompositeOp:
    case BlendCompositeOp:
    case ReplaceRedCompositeOp:
    case ReplaceGreenCompositeOp:
    case ReplaceBlueCompositeOp:
    {
      image->class=DirectClass;
      break;
    }
    case ReplaceMatteCompositeOp:
    {
      image->class=DirectClass;
      image->matte=True;
      break;
    }
    case DisplaceCompositeOp:
    {
      ColorPacket
        interpolated_color;

      double
        x_displace,
        y_displace;

      double
        horizontal_scale,
        vertical_scale;

      Image
        *displaced_image;

      register RunlengthPacket
        *r;

      /*
        Allocate the displaced image.
      */
      composite_image->orphan=True;
      displaced_image=CloneImage(composite_image,composite_image->columns,
        composite_image->rows,False);
      composite_image->orphan=False;
      if (displaced_image == (Image *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to display image",
            "Memory allocation failed");
          return;
        }
      horizontal_scale=20.0;
      vertical_scale=20.0;
      if (composite_image->geometry != (char *) NULL)
        {
          int
            count;

          /*
            Determine the horizontal and vertical displacement scale.
          */
          count=sscanf(composite_image->geometry,"%lfx%lf\n",
            &horizontal_scale,&vertical_scale);
          if (count == 1)
            vertical_scale=horizontal_scale;
        }
      /*
        Shift image pixels as defined by a displacement map.
      */
      p=composite_image->pixels;
      runlength=p->length+1;
      r=displaced_image->pixels;
      for (y=0; y < (int) composite_image->rows; y++)
      {
        if (((y_offset+y) < 0) || ((y_offset+y) >= (int) image->rows))
          {
            p+=composite_image->columns;
            continue;
          }
        q=image->pixels+(y_offset+y)*image->columns+x_offset;
        for (x=0; x < (int) composite_image->columns; x++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          if (((x_offset+x) < 0) || ((x_offset+x) >= (int) image->columns))
            {
              q++;
              continue;
            }
          x_displace=(horizontal_scale*((double) Intensity(*p)-
            ((MaxRGB+1) >> 1)))/((MaxRGB+1) >> 1);
          y_displace=x_displace;
          if (composite_image->matte)
            y_displace=(vertical_scale*((double) p->index-
              ((MaxRGB+1) >> 1)))/((MaxRGB+1) >> 1);
          interpolated_color=
            InterpolateColor(image,x_offset+x+x_displace,y_offset+y+y_displace);
          r->red=interpolated_color.red;
          r->green=interpolated_color.green;
          r->blue=interpolated_color.blue;
          r->index=interpolated_color.index;
          r->length=0;
          q++;
          r++;
        }
      }
      composite_image=displaced_image;
      break;
    }
    case ReplaceCompositeOp:
    {
      /*
        Promote image to DirectClass if colormaps differ.
      */
      if (image->class == PseudoClass)
        {
          if ((composite_image->class == DirectClass) ||
              (composite_image->colors != image->colors))
            image->class=DirectClass;
          else
            {
              int
                status;

              status=memcmp((char *) composite_image->colormap,
                (char *) image->colormap,composite_image->colors*
                sizeof(ColorPacket));
              if (status != 0)
                image->class=DirectClass;
            }
        }
      if (image->matte && !composite_image->matte)
        MatteImage(composite_image);
      break;
    }
    default:
    {
      /*
        Initialize image matte data.
      */
      if (!image->matte)
        {
          image->class=DirectClass;
          if (compose != AnnotateCompositeOp)
            MatteImage(image);
        }
      if (!composite_image->matte)
        {
          p=composite_image->pixels;
          red=p->red;
          green=p->green;
          blue=p->blue;
          if (IsMonochromeImage(composite_image))
            {
              red=composite_image->background_color.red;
              green=composite_image->background_color.green;
              blue=composite_image->background_color.blue;
            }
          for (i=0; i < (int) composite_image->packets; i++)
          {
            p->index=Opaque;
            if ((p->red == red) && (p->green == green) &&
                (p->blue == blue))
              p->index=Transparent;
            p++;
          }
          composite_image->class=DirectClass;
          composite_image->matte=True;
        }
      break;
    }
  }
  /*
    Initialize composited image.
  */
  composite_image->tainted=True;
  p=composite_image->pixels;
  runlength=p->length+1;
  for (y=0; y < (int) composite_image->rows; y++)
  {
    if (((y_offset+y) < 0) || ((y_offset+y) >= (int) image->rows))
      {
        p+=composite_image->columns;
        continue;
      }
    q=image->pixels+(y_offset+y)*image->columns+x_offset;
    for (x=0; x < (int) composite_image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      if (((x_offset+x) < 0) || ((x_offset+x) >= (int) image->columns))
        {
          q++;
          continue;
        }
      switch (compose)
      {
        case AnnotateCompositeOp:
        case OverCompositeOp:
        default:
        {
          if (p->index == Transparent)
            {
              red=q->red;
              green=q->green;
              blue=q->blue;
              index=q->index;
            }
          else
            if (p->index == Opaque)
              {
                red=p->red;
                green=p->green;
                blue=p->blue;
                index=p->index;
              }
            else
              {
                red=(long) ((unsigned long)
                  (p->red*p->index+q->red*(Opaque-p->index))/Opaque);
                green=(long) ((unsigned long)
                  (p->green*p->index+q->green*(Opaque-p->index))/Opaque);
                blue=(long) ((unsigned long)
                  (p->blue*p->index+q->blue*(Opaque-p->index))/Opaque);
                index=(long) ((unsigned long)
                  (p->index*p->index+q->index*(Opaque-p->index))/Opaque);
              }
          break;
        }
        case InCompositeOp:
        {
          red=((unsigned long) (p->red*q->index)/Opaque);
          green=((unsigned long) (p->green*q->index)/Opaque);
          blue=((unsigned long) (p->blue*q->index)/Opaque);
          index=((unsigned long) (p->index*q->index)/Opaque);
          break;
        }
        case OutCompositeOp:
        {
          red=((unsigned long) (p->red*(Opaque-q->index))/Opaque);
          green=((unsigned long) (p->green*(Opaque-q->index))/Opaque);
          blue=((unsigned long) (p->blue*(Opaque-q->index))/Opaque);
          index=((unsigned long) (p->index*(Opaque-q->index))/Opaque);
          break;
        }
        case AtopCompositeOp:
        {
          red=((unsigned long)
            (p->red*q->index+q->red*(Opaque-p->index))/Opaque);
          green=((unsigned long)
            (p->green*q->index+q->green*(Opaque-p->index))/Opaque);
          blue=((unsigned long)
            (p->blue*q->index+q->blue*(Opaque-p->index))/Opaque);
          index=((unsigned long)
            (p->index*q->index+q->index*(Opaque-p->index))/Opaque);
          break;
        }
        case XorCompositeOp:
        {
          red=((unsigned long)
            (p->red*(Opaque-q->index)+q->red*(Opaque-p->index))/Opaque);
          green=((unsigned long)
            (p->green*(Opaque-q->index)+q->green*(Opaque-p->index))/Opaque);
          blue=((unsigned long)
            (p->blue*(Opaque-q->index)+q->blue*(Opaque-p->index))/Opaque);
          index=((unsigned long)
            (p->index*(Opaque-q->index)+q->index*(Opaque-p->index))/Opaque);
          break;
        }
        case PlusCompositeOp:
        {
          red=p->red+q->red;
          green=p->green+q->green;
          blue=p->blue+q->blue;
          index=p->index+q->index;
          break;
        }
        case MinusCompositeOp:
        {
          red=p->red-(int) q->red;
          green=p->green-(int) q->green;
          blue=p->blue-(int) q->blue;
          index=Opaque;
          break;
        }
        case AddCompositeOp:
        {
          red=p->red+q->red;
          if (red > MaxRGB)
            red-=(MaxRGB+1);
          green=p->green+q->green;
          if (green > MaxRGB)
            green-=(MaxRGB+1);
          blue=p->blue+q->blue;
          if (blue > MaxRGB)
            blue-=(MaxRGB+1);
          index=p->index+q->index;
          if (index > Opaque)
            index-=(Opaque+1);
          break;
        }
        case SubtractCompositeOp:
        {
          red=p->red-(int) q->red;
          if (red < 0)
            red+=(MaxRGB+1);
          green=p->green-(int) q->green;
          if (green < 0)
            green+=(MaxRGB+1);
          blue=p->blue-(int) q->blue;
          if (blue < 0)
            blue+=(MaxRGB+1);
          index=p->index-(int) q->index;
          if (index < 0)
            index+=(MaxRGB+1);
          break;
        }
        case DifferenceCompositeOp:
        {
          red=AbsoluteValue(p->red-(int) q->red);
          green=AbsoluteValue(p->green-(int) q->green);
          blue=AbsoluteValue(p->blue-(int) q->blue);
          index=AbsoluteValue(p->index-(int) q->index);
          break;
        }
        case BumpmapCompositeOp:
        {
          shade=Intensity(*p);
          red=((unsigned long) (q->red*shade)/Opaque);
          green=((unsigned long) (q->green*shade)/Opaque);
          blue=((unsigned long) (q->blue*shade)/Opaque);
          index=((unsigned long) (q->index*shade)/Opaque);
          break;
        }
        case ReplaceCompositeOp:
        {
          red=p->red;
          green=p->green;
          blue=p->blue;
          index=p->index;
          break;
        }
        case ReplaceRedCompositeOp:
        {
          red=DownScale(Intensity(*p));
          green=q->green;
          blue=q->blue;
          index=q->index;
          break;
        }
        case ReplaceGreenCompositeOp:
        {
          red=q->red;
          green=DownScale(Intensity(*p));
          blue=q->blue;
          index=q->index;
          break;
        }
        case ReplaceBlueCompositeOp:
        {
          red=q->red;
          green=q->green;
          blue=DownScale(Intensity(*p));
          index=q->index;
          break;
        }
        case ReplaceMatteCompositeOp:
        {
          red=q->red;
          green=q->green;
          blue=q->blue;
          index=DownScale(Intensity(*p));
          break;
        }
        case BlendCompositeOp:
        {
          red=((unsigned long)
            (p->red*p->index+q->red*q->index)/Opaque);
          green=((unsigned long)
            (p->green*p->index+q->green*q->index)/Opaque);
          blue=((unsigned long)
            (p->blue*p->index+q->blue*q->index)/Opaque);
          index=Opaque;
          break;
        }
        case DisplaceCompositeOp:
        {
          red=p->red;
          green=p->green;
          blue=p->blue;
          index=p->index;
          break;
        }
      }
      q->red=(Quantum) ((red < 0) ? 0 : (red > MaxRGB) ? MaxRGB : red);
      q->green=(Quantum) ((green < 0) ? 0 : (green > MaxRGB) ? MaxRGB : green);
      q->blue=(Quantum) ((blue < 0) ? 0 : (blue > MaxRGB) ? MaxRGB : blue);
      q->index=(unsigned short) ((index < Transparent) ? Transparent :
        (index > Opaque) ? Opaque : index);
      q->length=0;
      q++;
    }
  }
  if (compose == BlendCompositeOp)
    image->matte=False;
  if (compose == DisplaceCompositeOp)
    {
      image->matte=False;
      DestroyImage(composite_image);
    }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o n d e n s e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CondenseImage compresses an image to the minimum number of
%  runlength-encoded packets.
%
%  The format of the CondenseImage method is:
%
%      void CondenseImage(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void CondenseImage(Image *image)
{
  register int
    i,
    runlength;

  register long
    packets;

  register RunlengthPacket
    *p,
    *q;

  assert(image != (Image *) NULL);
  if ((image->columns == 0) || (image->rows == 0))
    return;
  if (image->pixels == (RunlengthPacket *) NULL)
    return;
  /*
    Compress image pixels.
  */
  p=image->pixels;
  runlength=p->length+1;
  packets=0;
  q=image->pixels;
  q->length=MaxRunlength;
  if (image->matte || (image->colorspace == CMYKColorspace))
    for (i=0; i < (int) (image->columns*image->rows); i++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      if ((p->red == q->red) && (p->green == q->green) &&
          (p->blue == q->blue) && (p->index == q->index) &&
          ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (packets != 0)
            q++;
          packets++;
          *q=(*p);
          q->length=0;
        }
    }
  else
    for (i=0; i < (int) (image->columns*image->rows); i++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      if ((p->red == q->red) && (p->green == q->green) &&
          (p->blue == q->blue) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (packets != 0)
            q++;
          packets++;
          *q=(*p);
          q->length=0;
        }
    }
  SetRunlengthPackets(image,packets);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C r e a t e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CreateImage creates an image from the specified normalized pixel data
%  and returns it.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the CreateImage method is:
%
%      Image *CreateImage(const unsigned int width,const unsigned int height,
%        const float *red,const float *green,const float *blue,
%        const float *opacity)
%
%  A description of each parameter follows:
%
%    o image:  Method CreateImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o width: Specifies the width in pixels of the image.
%
%    o height: Specifies the height in pixels of the image.
%
%    o red,green,blue,opacity: This array of normalized float values [0..1]
%      contain the red, green, blue, and opacity components of the pixel data.
%      The length of the arrays must equal the area specified by the width and
%      height values.  If you do not want to initialize a particular
%      component, specify it as NULL.
%
%
*/
Image *CreateImage(const unsigned int width,const unsigned int height,
  const float *red,const float *green,const float *blue,const float *opacity)
{
  Image
    *image;

  register int
    i;

  RunlengthPacket
    *q;

  if ((width*height) == 0)
    {
      MagickWarning(CorruptImageWarning,"impossible image size",(char *) NULL);
      return((Image *) NULL);
    }
  /*
    Allocate image structure.
  */
  image=AllocateImage((ImageInfo *) NULL);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Initialize image structure.
  */
  image->matte=opacity != (float *) NULL;
  image->columns=width;
  image->rows=height;
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return((Image *) NULL);
    }
  SetImage(image);
  /*
    Convert a rectangular array of float pixels to runlength-encoded.
  */
  q=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    if (red != (float *) NULL)
      q->red=(Quantum) (MaxRGB*red[i]);
    if (green != (float *) NULL)
      q->green=(Quantum) (MaxRGB*green[i]);
    if (blue != (float *) NULL)
      q->blue=(Quantum) (MaxRGB*blue[i]);
    if (opacity != (float *) NULL)
      q->index=(unsigned short) (Opaque*opacity[i]);
    q->length=0;
    q++;
  }
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     C y c l e C o l o r m a p I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CycleColormapImage cycles the image colormap by a specified
%  amount.
%
%  The format of the CycleColormapImage method is:
%
%      CycleColormapImage(image,amount)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o amount:  An unsigned value that specifies the offset of the colormap.
%
%
*/
Export void CycleColormapImage(Image *image,const int amount)
{
#define CycleColormapImageText  "  Cycling image...  "

  int
    index;

  register int
    i;

  register RunlengthPacket
    *q;

  assert(image != (Image *) NULL);
  if (image->class == DirectClass)
    {
      QuantizeInfo
        quantize_info;

      GetQuantizeInfo(&quantize_info);
      quantize_info.number_colors=MaxColormapSize;
      (void) QuantizeImage(&quantize_info,image);
    }
  q=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    index=((int) q->index+amount) % image->colors;
    if (index < 0)
      index+=image->colors;
    q->index=(unsigned short) index;
    q++;
  }
  SyncImage(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s c r i b e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DescribeImage describes an image by printing its attributes to
%  stdout.
%
%  The format of the DescribeImage method is:
%
%      void DescribeImage(Image *image,FILE *file,const unsigned int verbose)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o file: send the image attributes to this file.
%
%    o verbose: an unsigned value other than zero prints detailed information
%      about the image.
%
%
*/
Export void DescribeImage(Image *image,FILE *file,const unsigned int verbose)
{
  char
    color[MaxTextExtent],
    **textlist;

  Image
    *p;

  MagickInfo
    *magick_info;

  register int
    i;

  unsigned int
    count;

  unsigned long
    number_colors;

  assert(image != (Image *) NULL);
  if (file == (FILE *) NULL)
    file=stdout;
  if (!verbose)
    {
      /*
        Display detailed info about the image.
      */
      if (*image->magick_filename != '\0')
        if (Latin1Compare(image->magick_filename,image->filename) != 0)
          (void) fprintf(file,"%.1024s=>",image->magick_filename);
       if ((image->previous == (Image *) NULL) &&
           (image->next == (Image *) NULL) && (image->scene == 0))
        (void) fprintf(file,"%.1024s ",image->filename);
      else
        (void) fprintf(file,"%.1024s[%u] ",image->filename,image->scene);
      if ((image->magick_columns != 0) || (image->magick_rows != 0))
        if ((image->magick_columns != image->columns) ||
            (image->magick_rows != image->rows))
          (void) fprintf(file,"%ux%u=>",image->magick_columns,
            image->magick_rows);
      if (image->page == (char *) NULL)
        (void) fprintf(file,"%ux%u ",image->columns,image->rows);
      else
        {
          int
            x,
            y;

          unsigned int
            sans;

          x=0;
          y=0;
          (void) ParseGeometry(image->page,&x,&y,&sans,&sans);
          (void) fprintf(file,"%ux%u%+d%+d ",image->columns,image->rows,x,y);
        }
      if (image->class == DirectClass)
        {
          (void) fprintf(file,"DirectClass ");
          if (image->total_colors != 0)
            (void) fprintf(file,"%luc ",image->total_colors);
        }
      else
        if (image->total_colors <= image->colors)
          (void) fprintf(file,"PseudoClass %uc ",image->colors);
        else
          {
            (void) fprintf(file,"PseudoClass %lu=>%uc ",image->total_colors,
              image->colors);
            (void) fprintf(file,"%u/%.6f/%.6fe ",image->mean_error_per_pixel,
              image->normalized_mean_error,image->normalized_maximum_error);
          }
      if (image->filesize != 0)
        {
          if (image->filesize >= (1 << 24))
            (void) fprintf(file,"%ldmb ",image->filesize/1024/1024);
          else
            if (image->filesize >= (1 << 14))
              (void) fprintf(file,"%ldkb ",image->filesize/1024);
            else
              (void) fprintf(file,"%ldb ",image->filesize);
        }
      (void) fprintf(file,"%.1024s %lds\n",image->magick,time((time_t *) NULL)-
        image->magick_time+1);
      return;
    }
  /*
    Display verbose info about the image.
  */
  number_colors=GetNumberColors(image,(FILE *) NULL);
  (void) fprintf(file,"Image: %.1024s\n",image->filename);
  magick_info=(MagickInfo *) GetMagickInfo(image->magick);
  if ((magick_info == (MagickInfo *) NULL) ||
      (*magick_info->description == '\0'))
    (void) fprintf(file,"  format: %.1024s\n",image->magick);
  else
    (void) fprintf(file,"  format: %.1024s (%s)\n",image->magick,
      magick_info->description);
  (void) fprintf(file,"  type: ");
  switch (GetImageType(image))
  {
    case BilevelType: (void) fprintf(file,"bilevel"); break;
    case GrayscaleType: (void) fprintf(file,"grayscale"); break;
    case PaletteType: (void) fprintf(file,"palette"); break;
    case TrueColorType: (void) fprintf(file,"true color"); break;
    case MatteType: (void) fprintf(file,"true color with transparency"); break;
    case ColorSeparationType: (void) fprintf(file,"color separated"); break;
    default: (void) fprintf(file,"undefined"); break;
  }
  (void) fprintf(file,"\n");
  if (image->class == DirectClass)
    (void) fprintf(file,"  class: DirectClass\n");
  else
    (void) fprintf(file,"  class: PseudoClass\n");
  if (!image->matte)
    (void) fprintf(file,"  matte: False\n");
  else
    if ((strcmp(image->magick,"GIF") != 0) || image->tainted)
      (void) fprintf(file,"  matte: True\n");
    else
      {
        RunlengthPacket
          *q;

        q=image->pixels;
        for (i=0; i < (int) (image->packets-1); i++)
        {
          if (q->index == Transparent)
            break;
          q++;
        }
        if (image->depth == 8)
          (void) fprintf(file,"  matte: (%3d,%3d,%3d) #%02x%02x%02x\n",
            q->red,q->green,q->blue,q->red,q->green,q->blue);
        else
          (void) fprintf(file,"  matte: (%5d,%5d,%5d) #%04x%04x%04x\n",
            q->red,q->green,q->blue,q->red,q->green,q->blue);
      }
  if (image->class == DirectClass)
    (void) fprintf(file,"  colors: %lu\n",number_colors);
  else
    if (number_colors <= image->colors)
      (void) fprintf(file,"  colors: %u\n",image->colors);
    else
      (void) fprintf(file,"  colors: %lu=>%u\n",number_colors,image->colors);
  if (image->class == DirectClass)
    {
      if (number_colors < 1024)
        (void) GetNumberColors(image,file);
    }
  else
    {
      char
        name[MaxTextExtent];

      register ColorPacket
        *p;

      /*
        Display image colormap.
      */
      p=image->colormap;
      for (i=0; i < (int) image->colors; i++)
      {
        (void) fprintf(file,"    %d: (%3d,%3d,%3d)",i,p->red,p->green,p->blue);
        (void) fprintf(file,"  ");
        (void) QueryColorName(p,name);
        (void) fprintf(file,"  %.1024s",name);
        (void) fprintf(file,"\n");
        p++;
      }
    }
  if (image->mean_error_per_pixel != 0)
    (void) fprintf(file,"  mean error per pixel: %d\n",
      image->mean_error_per_pixel);
  if (image->normalized_mean_error != 0)
    (void) fprintf(file,"  normalized mean error: %.6f\n",
      image->normalized_mean_error);
  if (image->normalized_maximum_error != 0)
    (void) fprintf(file,"  normalized maximum error: %.6f\n",
      image->normalized_maximum_error);
  if (image->rendering_intent == SaturationIntent)
    (void) fprintf(file,"  rendering-intent: saturation\n");
  else
    if (image->rendering_intent == PerceptualIntent)
      (void) fprintf(file,"  rendering-intent: perceptual\n");
    else
      if (image->rendering_intent == AbsoluteIntent)
        (void) fprintf(file,"  rendering-intent: absolute\n");
    else
      if (image->rendering_intent == RelativeIntent)
        (void) fprintf(file,"  rendering-intent: relative\n");
  if (image->gamma != 0.0)
    (void) fprintf(file,"  gamma: %g\n",image->gamma);
  if (image->chromaticity.white_point.x != 0.0)
    {
      /*
        Display image chromaticity.
      */
      (void) fprintf(file,"  chromaticity:\n");
      (void) fprintf(file,"    red primary: (%g,%g)\n",
        image->chromaticity.red_primary.x,image->chromaticity.red_primary.y);
      (void) fprintf(file,"    green primary: (%g,%g)\n",
        image->chromaticity.green_primary.x,
        image->chromaticity.green_primary.y);
      (void) fprintf(file,"    blue primary: (%g,%g)\n",
        image->chromaticity.blue_primary.x,image->chromaticity.blue_primary.y);
      (void) fprintf(file,"    white point: (%g,%g)\n",
        image->chromaticity.white_point.x,image->chromaticity.white_point.y);
    }
  if (image->color_profile.length > 0)
    (void) fprintf(file,"  color profile: %u bytes\n",
      image->color_profile.length);
  if (image->iptc_profile.length > 0)
    {
      char
        *tag;

      int
        c;

      register int
        j;

      unsigned short
        length;

      /*
        Describe IPTC data.
      */
      (void) fprintf(file,"  IPTC profile: %u bytes\n",
        image->iptc_profile.length);
      for (i=0; i < image->iptc_profile.length; i++)
      {
        if (image->iptc_profile.info[i] != 0x1c)
          continue;
        i++;  /* skip dataset byte */
        switch (image->iptc_profile.info[++i])
        {
          case 5: tag="Image Name"; break;
          case 7: tag="Edit Status"; break;
          case 10: tag="Priority"; break;
          case 15: tag="Category"; break;
          case 20: tag="Supplemental Category"; break;
          case 22: tag="Fixture Identifier"; break;
          case 25: tag="Keyword"; break;
          case 30: tag="Release Date"; break;
          case 35: tag="Release Time"; break;
          case 40: tag="Special Instructions"; break;
          case 45: tag="Reference Service"; break;
          case 47: tag="Reference Date"; break;
          case 50: tag="Reference Number"; break;
          case 55: tag="Created Date"; break;
          case 60: tag="Created Time"; break;
          case 65: tag="Originating Program"; break;
          case 70: tag="Program Version"; break;
          case 75: tag="Object Cycle"; break;
          case 80: tag="Byline"; break;
          case 85: tag="Byline Title"; break;
          case 90: tag="City"; break;
          case 95: tag="Province State"; break;
          case 100: tag="Country Code"; break;
          case 101: tag="Country"; break;
          case 103: tag="Original Transmission Reference"; break;
          case 105: tag="Headline"; break;
          case 110: tag="Credit"; break;
          case 115: tag="Source"; break;
          case 116: tag="Copyright String"; break;
          case 120: tag="Caption"; break;
          case 121: tag="Local Caption"; break;
          case 122: tag="Caption Writer"; break;
          case 200: tag="Custom Field 1"; break;
          case 201: tag="Custom Field 2"; break;
          case 202: tag="Custom Field 3"; break;
          case 203: tag="Custom Field 4"; break;
          case 204: tag="Custom Field 5"; break;
          case 205: tag="Custom Field 6"; break;
          case 206: tag="Custom Field 7"; break;
          case 207: tag="Custom Field 8"; break;
          case 208: tag="Custom Field 9"; break;
          case 209: tag="Custom Field 10"; break;
          case 210: tag="Custom Field 11"; break;
          case 211: tag="Custom Field 12"; break;
          case 212: tag="Custom Field 13"; break;
          case 213: tag="Custom Field 14"; break;
          case 214: tag="Custom Field 15"; break;
          case 215: tag="Custom Field 16"; break;
          case 216: tag="Custom Field 17"; break;
          case 217: tag="Custom Field 18"; break;
          case 218: tag="Custom Field 19"; break;
          case 219: tag="Custom Field 20"; break;
          default: tag="unknown"; break;
        }
        (void) fprintf(file,"    %s: ",tag);
        length=image->iptc_profile.info[++i] << 8;
        length|=image->iptc_profile.info[++i];
        for (j=0; j < length; j++)
        {
          i++;
          c=image->iptc_profile.info[i];
          if ((c >= 32) || isspace((int) c))
            (void) fprintf(file,"%c",c);
        }
        (void) fprintf(file,"\n");
      }
    }
  if ((image->magick_columns != 0) || (image->magick_rows != 0))
    if ((image->magick_columns != image->columns) ||
        (image->magick_rows != image->rows))
      (void) fprintf(file,"  base geometry: %ux%u\n",image->magick_columns,
        image->magick_rows);
  (void) fprintf(file,"  geometry: %ux%u\n",image->columns,image->rows);
  if ((image->tile_info.width*image->tile_info.height) != 0)
    (void) fprintf(file,"  tile geometry: %ux%u%+d%+d\n",image->tile_info.width,
      image->tile_info.height,image->tile_info.x,image->tile_info.y);
  if ((image->x_resolution != 0.0) && (image->y_resolution != 0.0))
    {
      /*
        Display image resolution.
      */
      (void) fprintf(file,"  resolution: %gx%g",image->x_resolution,
        image->y_resolution);
      if (image->units == UndefinedResolution)
        (void) fprintf(file," pixels\n");
      else
        if (image->units == PixelsPerInchResolution)
          (void) fprintf(file," pixels/inch\n");
        else
          if (image->units == PixelsPerCentimeterResolution)
            (void) fprintf(file," pixels/centimeter\n");
          else
            (void) fprintf(file,"\n");
    }
  (void) fprintf(file,"  depth: %u\n",image->depth);
  if (image->filesize != 0)
    {
      if (image->filesize >= (1 << 24))
        (void) fprintf(file,"  filesize: %ldmb\n",image->filesize/1024/1024);
      else
        if (image->filesize >= (1 << 14))
          (void) fprintf(file,"  filesize: %ldkb\n",image->filesize/1024);
        else
          (void) fprintf(file,"  filesize: %ldb\n",image->filesize);
    }
  if (image->interlace == NoInterlace)
    (void) fprintf(file,"  interlace: None\n");
  else
    if (image->interlace == LineInterlace)
      (void) fprintf(file,"  interlace: Line\n");
    else
      if (image->interlace == PlaneInterlace)
        (void) fprintf(file,"  interlace: Plane\n");
    else
      if (image->interlace == PartitionInterlace)
        (void) fprintf(file,"  interlace: Partition\n");
  (void) QueryColorName(&image->background_color,color);
  (void) fprintf(file,"  background-color: %.1024s\n",color);
  (void) QueryColorName(&image->border_color,color);
  (void) fprintf(file,"  border-color: %.1024s\n",color);
  (void) QueryColorName(&image->matte_color,color);
  (void) fprintf(file,"  matte-color: %.1024s\n",color);
  if (image->page != (char *) NULL)
    (void) fprintf(file,"  page geometry: %.1024s\n",image->page);
  if (image->dispose != 0)
    (void) fprintf(file,"  dispose method: %d\n",image->dispose);
  if (image->delay != 0)
    (void) fprintf(file,"  delay: %d\n",image->delay);
  if (image->iterations != 1)
    (void) fprintf(file,"  iterations: %d\n",image->iterations);
  p=image;
  while (p->previous != (Image *) NULL)
    p=p->previous;
  for (count=1; p->next != (Image *) NULL; count++)
    p=p->next;
  if (count > 1)
    (void) fprintf(file,"  scene: %u of %u\n",image->scene,count);
  else
    if (image->scene != 0)
      (void) fprintf(file,"  scene: %u\n",image->scene);
  if (image->label != (char *) NULL)
    (void) fprintf(file,"  label: %.1024s\n",image->label);
  (void) fprintf(file,"  compression: ");
  switch (image->compression)
  {
    case NoCompression: (void) fprintf(file,"None\n"); break;
    case BZipCompression: (void) fprintf(file,"BZip\n"); break;
    case FaxCompression: (void) fprintf(file,"Fax\n"); break;
    case Group4Compression: (void) fprintf(file,"Group 4\n"); break;
    case JPEGCompression: (void) fprintf(file,"JPEG\n"); break;
    case LZWCompression: (void) fprintf(file,"LZW\n"); break;
    case RunlengthEncodedCompression:
      (void) fprintf(file,"Runlength Encoded\n"); break;
    case ZipCompression: (void) fprintf(file,"Zip\n"); break;
    default: (void) fprintf(file,"\n");  break;
  }
  if (image->packets < (image->columns*image->rows))
    (void) fprintf(file,"  runlength packets: %lu of %u\n",image->packets,
      image->columns*image->rows);
  if (image->tainted)
    (void) fprintf(file,"  tainted: True\n");
  else
    (void) fprintf(file,"  tainted: False\n");
  SignatureImage(image);
  (void) fprintf(file,"  signature: %.1024s\n",image->signature);
  if (image->comments != (char *) NULL)
    {
      /*
        Display image comment.
      */
      (void) fprintf(file,"  comments:\n");
      textlist=StringToList(image->comments);
      if (textlist != (char **) NULL)
        {
          for (i=0; textlist[i] != (char *) NULL; i++)
          {
            (void) fprintf(file,"  %.1024s\n",textlist[i]);
            FreeMemory(textlist[i]);
          }
          FreeMemory((char *) textlist);
        }
    }
  if (image->montage != (char *) NULL)
    (void) fprintf(file,"  montage: %.1024s\n",image->montage);
  if (image->directory != (char *) NULL)
    {
      ErrorHandler
        handler;

      Image
        *tile;

      ImageInfo
        image_info;

      register char
        *p,
        *q;

      /*
        Display visual image directory.
      */
      GetImageInfo(&image_info);
      (void) CloneString(&image_info.size,"64x64");
      (void) fprintf(file,"  directory:\n");
      for (p=image->directory; *p != '\0'; p++)
      {
        q=p;
        while ((*q != '\n') && (*q != '\0'))
          q++;
        (void) strncpy(image_info.filename,p,q-p);
        image_info.filename[q-p]='\0';
        p=q;
        (void) fprintf(file,"    %.1024s",image_info.filename);
        handler=SetWarningHandler((ErrorHandler) NULL);
        tile=ReadImage(&image_info);
        (void) SetWarningHandler(handler);
        if (tile == (Image *) NULL)
          {
            (void) fprintf(file,"\n");
            continue;
          }
        (void) fprintf(file," %ux%u %.1024s\n",tile->magick_columns,
          tile->magick_rows,tile->magick);
        if (tile->comments != (char *) NULL)
          {
            /*
              Display tile comment.
            */
            textlist=StringToList(tile->comments);
            if (textlist != (char **) NULL)
              {
                for (i=0; textlist[i] != (char *) NULL; i++)
                {
                  (void) fprintf(file,"    %.1024s\n",textlist[i]);
                  FreeMemory(textlist[i]);
                }
                FreeMemory((char *) textlist);
              }
          }
        DestroyImage(tile);
      }
    }
  (void) fflush(file);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s t r o y I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DestroyImage deallocates memory associated with an image.
%
%  The format of the DestroyImage method is:
%
%      void DestroyImage(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void DestroyImage(Image *image)
{
  /*
    Close image.
  */
  assert(image != (Image *) NULL);
  if (image->file != (FILE *) NULL)
    {
      CloseBlob(image);
      if (image->temporary)
        (void) remove(image->filename);
    }
  /*
    Deallocate the image comments.
  */
  if (image->comments != (char *) NULL)
    FreeMemory((char *) image->comments);
  /*
    Deallocate the image label.
  */
  if (image->label != (char *) NULL)
    FreeMemory((char *) image->label);
  /*
    Deallocate the image montage directory.
  */
  if (image->montage != (char *) NULL)
    FreeMemory((char *) image->montage);
  if (image->directory != (char *) NULL)
    FreeMemory((char *) image->directory);
  /*
    Deallocate the image colormap.
  */
  if (image->colormap != (ColorPacket *) NULL)
    FreeMemory((char *) image->colormap);
  /*
    Deallocate the image ICC profile.
  */
  if (image->color_profile.length > 0)
    FreeMemory((char *) image->color_profile.info);
  /*
    Deallocate the image IPTC profile.
  */
  if (image->iptc_profile.length > 0)
    FreeMemory((char *) image->iptc_profile.info);
  /*
    Deallocate the image signature.
  */
  if (image->signature != (char *) NULL)
    FreeMemory((char *) image->signature);
  /*
    Deallocate the image pixels.
  */
  if (image->pixels != (RunlengthPacket *) NULL)
    FreeMemory((char *) image->pixels);
  if (image->packed_pixels != (unsigned char *) NULL)
    FreeMemory((char *) image->packed_pixels);
  /*
    Deallocate the image page geometry.
  */
  if (image->page != (char *) NULL)
    FreeMemory((char *) image->page);
  if (!image->orphan)
    {
      /*
        Unlink from linked list.
      */
      if (image->previous != (Image *) NULL)
        {
          if (image->next != (Image *) NULL)
            image->previous->next=image->next;
          else
            image->previous->next=(Image *) NULL;
        }
      if (image->next != (Image *) NULL)
        {
          if (image->previous != (Image *) NULL)
            image->next->previous=image->previous;
          else
            image->next->previous=(Image *) NULL;
        }
    }
  /*
    Deallocate the image structure.
  */
  FreeMemory((char *) image);
  image=(Image *) NULL;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s t r o y I m a g e I n f o                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DestroyImageInfo deallocates memory associated with an ImageInfo
%  structure.
%
%  The format of the DestroyImageInfo method is:
%
%      void DestroyImageInfo(ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export void DestroyImageInfo(ImageInfo *image_info)
{
  assert(image_info != (ImageInfo *) NULL);
  if (image_info->server_name != (char *) NULL)
    FreeMemory((char *) image_info->server_name);
  image_info->server_name=(char *) NULL;
  if (image_info->size != (char *) NULL)
    FreeMemory((char *) image_info->size);
  image_info->size=(char *) NULL;
  if (image_info->tile != (char *) NULL)
    FreeMemory((char *) image_info->tile);
  image_info->tile=(char *) NULL;
  if (image_info->density != (char *) NULL)
    FreeMemory((char *) image_info->density);
  image_info->density=(char *) NULL;
  if (image_info->page != (char *) NULL)
    FreeMemory((char *) image_info->page);
  image_info->page=(char *) NULL;
  if (image_info->dispose != (char *) NULL)
    FreeMemory((char *) image_info->dispose);
  image_info->dispose=(char *) NULL;
  if (image_info->delay != (char *) NULL)
    FreeMemory((char *) image_info->delay);
  image_info->delay=(char *) NULL;
  if (image_info->iterations != (char *) NULL)
    FreeMemory((char *) image_info->iterations);
  image_info->iterations=(char *) NULL;
  if (image_info->texture != (char *) NULL)
    FreeMemory((char *) image_info->texture);
  image_info->texture=(char *) NULL;
  if (image_info->box != (char *) NULL)
    FreeMemory((char *) image_info->box);
  image_info->box=(char *) NULL;
  if (image_info->font != (char *) NULL)
    FreeMemory((char *) image_info->font);
  image_info->font=(char *) NULL;
  if (image_info->pen != (char *) NULL)
    FreeMemory((char *) image_info->pen);
  image_info->pen=(char *) NULL;
  if (image_info->view != (char *) NULL)
    FreeMemory((char *) image_info->view);
  image_info->view=(char *) NULL;
  if (image_info->background_color != (char *) NULL)
    FreeMemory((char *) image_info->background_color);
  image_info->background_color=(char *) NULL;
  if (image_info->border_color != (char *) NULL)
    FreeMemory((char *) image_info->border_color);
  image_info->border_color=(char *) NULL;
  if (image_info->matte_color != (char *) NULL)
    FreeMemory((char *) image_info->matte_color);
  image_info->matte_color=(char *) NULL;
  FreeMemory((ImageInfo *) image_info);
  image_info=(ImageInfo *) NULL;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s t r o y I m a g e s                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DestroyImages deallocates memory associated with a linked list
%  of images.
%
%  The format of the DestroyImages method is:
%
%      void DestroyImages(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void DestroyImages(Image *image)
{
  Image
    *next_image;

  /*
    Proceed to the top of the image list.
  */
  assert(image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  do
  {
    /*
      Destroy this image.
    */
    next_image=image->next;
    if (next_image != (Image *)NULL)
      next_image->previous=(Image *)NULL;
    DestroyImage(image);
    image=next_image;
  } while (image != (Image *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D i s p l a y I m a g e s                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DisplayImages displays one or more images to an X window.
%
%  The format of the AllocateNextImage method is:
%
%      unsigned int DisplayImages(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method DisplayImages returns True if the images are displayed
%      in an X window, otherwise False is returned.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int DisplayImages(const ImageInfo *image_info,Image *image)
{
#if defined(HasX11)
  char
    *client_name;

  Display
    *display;

  Image
    *next;

  unsigned long
    state;

  XrmDatabase
    resource_database;

  XResourceInfo
    resource;

  display=XOpenDisplay((char *) NULL);
  if (display == (Display *) NULL)
    return(False);
  XSetErrorHandler(XError);
  client_name=SetClientName((char *) NULL);
  resource_database=XGetResourceDatabase(display,client_name);
  XGetResourceInfo(resource_database,client_name,&resource);
  *resource.image_info=(*image_info);
  resource.immutable=True;
  if (image_info->delay != (char *) NULL)
    resource.delay=atoi(image_info->delay);
  for (next=image; next; next=next->next)
  {
    state=DefaultState;
    (void) XDisplayImage(display,&resource,&client_name,1,&next,&state);
    if (state & ExitState)
      break;
  }
  XCloseDisplay(display);
  return(True);
#else
  MagickWarning(MissingDelegateWarning,"X11 library is not available",
    image->filename);
  return(False);
#endif
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t I m a g e I n f o                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetImageInfo initializes the ImageInfo structure.
%
%  The format of the GetImageInfo method is:
%
%      void GetImageInfo(ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export void GetImageInfo(ImageInfo *image_info)
{
  /*
    File and image dimension members.
  */
  assert(image_info != (ImageInfo *) NULL);
  GetBlobInfo(&(image_info->blob));
  image_info->file=(FILE *) NULL;
  *image_info->filename='\0';
  *image_info->magick='\0';
  TemporaryFilename(image_info->unique);
  (void) strcat(image_info->unique,"u");
  TemporaryFilename(image_info->zero);
  image_info->affirm=False;
  image_info->temporary=False;
  image_info->adjoin=True;
  image_info->subimage=0;
  image_info->subrange=0;
  image_info->ping=False;
  image_info->depth=QuantumDepth;
  image_info->size=(char *) NULL;
  image_info->tile=(char *) NULL;
  image_info->page=(char *) NULL;
  image_info->interlace=DefaultInterlace;
  image_info->units=UndefinedResolution;
  /*
    Compression members.
  */
  image_info->compression=UndefinedCompression;
  image_info->quality=75;
  /*
    Annotation members.
  */
  image_info->server_name=(char *) NULL;
  image_info->box=(char *) NULL;
  image_info->font=(char *) NULL;
  image_info->pen=(char *) NULL;
  image_info->texture=(char *) NULL;
  image_info->density=(char *) NULL;
  image_info->linewidth=1;
  image_info->pointsize=atoi(DefaultPointSize);
  image_info->antialias=True;
  image_info->fuzz=0;
  image_info->background_color=(char *) NULL;
  image_info->border_color=(char *) NULL;
  image_info->matte_color=(char *) NULL;
  /*
    Color reduction members.
  */
  image_info->dither=True;
  image_info->monochrome=False;
  image_info->colorspace=UndefinedColorspace;
  /*
    Animation members.
  */
  image_info->dispose=(char *) NULL;
  image_info->delay=(char *) NULL;
  image_info->iterations=(char *) NULL;
  image_info->coalesce_frames=False;
  image_info->insert_backdrops=False;
  /*
    Miscellaneous members.
  */
  image_info->verbose=False;
  image_info->preview_type=JPEGPreview;
  image_info->view=(char *) NULL;
  image_info->group=0L;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t I m a g e T y p e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetImageType returns the type of image (e.g. bilevel, palette, etc).
%
%  The format of the GetImageType method is:
%
%      ImageType GetImageType(Image *image)
%
%  A description of each parameter follows:
%
%    o type: Method GetImageType returns a ImageType enum that specifies the
%      type of the specified image (e.g. bilevel, palette, etc).
%
%    o image: The address of a structure of type Image.
%
%
*/
Export ImageType GetImageType(Image *image)
{
  assert(image != (Image *) NULL);
  if (image->colorspace == CMYKColorspace)
    return(ColorSeparationType);
  if (IsMonochromeImage(image))
    return(BilevelType);
  if (IsGrayImage(image))
    return(GrayscaleType);
  if (IsPseudoClass(image))
    return(PaletteType);
  if (!image->matte)
    return(TrueColorType);
  return(MatteType);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t N u m b e r S c e n e s                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetNumberScenes returns the number of scenes in an image sequence.
%
%  The format of the GetNumberScenes method is:
%
%      unsigned int GetNumberScenes(const Image *image)
%
%  A description of each parameter follows:
%
%    o scenes:  Method GetNumberScenes returns the number of scenes in an
%      image sequence.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int GetNumberScenes(const Image *image)
{
  const Image
    *next_image;

  unsigned int
    number_scenes;

  /*
    Compute the number of scenes in the image.
  */
  assert(image != (const Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  next_image=image;
  for (number_scenes=0; next_image != (Image *) NULL; number_scenes++)
    next_image=next_image->next;
  return(number_scenes);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t P i x e l s                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetPixels returns the pixel data of an image as normalized red,
%  green, blue, and opacity values.
%
%  The format of the GetPixels method is:
%
%      unsigned int IsGeometry(const char *geometry)
%
%  A description of each parameter follows:
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%    o red_pixels,green_pixels,blue_pixels,opacity_pixels: These arrays are
%      returned with the correpondingred, green, blue, and opacity values from
%      the image.  The length of the arrays must equal the area specified by
%      the columns and row values values of the Image structure.  If you do
%      not want to initialize a particular component, specify it as NULL.
%
%
*/
void GetPixels(const Image *image,float *red_pixels,float *green_pixels,
  float *blue_pixels,float *opacity_pixels)
{
  double
    blue,
    green,
    opacity,
    red;

  register int
    i,
    j,
    x;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  x=0;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    red=(double) p->red/MaxRGB;
    green=(double) p->green/MaxRGB;
    blue=(double) p->blue/MaxRGB;
    opacity=(double) (image->matte ? p->index/Opaque : 0);
    for (j=0; j <= (int) p->length; j++)
    {
      if (red_pixels != (float *) NULL)
        red_pixels[x]=red;
      if (green_pixels != (float *) NULL)
        green_pixels[x]=green;
      if (blue_pixels != (float *) NULL)
        blue_pixels[x]=blue;
      if (opacity_pixels != (float *) NULL)
        opacity_pixels[x]=opacity;
      x++;
    }
    p++;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     I s G e o m e t r y                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method IsGeometry returns True if the geometry specification is valid
%  as determined by ParseGeometry.
%
%  The format of the IsGeometry method is:
%
%      unsigned int IsGeometry(const char *geometry)
%
%  A description of each parameter follows:
%
%    o status: Method IsGeometry returns True if the geometry specification
%      is valid otherwise False is returned.
%
%    o geometry: This string is the geometry specification.
%
%
*/
Export unsigned int IsGeometry(const char *geometry)
{
  double
    value;

  int
    x,
    y;

  unsigned int
    flags,
    height,
    width;

  if (geometry == (const char *) NULL)
    return(False);
  flags=ParseGeometry((char *) geometry,&x,&y,&width,&height);
  return((flags != NoValue) || sscanf(geometry,"%lf",&value));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     I s S u b i m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method IsSubimage returns True if the geometry is a valid subimage
%  specification (e.g. [1], [1-9], [1,7,4]).
%
%  The format of the IsSubimage method is:
%
%      unsigned int IsSubimage(const char *geometry,const unsigned int pedantic)
%
%  A description of each parameter follows:
%
%    o status: Method IsSubimage returns True if the geometry is a valid
%      subimage specification otherwise False is returned.
%
%    o geometry: This string is the geometry specification.
%
%    o pedantic: A value other than 0 invokes a more restriction set of
%      conditions for a valid specification (e.g. [1], [1-4], [4-1]).
%
%
*/
Export unsigned int IsSubimage(const char *geometry,const unsigned int pedantic)
{
  int
    x,
    y;

  unsigned int
    flags,
    height,
    width;

  if (geometry == (const char *) NULL)
    return(False);
  flags=ParseGeometry((char *) geometry,&x,&y,&width,&height);
  if (pedantic)
    return((flags != NoValue) && !(flags & HeightValue));
  return(IsGeometry(geometry) && !(flags & HeightValue));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     I s T a i n t e d                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method IsTainted returns True if the image has been altered since it
%  was first read or if any image in the sequence has a difference magic or
%  filename.
%
%  The format of the IsTainted method is:
%
%      unsigned int IsTainted(const Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method IsTainted returns True if the image has been altered
%      since it was first read.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int IsTainted(const Image *image)
{
  char
    magick[MaxTextExtent],
    filename[MaxTextExtent];

  register const Image
    *p;

  assert(image != (Image *) NULL);
  (void) strcpy(magick,image->magick);
  (void) strcpy(filename,image->filename);
  for (p=image; p != (Image *) NULL; p=p->next)
  {
    if (p->tainted)
      return(True);
    if (Latin1Compare(p->magick,magick) != 0)
      return(True);
    if (Latin1Compare(p->filename,filename) != 0)
      return(True);
  }
  return(False);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   L a b e l I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method LabelImage initializes an image label.  Optionally the label
%  can include the image filename, type, width, height, or scene number by
%  embedding special format characters.
%
%  The format of the LabelImage method is:
%
%      void LabelImage(Image *image,const char *label)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o label: The address of a character string containing the label format.
%
%
*/
Export void LabelImage(Image *image,const char *label)
{
  if (image->label != (char *) NULL)
    FreeMemory((char *) image->label);
  image->label=TranslateText((ImageInfo *) NULL,image,label);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     L a y e r I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method LayerImage extracts the specified layer from the references image.
%
%  The format of the LayerImage method is:
%
%      void LayerImage(Image *image,const LayerType layer)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o layer: A value of type LayerType that identifies which layer to extract.
%
%
*/
Export void LayerImage(Image *image,const LayerType layer)
{
#define LayerImageText  "  Extracting the layer from the image...  "

  QuantizeInfo
    quantize_info;

  register int
    i;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  if ((layer == MatteLayer) && !image->matte)
    {
      MagickWarning(OptionWarning,"Unable to extract layer",
        "image does not have a matte layer");
      return;
    }
  /*
    Layer DirectClass packets.
  */
  image->class=DirectClass;
  image->matte=False;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    switch (layer)
    {
      case RedLayer:
      {
        p->green=p->red;
        p->blue=p->red;
        break;
      }
      case GreenLayer:
      {
        p->red=p->green;
        p->blue=p->green;
        break;
      }
      case BlueLayer:
      {
        p->red=p->blue;
        p->green=p->blue;
        break;
      }
      case MatteLayer:
      default:
      {
        p->red=p->index;
        p->green=p->index;
        p->blue=p->index;
        break;
      }
    }
    p++;
    if (QuantumTick(i,image->packets))
      ProgressMonitor(LayerImageText,i,image->packets);
  }
  /*
    Treat layer as gray scale image.
  */
  GetQuantizeInfo(&quantize_info);
  quantize_info.colorspace=GRAYColorspace;
  (void) QuantizeImage(&quantize_info,image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     L i s t T o G r o u p I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ListToGroupImage converts a linked list of images to a sequential
%  array.
%
%  The format of the ListToGroupImage method is:
%
%      Image **ListToGroupImage(const Image *image,unsigned int *number_images)
%
%  A description of each parameter follows:
%
%    o images: Method ListToGroupImage converts a linked list of images to
%      a sequential array and returns the array..
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o number_images:  A pointer to an unsigned integer.  The number of images
%      in the image array is returned here.
%
%
*/
Export Image **ListToGroupImage(const Image *image,unsigned int *number_images)
{
  Image
    **images,
    *next_image;

  register int
    i;

  /*
    Determine the number of images in the list.
  */
  assert(image != (Image *) NULL);
  assert(number_images != (unsigned int *) NULL);
  next_image=(Image *) image;
  for (i=0; next_image != (Image *) NULL; i++)
    next_image=next_image->next;
  images=(Image **) AllocateMemory(i*sizeof(Image *));
  if (images == (Image **) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to convert image list",
        "Memory allocation failed");
      return((Image **) NULL);
    }
  *number_images=i;
  /*
    Add each image in the linked list to the group.
  */
  next_image=(Image *) image;
  for (i=0; next_image != (Image *) NULL; i++)
  {
    images[i]=next_image;
    next_image=next_image->next;
  }
  return(images);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     M a t t e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MatteImage initializes the matte channel of the reference image to
%  opaque.
%
%  The format of the MatteImage method is:
%
%      void MatteImage(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export void MatteImage(Image *image)
{
  register int
    i;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  image->class=DirectClass;
  image->matte=True;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    p->index=Opaque;
    p++;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     M o g r i f y I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MogrifyImage applies image processing options to an image as
%  prescribed by command line options.
%
%  The format of the MogrifyImage method is:
%
%      void MogrifyImage(const ImageInfo *image_info,const int argc,char **argv,
%        Image **image)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o argc: Specifies a pointer to an integer describing the number of
%      elements in the argument vector.
%
%    o argv: Specifies a pointer to a text array containing the command line
%      arguments.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export void MogrifyImage(const ImageInfo *image_info,const int argc,char **argv,
  Image **image)
{
  char
    *geometry,
    *option;

  ColorPacket
    target_color;

  Image
    *map_image,
    *region_image;

  ImageInfo
    *local_info;

  int
    flags,
    x,
    y;

  QuantizeInfo
    quantize_info;

  RectangleInfo
    region_info;

  register int
    i;

  unsigned int
    compress,
    gravity,
    matte,
    height,
    width;

  /*
    Verify option length.
  */
  assert(image_info != (ImageInfo *) NULL);
  assert(image != (Image **) NULL);
  for (i=1; i < argc; i++)
    if (Extent(argv[i]) > (MaxTextExtent/2-1))
      {
        MagickWarning(ResourceLimitWarning,"Option length exceeds limit",
          argv[i]);
        return;
      }
  /*
    Initialize method variables.
  */
  local_info=CloneImageInfo(image_info);
  GetQuantizeInfo(&quantize_info);
  compress=(*image)->packets < (((*image)->columns*(*image)->rows*3) >> 2);
  geometry=(char *) NULL;
  gravity=ForgetGravity;
  map_image=(Image *) NULL;
  quantize_info.number_colors=0;
  quantize_info.tree_depth=0;
  quantize_info.dither=True;
  if (local_info->monochrome)
    if (!IsMonochromeImage(*image))
      {
        quantize_info.number_colors=2;
        quantize_info.tree_depth=8;
        quantize_info.colorspace=GRAYColorspace;
      }
  region_image=(Image *) NULL;
  region_info.width=(*image)->columns;
  region_info.height=(*image)->rows;
  region_info.x=0;
  region_info.y=0;
  /*
    Transmogrify the image.
  */
  for (i=1; i < argc; i++)
  {
    option=argv[i];
    if ((Extent(option) <= 1) || ((*option != '-') && (*option != '+')))
      continue;
    if (strncmp("antialias",option+1,3) == 0)
      {
        local_info->antialias=(*option == '-');
        continue;
      }
    if (strncmp("-background",option,6) == 0)
      {
        (void) CloneString(&local_info->background_color,argv[++i]);
        (void) QueryColorDatabase(local_info->background_color,&target_color);
        (*image)->background_color.red=XDownScale(target_color.red);
        (*image)->background_color.green=XDownScale(target_color.green);
        (*image)->background_color.blue=XDownScale(target_color.blue);
        continue;
      }
    if (strncmp("-blur",option,4) == 0)
      {
        double
          factor;

        Image
          *blurred_image;

        /*
          Blur an image.
        */
        factor=atof(argv[++i]);
        blurred_image=BlurImage(*image,factor);
        if (blurred_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=blurred_image;
          }
        continue;
      }
    if (Latin1Compare("-border",option) == 0)
      {
        Image
          *bordered_image;

        RectangleInfo
          border_info;

        /*
          Surround image with a border of solid color.
        */
        border_info.width=0;
        border_info.height=0;
        border_info.x=0;
        border_info.y=0;
        flags=ParseGeometry(argv[++i],&border_info.x,&border_info.y,
          &border_info.width,&border_info.height);
        if ((flags & HeightValue) == 0)
          border_info.height=border_info.width;
        bordered_image=BorderImage(*image,&border_info);
        if (bordered_image != (Image *) NULL)
          {
            DestroyImage(*image);
            bordered_image->class=DirectClass;
            *image=bordered_image;
          }
        continue;
      }
    if (strncmp("-bordercolor",option,8) == 0)
      {
        (void) CloneString(&local_info->border_color,argv[++i]);
        (void) QueryColorDatabase(local_info->border_color,&target_color);
        (*image)->border_color.red=XDownScale(target_color.red);
        (*image)->border_color.green=XDownScale(target_color.green);
        (*image)->border_color.blue=XDownScale(target_color.blue);
        continue;
      }
    if (Latin1Compare("-box",option) == 0)
      {
        (void) CloneString(&local_info->box,argv[++i]);
        continue;
      }
    if (strncmp("-charcoal",option,3) == 0)
      {
        char
          *commands[7];

        QuantizeInfo
          quantize_info;

        /*
          Charcoal drawing.
        */
        i++;
        GetQuantizeInfo(&quantize_info);
        quantize_info.dither=image_info->dither;
        quantize_info.colorspace=GRAYColorspace;
        (void) QuantizeImage(&quantize_info,*image);
        SyncImage(*image);
        commands[0]=SetClientName((char *) NULL);
        commands[1]="-edge";
        commands[2]=argv[i];
        commands[3]="-blur";
        commands[4]=argv[i];
        commands[5]="-normalize";
        commands[6]="-negate";
        MogrifyImage(local_info,7,commands,image);
        (void) QuantizeImage(&quantize_info,*image);
        SyncImage(*image);
        continue;
      }
    if (strncmp("-colorize",option,8) == 0)
      {
        ColorizeImage(*image,argv[++i],local_info->pen);
        continue;
      }
    if (Latin1Compare("-colors",option) == 0)
      {
        quantize_info.number_colors=atoi(argv[++i]);
        continue;
      }
    if (strncmp("-colorspace",option,8) == 0)
      {
        option=argv[++i];
        if (Latin1Compare("cmyk",option) == 0)
          {
            RGBTransformImage(*image,CMYKColorspace);
            quantize_info.colorspace=CMYKColorspace;
          }
        if (Latin1Compare("gray",option) == 0)
          {
            quantize_info.colorspace=GRAYColorspace;
            if (quantize_info.number_colors == 0)
              quantize_info.number_colors=256;
            quantize_info.tree_depth=8;
          }
        if (Latin1Compare("ohta",option) == 0)
          quantize_info.colorspace=OHTAColorspace;
        if (Latin1Compare("rgb",option) == 0)
          {
            TransformRGBImage(*image,RGBColorspace);
            quantize_info.colorspace=RGBColorspace;
          }
        if (Latin1Compare("srgb",option) == 0)
          quantize_info.colorspace=sRGBColorspace;
        if (Latin1Compare("transparent",option) == 0)
          quantize_info.colorspace=TransparentColorspace;
        if (Latin1Compare("xyz",option) == 0)
          quantize_info.colorspace=XYZColorspace;
        if (Latin1Compare("ycbcr",option) == 0)
          quantize_info.colorspace=YCbCrColorspace;
        if (Latin1Compare("ycc",option) == 0)
          quantize_info.colorspace=YCCColorspace;
        if (Latin1Compare("yiq",option) == 0)
          quantize_info.colorspace=YIQColorspace;
        if (Latin1Compare("ypbpr",option) == 0)
          quantize_info.colorspace=YPbPrColorspace;
        if (Latin1Compare("yuv",option) == 0)
          quantize_info.colorspace=YUVColorspace;
        local_info->colorspace=quantize_info.colorspace;
        continue;
      }
    if (strncmp("comment",option+1,4) == 0)
      {
        if (*option == '-')
          CommentImage(*image,argv[++i]);
        else
          CommentImage(*image,(char *) NULL);
        continue;
      }
    if (strncmp("contrast",option+1,3) == 0)
      {
        ContrastImage(*image,(unsigned int) (*option == '-'));
        continue;
      }
    if (strncmp("-crop",option,3) == 0)
      {
        TransformImage(image,argv[++i],(char *) NULL);
        continue;
      }
    if (strncmp("-cycle",option,3) == 0)
      {
        /*
          Cycle an image colormap.
        */
        CycleColormapImage(*image,atoi(argv[++i]));
        continue;
      }
    if (strncmp("-density",option,4) == 0)
      {
        int
          count;

        /*
          Set image density.
        */
        (void) CloneString(&local_info->density,argv[++i]);
        count=sscanf(local_info->density,"%lfx%lf",
          &(*image)->x_resolution,&(*image)->y_resolution);
        if (count != 2)
          (*image)->y_resolution=(*image)->x_resolution;
      }
    if (strncmp("-depth",option,4) == 0)
      {
        (*image)->depth=atoi(argv[++i]) <= 8 ? 8 : 16;
        continue;
      }
    if (strncmp("-despeckle",option,4) == 0)
      {
        Image
          *despeckled_image;

        /*
          Reduce the speckles within an image.
        */
        despeckled_image=DespeckleImage(*image);
        if (despeckled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=despeckled_image;
          }
        continue;
      }
    if (strncmp("-display",option,6) == 0)
      {
        (void) CloneString(&local_info->server_name,argv[++i]);
        continue;
      }
    if (strncmp("dither",option+1,3) == 0)
      {
        local_info->dither=(*option == '-');
        quantize_info.dither=(*option == '-');
        continue;
      }
    if (strncmp("-draw",option,3) == 0)
      {
        AnnotateInfo
          annotate_info;

        /*
          Draw image.
        */
        GetAnnotateInfo(local_info,&annotate_info);
        (void) CloneString(&annotate_info.primitive,argv[++i]);
        if (geometry != (char *) NULL)
          (void) CloneString(&annotate_info.geometry,geometry);
        if (gravity != ForgetGravity)
          annotate_info.gravity=gravity;
        DrawImage(*image,&annotate_info);
        DestroyAnnotateInfo(&annotate_info);
        continue;
      }
    if (strncmp("-edge",option,3) == 0)
      {
        double
          factor;

        Image
          *edged_image;

        /*
          Detect edges in the image.
        */
        factor=atof(argv[++i]);
        edged_image=EdgeImage(*image,factor);
        if (edged_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=edged_image;
          }
        continue;
      }
    if (strncmp("-emboss",option,3) == 0)
      {
        Image
          *embossed_image;

        /*
          Emboss image.
        */
        embossed_image=EmbossImage(*image);
        if (embossed_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=embossed_image;
          }
        continue;
      }
    if (strncmp("-enhance",option,3) == 0)
      {
        Image
          *enhanced_image;

        /*
          Enhance image.
        */
        enhanced_image=EnhanceImage(*image);
        if (enhanced_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=enhanced_image;
          }
        continue;
      }
    if (strncmp("-equalize",option,3) == 0)
      {
        /*
          Equalize image.
        */
        EqualizeImage(*image);
        continue;
      }
    if (strncmp("filter",option+1,3) == 0)
      {
        if (*option == '-')
          {
            FilterType
              filter;

            option=argv[++i];
            filter=LanczosFilter;
            if (Latin1Compare("Point",option) == 0)
              filter=PointFilter;
            if (Latin1Compare("Box",option) == 0)
              filter=BoxFilter;
            if (Latin1Compare("Triangle",option) == 0)
              filter=TriangleFilter;
            if (Latin1Compare("Hermite",option) == 0)
              filter=HermiteFilter;
            if (Latin1Compare("Hanning",option) == 0)
              filter=HanningFilter;
            if (Latin1Compare("Hamming",option) == 0)
              filter=HammingFilter;
            if (Latin1Compare("Blackman",option) == 0)
              filter=BlackmanFilter;
            if (Latin1Compare("Gaussian",option) == 0)
              filter=GaussianFilter;
            if (Latin1Compare("Quadratic",option) == 0)
              filter=QuadraticFilter;
            if (Latin1Compare("Cubic",option) == 0)
              filter=CubicFilter;
            if (Latin1Compare("Catrom",option) == 0)
              filter=CatromFilter;
            if (Latin1Compare("Mitchell",option) == 0)
              filter=MitchellFilter;
            if (Latin1Compare("Lanczos",option) == 0)
              filter=LanczosFilter;
            if (Latin1Compare("Bessel",option) == 0)
              filter=BesselFilter;
            if (Latin1Compare("Sinc",option) == 0)
              filter=SincFilter;
            (*image)->filter=filter;
          }
        continue;
      }
    if (strncmp("-flip",option,4) == 0)
      {
        Image
          *flipped_image;

        /*
          Flip image scanlines.
        */
        flipped_image=FlipImage(*image);
        if (flipped_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=flipped_image;
          }
        continue;
      }
    if (strncmp("-flop",option,4) == 0)
      {
        Image
          *flopped_image;

        /*
          Flop image scanlines.
        */
        flopped_image=FlopImage(*image);
        if (flopped_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=flopped_image;
          }
        continue;
      }
    if (Latin1Compare("-frame",option) == 0)
      {
        Image
          *framed_image;

        FrameInfo
          frame_info;

        /*
          Surround image with an ornamental border.
        */
        frame_info.width=0;
        frame_info.height=0;
        frame_info.outer_bevel=0;
        frame_info.inner_bevel=0;
        flags=ParseGeometry(argv[++i],&frame_info.outer_bevel,
          &frame_info.inner_bevel,&frame_info.width,&frame_info.height);
        if ((flags & HeightValue) == 0)
          frame_info.height=frame_info.width;
        if ((flags & XValue) == 0)
          frame_info.outer_bevel=(frame_info.width >> 2)+1;
        if ((flags & YValue) == 0)
          frame_info.inner_bevel=frame_info.outer_bevel;
        frame_info.x=frame_info.width;
        frame_info.y=frame_info.height;
        frame_info.width=(*image)->columns+(frame_info.width << 1);
        frame_info.height=(*image)->rows+(frame_info.height << 1);
        framed_image=FrameImage(*image,&frame_info);
        if (framed_image != (Image *) NULL)
          {
            DestroyImage(*image);
            framed_image->class=DirectClass;
            *image=framed_image;
          }
        continue;
      }
    if (strncmp("-fuzz",option,3) == 0)
      {
        (*image)->fuzz=atoi(argv[++i]);
        continue;
      }
    if (Latin1Compare("-font",option) == 0)
      {
        (void) CloneString(&local_info->font,argv[++i]);
        continue;
      }
    if (strncmp("gamma",option+1,2) == 0)
      {
        if (*option == '+')
          (*image)->gamma=atof(argv[++i]);
        else
          GammaImage(*image,argv[++i]);
        continue;
      }
    if (strncmp("-geometry",option,4) == 0)
      {
        Image
          *zoomed_image;

        /*
          Resize image.
        */
        width=(*image)->columns;
        height=(*image)->rows;
        x=0;
        y=0;
        (void) CloneString(&geometry,argv[++i]);
        (void) ParseImageGeometry(geometry,&x,&y,&width,&height);
        zoomed_image=ZoomImage(*image,width,height);
        if (zoomed_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=zoomed_image;
          }
        continue;
      }
    if (strncmp("gravity",option+1,2) == 0)
      {
        gravity=NorthWestGravity;
        if (*option == '-')
          {
            option=argv[++i];
            if (Latin1Compare("NorthWest",option) == 0)
              gravity=NorthWestGravity;
            if (Latin1Compare("North",option) == 0)
              gravity=NorthGravity;
            if (Latin1Compare("NorthEast",option) == 0)
              gravity=NorthEastGravity;
            if (Latin1Compare("West",option) == 0)
              gravity=WestGravity;
            if (Latin1Compare("Center",option) == 0)
              gravity=CenterGravity;
            if (Latin1Compare("East",option) == 0)
              gravity=EastGravity;
            if (Latin1Compare("SouthWest",option) == 0)
              gravity=SouthWestGravity;
            if (Latin1Compare("South",option) == 0)
              gravity=SouthGravity;
            if (Latin1Compare("SouthEast",option) == 0)
              gravity=SouthEastGravity;
          }
        continue;
      }
    if (strncmp("-implode",option,4) == 0)
      {
        double
          amount;

        Image
          *imploded_image;

        /*
          Implode image.
        */
        amount=atof(argv[++i]);
        imploded_image=ImplodeImage(*image,amount);
        if (imploded_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=imploded_image;
          }
        continue;
      }
    if (strncmp("label",option+1,3) == 0)
      {
        if (*option == '-')
          LabelImage(*image,argv[++i]);
        else
          LabelImage(*image,(char *) NULL);
        continue;
      }
    if (strncmp("layer",option+1,3) == 0)
      {
        LayerType
          layer;

        layer=UndefinedLayer;
        if (*option == '-')
          {
            option=argv[++i];
            if (Latin1Compare("Red",option) == 0)
              layer=RedLayer;
            if (Latin1Compare("Green",option) == 0)
              layer=GreenLayer;
            if (Latin1Compare("Blue",option) == 0)
              layer=BlueLayer;
            if (Latin1Compare("Matte",option) == 0)
              layer=MatteLayer;
          }
        LayerImage(*image,layer);
        continue;
      }
    if (strncmp("-linewidth",option,3) == 0)
      {
        local_info->linewidth=atoi(argv[++i]);
        continue;
      }
    if (Latin1Compare("-map",option) == 0)
      {
        /*
          Transform image colors to match this set of colors.
        */
        (void) strcpy(local_info->filename,argv[++i]);
        map_image=ReadImage(local_info);
        continue;
      }
    if (Latin1Compare("matte",option+1) == 0)
      {
        if (*option == '-')
          if (!(*image)->matte)
            MatteImage(*image);
        (*image)->matte=(*option == '-');
        continue;
      }
    if (strncmp("-mattecolor",option,7) == 0)
      {
        (void) CloneString(&local_info->matte_color,argv[++i]);
        (void) QueryColorDatabase(local_info->matte_color,&target_color);
        (*image)->matte_color.red=XDownScale(target_color.red);
        (*image)->matte_color.green=XDownScale(target_color.green);
        (*image)->matte_color.blue=XDownScale(target_color.blue);
        continue;
      }
    if (strncmp("-modulate",option,4) == 0)
      {
        ModulateImage(*image,argv[++i]);
        continue;
      }
    if (strncmp("-monochrome",option,4) == 0)
      {
        local_info->monochrome=True;
        quantize_info.number_colors=2;
        quantize_info.tree_depth=8;
        quantize_info.colorspace=GRAYColorspace;
        continue;
      }
    if (strncmp("negate",option+1,3) == 0)
      {
        NegateImage(*image,*option == '+');
        continue;
      }
    if (strncmp("noise",option+1,4) == 0)
      {
        Image
          *noisy_image;

        /*
          Reduce noise in image.
        */
        if (*option == '-')
          noisy_image=ReduceNoiseImage(*image);
        else
          {
            NoiseType
              noise_type;

            option=argv[++i];
            noise_type=UniformNoise;
            if (Latin1Compare("Gaussian",option) == 0)
              noise_type=GaussianNoise;
            if (Latin1Compare("multiplicative",option) == 0)
              noise_type=MultiplicativeGaussianNoise;
            if (Latin1Compare("impulse",option) == 0)
              noise_type=ImpulseNoise;
            if (Latin1Compare("laplacian",option) == 0)
              noise_type=LaplacianNoise;
            if (Latin1Compare("Poisson",option) == 0)
              noise_type=PoissonNoise;
            noisy_image=AddNoiseImage(*image,noise_type);
          }
        if (noisy_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=noisy_image;
          }
        continue;
      }
    if (strncmp("-normalize",option,4) == 0)
      {
        NormalizeImage(*image);
        continue;
      }
    if (strncmp("-opaque",option,3) == 0)
      {
        OpaqueImage(*image,argv[++i],local_info->pen);
        continue;
      }
    if (strncmp("-paint",option,4) == 0)
      {
        Image
          *painted_image;

        /*
          Oil paint image.
        */
        painted_image=OilPaintImage(*image,atoi(argv[++i]));
        if (painted_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=painted_image;
          }
        continue;
      }
    if (Latin1Compare("-pen",option) == 0)
      {
        (void) CloneString(&local_info->pen,argv[++i]);
        continue;
      }
    if (strncmp("pointsize",option+1,2) == 0)
      {
        local_info->pointsize=atoi(argv[++i]);
        continue;
      }
    if (strncmp("profile",option+1,4) == 0)
      {
        Image
          *profile;

        if (*option == '+')
          {
            /*
              Remove a ICC or IPTC profile from the image.
            */
            option=argv[++i];
            if (Latin1Compare("icc",option) == 0)
              {
                if ((*image)->color_profile.length != 0)
                  FreeMemory((char *) (*image)->color_profile.info);
                (*image)->color_profile.length=0;
                (*image)->color_profile.info=(unsigned char *) NULL;
              }
            if (Latin1Compare("iptc",option) == 0)
              {
                if ((*image)->iptc_profile.length != 0)
                  FreeMemory((char *) (*image)->iptc_profile.info);
                (*image)->iptc_profile.length=0;
                (*image)->iptc_profile.info=(unsigned char *) NULL;
              }
            continue;
          }
        /*
          Add a ICC or IPTC profile to the image.
        */
        (void) strcpy(local_info->filename,argv[++i]);
        profile=ReadImage(local_info);
        if (profile == (Image *) NULL)
          continue;
        if (Latin1Compare("icc",profile->magick) == 0)
          {
            if ((*image)->color_profile.length != 0)
              FreeMemory((char *) (*image)->color_profile.info);
            (*image)->color_profile.length=profile->color_profile.length;
            (*image)->color_profile.info=profile->color_profile.info;
            profile->color_profile.length=0;
            profile->color_profile.info=(unsigned char *) NULL;
          }
        if (Latin1Compare("iptc",profile->magick) == 0)
          {
            if ((*image)->iptc_profile.length != 0)
              FreeMemory((char *) (*image)->iptc_profile.info);
            (*image)->iptc_profile.length=profile->iptc_profile.length;
            (*image)->iptc_profile.info=profile->iptc_profile.info;
            profile->iptc_profile.length=0;
            profile->iptc_profile.info=(unsigned char *) NULL;
          }
        DestroyImage(profile);
        continue;
      }
    if (strncmp("raise",option+1,2) == 0)
      {
        RectangleInfo
          raise_info;

        /*
          Surround image with a raise of solid color.
        */
        raise_info.width=0;
        raise_info.height=0;
        raise_info.x=0;
        raise_info.y=0;
        flags=ParseGeometry(argv[++i],&raise_info.x,&raise_info.y,
          &raise_info.width,&raise_info.height);
        if ((flags & HeightValue) == 0)
          raise_info.height=raise_info.width;
        RaiseImage(*image,&raise_info,*option == '-');
        continue;
      }
    if (strncmp("region",option+1,3) == 0)
      {
        Image
          *cropped_image;

        if (region_image != (Image *) NULL)
          {
            /*
              Composite region.
            */
            matte=region_image->matte;
            CompositeImage(region_image,
              (*image)->matte ? OverCompositeOp : ReplaceCompositeOp,*image,
              region_info.x,region_info.y);
            DestroyImage(*image);
            *image=region_image;
            (*image)->matte=matte;
          }
        if (*option == '+')
          continue;
        /*
          Apply transformations to a selected region of the image.
        */
        region_info.width=(*image)->columns;
        region_info.height=(*image)->rows;
        region_info.x=0;
        region_info.y=0;
        (void) ParseGeometry(argv[++i],&region_info.x,&region_info.y,
          &region_info.width,&region_info.height);
        cropped_image=CropImage(*image,&region_info);
        if (cropped_image == (Image *) NULL)
          continue;
        region_image=(*image);
        *image=cropped_image;
        continue;
      }
    if (strncmp("-roll",option,4) == 0)
      {
        Image
          *rolled_image;

        /*
          Roll image.
        */
        x=0;
        y=0;
        flags=ParseGeometry(argv[++i],&x,&y,&width,&height);
        rolled_image=RollImage(*image,x,y);
        if (rolled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=rolled_image;
          }
        continue;
      }
    if (strncmp("-rotate",option,4) == 0)
      {
        double
          degrees;

        Image
          *rotated_image;

        /*
          Check for conditional image rotation.
        */
        i++;
        if (strchr(argv[i],'>') != (char *) NULL)
          if ((*image)->columns <= (*image)->rows)
            break;
        if (strchr(argv[i],'<') != (char *) NULL)
          if ((*image)->columns >= (*image)->rows)
            break;
        /*
          Rotate image.
        */
        degrees=atof(argv[i]);
        rotated_image=RotateImage(*image,degrees,False,True);
        if (rotated_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=rotated_image;
          }
        continue;
      }
    if (strncmp("-sample",option,3) == 0)
      {
        Image
          *sampled_image;

        /*
          Sample image with pixel replication.
        */
        width=(*image)->columns;
        height=(*image)->rows;
        x=0;
        y=0;
        (void) ParseImageGeometry(argv[++i],&x,&y,&width,&height);
        sampled_image=SampleImage(*image,width,height);
        if (sampled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=sampled_image;
          }
        continue;
      }
    if (strncmp("sans",option+1,2) == 0)
      if (*option == '-')
        i++;
    if (Latin1Compare("-scene",option) == 0)
      {
        (*image)->scene=atoi(argv[++i]);
        continue;
      }
    if (strncmp("-segment",option,4) == 0)
      {
        double
          cluster_threshold,
          smoothing_threshold;

        /*
          Segment image.
        */
        cluster_threshold=1.0;
        smoothing_threshold=1.5;
        (void) sscanf(argv[++i],"%lfx%lf",&cluster_threshold,
          &smoothing_threshold);
        (void) SegmentImage(*image,quantize_info.colorspace,local_info->verbose,
          (double) cluster_threshold,(double) smoothing_threshold);
        SyncImage(*image);
        continue;
      }
    if (strncmp("shade",option+1,5) == 0)
      {
        double
          azimuth,
          elevation;

        Image
          *shaded_image;

        /*
          Shade image.
        */
        azimuth=30.0;
        elevation=30.0;
        if (*option == '-')
          (void) sscanf(argv[++i],"%lfx%lf",&azimuth,&elevation);
        shaded_image=ShadeImage(*image,*option == '-',(double) azimuth,
          (double) elevation);
        if (shaded_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=shaded_image;
          }
        continue;
      }
    if (strncmp("-sharpen",option,5) == 0)
      {
        double
          factor;

        Image
          *sharpened_image;

        /*
          Sharpen an image.
        */
        factor=atof(argv[++i]);
        sharpened_image=SharpenImage(*image,factor);
        if (sharpened_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=sharpened_image;
          }
        continue;
      }
    if (strncmp("-shear",option,4) == 0)
      {
        double
          x_shear,
          y_shear;

        Image
          *sheared_image;

        /*
          Shear image.
        */
        x_shear=0.0;
        y_shear=0.0;
        (void) sscanf(argv[++i],"%lfx%lf",&x_shear,&y_shear);
        sheared_image=
          ShearImage(*image,(double) x_shear,(double) y_shear,False);
        if (sheared_image != (Image *) NULL)
          {
            DestroyImage(*image);
            sheared_image->class=DirectClass;
            *image=sheared_image;
          }
        continue;
      }
    if (strncmp("-solarize",option,3) == 0)
      {
        SolarizeImage(*image,atof(argv[++i]));
        continue;
      }
    if (strncmp("-spread",option,3) == 0)
      {
        unsigned int
          amount;

        Image
          *spread_image;

        /*
          Spread an image.
        */
        amount=atoi(argv[++i]);
        spread_image=SpreadImage(*image,amount);
        if (spread_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=spread_image;
          }
        continue;
      }
    if (strncmp("-swirl",option,3) == 0)
      {
        double
          degrees;

        Image
          *swirled_image;

        /*
          Swirl image.
        */
        degrees=atof(argv[++i]);
        swirled_image=SwirlImage(*image,degrees);
        if (swirled_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=swirled_image;
          }
        continue;
      }
    if (strncmp("-threshold",option,3) == 0)
      {
        double
          threshold;

        /*
          Threshold image.
        */
        threshold=atof(argv[++i]);
        ThresholdImage(*image,threshold);
        continue;
      }
    if (strncmp("-transparent",option,4) == 0)
      {
        TransparentImage(*image,argv[++i]);
        continue;
      }
    if (strncmp("-treedepth",option,4) == 0)
      {
        quantize_info.tree_depth=atoi(argv[++i]);
        continue;
      }
    if (strncmp("units",option+1,3) == 0)
      {
        (*image)->units=UndefinedResolution;
        if (*option == '-')
          {
            option=argv[++i];
            if (Latin1Compare("PixelsPerInch",option) == 0)
              (*image)->units=PixelsPerInchResolution;
            if (Latin1Compare("PixelsPerCentimeter",option) == 0)
              (*image)->units=PixelsPerCentimeterResolution;
          }
        continue;
      }
    if (Latin1Compare("wave",option+1) == 0)
      {
        double
          amplitude,
          wavelength;

        Image
          *waved_image;

        /*
          Wave image.
        */
        amplitude=25.0;
        wavelength=150.0;
        if (*option == '-')
          (void) sscanf(argv[++i],"%lfx%lf",&amplitude,&wavelength);
        waved_image=WaveImage(*image,(double) amplitude,(double) wavelength);
        if (waved_image != (Image *) NULL)
          {
            DestroyImage(*image);
            *image=waved_image;
          }
        continue;
      }
  }
  if (region_image != (Image *) NULL)
    {
      /*
        Composite transformed region onto image.
      */
      matte=region_image->matte;
      CompositeImage(region_image,
        (*image)->matte ? OverCompositeOp : ReplaceCompositeOp,*image,
        region_info.x,region_info.y);
      DestroyImage(*image);
      *image=region_image;
      (*image)->matte=matte;
    }
  if (compress)
    CondenseImage(*image);
  if ((quantize_info.number_colors != 0) ||
      (quantize_info.colorspace == GRAYColorspace))
    {
      /*
        Reduce the number of colors in the image.
      */
      if (((*image)->class == DirectClass) ||
          ((*image)->colors > quantize_info.number_colors) ||
          (quantize_info.colorspace == GRAYColorspace))
        (void) QuantizeImage(&quantize_info,*image);
      else
        CompressColormap(*image);
      /*
        Measure quantization error.
      */
      if (local_info->verbose)
        (void) QuantizationError(*image);
      SyncImage(*image);
    }
  if (map_image != (Image *) NULL)
    {
      (void) MapImage(*image,map_image,quantize_info.dither);
      DestroyImage(map_image);
    }
  /*
    Free resources.
  */
  if (geometry != (char *) NULL)
    FreeMemory((char *) geometry);
  DestroyImageInfo(local_info);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     M o g r i f y I m a g e s                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MogrifyImages applies image processing options to a sequence of
%  images as prescribed by command line options.
%
%  The format of the MogrifyImage method is:
%
%      void MogrifyImages(const ImageInfo *image_info,const int argc,
%        char **argv,Image **images)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o argc: Specifies a pointer to an integer describing the number of
%      elements in the argument vector.
%
%    o argv: Specifies a pointer to a text array containing the command line
%      arguments.
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export void MogrifyImages(const ImageInfo *image_info,const int argc,
  char **argv,Image **images)
{
#define MogrifyImageText  "  Transforming images...  "

  Image
    *image,
    *mogrify_image;

  register int
    i;

  MonitorHandler
    handler;

  unsigned int
    number_images;

  assert(image_info != (ImageInfo *) NULL);
  assert(images != (Image **) NULL);
  image=(*images);
  for (number_images=1; image->next != (Image *) NULL; number_images++)
    image=image->next;
  ProgressMonitor(MogrifyImageText,0,number_images);
  handler=SetMonitorHandler((MonitorHandler) NULL);
  MogrifyImage(image_info,argc,argv,images);
  (void) SetMonitorHandler(handler);
  image=(*images);
  mogrify_image=(*images)->next;
  if (image_info->verbose)
    DescribeImage(image,stdout,False);
  for (i=1; mogrify_image != (Image *) NULL; i++)
  {
    handler=SetMonitorHandler((MonitorHandler) NULL);
    MogrifyImage(image_info,argc,argv,&mogrify_image);
    image->next=mogrify_image;
    image->next->previous=image;
    image=image->next;
    if (image_info->verbose)
      DescribeImage(mogrify_image,stdout,False);
    mogrify_image=mogrify_image->next;
    (void) SetMonitorHandler(handler);
    ProgressMonitor(MogrifyImageText,i,number_images);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   P a r s e I m a g e G e o m e t r y                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ParseImageGeometry parses a geometry specification and returns the
%  width, height, x, and y values.  It also returns flags that indicates
%  which of the four values (width, height, xoffset, yoffset) were located
%  in the string, and whether the x and y values are negative.  In addition,
%  there are flags to report any meta characters (%, !, <, and >).
%
%  The format of the ParseImageGeometry method is:
%
%      int ParseImageGeometry(const char *geometry,int *x,int *y,
%        unsigned int *width,unsigned int *height)
%
%  A description of each parameter follows:
%
%    o flags:  Method ParseImageGeometry returns a bitmask that indicates
%      which of the four values were located in the geometry string.
%
%    o image_geometry:  Specifies a character string representing the geometry
%      specification.
%
%    o x,y:  A pointer to an integer.  The x and y offset as determined by
%      the geometry specification is returned here.
%
%    o width,height:  A pointer to an unsigned integer.  The width and height
%      as determined by the geometry specification is returned here.
%
%
*/
Export int ParseImageGeometry(const char *geometry,int *x,int *y,
  unsigned int *width,unsigned int *height)
{
  int
    delta,
    flags;

  RectangleInfo
    media_info;

  unsigned int
    former_height,
    former_width;

  /*
    Ensure the image geometry is valid.
  */
  assert(x != (int *) NULL);
  assert(y != (int *) NULL);
  assert(width != (unsigned int *) NULL);
  assert(height != (unsigned int *) NULL);
  if ((geometry == (char *) NULL) || (*geometry == '\0'))
    return(NoValue);
  /*
    Parse geometry using ParseGeometry.
  */
  former_width=(*width);
  former_height=(*height);
  flags=GetGeometry(geometry,x,y,width,height);
  if (flags & PercentValue)
    {
      int
        count;

      double
        x_scale,
        y_scale;

      /*
        Geometry is a percentage of the image size.
      */
      x_scale=(*width);
      y_scale=(*height);
      count=sscanf(geometry,"%lf%%x%lf",&x_scale,&y_scale);
      if (count != 2)
        count=sscanf(geometry,"%lfx%lf",&x_scale,&y_scale);
      if (count == 1)
        y_scale=x_scale;
      *width=Max((unsigned int) ((x_scale*former_width)/100.0),1);
      *height=Max((unsigned int) ((y_scale*former_height)/100.0),1);
      former_width=(*width);
      former_height=(*height);
    }
  if (!(flags & AspectValue))
    {
      unsigned long
        scale_factor;

      /*
        Respect aspect ratio of the image.
      */
      scale_factor=UpShift(1);
      if ((former_width*former_height) != 0)
        {
          if (((flags & WidthValue) != 0) && (flags & HeightValue) != 0)
            {
              scale_factor=UpShift(*width)/former_width;
              if (scale_factor > (UpShift(*height)/former_height))
                scale_factor=UpShift(*height)/former_height;
            }
          else
            if ((flags & WidthValue) != 0)
              scale_factor=UpShift(*width)/former_width;
            else
              scale_factor=UpShift(*height)/former_height;
        }
      *width=Max(DownShift(former_width*scale_factor),1);
      *height=Max(DownShift(former_height*scale_factor),1);
    }
  if ((flags & XValue) == 0)
    *width-=(*x) << 1;
  if ((flags & YValue) == 0)
    *height-=(*y) << 1;
  if (flags & GreaterValue)
    {
      if (former_width < *width)
        *width=former_width;
      if (former_height < *height)
        *height=former_height;
    }
  if (flags & LessValue)
    {
      if (former_width > *width)
        *width=former_width;
      if (former_height > *height)
        *height=former_height;
    }
  media_info.width=(*width);
  media_info.height=(*height);
  media_info.x=(*x);
  media_info.y=(*y);
  (void) GetGeometry(geometry,&media_info.x,&media_info.y,&media_info.width,
    &media_info.height);
  if ((flags & XValue) == 0)
    {
      /*
        Center image in the X direction.
      */
      delta=media_info.width-(*width);
      if (delta >= 0)
        *x=delta >> 1;
    }
  else
    if ((flags & XNegative) != 0)
      *x+=media_info.width-(*width);
  if ((flags & YValue) == 0)
    {
      /*
        Center image in the Y direction.
      */
      delta=media_info.height-(*height);
      if (delta >= 0)
        *y=delta >> 1;
    }
  else
    if ((flags & YNegative) != 0)
      *y+=media_info.height-(*height);
  if (flags & GreaterValue)
    {
      if ((*width+((*x) << 1)) > media_info.width)
        {
          if ((int) *width > ((*x) << 1))
            *width-=(*x) << 1;
          if ((int) *height > ((*y) << 1))
            *height-=(*y) << 1;
        }
      if ((*height+((*y) << 1)) > media_info.height)
        {
          if ((int) *width > ((*x) << 1))
            *width-=(*x) << 1;
          if ((int) *height > ((*y) << 1))
            *height-=(*y) << 1;
        }
    }
  return(flags);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   P i n g I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method PingImage returns the image size in bytes if it exists and can be
%  read (at %  least up until it reveals it's size).  The width and height of
%  the image is returned as well.  Note, only the first image in a multi-frame
%  image file is pinged.
%
%  The format of the PingImage method is:
%
%      Image *PingImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o Image: Method PingImage returns the image size in bytes if the
%      image file exists and it size can be determined otherwise 0.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *PingImage(const ImageInfo *image_info)
{
  Image
    *image;

  ImageInfo
    *ping_info;

  ping_info=CloneImageInfo(image_info);
  ping_info->ping=True;
  ping_info->verbose=False;
  ping_info->subimage=0;
  ping_info->subrange=0;
  image=ReadImage(ping_info);
  DestroyImageInfo(ping_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  if (image_info->verbose)
    DescribeImage(image,stdout,False);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadImage reads an image and returns it.  It allocates
%  the memory necessary for the new Image structure and returns a pointer to
%  the new image.  By default, the image format is determined by its magic
%  number. To specify a particular image format, precede the filename with an
%  explicit image format name and a colon (i.e.  ps:image) or as the filename
%  suffix  (i.e. image.ps).
%
%  The format of the ReadImage method is:
%
%      Image *ReadImage(ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image: Method ReadImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadImage(ImageInfo *image_info)
{
  char
    filename[MaxTextExtent];

  DelegateInfo
    delegate_info;

  Image
    *image,
    *next_image;

  MagickInfo
    *magick_info;

  register char
    *p;

  /*
    Determine image type from filename prefix or suffix (e.g. image.jpg).
  */
  assert(image_info != (ImageInfo *) NULL);
  assert(image_info->filename != (char *) NULL);
  if (*image_info->filename == '@')
    return(ReadImages(image_info));
  SetImageInfo(image_info,False);
  (void) strcpy(filename,image_info->filename);
  /*
    Call appropriate image reader based on image type.
  */
  image=(Image *) NULL;
  magick_info=(MagickInfo *) GetMagickInfo(image_info->magick);
  if ((magick_info != (MagickInfo *) NULL) &&
      (magick_info->decoder != (Image *(*)(const ImageInfo *)) NULL))
    image=(magick_info->decoder)(image_info);
  else
    if (!GetDelegateInfo(image_info->magick,(char *) NULL,&delegate_info))
      MagickWarning(MissingDelegateWarning,"no delegate for this image format",
        image_info->magick);
    else
      {
        unsigned int
          status;

        /*
          Let our decoding delegate process the image.
        */
        image=AllocateImage(image_info);
        if (image == (Image *) NULL)
          return((Image *) NULL);
        (void) strcpy(image->filename,image_info->filename);
        TemporaryFilename(image_info->filename);
        status=
          InvokeDelegate(image_info,image,image_info->magick,(char *) NULL);
        DestroyImages(image);
        image=(Image *) NULL;
        if (status == False)
          image_info->temporary=True;
        SetImageInfo(image_info,False);
        magick_info=(MagickInfo *) GetMagickInfo(image_info->magick);
        if ((magick_info != (MagickInfo *) NULL) &&
            (magick_info->decoder != (Image *(*)(const ImageInfo *)) NULL))
          image=(magick_info->decoder)(image_info);
        else
          MagickWarning(MissingDelegateWarning,
            "no delegate for this image format",image_info->magick);
      }
  if (image_info->temporary)
    {
      (void) remove(image_info->filename);
      image_info->temporary=False;
      if (image != (Image *) NULL)
        (void) strcpy(image->filename,filename);
    }
  if (image == (Image *) NULL)
    return(image);
  if (image->temporary)
    (void) remove(image_info->filename);
  if (IsSubimage(image_info->tile,False))
    {
      int
        count,
        offset,
        quantum;

      Image
        *subimages;

      unsigned int
        last,
        target;

      /*
        User specified subimages (e.g. image.miff[1,3-5,7-6,2]).
      */
      subimages=(Image *) NULL;
      target=atoi(image_info->tile);
      for (p=image_info->tile; *p != '\0'; p+=Max(offset,1))
      {
        offset=0;
        count=sscanf(p,"%u%n-%u%n",&target,&offset,&last,&offset);
        if (count == 0)
          continue;
        if (count == 1)
          last=target;
        quantum=target > last ? -1 : 1;
        for ( ; target != (last+quantum); target+=quantum)
        {
          for (next_image=image; next_image; next_image=next_image->next)
          {
            Image
              *clone_image;

            if (next_image->scene != target)
              continue;
            /*
              Clone this subimage.
            */
            next_image->orphan=True;
            clone_image=CloneImage(next_image,next_image->columns,
              next_image->rows,True);
            next_image->orphan=False;
            if (clone_image == (Image *) NULL)
              {
                MagickWarning(ResourceLimitWarning,"Memory allocation failed",
                  image_info->filename);
                break;
              }
            if (subimages == (Image *) NULL)
              {
                subimages=clone_image;
                continue;
              }
            subimages->next=clone_image;
            subimages->next->previous=subimages;
            subimages=subimages->next;
          }
        }
      }
      DestroyImages(image);
      image=(Image *) NULL;
      if (subimages == (Image *) NULL)
        {
          MagickWarning(OptionWarning,
            "Subimage specification returns no images",image_info->filename);
          return((Image *) NULL);
        }
      while (subimages->previous != (Image *) NULL)
        subimages=subimages->previous;
      image=subimages;
    }
  else
    if ((image_info->subrange != 0) && (image->next != (Image *) NULL))
      {
        int
          retain;

        /*
          User specified subimages (e.g. image.miff[1]).
        */
        for ( ; ; )
        {
          retain=(image->scene >= image_info->subimage) &&
            (image->scene <= (image_info->subimage+image_info->subrange-1));
          if (image->next != (Image *) NULL)
            {
              image=image->next;
              if (!retain)
                DestroyImage(image->previous);
              continue;
            }
          if (image->previous != (Image *) NULL)
            {
              image=image->previous;
              if (!retain)
                DestroyImage(image->next);
              break;
            }
          if (!retain)
            {
              DestroyImage(image);
              image=(Image *) NULL;
            }
          break;
        }
        if (image == (Image *) NULL)
          {
            MagickWarning(OptionWarning,
              "Subimage specification returns no images",image_info->filename);
            return((Image *) NULL);
          }
        while (image->previous != (Image *) NULL)
          image=image->previous;
      }
  if (image->status)
    MagickWarning(CorruptImageWarning,"An error has occurred reading file",
      image->filename);
  for (next_image=image; next_image; next_image=next_image->next)
  {
    next_image->tainted=False;
    (void) strcpy(next_image->magick_filename,image_info->filename);
    if (image->temporary)
      (void) strcpy(next_image->filename,image_info->filename);
    if (next_image->magick_columns == 0)
      next_image->magick_columns=next_image->columns;
    if (next_image->magick_rows == 0)
      next_image->magick_rows=next_image->rows;
    if (next_image->class == PseudoClass)
      if (IsMonochromeImage(next_image))
        {
          next_image->background_color.red=MaxRGB;
          next_image->background_color.green=MaxRGB;
          next_image->background_color.blue=MaxRGB;
        }
  }
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d I m a g e s                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadImages reads a list of image names from a file and then returns
%  the images as a linked list.
%
%  The format of the ReadImage method is:
%
%      Image *ReadImages(ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image: Method ReadImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadImages(ImageInfo *image_info)
{
  char
    *command,
    **images;

  FILE
    *file;

  Image
    *image,
    *next_image;

  int
    c,
    length,
    number_images;

  register char
    *p;

  register int
    i;

  /*
    Read image list from a file.
  */
  file=(FILE *) fopen(image_info->filename+1,"r");
  if (file == (FILE *) NULL)
    {
      MagickWarning(FileOpenWarning,"Unable to read image list",
        image_info->filename);
      return(False);
    }
  length=MaxTextExtent;
  command=(char *) AllocateMemory(length);
  for (p=command; command != (char *) NULL; p++)
  {
    c=fgetc(file);
    if (c == EOF)
      break;
    if ((p-command+1) >= length)
      {
        *p='\0';
        length<<=1;
        command=(char *) ReallocateMemory((char *) command,length);
        if (command == (char *) NULL)
          break;
        p=command+Extent(command);
      }
    *p=(unsigned char) c;
  }
  (void) fclose(file);
  if (command == (char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to read image list",
        "Memory allocation failed");
      return(False);
    }
  *p='\0';
  Strip(command);
  images=StringToArgv(command,&number_images);
  FreeMemory((char *) command);
  /*
    Read the images into a linked list.
  */
  image=(Image *) NULL;
  for (i=1; i < number_images; i++)
  {
    (void) strcpy(image_info->filename,images[i]);
    next_image=ReadImage(image_info);
    if (next_image == (Image *) NULL)
      continue;
    if (image == (Image *) NULL)
      image=next_image;
    else
      {
        register Image
          *q;

        /*
          Link image into image list.
        */
        for (q=image; q->next != (Image *) NULL; q=q->next);
        next_image->previous=q;
        q->next=next_image;
      }
  }
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     R G B T r a n s f o r m I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method RGBTransformImage converts the reference image from RGB to
%  an alternate colorspace.  The transformation matrices are not the standard
%  ones: the weights are rescaled to normalized the range of the transformed
%  values to be [0..MaxRGB].
%
%  The format of the RGBTransformImage method is:
%
%      void RGBTransformImage(Image *image,const ColorspaceType colorspace)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o colorspace: An unsigned integer value that indicates which colorspace
%      to transform the image.
%
%
*/
Export void RGBTransformImage(Image *image,const ColorspaceType colorspace)
{
#define RGBTransformImageText  "  Transforming image colors...  "
#define X 0
#define Y (MaxRGB+1)
#define Z (MaxRGB+1)*2

  double
    tx,
    ty,
    tz;

  Quantum
    *range_table;

  register double
    *x,
    *y,
    *z;

  register int
    blue,
    green,
    i,
    red;

  register Quantum
    *range_limit;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  if ((colorspace == RGBColorspace) || (colorspace == TransparentColorspace))
    return;
  if (colorspace == CMYKColorspace)
    {
      Quantum
        black,
        cyan,
        magenta,
        yellow;

      /*
        Convert RGB to CMYK colorspace.
      */
      image->colorspace=CMYKColorspace;
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        cyan=MaxRGB-p->red;
        magenta=MaxRGB-p->green;
        yellow=MaxRGB-p->blue;
        black=cyan;
        if (magenta < black)
          black=magenta;
        if (yellow < black)
          black=yellow;
        p->red=cyan;
        p->green=magenta;
        p->blue=yellow;
        p->index=black;
        p++;
      }
      return;
    }
  if (colorspace == GRAYColorspace)
    {
      /*
        Return if the image is already gray_scale.
      */
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        if ((p->red != p->green) || (p->green != p->blue))
          break;
        p++;
      }
      if (i == (int) image->packets)
        return;
    }
  /*
    Allocate the tables.
  */
  x=(double *) AllocateMemory(3*(MaxRGB+1)*sizeof(double));
  y=(double *) AllocateMemory(3*(MaxRGB+1)*sizeof(double));
  z=(double *) AllocateMemory(3*(MaxRGB+1)*sizeof(double));
  range_table=(Quantum *) AllocateMemory(4*(MaxRGB+1)*sizeof(Quantum));
  if ((x == (double *) NULL) || (y == (double *) NULL) ||
      (z == (double *) NULL) || (range_table == (Quantum *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Unable to transform color space",
        "Memory allocation failed");
      return;
    }
  /*
    Pre-compute conversion tables.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  for (i=0; i <= MaxRGB; i++)
    range_table[i+(MaxRGB+1)*3]=MaxRGB;
  range_limit=range_table+(MaxRGB+1);
  tx=0;
  ty=0;
  tz=0;
  switch (colorspace)
  {
    case GRAYColorspace:
    {
      /*
        Initialize GRAY tables:

          G = 0.29900*R+0.58700*G+0.11400*B
      */
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=0.299*i;
        y[i+X]=0.587*i;
        z[i+X]=0.114*i;
        x[i+Y]=0.299*i;
        y[i+Y]=0.587*i;
        z[i+Y]=0.114*i;
        x[i+Z]=0.299*i;
        y[i+Z]=0.587*i;
        z[i+Z]=0.114*i;
      }
      break;
    }
    case OHTAColorspace:
    {
      /*
        Initialize OHTA tables:

          I1 = 0.33333*R+0.33334*G+0.33333*B
          I2 = 0.50000*R+0.00000*G-0.50000*B
          I3 =-0.25000*R+0.50000*G-0.25000*B

        I and Q, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=(MaxRGB+1) >> 1;
      tz=(MaxRGB+1) >> 1;
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=0.33333*i;
        y[i+X]=0.33334*i;
        z[i+X]=0.33333*i;
        x[i+Y]=0.5*i;
        y[i+Y]=0.0;
        z[i+Y]=(-0.5)*i;
        x[i+Z]=(-0.25)*i;
        y[i+Z]=0.5*i;
        z[i+Z]=(-0.25)*i;
      }
      break;
    }
    case sRGBColorspace:
    {
      /*
        Initialize sRGB tables:

          Y =  0.29900*R+0.58700*G+0.11400*B
          C1= -0.29900*R-0.58700*G+0.88600*B
          C2=  0.70100*R-0.58700*G-0.11400*B

        sRGB is scaled by 1.3584.  C1 zero is 156 and C2 is at 137.
      */
      ty=UpScale(156);
      tz=UpScale(137);
      for (i=0; i <= (int) (0.018*MaxRGB); i++)
      {
        x[i+X]=0.003962014134275617*MaxRGB*i;
        y[i+X]=0.007778268551236748*MaxRGB*i;
        z[i+X]=0.001510600706713781*MaxRGB*i;
        x[i+Y]=(-0.002426619775463276)*MaxRGB*i;
        y[i+Y]=(-0.004763965913702149)*MaxRGB*i;
        z[i+Y]=0.007190585689165425*MaxRGB*i;
        x[i+Z]=0.006927257754597858*MaxRGB*i;
        y[i+Z]=(-0.005800713697502058)*MaxRGB*i;
        z[i+Z]=(-0.0011265440570958)*MaxRGB*i;
      }
      for ( ; i <= MaxRGB; i++)
      {
        x[i+X]=0.2201118963486454*(1.099*i-0.099);
        y[i+X]=0.4321260306242638*(1.099*i-0.099);
        z[i+X]=0.08392226148409894*(1.099*i-0.099);
        x[i+Y]=(-0.1348122097479598)*(1.099*i-0.099);
        y[i+Y]=(-0.2646647729834528)*(1.099*i-0.099);
        z[i+Y]=0.3994769827314126*(1.099*i-0.099);
        x[i+Z]=0.3848476530332144*(1.099*i-0.099);
        y[i+Z]=(-0.3222618720834477)*(1.099*i-0.099);
        z[i+Z]=(-0.06258578094976668)*(1.099*i-0.099);
      }
      break;
    }
    case XYZColorspace:
    {
      /*
        Initialize CIE XYZ tables:

          X = 0.412453*X+0.357580*Y+0.180423*Z
          Y = 0.212671*X+0.715160*Y+0.072169*Z
          Z = 0.019334*X+0.119193*Y+0.950227*Z
      */
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=0.412453*i;
        y[i+X]=0.35758*i;
        z[i+X]=0.180423*i;
        x[i+Y]=0.212671*i;
        y[i+Y]=0.71516*i;
        z[i+Y]=0.072169*i;
        x[i+Z]=0.019334*i;
        y[i+Z]=0.119193*i;
        z[i+Z]=0.950227*i;
      }
      break;
    }
    case YCbCrColorspace:
    {
      /*
        Initialize YCbCr tables:

          Y =  0.299000*R+0.587000*G+0.114000*B
          Cb= -0.168736*R-0.331264*G+0.500000*B
          Cr=  0.500000*R-0.418688*G-0.083168*B

        Cb and Cr, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=(MaxRGB+1) >> 1;
      tz=(MaxRGB+1) >> 1;
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=0.299*i;
        y[i+X]=0.587*i;
        z[i+X]=0.114*i;
        x[i+Y]=(-0.16873)*i;
        y[i+Y]=(-0.331264)*i;
        z[i+Y]=0.500000*i;
        x[i+Z]=0.500000*i;
        y[i+Z]=(-0.418688)*i;
        z[i+Z]=(-0.081312)*i;
      }
      break;
    }
    case YCCColorspace:
    {
      /*
        Initialize YCC tables:

          Y =  0.29900*R+0.58700*G+0.11400*B
          C1= -0.29900*R-0.58700*G+0.88600*B
          C2=  0.70100*R-0.58700*G-0.11400*B

        YCC is scaled by 1.3584.  C1 zero is 156 and C2 is at 137.
      */
      ty=UpScale(156);
      tz=UpScale(137);
      for (i=0; i <= (int) (0.018*MaxRGB); i++)
      {
        x[i+X]=0.003962014134275617*MaxRGB*i;
        y[i+X]=0.007778268551236748*MaxRGB*i;
        z[i+X]=0.001510600706713781*MaxRGB*i;
        x[i+Y]=(-0.002426619775463276)*MaxRGB*i;
        y[i+Y]=(-0.004763965913702149)*MaxRGB*i;
        z[i+Y]=0.007190585689165425*MaxRGB*i;
        x[i+Z]=0.006927257754597858*MaxRGB*i;
        y[i+Z]=(-0.005800713697502058)*MaxRGB*i;
        z[i+Z]=(-0.0011265440570958)*MaxRGB*i;
      }
      for ( ; i <= MaxRGB; i++)
      {
        x[i+X]=0.2201118963486454*(1.099*i-0.099);
        y[i+X]=0.4321260306242638*(1.099*i-0.099);
        z[i+X]=0.08392226148409894*(1.099*i-0.099);
        x[i+Y]=(-0.1348122097479598)*(1.099*i-0.099);
        y[i+Y]=(-0.2646647729834528)*(1.099*i-0.099);
        z[i+Y]=0.3994769827314126*(1.099*i-0.099);
        x[i+Z]=0.3848476530332144*(1.099*i-0.099);
        y[i+Z]=(-0.3222618720834477)*(1.099*i-0.099);
        z[i+Z]=(-0.06258578094976668)*(1.099*i-0.099);
      }
      break;
    }
    case YIQColorspace:
    {
      /*
        Initialize YIQ tables:

          Y = 0.29900*R+0.58700*G+0.11400*B
          I = 0.59600*R-0.27400*G-0.32200*B
          Q = 0.21100*R-0.52300*G+0.31200*B

        I and Q, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=(MaxRGB+1) >> 1;
      tz=(MaxRGB+1) >> 1;
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=0.299*i;
        y[i+X]=0.587*i;
        z[i+X]=0.114*i;
        x[i+Y]=0.596*i;
        y[i+Y]=(-0.274)*i;
        z[i+Y]=(-0.322)*i;
        x[i+Z]=0.211*i;
        y[i+Z]=(-0.523)*i;
        z[i+Z]=0.312*i;
      }
      break;
    }
    case YPbPrColorspace:
    {
      /*
        Initialize YPbPr tables:

          Y =  0.299000*R+0.587000*G+0.114000*B
          Pb= -0.168736*R-0.331264*G+0.500000*B
          Pr=  0.500000*R-0.418688*G-0.081312*B

        Pb and Pr, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.
      */
      ty=(MaxRGB+1) >> 1;
      tz=(MaxRGB+1) >> 1;
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=0.299*i;
        y[i+X]=0.587*i;
        z[i+X]=0.114*i;
        x[i+Y]=(-0.168736)*i;
        y[i+Y]=(-0.331264)*i;
        z[i+Y]=0.5*i;
        x[i+Z]=0.5*i;
        y[i+Z]=(-0.418688)*i;
        z[i+Z]=(-0.081312)*i;
      }
      break;
    }
    case YUVColorspace:
    default:
    {
      /*
        Initialize YUV tables:

          Y =  0.29900*R+0.58700*G+0.11400*B
          U = -0.14740*R-0.28950*G+0.43690*B
          V =  0.61500*R-0.51500*G-0.10000*B

        U and V, normally -0.5 through 0.5, are normalized to the range 0
        through MaxRGB.  Note that U = 0.493*(B-Y), V = 0.877*(R-Y).
      */
      ty=(MaxRGB+1) >> 1;
      tz=(MaxRGB+1) >> 1;
      for (i=0; i <= MaxRGB; i++)
      {
        x[i+X]=0.299*i;
        y[i+X]=0.587*i;
        z[i+X]=0.114*i;
        x[i+Y]=(-0.1474)*i;
        y[i+Y]=(-0.2895)*i;
        z[i+Y]=0.4369*i;
        x[i+Z]=0.615*i;
        y[i+Z]=(-0.515)*i;
        z[i+Z]=(-0.1)*i;
      }
      break;
    }
  }
  /*
    Convert from RGB.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Convert DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        red=p->red;
        green=p->green;
        blue=p->blue;
        p->red=range_limit[(int) (x[red+X]+y[green+X]+z[blue+X]+tx)];
        p->green=range_limit[(int) (x[red+Y]+y[green+Y]+z[blue+Y]+ty)];
        p->blue=range_limit[(int) (x[red+Z]+y[green+Z]+z[blue+Z]+tz)];
        p++;
        if (QuantumTick(i,image->packets))
          ProgressMonitor(RGBTransformImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Convert PseudoClass image.
      */
      for (i=0; i < (int) image->colors; i++)
      {
        red=image->colormap[i].red;
        green=image->colormap[i].green;
        blue=image->colormap[i].blue;
        image->colormap[i].red=
          range_limit[(int) (x[red+X]+y[green+X]+z[blue+X]+tx)];
        image->colormap[i].green=
          range_limit[(int) (x[red+Y]+y[green+Y]+z[blue+Y]+ty)];
        image->colormap[i].blue=
          range_limit[(int) (x[red+Z]+y[green+Z]+z[blue+Z]+tz)];
      }
      SyncImage(image);
      break;
    }
  }
  /*
    Free allocated memory.
  */
  FreeMemory((char *) range_table);
  FreeMemory((char *) z);
  FreeMemory((char *) y);
  FreeMemory((char *) x);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%   S e t I m a g e                                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SetImage initializes the reference image to the background color.
%
%  The format of the SetImage method is:
%
%      void SetImage(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export void SetImage(Image *image)
{
  ColorPacket
    color;

  register int
    i;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  color=image->background_color;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    p->red=color.red;
    p->green=color.green;
    p->blue=color.blue;
    p->length=0;
    p->index=0;
    p++;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S e t I m a g e I n f o                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SetImageInfo initializes the `magick' field of the ImageInfo
%  structure.  It is set to a type of image format based on the prefix or
%  suffix of the filename.  For example, `ps:image' returns PS indicating
%  a Postscript image.  JPEG is returned for this filename: `image.jpg'.
%  The filename prefix has precendence over the suffix.  Use an optional index
%  enclosed in brackets after a file name to specify a desired subimage of a
%  multi-resolution image format like Photo CD (e.g. img0001.pcd[4]).
%
%  The format of the SetImageInfo method is:
%
%      void SetImageInfo(ImageInfo *image_info,const unsigned int rectify)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o rectify: an unsigned value other than zero rectifies the attribute for
%      multi-frame support (user may want multi-frame but image format may not
%      support it).
%
%
*/
Export void SetImageInfo(ImageInfo *image_info,const unsigned int rectify)
{
  char
    magick[MaxTextExtent];

  Image
    *image;

  register char
    *p,
    *q;

  register MagickInfo
    *r;

  unsigned int
    status;

  /*
    Look for 'image.format' in filename.
  */
  assert(image_info != (ImageInfo *) NULL);
  *magick='\0';
  p=image_info->filename+Extent(image_info->filename)-1;
  if (*p == ']')
    for (q=p-1; q > image_info->filename; q--)
    {
      char
        *tile;

      /*
        Look for sub-image specification (e.g. img0001.pcd[4]).
      */
      if (*q != '[')
        continue;
      if (!IsGeometry(q+1))
        break;
      tile=(char *) AllocateMemory((p-q)*sizeof(char));
      if (tile == (char *) NULL)
        break;
      (void) strncpy(tile,q+1,p-q-1);
      tile[p-q-1]='\0';
      *q='\0';
      p=q;
      (void) CloneString(&image_info->tile,tile);
      FreeMemory((char *) tile);
      if (!IsSubimage(image_info->tile,True))
        break;
      /*
        Determine sub-image range.
      */
      image_info->subimage=atoi(image_info->tile);
      image_info->subrange=atoi(image_info->tile);
      (void) sscanf(image_info->tile,"%u-%u",&image_info->subimage,
        &image_info->subrange);
      if (image_info->subrange < image_info->subimage)
        Swap(image_info->subimage,image_info->subrange);
      else
        {
          FreeMemory(image_info->tile);
          image_info->tile=(char *) NULL;
        }
      image_info->subrange-=image_info->subimage-1;
      break;
    }
  while ((*p != '.') && (p > (image_info->filename+1)))
    p--;
  if ((Latin1Compare(p,".gz") == 0) || (Latin1Compare(p,".Z") == 0) ||
      (Latin1Compare(p,".bz2") == 0))
    do
    {
      p--;
    } while ((*p != '.') && (p > (image_info->filename+1)));
  if ((*p == '.') && (Extent(p) < (int) sizeof(magick)))
    {
      /*
        User specified image format.
      */
      (void) strcpy(magick,p+1);
      for (q=magick; *q != '\0'; q++)
        if (*q == '.')
          {
            *q='\0';
            break;
          }
      Latin1Upper(magick);
      /*
        SGI and RGB are ambiguous;  TMP must be set explicitly.
      */
      if (((strncmp(image_info->magick,"SGI",3) != 0) ||
          (Latin1Compare(magick,"RGB") != 0)) &&
          (Latin1Compare(magick,"TMP") != 0))
        (void) strcpy(image_info->magick,magick);
    }
  /*
    Look for explicit 'format:image' in filename.
  */
  image_info->affirm=image_info->file != (FILE *) NULL;
  p=image_info->filename;
  while (isalnum((int) *p))
    p++;
#if defined(vms)
  if (*(p+1) == '[')
    p++;
#endif
  if ((*p == ':') && ((p-image_info->filename) < (int) sizeof(magick)))
    {
      /*
        User specified image format.
      */
      (void) strncpy(magick,image_info->filename,p-image_info->filename);
      magick[p-image_info->filename]='\0';
      Latin1Upper(magick);
#if defined(macintosh) || defined(WIN32)
      if (!ImageFormatConflict(magick))
#endif
        {
          /*
            Strip off image format prefix.
          */
          p++;
          (void) strcpy(image_info->filename,p);
          if (Latin1Compare(magick,"IMPLICIT") != 0)
            {
              (void) strcpy(image_info->magick,magick);
              if (Latin1Compare(magick,"TMP") != 0)
                image_info->affirm=True;
              else
                image_info->temporary=True;
            }
        }
    }
  if (rectify)
    {
      char
        filename[MaxTextExtent];

      MagickInfo
        *magick_info;

      /*
        Rectify multi-image file support.
      */
      FormatString(filename,image_info->filename,0);
      if ((Latin1Compare(filename,image_info->filename) != 0) &&
          (strchr(filename,'%') == (char *) NULL))
        image_info->adjoin=False;
      magick_info=(MagickInfo *) GetMagickInfo(magick);
      if (magick_info != (MagickInfo *) NULL)
        image_info->adjoin&=magick_info->adjoin;
      return;
    }
  if (image_info->affirm)
    return;
  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return;
  /*
    Determine the image format from the first few bytes of the file.
  */
  (void) strcpy(image->filename,image_info->filename);
  status=OpenBlob(image_info,image,ReadBinaryType);
  if (status == False)
    return;
  if ((image->blob.data != (char *) NULL)  || !image->exempt)
    (void) ReadBlob(image,MaxTextExtent,magick);
  else
    {
      FILE
        *file;

      register int
        c,
        i;

      /*
        Copy standard input or pipe to temporary file.
      */
      image_info->file=(FILE *) NULL;
      TemporaryFilename(image->filename);
      image_info->temporary=True;
      FormatString(image_info->filename,"%.1024s",image->filename);
      file=fopen(image->filename,WriteBinaryType);
      if (file == (FILE *) NULL)
        {
          MagickWarning(FileOpenWarning,"Unable to write file",image->filename);
          return;
        }
      i=0;
      for (c=fgetc(image->file); c != EOF; c=fgetc(image->file))
      {
        if (i < MaxTextExtent)
          magick[i++]=c;
        (void) putc(c,file);
      }
      (void) fclose(file);
    }
  DestroyImage(image);
  magick[MaxTextExtent-1]='\0';
  if (strncmp(magick,"BEGMF",3) == 0)
    (void) strcpy(image_info->magick,"CGM");
  if (strncmp(magick,"digraph",7) == 0)
    (void) strcpy(image_info->magick,"DOT");
  if (strncmp(magick,"#FIG",4) == 0)
    (void) strcpy(image_info->magick,"FIG");
  if (strncmp(magick,"#!/usr/local/bin/gnuplot",24) == 0)
    (void) strcpy(image_info->magick,"GPLT");
  if (strncmp(magick,"IN;",3) == 0)
    (void) strcpy(image_info->magick,"HPGL");
  if (strncmp(magick+8,"ILBM",2) == 0)
    (void) strcpy(image_info->magick,"ILBM");
  if ((magick[0] == 0x00) && (magick[1] == 0x00))
    if ((magick[2] == 0x01) && ((unsigned char) magick[3] == 0xb3))
      (void) strcpy(image_info->magick,"M2V");
  if (strncmp(magick,"#?RADIANCE",10) == 0)
    (void) strcpy(image_info->magick,"RAD");
  if (strncmp(magick,"gimp xcf file",13) == 0)
    (void) strcpy(image_info->magick,"XCF");
  for (r=GetMagickInfo((char *) NULL); r != (MagickInfo *) NULL; r=r->next)
    if (r->magick)
      if (r->magick((unsigned char *) magick,MaxTextExtent))
        (void) strcpy(image_info->magick,r->tag);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S o r t C o l o r m a p B y I n t e n t s i t y                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SortColormapByIntensity sorts the colormap of a PseudoClass image
%  by decreasing color intensity.
%
%  The format of the SortColormapByIntensity method is:
%
%      void SortColormapByIntensity(Image *image)
%
%  A description of each parameter follows:
%
%    o image: A pointer to a Image structure.
%
%
*/

static int IntensityCompare(const void *x,const void *y)
{
  ColorPacket
    *color_1,
    *color_2;

  color_1=(ColorPacket *) x;
  color_2=(ColorPacket *) y;
  return((int) Intensity(*color_2)-(int) Intensity(*color_1));
}

Export void SortColormapByIntensity(Image *image)
{
  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned short
    index;

  unsigned short
    *pixels;

  assert(image != (Image *) NULL);
  if (image->class != PseudoClass)
    return;
  /*
    Allocate memory for pixel indexes.
  */
  pixels=(unsigned short *)
    AllocateMemory(image->colors*sizeof(unsigned short));
  if (pixels == (unsigned short *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to sort colormap",
        "Memory allocation failed");
      return;
    }
  /*
    Assign index values to colormap entries.
  */
  for (i=0; i < (int) image->colors; i++)
    image->colormap[i].index=(unsigned short) i;
  /*
    Sort image colormap by decreasing color popularity.
  */
  qsort((void *) image->colormap,(int) image->colors,sizeof(ColorPacket),
    (int (*)(const void *, const void *)) IntensityCompare);
  /*
    Update image colormap indexes to sorted colormap order.
  */
  for (i=0; i < (int) image->colors; i++)
    pixels[image->colormap[i].index]=(unsigned short) i;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    index=pixels[p->index];
    p->red=image->colormap[index].red;
    p->green=image->colormap[index].green;
    p->blue=image->colormap[index].blue;
    p->index=index;
    p++;
  }
  FreeMemory((char *) pixels);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S y n c I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SyncImage initializes the red, green, and blue intensities of each
%  pixel as defined by the colormap index.
%
%  The format of the SyncImage method is:
%
%      void SyncImage(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void SyncImage(Image *image)
{
  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned short
    index;

  assert(image != (Image *) NULL);
  if (image->class == DirectClass)
    return;
  for (i=0; i < (int) image->colors; i++)
  {
    image->colormap[i].index=0;
    image->colormap[i].flags=0;
  }
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    index=p->index;
    p->red=image->colormap[index].red;
    p->green=image->colormap[index].green;
    p->blue=image->colormap[index].blue;
    p++;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T e x t u r e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method TextureImage layers a texture onto the background of an image.
%
%  The format of the TextureImage method is:
%
%      void TextureImage(Image *image,Image *texture)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o texture: This image contains the texture to layer on the background.
%
%
*/
Export void TextureImage(Image *image,Image *texture)
{
#define TextureImageText  "  Appling image texture...  "

  int
    x,
    y;

  assert(image != (Image *) NULL);
  if (texture == (const Image *) NULL)
    return;
  /*
    Tile texture onto the image background.
  */
  for (y=0; y < (int) image->rows; y+=texture->rows)
  {
    for (x=0; x < (int) image->columns; x+=texture->columns)
      CompositeImage(image,ReplaceCompositeOp,texture,x,y);
    if (QuantumTick(y,image->rows))
      ProgressMonitor(TextureImageText,y,image->rows);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   T r a n s f o r m I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method TransformImage creates a new image that is a transformed size of
%  of existing one as specified by the crop and image geometries.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  If a crop geometry is specified a subregion of the image is obtained.
%  If the specified image size, as defined by the image and scale geometries,
%  is smaller than the actual image size, the image is first minified to an
%  integral of the specified image size with an antialias digital filter.  The
%  image is then scaled to the exact specified image size with pixel
%  replication.  If the specified image size is greater than the actual image
%  size, the image is first enlarged to an integral of the specified image
%  size with bilinear interpolation.  The image is then scaled to the exact
%  specified image size with pixel replication.
%
%  The format of the TransformImage method is:
%
%      void TransformImage(Image **image,const char *crop_geometry,
%        const char *image_geometry)
%
%  A description of each parameter follows:
%
%    o image: The address of an address of a structure of type Image.  The
%      transformed image is returned as this parameter.
%
%    o crop_geometry: Specifies a pointer to a crop geometry string.
%      This geometry defines a subregion of the image.
%
%    o image_geometry: Specifies a pointer to a image geometry string.
%      The specified width and height of this geometry string are absolute.
%
%
*/
Export void TransformImage(Image **image,const char *crop_geometry,
  const char *image_geometry)
{
  Image
    *transformed_image;

  int
    flags,
    x,
    y;

  unsigned int
    height,
    width;

  assert(image != (Image **) NULL);
  transformed_image=(*image);
  if (crop_geometry != (const char *) NULL)
    {
      Image
        *cropped_image;

      RectangleInfo
        crop_info;

      /*
        Crop image to a user specified size.
      */
      width=transformed_image->columns;
      height=transformed_image->rows;
      crop_info.x=0;
      crop_info.y=0;
      flags=ParseGeometry((char *) crop_geometry,&crop_info.x,&crop_info.y,
        &width,&height);
      if ((flags & WidthValue) == 0)
        width=(unsigned int) ((int) transformed_image->columns-crop_info.x);
      if ((flags & HeightValue) == 0)
        height=(unsigned int) ((int) transformed_image->rows-crop_info.y);
      if ((flags & XNegative) != 0)
        crop_info.x+=transformed_image->columns-width;
      if ((flags & YNegative) != 0)
        crop_info.y+=transformed_image->rows-height;
      if (strchr(crop_geometry,'%') != (char *) NULL)
        {
          /*
            Crop geometry is relative to image size.
          */
          x=0;
          y=0;
          (void) ParseImageGeometry(crop_geometry,&x,&y,&width,&height);
          if (width > transformed_image->columns)
            width=transformed_image->columns;
          if (height > transformed_image->rows)
            height=transformed_image->rows;
          crop_info.x=width >> 1;
          crop_info.y=height >> 1;
          width=transformed_image->columns-width;
          height=transformed_image->rows-height;
          flags|=XValue | YValue;
        }
      crop_info.width=width;
      crop_info.height=height;
      if ((width == 0) || (height == 0) ||
          ((flags & XValue) != 0) || ((flags & YValue) != 0))
        cropped_image=CropImage(transformed_image,&crop_info);
      else
        {
          Image
            *next_image;

          /*
            Crop repeatedly to create uniform subimages.
          */
          next_image=(Image *) NULL;
          cropped_image=(Image *) NULL;
          for (y=0; y < (int) transformed_image->rows; y+=height)
          {
            for (x=0; x < (int) transformed_image->columns; x+=width)
            {
              crop_info.width=width;
              crop_info.height=height;
              crop_info.x=x;
              crop_info.y=y;
              next_image=CropImage(transformed_image,&crop_info);
              if (next_image == (Image *) NULL)
                break;
              if (cropped_image == (Image *) NULL)
                cropped_image=next_image;
              else
                {
                  next_image->previous=cropped_image;
                  cropped_image->next=next_image;
                  cropped_image=cropped_image->next;
                }
            }
            if (next_image == (Image *) NULL)
              break;
          }
        }
      if (cropped_image != (Image *) NULL)
        {
          DestroyImage(transformed_image);
          while (cropped_image->previous != (Image *) NULL)
            cropped_image=cropped_image->previous;
          transformed_image=cropped_image;
        }
    }
  /*
    Scale image to a user specified size.
  */
  width=transformed_image->columns;
  height=transformed_image->rows;
  x=0;
  y=0;
  (void) ParseImageGeometry(image_geometry,&x,&y,&width,&height);
  if ((transformed_image->columns != width) ||
      (transformed_image->rows != height))
    {
      Image
        *zoomed_image;

      /*
        Zoom image.
      */
      zoomed_image=ZoomImage(transformed_image,width,height);
      if (zoomed_image == (Image *) NULL)
        zoomed_image=ScaleImage(transformed_image,width,height);
      if (zoomed_image != (Image *) NULL)
        {
          DestroyImage(transformed_image);
          transformed_image=zoomed_image;
        }
    }
  *image=transformed_image;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T r a n s f o r m R G B I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method TransformRGBImage converts the reference image from an alternate
%  colorspace.  The transformation matrices are not the standard ones:  the
%  weights are rescaled to normalized the range of the transformed values to
%  be [0..MaxRGB].
%
%  The format of the TransformRGBImage method is:
%
%      void TransformRGBImage(Image *image,const ColorspaceType colorspace)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o colorspace: An unsigned integer value that indicates the colorspace
%      the image is currently in.  On return the image is in the RGB
%      color space.
%
%
*/
Export void TransformRGBImage(Image *image,const ColorspaceType colorspace)
{
#define B (MaxRGB+1)*2
#define G (MaxRGB+1)
#define R 0
#define TransformRGBImageText  "  Transforming image colors...  "

  static const Quantum
    sRGBMap[351] =
    {
        0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,
       14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
       28,  29,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
       41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,
       55,  56,  57,  58,  59,  60,  61,  62,  63,  65,  66,  67,  68,  69,
       70,  71,  72,  73,  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,
       84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  95,  96,  97,  98,
       99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
      114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
      128, 129, 130, 131, 132, 133, 135, 136, 137, 138, 139, 140, 141, 142,
      143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156,
      157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170,
      171, 172, 173, 174, 175, 175, 176, 177, 178, 179, 180, 181, 182, 183,
      184, 185, 186, 187, 187, 188, 189, 190, 191, 192, 193, 194, 194, 195,
      196, 197, 198, 199, 199, 200, 201, 202, 203, 203, 204, 205, 206, 207,
      207, 208, 209, 210, 210, 211, 212, 213, 213, 214, 215, 215, 216, 217,
      218, 218, 219, 220, 220, 221, 222, 222, 223, 223, 224, 225, 225, 226,
      227, 227, 228, 228, 229, 229, 230, 230, 231, 232, 232, 233, 233, 234,
      234, 235, 235, 235, 236, 236, 237, 237, 238, 238, 238, 239, 239, 240,
      240, 240, 241, 241, 242, 242, 242, 243, 243, 243, 243, 244, 244, 244,
      245, 245, 245, 245, 246, 246, 246, 247, 247, 247, 247, 247, 248, 248,
      248, 248, 249, 249, 249, 249, 249, 249, 250, 250, 250, 250, 250, 250,
      251, 251, 251, 251, 251, 251, 252, 252, 252, 252, 252, 252, 252, 252,
      252, 253, 253, 253, 253, 253, 253, 253, 253, 253, 254, 254, 254, 254,
      254, 254, 254, 254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255,
      255
    },
    YCCMap[351] =  /* Photo CD information beyond 100% white, Gamma 2.2 */
    {
        0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,
        14, 15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
        28, 29,  30,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,
        43, 45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  56,  57,  58,
        59, 60,  61,  62,  63,  64,  66,  67,  68,  69,  70,  71,  72,  73,
        74, 76,  77,  78,  79,  80,  81,  82,  83,  84,  86,  87,  88,  89,
        90, 91,  92,  93,  94,  95,  97,  98,  99, 100, 101, 102, 103, 104,
      105, 106, 107, 108, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
      120, 121, 122, 123, 124, 125, 126, 127, 129, 130, 131, 132, 133, 134,
      135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148,
      149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162,
      163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176,
      176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
      190, 191, 192, 193, 193, 194, 195, 196, 197, 198, 199, 200, 201, 201,
      202, 203, 204, 205, 206, 207, 207, 208, 209, 210, 211, 211, 212, 213,
      214, 215, 215, 216, 217, 218, 218, 219, 220, 221, 221, 222, 223, 224,
      224, 225, 226, 226, 227, 228, 228, 229, 230, 230, 231, 232, 232, 233,
      234, 234, 235, 236, 236, 237, 237, 238, 238, 239, 240, 240, 241, 241,
      242, 242, 243, 243, 244, 244, 245, 245, 245, 246, 246, 247, 247, 247,
      248, 248, 248, 249, 249, 249, 249, 250, 250, 250, 250, 251, 251, 251,
      251, 251, 252, 252, 252, 252, 252, 253, 253, 253, 253, 253, 253, 253,
      253, 253, 253, 253, 253, 253, 254, 254, 254, 254, 254, 254, 254, 254,
      254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      255
    };

  double
    *blue,
    *green,
    *red;

  Quantum
    *range_table;

  register int
    i,
    x,
    y,
    z;

  register Quantum
    *range_limit;

  register RunlengthPacket
    *p;

  assert(image != (Image *) NULL);
  if ((image->colorspace == CMYKColorspace) && (colorspace == RGBColorspace))
    {
      unsigned int
        black,
        cyan,
        magenta,
        yellow;

      /*
        Transform image from CMYK to RGB.
      */
      image->colorspace=RGBColorspace;
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        cyan=p->red;
        magenta=p->green;
        yellow=p->blue;
        black=p->index;
        if ((cyan+black) > MaxRGB)
          p->red=0;
        else
          p->red=MaxRGB-(cyan+black);
        if ((magenta+black) > MaxRGB)
          p->green=0;
        else
          p->green=MaxRGB-(magenta+black);
        if ((yellow+black) > MaxRGB)
          p->blue=0;
        else
          p->blue=MaxRGB-(yellow+black);
        p->index=0;
        p++;
      }
      return;
    }
  if ((colorspace == RGBColorspace) || (colorspace == GRAYColorspace) ||
      (colorspace == TransparentColorspace))
    return;
  /*
    Allocate the tables.
  */
  red=(double *) AllocateMemory(3*(MaxRGB+1)*sizeof(double));
  green=(double *) AllocateMemory(3*(MaxRGB+1)*sizeof(double));
  blue=(double *) AllocateMemory(3*(MaxRGB+1)*sizeof(double));
  range_table=(Quantum *) AllocateMemory(4*(MaxRGB+1)*sizeof(Quantum));
  if ((red == (double *) NULL) || (green == (double *) NULL) ||
      (blue == (double *) NULL) || (range_table == (Quantum *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Unable to transform color space",
        "Memory allocation failed");
      return;
    }
  /*
    Initialize tables.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  for (i=0; i <= MaxRGB; i++)
    range_table[i+(MaxRGB+1)*3]=MaxRGB;
  range_limit=range_table+(MaxRGB+1);
  switch (colorspace)
  {
    case OHTAColorspace:
    {
      /*
        Initialize OHTA tables:

          R = I1+1.00000*I2-0.66668*I3
          G = I1+0.00000*I2+1.33333*I3
          B = I1-1.00000*I2-0.66668*I3

        I and Q, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=i;
        green[i+R]=0.5*(2.0*i-MaxRGB);
        blue[i+R]=(-0.33334)*(2.0*i-MaxRGB);
        red[i+G]=i;
        green[i+G]=0.0;
        blue[i+G]=0.666665*(2.0*i-MaxRGB);
        red[i+B]=i;
        green[i+B]=(-0.5)*(2.0*i-MaxRGB);
        blue[i+B]=(-0.33334)*(2.0*i-MaxRGB);
      }
      break;
    }
    case sRGBColorspace:
    {
      /*
        Initialize sRGB tables:

          R = Y            +1.032096*C2
          G = Y-0.326904*C1-0.704445*C2
          B = Y+1.685070*C1

        sRGB is scaled by 1.3584.  C1 zero is 156 and C2 is at 137.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=1.40200*i;
        green[i+R]=0.0;
        blue[i+R]=1.88000*(i-(double) UpScale(137));
        red[i+G]=1.40200*i;
        green[i+G]=(-0.444066)*(i-(double) UpScale(156));
        blue[i+G]=(-0.95692)*(i-(double) UpScale(137));
        red[i+B]=1.40200*i;
        green[i+B]=2.28900*(i-(double) UpScale(156));
        blue[i+B]=0.0;
        range_table[i+(MaxRGB+1)]=(Quantum) UpScale(sRGBMap[DownScale(i)]);
      }
      for ( ; i < (int) UpScale(351); i++)
        range_table[i+(MaxRGB+1)]=(Quantum) UpScale(sRGBMap[DownScale(i)]);
      break;
    }
    case XYZColorspace:
    {
      /*
        Initialize CIE XYZ tables:

          R =  3.240479*R-1.537150*G-0.498535*B
          G = -0.969256*R+1.875992*G+0.041556*B
          B =  0.055648*R-0.204043*G+1.057311*B
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=3.240479*i;
        green[i+R]=(-1.537150)*i;
        blue[i+R]=(-0.498535)*i;
        red[i+G]=(-0.969256)*i;
        green[i+G]=1.875992*i;
        blue[i+G]=0.041556*i;
        red[i+B]=0.055648*i;
        green[i+B]=(-0.204043)*i;
        blue[i+B]=1.057311*i;
      }
      break;
    }
    case YCbCrColorspace:
    {
      /*
        Initialize YCbCr tables:

          R = Y            +1.402000*Cr
          G = Y-0.344136*Cb-0.714136*Cr
          B = Y+1.772000*Cb

        Cb and Cr, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=i;
        green[i+R]=0.0;
        blue[i+R]=(1.402000*0.5)*(2.0*i-MaxRGB);
        red[i+G]=i;
        green[i+G]=(-0.344136*0.5)*(2.0*i-MaxRGB);
        blue[i+G]=(-0.714136*0.5)*(2.0*i-MaxRGB);
        red[i+B]=i;
        green[i+B]=(1.772000*0.5)*(2.0*i-MaxRGB);
        blue[i+B]=0.0;
      }
      break;
    }
    case YCCColorspace:
    {
      /*
        Initialize YCC tables:

          R = Y            +1.340762*C2
          G = Y-0.317038*C1-0.682243*C2
          B = Y+1.632639*C1

        YCC is scaled by 1.3584.  C1 zero is 156 and C2 is at 137.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=1.3584*i;
        green[i+R]=0.0;
        blue[i+R]=1.8215*(i-(double) UpScale(137));
        red[i+G]=1.3584*i;
        green[i+G]=(-0.4302726)*(i-(double) UpScale(156));
        blue[i+G]=(-0.9271435)*(i-(double) UpScale(137));
        red[i+B]=1.3584*i;
        green[i+B]=2.2179*(i-(double) UpScale(156));
        blue[i+B]=0.0;
        range_table[i+(MaxRGB+1)]=(Quantum) UpScale(YCCMap[DownScale(i)]);
      }
      for ( ; i < (int) UpScale(351); i++)
        range_table[i+(MaxRGB+1)]=(Quantum) UpScale(YCCMap[DownScale(i)]);
      break;
    }
    case YIQColorspace:
    {
      /*
        Initialize YIQ tables:

          R = Y+0.95620*I+0.62140*Q
          G = Y-0.27270*I-0.64680*Q
          B = Y-1.10370*I+1.70060*Q

        I and Q, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=i;
        green[i+R]=0.4781*(2.0*i-MaxRGB);
        blue[i+R]=0.3107*(2.0*i-MaxRGB);
        red[i+G]=i;
        green[i+G]=(-0.13635)*(2.0*i-MaxRGB);
        blue[i+G]=(-0.3234)*(2.0*i-MaxRGB);
        red[i+B]=i;
        green[i+B]=(-0.55185)*(2.0*i-MaxRGB);
        blue[i+B]=0.8503*(2.0*i-MaxRGB);
      }
      break;
    }
    case YPbPrColorspace:
    {
      /*
        Initialize YPbPr tables:

          R = Y            +1.402000*C2
          G = Y-0.344136*C1+0.714136*C2
          B = Y+1.772000*C1

        Pb and Pr, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=i;
        green[i+R]=0.0;
        blue[i+R]=0.701*(2.0*i-MaxRGB);
        red[i+G]=i;
        green[i+G]=(-0.172068)*(2.0*i-MaxRGB);
        blue[i+G]=0.357068*(2.0*i-MaxRGB);
        red[i+B]=i;
        green[i+B]=0.886*(2.0*i-MaxRGB);
        blue[i+B]=0.0;
      }
      break;
    }
    case YUVColorspace:
    default:
    {
      /*
        Initialize YUV tables:

          R = Y          +1.13980*V
          G = Y-0.39380*U-0.58050*V
          B = Y+2.02790*U

        U and V, normally -0.5 through 0.5, must be normalized to the range 0
        through MaxRGB.
      */
      for (i=0; i <= MaxRGB; i++)
      {
        red[i+R]=i;
        green[i+R]=0.0;
        blue[i+R]=0.5699*(2.0*i-MaxRGB);
        red[i+G]=i;
        green[i+G]=(-0.1969)*(2.0*i-MaxRGB);
        blue[i+G]=(-0.29025)*(2.0*i-MaxRGB);
        red[i+B]=i;
        green[i+B]=1.01395*(2.0*i-MaxRGB);
        blue[i+B]=0;
      }
      break;
    }
  }
  /*
    Convert to RGB.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      /*
        Convert DirectClass image.
      */
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        x=p->red;
        y=p->green;
        z=p->blue;
        p->red=range_limit[(int) (red[x+R]+green[y+R]+blue[z+R])];
        p->green=range_limit[(int) (red[x+G]+green[y+G]+blue[z+G])];
        p->blue=range_limit[(int) (red[x+B]+green[y+B]+blue[z+B])];
        p++;
        if (QuantumTick(i,image->packets))
          ProgressMonitor(TransformRGBImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      /*
        Convert PseudoClass image.
      */
      for (i=0; i < (int) image->colors; i++)
      {
        x=image->colormap[i].red;
        y=image->colormap[i].green;
        z=image->colormap[i].blue;
        image->colormap[i].red=
          range_limit[(int) (red[x+R]+green[y+R]+blue[z+R])];
        image->colormap[i].green=
          range_limit[(int) (red[x+G]+green[y+G]+blue[z+G])];
        image->colormap[i].blue=
          range_limit[(int) (red[x+B]+green[y+B]+blue[z+B])];
      }
      SyncImage(image);
      break;
    }
  }
  /*
    Free allocated memory.
  */
  FreeMemory((char *) range_table);
  FreeMemory((char *) blue);
  FreeMemory((char *) green);
  FreeMemory((char *) red);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     T r a n s p a r e n t I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method TransparentImage creates a matte image associated with the
%  image.  All pixel locations are initially set to opaque.  Any pixel
%  that matches the specified color are set to transparent.
%
%  The format of the TransparentImage method is:
%
%      void TransparentImage(Image *image,const char *color)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o color: A character string that contain an X11 color string.
%
%
*/
Export void TransparentImage(Image *image,const char *color)
{
#define TransparentImageText  "  Setting transparent color in the image...  "

  ColorPacket
    target,
    target_color;

  register int
    i;

  register RunlengthPacket
    *p;

  unsigned int
    status;

  /*
    Determine RGB values of the transparent color.
  */
  assert(image != (Image *) NULL);
  status=QueryColorDatabase(color,&target_color);
  if (status == False)
    return;
  target.red=XDownScale(target_color.red);
  target.green=XDownScale(target_color.green);
  target.blue=XDownScale(target_color.blue);
  /*
    Make image color transparent.
  */
  if (!image->matte)
    MatteImage(image);
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    if (ColorMatch(*p,target,image->fuzz))
      p->index=Transparent;
    p++;
    if (QuantumTick(i,image->packets))
      ProgressMonitor(TransparentImageText,i,image->packets);
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   U n c o n d e n s e I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method UncondenseImage uncompresses runlength-encoded pixels packets to
%  a rectangular array of pixels.
%
%  The format of the UncondenseImage method is:
%
%      unsigned int UncondenseImage(Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method UncondenseImage returns True if the image is
%      uncompressed otherwise False.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned int UncondenseImage(Image *image)
{
  int
    length;

  register int
    i,
    j;

  register RunlengthPacket
    *p,
    *q;

  RunlengthPacket
    *uncompressed_pixels;

  assert(image != (Image *) NULL);
  if (image->packets == (image->columns*image->rows))
    return(True);
  /*
    Uncompress runlength-encoded packets.
  */
  uncompressed_pixels=(RunlengthPacket *) ReallocateMemory((char *)
    image->pixels,image->columns*image->rows*sizeof(RunlengthPacket));
  if (uncompressed_pixels == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to uncompress image",
        "Memory allocation failed");
      return(False);
    }
  p=uncompressed_pixels+(image->packets-1);
  q=uncompressed_pixels+(image->columns*image->rows-1);
  for (i=0; i < (int) image->packets; i++)
  {
    length=p->length;
    for (j=0; j <= length; j++)
    {
      *q=(*p);
      q->length=0;
      q--;
    }
    p--;
  }
  image->packets=image->columns*image->rows;
  image->pixels=uncompressed_pixels;
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteImage writes an image to a file as defined by image->filename.
%  You can specify a particular image format by prefixing the file with the
%  image type and a colon (i.e. ps:image) or specify the image type as the
%  filename suffix (i.e. image.ps).  The image may be modified to adapt it
%  to the requirements of the image format.  For example, DirectClass images
%  must be color-reduced to PseudoClass if the format is GIF.
%
%  The format of the WriteImage method is:
%
%      unsigned int WriteImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method WriteImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: A pointer to a Image structure.
%
%
*/
Export unsigned int WriteImage(const ImageInfo *image_info,Image *image)
{
  DelegateInfo
    delegate_info;

  ImageInfo
    *local_info;

  MagickInfo
    *magick_info;

  unsigned int
    status;

  /*
    Determine image type from filename prefix or suffix (e.g. image.jpg).
  */
  assert(image_info != (ImageInfo *) NULL);
  assert(image_info->filename != (char *) NULL);
  assert(image != (Image *) NULL);
  local_info=CloneImageInfo(image_info);
  (void) strcpy(local_info->filename,image->filename);
  (void) strcpy(local_info->magick,image->magick);
  SetImageInfo(local_info,True);
  (void) strcpy(image->filename,local_info->filename);
  if ((image->next == (Image *) NULL) || local_info->adjoin)
    if ((image->previous == (Image *) NULL) && !IsTainted(image))
      if (IsAccessible(image->magick_filename))
        if (GetDelegateInfo(image->magick,local_info->magick,&delegate_info))
          {
            /*
              Let our bi-directional delegate process the image.
            */
            (void) strcpy(image->filename,image->magick_filename);
            status=
              InvokeDelegate(local_info,image,image->magick,local_info->magick);
            DestroyImageInfo(local_info);
            return(status);
          }
  /*
    Call appropriate image writer based on image type.
  */
  status=False;
  magick_info=(MagickInfo *) GetMagickInfo(local_info->magick);
  if ((magick_info != (MagickInfo *) NULL) &&
      (magick_info->encoder !=
      (unsigned int (*)(const ImageInfo *,Image *)) NULL))
    status=(magick_info->encoder)(local_info,image);
  else
    if (!GetDelegateInfo((char *) NULL,local_info->magick,&delegate_info))
      {
        MagickWarning(MissingDelegateWarning,
          "no encode delegate for this image format",local_info->magick);
        magick_info=(MagickInfo *) GetMagickInfo(image->magick);
        if ((magick_info != (MagickInfo *) NULL) &&
            (magick_info->encoder !=
            (unsigned int (*)(const ImageInfo *,Image *)) NULL))
          status=(magick_info->encoder)(local_info,image);
        else
          MagickWarning(MissingDelegateWarning,
            "no encode delegate for this image format",image->magick);
      }
    else
      {
        /*
          Let our encoding delegate process the image.
        */
        TemporaryFilename(image->filename);
        status=
          InvokeDelegate(local_info,image,(char *) NULL,local_info->magick);
        (void) remove(image->filename);
        DestroyImageInfo(local_info);
        return(status);
      }
  if (image->status)
    {
      MagickWarning(CorruptImageWarning,"An error has occurred writing to file",
        image->filename);
      DestroyImageInfo(local_info);
      return(False);
    }
  (void) strcpy(image->magick,local_info->magick);
  DestroyImageInfo(local_info);
  return(status);
}
