/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            DDDD   PPPP   SSSSS                              %
%                            D   D  P   P  SS                                 %
%                            D   D  PPPP    SSS                               %
%                            D   D  P         SS                              %
%                            DDDD   P      SSSSS                              %
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

#if defined(HasDPS)
#include "xwindows.h"
#include <DPS/dpsXclient.h>
#include <DPS/dpsXpreview.h>
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d D P S I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadDPSImage reads a Adobe Postscript image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadDPSImage method is:
%
%      Image *ReadDPSImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadDPSImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadDPSImage(const ImageInfo *image_info)
{
  char
    *client_name;

  Display
    *display;

  float
    pixels_per_point;

  Image
    *image;

  int
    sans,
    status,
    x,
    y;

  Pixmap
    pixmap;

  register int
    i;

  register RunlengthPacket
    *p;

  register unsigned long
    pixel;

  Screen
    *screen;

  XColor
    *colors;

  XImage
    *dps_image;

  XRectangle
    page_info,
    bits_per_pixel;

  XResourceInfo
    resource_info;

  XrmDatabase
    resource_database;

  XStandardColormap
    *map_info;

  XVisualInfo
    *visual_info;

  /*
    Open X server connection.
  */
  display=XOpenDisplay(image_info->server_name);
  if (display == (Display *) NULL)
    return((Image *) NULL);
  /*
    Set our forgiving error handler.
  */
  XSetErrorHandler(XError);
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
    return((Image *) NULL);
  /*
    Get user defaults from X resource database.
  */
  client_name=SetClientName((char *) NULL);
  resource_database=XGetResourceDatabase(display,client_name);
  XGetResourceInfo(resource_database,client_name,&resource_info);
  /*
    Allocate standard colormap.
  */
  map_info=XAllocStandardColormap();
  visual_info=(XVisualInfo *) NULL;
  if (map_info == (XStandardColormap *) NULL)
    MagickWarning(ResourceLimitWarning,"Unable to create standard colormap",
      "Memory allocation failed");
  else
    {
      /*
        Initialize visual info.
      */
      (void) CloneString(&resource_info.visual_type,"default");
      visual_info=XBestVisualInfo(display,map_info,&resource_info);
      map_info->colormap=(Colormap) NULL;
    }
  if ((map_info == (XStandardColormap *) NULL) ||
      (visual_info == (XVisualInfo *) NULL))
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Create a pixmap the appropriate size for the image.
  */
  screen=ScreenOfDisplay(display,visual_info->screen);
  pixels_per_point=XDPSPixelsPerPoint(screen);
  if ((image->x_resolution != 0.0) && (image->y_resolution != 0.0))
    pixels_per_point=Min(image->x_resolution,image->y_resolution)/72.0;
  status=XDPSCreatePixmapForEPSF((DPSContext) NULL,screen,image->file,
    visual_info->depth,pixels_per_point,&pixmap,&bits_per_pixel,&page_info);
  if ((status == dps_status_failure) || (status == dps_status_no_extension))
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Rasterize the file into the pixmap.
  */
  status=XDPSImageFileIntoDrawable((DPSContext) NULL,screen,pixmap,image->file,
    bits_per_pixel.height,visual_info->depth,&page_info,-page_info.x,
    -page_info.y,pixels_per_point,True,False,True,&sans);
  if (status != dps_status_success)
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Initialize DPS X image.
  */
  dps_image=XGetImage(display,pixmap,0,0,bits_per_pixel.width,
    bits_per_pixel.height,AllPlanes,ZPixmap);
  XFreePixmap(display,pixmap);
  if (dps_image == (XImage *) NULL)
    {
      DestroyImage(image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  /*
    Get the colormap colors.
  */
  colors=(XColor *) AllocateMemory(visual_info->colormap_size*sizeof(XColor));
  if (colors == (XColor *) NULL)
    {
      DestroyImage(image);
      XDestroyImage(dps_image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  if ((visual_info->class != DirectColor) &&
      (visual_info->class != TrueColor))
    for (i=0; i < visual_info->colormap_size; i++)
    {
      colors[i].pixel=i;
      colors[i].pad=0;
    }
  else
    {
      unsigned long
        blue,
        blue_bit,
        green,
        green_bit,
        red,
        red_bit;

      /*
        DirectColor or TrueColor visual.
      */
      red=0;
      green=0;
      blue=0;
      red_bit=visual_info->red_mask & (~(visual_info->red_mask)+1);
      green_bit=visual_info->green_mask & (~(visual_info->green_mask)+1);
      blue_bit=visual_info->blue_mask & (~(visual_info->blue_mask)+1);
      for (i=0; i < visual_info->colormap_size; i++)
      {
        colors[i].pixel=red | green | blue;
        colors[i].pad=0;
        red+=red_bit;
        if (red > visual_info->red_mask)
          red=0;
        green+=green_bit;
        if (green > visual_info->green_mask)
          green=0;
        blue+=blue_bit;
        if (blue > visual_info->blue_mask)
          blue=0;
      }
    }
  XQueryColors(display,XDefaultColormap(display,visual_info->screen),colors,
    visual_info->colormap_size);
  /*
    Convert X image to MIFF format.
  */
  if ((visual_info->class != TrueColor) &&
      (visual_info->class != DirectColor))
    image->class=PseudoClass;
  image->columns=dps_image->width;
  image->rows=dps_image->height;
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      DestroyImage(image);
      FreeMemory((char *) colors);
      XDestroyImage(dps_image);
      XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
        (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
      return((Image *) NULL);
    }
  SetImage(image);
  p=image->pixels;
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      register unsigned long
        color,
        index;

      unsigned long
        blue_mask,
        blue_shift,
        green_mask,
        green_shift,
        red_mask,
        red_shift;

      /*
        Determine shift and mask for red, green, and blue.
      */
      red_mask=visual_info->red_mask;
      red_shift=0;
      while ((red_mask & 0x01) == 0)
      {
        red_mask>>=1;
        red_shift++;
      }
      green_mask=visual_info->green_mask;
      green_shift=0;
      while ((green_mask & 0x01) == 0)
      {
        green_mask>>=1;
        green_shift++;
      }
      blue_mask=visual_info->blue_mask;
      blue_shift=0;
      while ((blue_mask & 0x01) == 0)
      {
        blue_mask>>=1;
        blue_shift++;
      }
      /*
        Convert X image to DirectClass packets.
      */
      if ((visual_info->colormap_size > 0) &&
          (visual_info->class == DirectColor))
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            pixel=XGetPixel(dps_image,x,y);
            index=(pixel >> red_shift) & red_mask;
            p->red=XDownScale(colors[index].red);
            index=(pixel >> green_shift) & green_mask;
            p->green=XDownScale(colors[index].green);
            index=(pixel >> blue_shift) & blue_mask;
            p->blue=XDownScale(colors[index].blue);
            p->index=0;
            p->length=0;
            p++;
          }
          if (QuantumTick(y,image->rows))
            ProgressMonitor(LoadImageText,y,image->rows);
        }
      else
        for (y=0; y < (int) image->rows; y++)
        {
          for (x=0; x < (int) image->columns; x++)
          {
            pixel=XGetPixel(dps_image,x,y);
            color=(pixel >> red_shift) & red_mask;
            p->red=XDownScale((color*65535L)/red_mask);
            color=(pixel >> green_shift) & green_mask;
            p->green=XDownScale((color*65535L)/green_mask);
            color=(pixel >> blue_shift) & blue_mask;
            p->blue=XDownScale((color*65535L)/blue_mask);
            p->index=0;
            p->length=0;
            p++;
          }
          if (QuantumTick(y,image->rows))
            ProgressMonitor(LoadImageText,y,image->rows);
        }
      break;
    }
    case PseudoClass:
    {
      /*
        Create colormap.
      */
      image->colors=visual_info->colormap_size;
      image->colormap=(ColorPacket *)
        AllocateMemory(image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        {
          DestroyImage(image);
          FreeMemory((char *) colors);
          XDestroyImage(dps_image);
          XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
            (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
          return((Image *) NULL);
        }
      for (i=0; i < (int) image->colors; i++)
      {
        image->colormap[colors[i].pixel].red=XDownScale(colors[i].red);
        image->colormap[colors[i].pixel].green=XDownScale(colors[i].green);
        image->colormap[colors[i].pixel].blue=XDownScale(colors[i].blue);
      }
      /*
        Convert X image to PseudoClass packets.
      */
      for (y=0; y < (int) image->rows; y++)
      {
        for (x=0; x < (int) image->columns; x++)
        {
          p->index=(unsigned short) XGetPixel(dps_image,x,y);
          p->length=0;
          p++;
        }
        if (QuantumTick(y,image->rows))
          ProgressMonitor(LoadImageText,y,image->rows);
      }
      break;
    }
  }
  FreeMemory((char *) colors);
  XDestroyImage(dps_image);
  if (image->class == PseudoClass)
    SyncImage(image);
  CondenseImage(image);
  /*
    Rasterize matte image.
  */
  status=XDPSCreatePixmapForEPSF((DPSContext) NULL,screen,image->file,1,
    pixels_per_point,&pixmap,&bits_per_pixel,&page_info);
  if ((status != dps_status_failure) && (status != dps_status_no_extension))
    {
      status=XDPSImageFileIntoDrawable((DPSContext) NULL,screen,pixmap,
        image->file,bits_per_pixel.height,1,&page_info,-page_info.x,
        -page_info.y,pixels_per_point,True,True,True,&sans);
      if (status == dps_status_success)
        {
          XImage
            *matte_image;

          /*
            Initialize image matte.
          */
          matte_image=XGetImage(display,pixmap,0,0,bits_per_pixel.width,
            bits_per_pixel.height,AllPlanes,ZPixmap);
          XFreePixmap(display,pixmap);
          if (matte_image != (XImage *) NULL)
            {
              image->class=DirectClass;
              image->matte=True;
              p=image->pixels;
              for (y=0; y < (int) image->rows; y++)
                for (x=0; x < (int) image->columns; x++)
                {
                  p->index=Opaque;
                  if (!XGetPixel(matte_image,x,y))
                    p->index=Transparent;
                  p++;
                }
              XDestroyImage(matte_image);
            }
        }
    }
  /*
    Free resources.
  */
  XFreeResources(display,visual_info,map_info,(XPixelInfo *) NULL,
    (XFontStruct *) NULL,&resource_info,(XWindowInfo *) NULL);
  CloseBlob(image);
  return(image);
}
#else
Export Image *ReadDPSImage(const ImageInfo *image_info)
{
  MagickWarning(MissingDelegateWarning,"Cannot read DPS images",
    image_info->filename);
  return((Image *) NULL);
}
#endif
