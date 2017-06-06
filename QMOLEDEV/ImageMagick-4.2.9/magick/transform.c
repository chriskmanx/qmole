/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%       TTTTT  RRRR    AAA   N   N  SSSSS  FFFFF   OOO   RRRR   M   M         %
%         T    R   R  A   A  NN  N  SS     F      O   O  R   R  MM MM         %
%         T    RRRR   AAAAA  N N N   SSS   FFF    O   O  RRRR   M M M         %
%         T    R R    A   A  N  NN     SS  F      O   O  R R    M   M         %
%         T    R  R   A   A  N   N  SSSSS  F       OOO   R  R   M   M         %
%                                                                             %
%                                                                             %
%                   ImageMagick Image Transform Methods                       %
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

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C h o p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ChopImage creates a new image that is a subregion of an existing
%  one.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.
%
%  The format of the ChopImage method is:
%
%      Image *ChopImage(const Image *image,const RectangleInfo *chop_info)
%
%  A description of each parameter follows:
%
%    o chop_image: Method ChopImage returns a pointer to the chop
%      image.  A null image is returned if there is a memory shortage or
%      if the image width or height is zero.
%
%    o image: The address of a structure of type Image.
%
%    o chop_info: Specifies a pointer to a RectangleInfo which defines the
%      region of the image to crop.
%
%
*/
Export Image *ChopImage(const Image *image,const RectangleInfo *chop_info)
{
#define ChopImageText  "  Chopping image...  "

  Image
    *chopped_image;

  int
    y;

  RectangleInfo
    local_info;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q;

  unsigned int
    height;

  /*
    Check chop geometry.
  */
  assert(image != (Image *) NULL);
  assert(chop_info != (RectangleInfo *) NULL);
  if (((chop_info->x+(int) chop_info->width) < 0) ||
      ((chop_info->y+(int) chop_info->height) < 0) ||
      (chop_info->x >= (int) image->columns) ||
      (chop_info->y >= (int) image->rows))
    {
      MagickWarning(OptionWarning,"Unable to chop image",
        "geometry does not contain image");
      return((Image *) NULL);
    }
  local_info=(*chop_info);
  if ((local_info.x+(int) local_info.width) > (int) image->columns)
    local_info.width=(unsigned int) ((int) image->columns-local_info.x);
  if ((local_info.y+(int) local_info.height) > (int) image->rows)
    local_info.height=(unsigned int) ((int) image->rows-local_info.y);
  if (local_info.x < 0)
    {
      local_info.width-=(unsigned int) (-local_info.x);
      local_info.x=0;
    }
  if (local_info.y < 0)
    {
      local_info.height-=(unsigned int) (-local_info.y);
      local_info.y=0;
    }
  /*
    Initialize chop image attributes.
  */
  chopped_image=CloneImage(image,image->columns-local_info.width,
    image->rows-local_info.height,False);
  if (chopped_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to chop image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Extract chop image.
  */
  p=image->pixels;
  runlength=p->length+1;
  q=chopped_image->pixels;
  for (y=0; y < local_info.y; y++)
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      if ((x < local_info.x) || (x >= (int) (local_info.x+local_info.width)))
        {
          *q=(*p);
          q->length=0;
          q++;
        }
    }
  /*
    Skip pixels up to the chop image.
  */
  for (x=0; x < (int) (local_info.height*image->columns); x++)
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
  /*
    Extract chop image.
  */
  height=image->rows-(local_info.y+local_info.height);
  for (y=0; y < (int) height; y++)
  {
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      if ((x < local_info.x) || (x >= (int) (local_info.x+local_info.width)))
        {
          *q=(*p);
          q->length=0;
          q++;
        }
    }
    if (QuantumTick(y,height))
      ProgressMonitor(ChopImageText,y,height);
  }
  return(chopped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     C o a l e s c e I m a g e s                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CoalesceImages merges a sequence of images.  This is useful for GIF
%  and MNG animation sequences that have page offsets and disposal methods.
%
%  The format of the CoalesceImages method is:
%
%      void CoalesceImages(Image *images)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.  It points to the first image in the group to be
%      coalesced.
%
*/
Export void CoalesceImages(Image *images)
{
  char
    geometry[MaxTextExtent];

  Image
    *cloned_image,
    *image;

  int
    x,
    y;

  unsigned int
    matte,
    sans;

  SegmentInfo
    bounding_box,
    previous_box;

  /*
    Coalesce the image sequence.
  */
  assert(images != (Image *) NULL);
  if (images->next == (Image *) NULL)
    {
      MagickWarning(OptionWarning,"Unable to coalesce images",
        "image sequence required");
      return;
    }
  for (image=images->next; image != (Image *) NULL; image=image->next)
  {
    assert(image->previous != (Image *) NULL);
    x=0;
    y=0;
    if (image->previous->page != (char *) NULL)
      (void) ParseGeometry(image->previous->page,&x,&y,&sans,&sans);
    previous_box.x1=x;
    previous_box.y1=y;
    previous_box.x2=image->previous->columns+x;
    previous_box.y2=image->previous->rows+y;
    x=0;
    y=0;
    if (image->page != (char *) NULL)
      (void) ParseGeometry(image->page,&x,&y,&sans,&sans);
    if (!image->matte && (x <= previous_box.x1) && (y <= previous_box.y1) &&
        ((image->columns+x) >= previous_box.x2) &&
        ((image->rows+y) >= previous_box.y2))
      continue; /* image completely obscures previous image */
    bounding_box.x1=x < previous_box.x1 ? x : previous_box.x1;
    bounding_box.y1=y < previous_box.y1 ? y : previous_box.y1;
    bounding_box.x2=(image->columns+x) > previous_box.x2 ?
      (image->columns+x) : previous_box.x2;
    bounding_box.y2=(image->rows+y) > (previous_box.y2) ?
      (image->rows+y) : previous_box.y2;
    assert(!image->orphan);
    image->orphan=True;
    cloned_image=CloneImage(image,image->columns,image->rows,True);
    image->orphan=False;
    if (cloned_image == (Image *) NULL)
      {
        MagickWarning(ResourceLimitWarning,"Unable to coalesce images",
          "Memory allocation failed for cloned image");
        return;
      }
    image->columns=(unsigned int) (bounding_box.x2-bounding_box.x1+0.5);
    image->rows=(unsigned int) (bounding_box.y2-bounding_box.y1+0.5);
    image->packets=image->columns*image->rows;
    image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
      image->pixels,image->packets*sizeof(RunlengthPacket));
    if (image->pixels == (RunlengthPacket *) NULL)
      {
        MagickWarning(ResourceLimitWarning,"Unable to coalesce images",
          "Memory reallocation failed");
        return;
      }
    image->matte |=
      ((((bounding_box.x1 != x) ||
         (bounding_box.y1 != y)) &&
        ((bounding_box.x1 != previous_box.x1) ||
         (bounding_box.y1 != previous_box.y1))) ||
       (((bounding_box.x2 != (image->columns+x)) ||
         (bounding_box.y2 != (image->rows+y))) &&
        ((bounding_box.x2 != previous_box.x2) ||
         (bounding_box.y2 != previous_box.y2))) ||
       (((bounding_box.x1 != x) ||
         (bounding_box.y2 != (image->rows+y))) &&
        ((bounding_box.x1 != previous_box.x1) ||
         (bounding_box.y2 != previous_box.y2))) ||
       (((bounding_box.x2 != (image->columns+x)) ||
         (bounding_box.y1 != y)) &&
        ((bounding_box.x2 != previous_box.x2) ||
         (bounding_box.y1 != previous_box.y1))) ||
       (previous_box.x2-previous_box.x1+image->columns <
        bounding_box.x2-bounding_box.x1) ||
       (previous_box.y2-previous_box.y1+image->rows <
        bounding_box.y2-bounding_box.y1));
    matte=image->matte;
    SetImage(image);
    CompositeImage(image,ReplaceCompositeOp,image->previous,
      (int) (previous_box.x1-bounding_box.x1+0.5),
      (int) (previous_box.y1-bounding_box.y1+0.5));
    CompositeImage(image,
      cloned_image->matte ? OverCompositeOp : ReplaceCompositeOp,
      cloned_image,(int) (x-bounding_box.x1+0.5),(int) (y-bounding_box.y1+0.5));
    cloned_image->orphan=True;
    DestroyImage(cloned_image);
    FormatString(geometry,"%ux%u%+d%+d",image->columns,image->rows,
      (int) bounding_box.x1,(int) bounding_box.y1);
    if (image->page != (char *) NULL)
      FreeMemory(image->page);
    image->page=PostscriptGeometry(geometry);
    image->matte=matte;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C r o p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CropImage creates a new image that is a subregion of an existing
%  one.  It allocates the memory necessary for the new Image structure and
%  returns a pointer to the new image.  This method is optimized to preserve
%  the runlength encoding.  That is, the cropped image will always use less
%  memory than the original.
%
%  The format of the CropImage method is:
%
%      Image *CropImage(const Image *image,const RectangleInfo *crop_info)
%
%  A description of each parameter follows:
%
%    o cropped_image: Method CropImage returns a pointer to the cropped
%      image.  A null image is returned if there is a memory shortage or
%      if the image width or height is zero.
%
%    o image: The address of a structure of type Image.
%
%    o crop_info: Specifies a pointer to a RectangleInfo which defines the
%      region of the image to crop.
%
%
*/
Export Image *CropImage(const Image *image,const RectangleInfo *crop_info)
{
#define CropImageText  "  Cropping image...  "

  Image
    *cropped_image;

  int
    y;

  RectangleInfo
    local_info;

  register int
    runlength,
    x;

  register long
    max_packets;

  register RunlengthPacket
    *p,
    *q;

  /*
    Check crop geometry.
  */
  assert(image != (Image *) NULL);
  assert(crop_info != (const RectangleInfo *) NULL);
  if (((crop_info->x+(int) crop_info->width) < 0) ||
      ((crop_info->y+(int) crop_info->height) < 0) ||
      (crop_info->x >= (int) image->columns) ||
      (crop_info->y >= (int) image->rows))
    {
      MagickWarning(OptionWarning,"Unable to crop image",
        "geometry does not contain any part of the image");
      return((Image *) NULL);
    }
  local_info=(*crop_info);
  if ((local_info.x+(int) local_info.width) > (int) image->columns)
    local_info.width=(unsigned int) ((int) image->columns-local_info.x);
  if ((local_info.y+(int) local_info.height) > (int) image->rows)
    local_info.height=(unsigned int) ((int) image->rows-local_info.y);
  if (local_info.x < 0)
    {
      local_info.width-=(unsigned int) (-local_info.x);
      local_info.x=0;
    }
  if (local_info.y < 0)
    {
      local_info.height-=(unsigned int) (-local_info.y);
      local_info.y=0;
    }
  if ((local_info.width == 0) && (local_info.height == 0))
    {
      int
        x_border,
        y_border;

      register int
        i;

      RunlengthPacket
        corners[4];

      /*
        Set bounding box to the image dimensions.
      */
      x_border=local_info.x;
      y_border=local_info.y;
      local_info.width=0;
      local_info.height=0;
      local_info.x=image->columns;
      local_info.y=image->rows;
      p=image->pixels;
      runlength=p->length+1;
      corners[0]=(*p);
      for (i=1; i <= (int) (image->rows*image->columns); i++)
      {
        if (runlength != 0)
          runlength--;
        else
          {
            p++;
            runlength=p->length;
          }
        if (i == (int) image->columns)
          corners[1]=(*p);
        if (i == (int) (image->rows*image->columns-image->columns+1))
          corners[2]=(*p);
        if (i == (int) (image->rows*image->columns))
          corners[3]=(*p);
      }
      p=image->pixels;
      runlength=p->length+1;
      for (y=0; y < (int) image->rows; y++)
      {
        for (x=0; x < (int) image->columns; x++)
        {
          if (runlength != 0)
            runlength--;
          else
            {
              p++;
              runlength=p->length;
            }
          if (!ColorMatch(*p,corners[0],image->fuzz))
            if (x < local_info.x)
              local_info.x=x;
          if (!ColorMatch(*p,corners[1],image->fuzz))
            if (x > (int) local_info.width)
              local_info.width=x;
          if (!ColorMatch(*p,corners[0],image->fuzz))
            if (y < local_info.y)
              local_info.y=y;
          if (!ColorMatch(*p,corners[2],image->fuzz))
            if (y > (int) local_info.height)
              local_info.height=y;
        }
      }
      if ((local_info.width != 0) || (local_info.height != 0))
        {
          local_info.width-=local_info.x-1;
          local_info.height-=local_info.y-1;
        }
      local_info.width+=x_border*2;
      local_info.height+=y_border*2;
      local_info.x-=x_border;
      if (local_info.x < 0)
        local_info.x=0;
      local_info.y-=y_border;
      if (local_info.y < 0)
        local_info.y=0;
      if ((((int) local_info.width+local_info.x) > (int) image->columns) ||
          (((int) local_info.height+local_info.y) > (int) image->rows))
        {
          MagickWarning(OptionWarning,"Unable to crop image",
            "geometry does not contain image");
          return((Image *) NULL);
        }
    }
  if ((local_info.width == 0) || (local_info.height == 0))
    {
      MagickWarning(OptionWarning,"Unable to crop image",
        "geometry dimensions are zero");
      return((Image *) NULL);
    }
  if ((local_info.width == image->columns) &&
      (local_info.height == image->rows) && (local_info.x == 0) &&
      (local_info.y == 0))
    return((Image *) NULL);
  /*
    Initialize cropped image attributes.
  */
  cropped_image=CloneImage(image,local_info.width,local_info.height,True);
  if (cropped_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to crop image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Skip pixels up to the cropped image.
  */
  p=image->pixels;
  runlength=p->length+1;
  for (x=0; x < (int) (local_info.y*image->columns+local_info.x); x++)
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
  /*
    Extract cropped image.
  */
  max_packets=0;
  q=cropped_image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; y < (int) (cropped_image->rows-1); y++)
  {
    /*
      Transfer scanline.
    */
    for (x=0; x < (int) cropped_image->columns; x++)
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
          if (max_packets != 0)
            q++;
          max_packets++;
          *q=(*p);
          q->length=0;
        }
    }
    /*
      Skip to next scanline.
    */
    for (x=0; x < (int) (image->columns-cropped_image->columns); x++)
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
    if (QuantumTick(y,cropped_image->rows))
      ProgressMonitor(CropImageText,y,cropped_image->rows-1);
  }
  /*
    Transfer last scanline.
  */
  for (x=0; x < (int) cropped_image->columns; x++)
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
        if (max_packets != 0)
          q++;
        max_packets++;
        *q=(*p);
        q->length=0;
      }
  }
  cropped_image->packets=max_packets;
  cropped_image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
    cropped_image->pixels,cropped_image->packets*sizeof(RunlengthPacket));
  return(cropped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     D e c o n s t r u c t I m a g e s                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DeconstructImages breaks down an image sequence into constituent
%  parts.  This is useful for creating GIF or MNG animation sequences.
%
%  The format of the DeconstructImages method is:
%
%      void DeconstructImages(Image *images)
%
%  A description of each parameter follows:
%
%    o images: The address of a structure of type Image;  returned from
%      ReadImage.  It points to the first image in the group to be
%      deconstructed.
%
*/
Export void DeconstructImages(Image *images)
{
  char
    geometry[MaxTextExtent];

  Image
    *deconstructed_image,
    *image;

  int
    x,
    y;

  RectangleInfo
    *bounding_box;

  register int
    i;

  register RunlengthPacket
    *p,
    *q;

  assert(images != (Image *) NULL);
  if (images->next == (Image *) NULL)
    {
      MagickWarning(OptionWarning,"Unable to disntegrate images",
        "image sequence required");
      return;
    }
  /*
    Ensure the images are the same size.
  */
  for (image=images; image != (Image *) NULL; image=image->next)
  {
    if ((image->columns != images->columns) || (image->rows != images->rows))
      {
        MagickWarning(OptionWarning,"Unable to deconstruct images",
          "images are not the same size");
        return;
      }
  }
  if (!UncondenseImage(images))
    return;
  /*
    Allocate memory.
  */
  bounding_box=(RectangleInfo *)
    AllocateMemory(GetNumberScenes(images)*sizeof(RectangleInfo));
  if (bounding_box == (RectangleInfo *) NULL)
    {
      MagickWarning(OptionWarning,"Unable to disintegrate images",
        "Memory allocation failed");
      return;
    }
  /*
    Compute the bounding box for each image in the sequence.
  */
  i=0;
  for (image=images->next; image != (Image *) NULL; image=image->next)
  {
    assert(image->previous != (Image *) NULL);
    if (!UncondenseImage(image))
      {
        FreeMemory((char *) bounding_box);
        return;
      }
    /*
      Set bounding box to the image dimensions.
    */
    for (x=0; x < (int) image->columns; x++)
    {
      p=image->pixels+x;
      q=image->previous->pixels+x;
      for (y=0; y < (int) image->rows; y++)
      {
        if (!ColorMatch(*p,*q,image->fuzz))
          break;
        p+=image->columns;
        q+=image->columns;
      }
      if (y < (int) image->rows)
        break;
    }
    bounding_box[i].x=x;
    for (y=0; y < (int) image->rows; y++)
    {
      p=image->pixels+y*image->columns;
      q=image->previous->pixels+y*image->previous->columns;
      for (x=0; x < (int) image->columns; x++)
      {
        if (!ColorMatch(*p,*q,image->fuzz))
          break;
        p++;
        q++;
      }
      if (x < (int) image->columns)
        break;
    }
    bounding_box[i].y=y;
    for (x=image->columns-1; x >= 0; x--)
    {
      p=image->pixels+x;
      q=image->previous->pixels+x;
      for (y=0; y < (int) image->rows; y++)
      {
        if (!ColorMatch(*p,*q,image->fuzz))
          break;
        p+=image->columns;
        q+=image->columns;
      }
      if (y < (int) image->rows)
        break;
    }
    bounding_box[i].width=x-bounding_box[i].x+1;
    for (y=image->rows-1; y >= 0; y--)
    {
      p=image->pixels+y*image->columns;
      q=image->previous->pixels+y*image->previous->columns;
      for (x=0; x < (int) image->columns; x++)
      {
        if (!ColorMatch(*p,*q,image->fuzz))
          break;
        p++;
        q++;
      }
      if (x < (int) image->columns)
        break;
    }
    bounding_box[i].height=y-bounding_box[i].y+1;
    i++;
  }
  /*
    Deconstruct the image sequence.
  */
  i=0;
  if (images->page != (char *) NULL)
    FreeMemory(images->page);
  images->page=(char *) NULL;
  for (image=images->next; image != (Image *) NULL; image=image->next)
  {
    image->orphan=True;
    deconstructed_image=CropImage(image,&bounding_box[i]);
    image->orphan=False;
    if (deconstructed_image == (Image *) NULL)
      break;
    FreeMemory(image->pixels);
    image->columns=deconstructed_image->columns;
    image->rows=deconstructed_image->rows;
    image->packets=deconstructed_image->packets;
    image->pixels=deconstructed_image->pixels;
    deconstructed_image->pixels=(RunlengthPacket *) NULL;
    DestroyImage(deconstructed_image);
    FormatString(geometry,"%ux%u%+d%+d",image->columns,image->rows,
      bounding_box[i].x,bounding_box[i].y);
    if (image->page != (char *) NULL)
      FreeMemory(image->page);
    image->page=PostscriptGeometry(geometry);
    i++;
  }
  FreeMemory((char *) bounding_box);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   F l i p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method FlipImage creates a new image that reflects each scanline in the
%  vertical direction It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the FlipImage method is:
%
%      Image *FlipImage(const Image *image)
%
%  A description of each parameter follows:
%
%    o flipped_image: Method FlipImage returns a pointer to the image
%      after reflecting.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export Image *FlipImage(const Image *image)
{
#define FlipImageText  "  Flipping image...  "

  Image
    *flipped_image;

  int
    y;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *s;

  RunlengthPacket
    *scanline;

  /*
    Initialize flipped image attributes.
  */
  assert(image != (Image *) NULL);
  flipped_image=CloneImage(image,image->columns,image->rows,False);
  if (flipped_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to flip image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate scan line buffer and column offset buffers.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reflect image",
        "Memory allocation failed");
      DestroyImage(flipped_image);
      return((Image *) NULL);
    }
  /*
    Flip each row.
  */
  p=image->pixels;
  runlength=p->length+1;
  q=flipped_image->pixels+flipped_image->packets-1;
  for (y=0; y < (int) flipped_image->rows; y++)
  {
    /*
      Read a scan line.
    */
    s=scanline;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Flip each column.
    */
    s=scanline+image->columns;
    for (x=0; x < (int) flipped_image->columns; x++)
    {
      s--;
      *q=(*s);
      q->length=0;
      q--;
    }
    if (QuantumTick(y,flipped_image->rows))
      ProgressMonitor(FlipImageText,y,flipped_image->rows);
  }
  FreeMemory((char *) scanline);
  return(flipped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   F l o p I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method FlopImage creates a new image that reflects each scanline in the
%  horizontal direction It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  The format of the FlopImage method is:
%
%      Image *FlopImage(const Image *image)
%
%  A description of each parameter follows:
%
%    o flopped_image: Method FlopImage returns a pointer to the image
%      after reflecting.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export Image *FlopImage(const Image *image)
{
#define FlopImageText  "  Flopping image...  "

  Image
    *flopped_image;

  int
    y;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *s;

  RunlengthPacket
    *scanline;

  /*
    Initialize flopped image attributes.
  */
  assert(image != (Image *) NULL);
  flopped_image=CloneImage(image,image->columns,image->rows,False);
  if (flopped_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reflect image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate scan line buffer and column offset buffers.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(image->columns*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reflect image",
        "Memory allocation failed");
      DestroyImage(flopped_image);
      return((Image *) NULL);
    }
  /*
    Flop each row.
  */
  p=image->pixels;
  runlength=p->length+1;
  q=flopped_image->pixels;
  for (y=0; y < (int) flopped_image->rows; y++)
  {
    /*
      Read a scan line.
    */
    s=scanline;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Flop each column.
    */
    s=scanline+image->columns;
    for (x=0; x < (int) flopped_image->columns; x++)
    {
      s--;
      *q=(*s);
      q->length=0;
      q++;
    }
    if (QuantumTick(y,flopped_image->rows))
      ProgressMonitor(FlopImageText,y,flopped_image->rows);
  }
  FreeMemory((char *) scanline);
  return(flopped_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R o l l I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method RollImage rolls an image vertically and horizontally.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the RollImage method is:
%
%      Image *RollImage(const Image *image,const int x_offset,
%        const int y_offset)
%
%  A description of each parameter follows:
%
%    o rolled_image: Method RollImage returns a pointer to the image after
%      rolling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o x_offset: An integer that specifies the number of columns to roll
%      in the horizontal direction.
%
%    o y_offset: An integer that specifies the number of rows to roll in the
%      vertical direction.
%
%
*/
Export Image *RollImage(const Image *image,const int x_offset,
  const int y_offset)
{
#define RollImageText  "  Rolling image...  "

  Image
    *rolled_image;

  int
    y;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q;

  PointInfo
    offset;

  /*
    Initialize rolled image attributes.
  */
  assert(image != (Image *) NULL);
  rolled_image=CloneImage(image,image->columns,image->rows,False);
  if (rolled_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to roll image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Roll image.
  */
  offset.x=x_offset % image->columns;
  offset.y=y_offset % image->rows;
  if (offset.x < 0)
    offset.x+=image->columns;
  if (offset.y < 0)
    offset.y+=image->rows;
  p=image->pixels;
  runlength=p->length+1;
  for (y=0; y < (int) image->rows; y++)
  {
    /*
      Transfer scanline.
    */
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      q=rolled_image->pixels+(((int) offset.y+y) % image->rows)*image->columns+
        (((int) offset.x+x) % image->columns);
      *q=(*p);
      q->length=0;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(RollImageText,y,image->rows);
  }
  return(rolled_image);
}
