/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%           AAA   N   N  N   N   OOO   TTTTT   AAA   TTTTT  EEEEE             %
%          A   A  NN  N  NN  N  O   O    T    A   A    T    E                 %
%          AAAAA  N N N  N N N  O   O    T    AAAAA    T    EEE               %
%          A   A  N  NN  N  NN  O   O    T    A   A    T    E                 %
%          A   A  N   N  N   N   OOO     T    A   A    T    EEEEE             %
%                                                                             %
%                                                                             %
%                  ImageMagick Image Annotation Methods                       %
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
  Constant declarations.
*/
const char
  *Alphabet = "`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?" \
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   A n n o t a t e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method AnnotateImage annotates an image with text.  Optionally the
%  annotation can include the image filename, type, width, height, or scene
%  number by embedding special format characters.
%
%  The format of the AnnotateImage method is:
%
%      void AnnotateImage(Image *image,const AnnotateInfo *annotate_info)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o annotate_info: The address of a AnnotateInfo structure.
%
%
*/
Export void AnnotateImage(Image *image,const AnnotateInfo *annotate_info)
{
  AnnotateInfo
    *local_info;

  char
    label[MaxTextExtent],
    size[MaxTextExtent],
    *text,
    **textlist;

  Image
    *box_image,
    *annotate_image;

  int
    x,
    y;

  register int
    i,
    j;

  unsigned int
    height,
    length,
    number_lines,
    width;

  /*
    Ensure the annotation info is valid.
  */
  assert(image != (Image *) NULL);
  assert(annotate_info != (AnnotateInfo *) NULL);
  if (annotate_info->text == (char *) NULL)
    return;
  if (*annotate_info->text == '\0')
    return;
  if (!UncondenseImage(image))
    return;
  /*
    Translate any embedded format characters (e.g. %f).
  */
  local_info=CloneAnnotateInfo(annotate_info->image_info,annotate_info);
  text=TranslateText((ImageInfo *) NULL,image,local_info->text);
  if (text == (char *) NULL)
    {
      DestroyAnnotateInfo(local_info);
      return;
    }
  textlist=StringToList(text);
  if (textlist == (char **) NULL)
    {
      DestroyAnnotateInfo(local_info);
      return;
    }
  length=Extent(textlist[0]);
  for (i=1; textlist[i] != (char *) NULL; i++)
    if (Extent(textlist[i]) > (int) length)
      length=Extent(textlist[i]);
  number_lines=i;
  text=(char *) AllocateMemory(length+MaxTextExtent);
  if (text == (char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to annotate image",
        "Memory allocation failed");
      DestroyAnnotateInfo(local_info);
      return;
    }
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  if (local_info->geometry != (char *) NULL)
    {
      int
        flags;

      /*
        User specified annotation geometry.
      */
      flags=ParseGeometry(local_info->geometry,&x,&y,&width,&height);
      if ((flags & XNegative) != 0)
        x+=image->columns;
      if ((flags & WidthValue) == 0)
        width-=x;
      if ((flags & YNegative) != 0)
        y+=image->rows;
      if ((flags & HeightValue) == 0)
        height-=y;
    }
  /*
    Annotate image.
  */
  for (i=0; textlist[i] != (char *) NULL; i++)
  {
    if (*textlist[i] == '\0')
      {
        FreeMemory(textlist[i]);
        continue;
      }
    /*
      Convert text to image.
    */
    FormatString(label,"%.1024s",textlist[i]);
    FreeMemory(textlist[i]);
    for (j=strlen(label)-1; ; j--)
    {
      (void) strcpy(local_info->image_info->filename,label);
      annotate_image=ReadLABELImage(local_info->image_info);
      if (annotate_image == (Image *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to annotate image",
            (char *) NULL);
          for ( ; textlist[i] != (char *) NULL; i++)
            FreeMemory(textlist[i]);
          FreeMemory((char *) textlist);
          break;
        }
      if ((annotate_image->columns <= width) || (strlen(label) < 4))
        break;
      DestroyImage(annotate_image);
      (void) strcpy(label+j,"...");
    }
    /*
      Composite text onto the image.
    */
    switch (local_info->gravity)
    {
      case NorthWestGravity:
      {
        local_info->bounds.x=x;
        local_info->bounds.y=i*local_info->bounds.height+y;
        break;
      }
      case NorthGravity:
      {
        local_info->bounds.x=(width >> 1)-(annotate_image->columns >> 1)+x;
        local_info->bounds.y=i*local_info->bounds.height+y;
        break;
      }
      case NorthEastGravity:
      {
        local_info->bounds.x=width-annotate_image->columns+x;
        local_info->bounds.y=i*local_info->bounds.height+y;
        break;
      }
      case WestGravity:
      {
        local_info->bounds.x=x;
        local_info->bounds.y=(height >> 1)-
          (number_lines*local_info->bounds.height >> 1)+
          i*local_info->bounds.height+y;
        break;
      }
      case ForgetGravity:
      case StaticGravity:
      case CenterGravity:
      default:
      {
        local_info->bounds.x=(width >> 1)-(annotate_image->columns >> 1)+x;
        local_info->bounds.y=(height >> 1)-
          (number_lines*local_info->bounds.height >> 1)+
          i*local_info->bounds.height+y;
        break;
      }
      case EastGravity:
      {
        local_info->bounds.x=width-annotate_image->columns-x;
        local_info->bounds.y=(height >> 1)-
          (number_lines*local_info->bounds.height >> 1)+
          i*local_info->bounds.height-y;
        break;
      }
      case SouthWestGravity:
      {
        local_info->bounds.x=x;
        local_info->bounds.y=height-(i+1)*local_info->bounds.height-y;
        break;
      }
      case SouthGravity:
      {
        local_info->bounds.x=(width >> 1)-(annotate_image->columns >> 1)-x;
        local_info->bounds.y=height-(i+1)*local_info->bounds.height-y;
        break;
      }
      case SouthEastGravity:
      {
        local_info->bounds.x=width-annotate_image->columns-x;
        local_info->bounds.y=height-(i+1)*local_info->bounds.height-y;
        break;
      }
    }
    if (local_info->image_info->box != (char *) NULL)
      {
        /*
          Surround text with a bounding box.
        */
        FormatString(local_info->image_info->filename,"xc:%.1024s",
          local_info->image_info->box);
        FormatString(size,"%ux%u",annotate_image->columns,annotate_image->rows);
        (void) CloneString(&local_info->image_info->size,size);
        box_image=ReadImage(local_info->image_info);
        if (box_image != (Image *) NULL)
          {
            CompositeImage(image,ReplaceCompositeOp,box_image,
              local_info->bounds.x,local_info->bounds.y);
            DestroyImage(box_image);
          }
      }
    if (annotate_info->degrees != 0.0)
      {
        Image
          *rotated_image;

        /*
          Rotate text.
        */
        rotated_image=
          RotateImage(annotate_image,annotate_info->degrees,False,False);
        if (rotated_image != (Image *) NULL)
          {
            DestroyImage(annotate_image);
            annotate_image=rotated_image;
          }
      }
    CompositeImage(image,AnnotateCompositeOp,annotate_image,
      local_info->bounds.x,local_info->bounds.y);
    DestroyImage(annotate_image);
  }
  DestroyAnnotateInfo(local_info);
  FreeMemory(text);
  for ( ; textlist[i] != (char *) NULL; i++)
    FreeMemory(textlist[i]);
  FreeMemory((char *) textlist);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C l o n e A n n o t a t e I n f o                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CloneAnnotateInfo makes a duplicate of the given annotate info, or if
%  annotate info is NULL, a new one.
%
%  The format of the CloneAnnotateInfo method is:
%
%      AnnotateInfo *CloneAnnotateInfo(const ImageInfo *image_info,
%        const AnnotateInfo *annotate_info)
%
%  A description of each parameter follows:
%
%    o cloned_info: Method CloneAnnotateInfo returns a duplicate of the given
%      annotate info, or if annotate info is NULL a new one.
%
%    o image_info: a structure of type info.
%
%    o annotate_info: a structure of type info.
%
%
*/
Export AnnotateInfo *CloneAnnotateInfo(const ImageInfo *image_info,
  const AnnotateInfo *annotate_info)
{
  AnnotateInfo
    *cloned_info;

  cloned_info=(AnnotateInfo *) AllocateMemory(sizeof(AnnotateInfo));
  if (cloned_info == (AnnotateInfo *) NULL)
    MagickError(ResourceLimitWarning,"Unable to clone annotate info",
      "Memory allocation failed");
  if (annotate_info == (AnnotateInfo *) NULL)
    {
      GetAnnotateInfo(image_info,cloned_info);
      return(cloned_info);
    }
  *cloned_info=(*annotate_info);
  if (annotate_info->image_info != (ImageInfo *) NULL)
    cloned_info->image_info=CloneImageInfo(annotate_info->image_info);
  if (annotate_info->geometry != (char *) NULL)
    cloned_info->geometry=AllocateString(annotate_info->geometry);
  if (annotate_info->text != (char *) NULL)
    cloned_info->text=AllocateString(annotate_info->text);
  if (annotate_info->primitive != (char *) NULL)
    cloned_info->primitive=AllocateString(annotate_info->primitive);
  if (annotate_info->font_name != (char *) NULL)
    cloned_info->font_name=AllocateString(annotate_info->font_name);
  if (annotate_info->tile != (Image *) NULL)
    cloned_info->tile=CloneImage(annotate_info->tile,
      annotate_info->tile->columns,annotate_info->tile->rows,True);
  return(cloned_info);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e s t r o y A n n o t a t e I n f o                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DestroyAnnotateInfo deallocates memory associated with an
%  AnnotateInfo structure.
%
%  The format of the DestroyAnnotateInfo method is:
%
%      void DestroyAnnotateInfo(AnnotateInfo *annotate_info)
%
%  A description of each parameter follows:
%
%    o annotate_info: Specifies a pointer to an AnnotateInfo structure.
%
%
*/
Export void DestroyAnnotateInfo(AnnotateInfo *annotate_info)
{
  assert(annotate_info != (AnnotateInfo *) NULL);
  DestroyImageInfo(annotate_info->image_info);
  if (annotate_info->geometry != (char *) NULL)
    FreeMemory((char *) annotate_info->geometry);
  annotate_info->geometry=(char *) NULL;
  if (annotate_info->text != (char *) NULL)
    FreeMemory((char *) annotate_info->text);
  annotate_info->text=(char *) NULL;
  if (annotate_info->primitive != (char *) NULL)
    FreeMemory((char *) annotate_info->primitive);
  annotate_info->primitive=(char *) NULL;
  if (annotate_info->font_name != (char *) NULL)
    FreeMemory((char *) annotate_info->font_name);
  annotate_info->font_name=(char *) NULL;
  if (annotate_info->tile != (Image *) NULL)
    DestroyImage(annotate_info->tile);
  annotate_info->tile=(Image *) NULL;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t A n n o t a t e I n f o                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetAnnotateInfo initializes the AnnotateInfo structure.
%
%  The format of the GetAnnotateInfo method is:
%
%      void GetAnnotateInfo(const ImageInfo *image_info,
%        AnnotateInfo *annotate_info)
%
%  A description of each parameter follows:
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o annotate_info: Specifies a pointer to a AnnotateInfo structure.
%
%
*/
Export void GetAnnotateInfo(const ImageInfo *image_info,
  AnnotateInfo *annotate_info)
{
  Image
    *annotate_image;

  assert(image_info != (ImageInfo *) NULL);
  assert(annotate_info != (AnnotateInfo *) NULL);
  annotate_info->image_info=CloneImageInfo(image_info);
  annotate_info->gravity=NorthWestGravity;
  annotate_info->geometry=(char *) NULL;
  annotate_info->text=(char *) NULL;
  annotate_info->primitive=(char *) NULL;
  annotate_info->font_name=(char *) NULL;
  annotate_info->degrees=0.0;
  annotate_info->bounds.width=annotate_info->image_info->pointsize;
  annotate_info->bounds.height=annotate_info->image_info->pointsize;
  annotate_info->bounds.x=0;
  annotate_info->bounds.y=0;
  /*
    Get tile.
  */
  if (annotate_info->image_info->pen == (char *) NULL)
    (void) strcpy(annotate_info->image_info->filename,"xc:black");
  else
    if (*annotate_info->image_info->pen == '@')
      (void) strcpy(annotate_info->image_info->filename,
        annotate_info->image_info->pen+1);
    else
      (void) FormatString(annotate_info->image_info->filename,"xc:%.1024s",
        annotate_info->image_info->pen);
  annotate_info->tile=ReadImage(annotate_info->image_info);
  if (annotate_info->image_info->font == (char *) NULL)
    return;
  /*
    Get font bounds.
  */
  FormatString(annotate_info->image_info->filename,"%.1024s",Alphabet);
  annotate_image=ReadLABELImage(annotate_info->image_info);
  if (annotate_image == (Image *) NULL)
    return;
  if (annotate_image->label != (char *) NULL)
    annotate_info->font_name=AllocateString(annotate_image->label);
  annotate_info->bounds.width=
    (annotate_image->columns+(strlen(Alphabet) >> 1))/strlen(Alphabet);
  annotate_info->bounds.height=annotate_image->rows;
  DestroyImage(annotate_image);
}
