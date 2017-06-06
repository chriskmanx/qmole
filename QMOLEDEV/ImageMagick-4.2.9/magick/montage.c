/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%              M   M   OOO   N   N  TTTTT   AAA    GGGG  EEEEE                %
%              MM MM  O   O  NN  N    T    A   A  G      E                    %
%              M M M  O   O  N N N    T    AAAAA  G  GG  EEE                  %
%              M   M  O   O  N  NN    T    A   A  G   G  E                    %
%              M   M   OOO   N   N    T    A   A   GGG   EEEEE                %
%                                                                             %
%                                                                             %
%              ImageMagick Methods to Create Image Thumbnails                 %
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
%   D e s t r o y M o n t a g e I n f o                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DestroyMontageInfo deallocates memory associated with an MontageInfo
%  structure.
%
%  The format of the DestroyMontageInfo method is:
%
%      void DestroyMontageInfo(MontageInfo *montage_info)
%
%  A description of each parameter follows:
%
%    o montage_info: Specifies a pointer to an MontageInfo structure.
%
%
*/
Export void DestroyMontageInfo(MontageInfo *montage_info)
{
  assert(montage_info != (MontageInfo *) NULL);
  if (montage_info->geometry != (char *) NULL)
    FreeMemory((char *) montage_info->geometry);
  montage_info->geometry=(char *) NULL;
  if (montage_info->tile != (char *) NULL)
    FreeMemory((char *) montage_info->tile);
  montage_info->tile=(char *) NULL;
  if (montage_info->background_color != (char *) NULL)
    FreeMemory((char *) montage_info->background_color);
  montage_info->background_color=(char *) NULL;
  if (montage_info->border_color != (char *) NULL)
    FreeMemory((char *) montage_info->border_color);
  montage_info->border_color=(char *) NULL;
  if (montage_info->matte_color != (char *) NULL)
    FreeMemory((char *) montage_info->matte_color);
  montage_info->matte_color=(char *) NULL;
  if (montage_info->title != (char *) NULL)
    FreeMemory((char *) montage_info->title);
  montage_info->title=(char *) NULL;
  if (montage_info->frame != (char *) NULL)
    FreeMemory((char *) montage_info->frame);
  montage_info->frame=(char *) NULL;
  if (montage_info->texture != (char *) NULL)
    FreeMemory((char *) montage_info->texture);
  montage_info->texture=(char *) NULL;
  if (montage_info->pen != (char *) NULL)
    FreeMemory((char *) montage_info->pen);
  montage_info->pen=(char *) NULL;
  if (montage_info->font != (char *) NULL)
    FreeMemory((char *) montage_info->font);
  montage_info->font=(char *) NULL;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t M o n t a g e I n f o                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetMontageInfo initializes the MontageInfo structure.
%
%  The format of the GetMontageInfo method is:
%
%      void GetMontageInfo(MontageInfo *montage_info)
%
%  A description of each parameter follows:
%
%    o montage_info: Specifies a pointer to a MontageInfo structure.
%
%
*/
Export void GetMontageInfo(MontageInfo *montage_info)
{
  assert(montage_info != (MontageInfo *) NULL);
  *montage_info->filename='\0';
  montage_info->geometry=AllocateString(DefaultTileGeometry);
  montage_info->tile=AllocateString("6x4");
  montage_info->background_color=AllocateString("#c0c0c0");
  montage_info->border_color=(char *) NULL;
  montage_info->matte_color=AllocateString("#bdbdbd");
  montage_info->title=(char *) NULL;
  montage_info->frame=(char *) NULL;
  montage_info->texture=(char *) NULL;
  montage_info->pen=(char *) NULL;
  montage_info->font=(char *) NULL;
  montage_info->pointsize=atoi(DefaultPointSize);
  montage_info->border_width=0;
  montage_info->gravity=CenterGravity;
  montage_info->shadow=False;
  montage_info->compose=ReplaceCompositeOp;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M o n t a g e I m a g e s                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MontageImages creates a composite image by combining several
%  separate images.
%
%  The format of the MontageImages method is:
%
%      Image *MontageImages(const Image *images,const MontageInfo *montage_info)
%
%  A description of each parameter follows:
%
%    o images: Specifies a pointer to an array of Image structures.
%
%    o montage_info: Specifies a pointer to a MontageInfo structure.
%
%
*/

static void FormatLabel(ImageInfo *image_info,char *label,
  const unsigned int width,unsigned int *font_height)
{
  Image
    *image;

  MonitorHandler
    handler;

  register char
    *p,
    *q;

  if (label == (const char *) NULL)
    return;
  if (*label == '\0')
    return;
  if (strchr(label,'\n') != (char *) NULL)
    return;
  /*
    Format label to fit within a specified width.
  */
  handler=SetMonitorHandler((MonitorHandler) NULL);
  p=label;
  for (q=p+1; *q != '\0'; q++)
  {
    (void) strcpy(image_info->filename,"label:");
    (void) strncat(image_info->filename+6,p,(int) (q-p+1));
    image=ReadImage(image_info);
    if (image == (Image *) NULL)
      break;
    if (image->columns > width)
      {
        while (!isspace((int) (*q)) && (q > p))
          q--;
        if (q == p)
          break;
        *q='\n';
        p=q+1;
      }
    if (image->rows > *font_height)
      *font_height=image->rows;
    DestroyImage(image);
  }
  (void) SetMonitorHandler(handler);
}

static int SceneCompare(const void *x,const void *y)
{
  Image
    **image_1,
    **image_2;

  image_1=(Image **) x;
  image_2=(Image **) y;
  return((int) (*image_1)->scene-(int) (*image_2)->scene);
}

Export Image *MontageImages(const Image *images,const MontageInfo *montage_info)
{
#define MontageImageText  "  Creating visual image directory...  "
#define TileImageText  "  Creating image tiles...  "

  AnnotateInfo
    annotate_info;

  char
    geometry[MaxTextExtent];

  FrameInfo
    frame_info;

  Image
    *image,
    **image_list,
    **master_list,
    *montage_image,
    *texture,
    *tiled_image;

  ImageInfo
    *local_info;

  int
    x,
    x_offset,
    y,
    y_offset;

  MonitorHandler
    handler;

  register int
    i;

  register RunlengthPacket
    *p;

  RectangleInfo
    bounding_box,
    tile_info;

  unsigned int
    border_width,
    bevel_width,
    concatenate,
    count,
    font_height,
    height,
    images_per_page,
    max_height,
    number_images,
    number_lines,
    tile,
    tiles,
    tiles_per_column,
    tiles_per_row,
    tiles_per_page,
    title_offset,
    total_tiles,
    width;

  assert(images != (Image *) NULL);
  assert(montage_info != (MontageInfo *) NULL);
  /*
    Convert image list to an image group.
  */
  image_list=ListToGroupImage(images,&number_images);
  if (image_list == (Image **) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to montage image_list",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  master_list=image_list;
  /*
    Create image tiles.
  */
  for (tile=0; tile < number_images; tile++)
  {
    handler=SetMonitorHandler((MonitorHandler) NULL);
    width=image_list[tile]->columns;
    height=image_list[tile]->rows;
    x=0;
    y=0;
    image_list[tile]->orphan=True;
    (void) ParseImageGeometry(montage_info->geometry,&x,&y,&width,&height);
    image_list[tile]->orphan=True;
    tiled_image=ZoomImage(image_list[tile],width,height);
    image_list[tile]->orphan=False;
    if (tiled_image == (Image *) NULL)
      {
        for (i=0; i < (int) tile; i++)
          DestroyImage(image_list[i]);
        (void) SetMonitorHandler(handler);
        return((Image *) NULL);
      }
    image_list[tile]=tiled_image;
    (void) SetMonitorHandler(handler);
    ProgressMonitor(TileImageText,tile,number_images);
  }
  /*
    Sort image_list by increasing tile number.
  */
  for (tile=0; tile < number_images; tile++)
    if (image_list[tile]->scene == 0)
      break;
  if (tile == number_images)
    qsort((void *) image_list,number_images,sizeof(Image *),
      (int (*)(const void *, const void *)) SceneCompare);
  /*
    Determine tiles per row and column.
  */
  tiles_per_row=1;
  tiles_per_column=1;
  while ((tiles_per_row*tiles_per_column) < number_images)
  {
    tiles_per_row++;
    tiles_per_column++;
  }
  if (montage_info->tile != (char *) NULL)
    {
      tiles_per_column=number_images;
      x=0;
      y=0;
      (void) ParseGeometry(montage_info->tile,&x,&y,&tiles_per_row,
        &tiles_per_column);
    }
  /*
    Determine tile sizes.
  */
  border_width=montage_info->border_width;
  bevel_width=0;
  if (montage_info->frame != (char *) NULL)
    {
      int
        flags;

      frame_info.width=0;
      frame_info.height=0;
      frame_info.outer_bevel=0;
      frame_info.inner_bevel=0;
      flags=ParseGeometry(montage_info->frame,&frame_info.outer_bevel,
        &frame_info.inner_bevel,&frame_info.width,&frame_info.height);
      if ((flags & HeightValue) == 0)
        frame_info.height=frame_info.width;
      if ((flags & XValue) == 0)
        frame_info.outer_bevel=(frame_info.width >> 2)+1;
      if ((flags & YValue) == 0)
        frame_info.inner_bevel=frame_info.outer_bevel;
      frame_info.x=frame_info.width;
      frame_info.y=frame_info.height;
      bevel_width=Max(frame_info.inner_bevel,frame_info.outer_bevel);
      border_width=Max(frame_info.width,frame_info.height);
    }
  tile_info.x=montage_info->border_width;
  tile_info.y=montage_info->border_width;
  tile_info.width=image_list[0]->columns;
  tile_info.height=image_list[0]->rows;
  concatenate=False;
  if (montage_info->geometry != (char *) NULL)
    {
      int
        flags;

      /*
        Initialize tile geometry.
      */
      (void) strcpy(geometry,montage_info->geometry);
      tile_info.x=0;
      tile_info.y=0;
      if (strchr(geometry,'%') == (char *) NULL)
        flags=GetGeometry(geometry,&tile_info.x,&tile_info.y,
          &tile_info.width,&tile_info.height);
      else
        flags=ParseImageGeometry(geometry,&tile_info.x,&tile_info.y,
          &tile_info.width,&tile_info.height);
      if ((tile_info.x == 0) && (tile_info.y == 0))
        concatenate=!((flags & WidthValue) || (flags & HeightValue));
      if (tile_info.x < 0)
        tile_info.x=0;
      if (tile_info.y < 0)
        tile_info.y=0;
    }
  for (tile=1; tile < number_images; tile++)
  {
    if (image_list[tile]->columns > tile_info.width)
      tile_info.width=image_list[tile]->columns;
    if (image_list[tile]->rows > tile_info.height)
      tile_info.height=image_list[tile]->rows;
  }
  /*
    Initialize annotate info.
  */
  local_info=CloneImageInfo((ImageInfo *) NULL);
  (void) CloneString(&local_info->pen,montage_info->pen);
  (void) CloneString(&local_info->font,montage_info->font);
  local_info->pointsize=montage_info->pointsize;
  (void) CloneString(&local_info->background_color,
    montage_info->background_color);
  (void) CloneString(&local_info->border_color,montage_info->border_color);
  GetAnnotateInfo(local_info,&annotate_info);
  annotate_info.gravity=NorthGravity;
  texture=(Image *) NULL;
  if (montage_info->texture != (char *) NULL)
    {
      (void) strcpy(local_info->filename,montage_info->texture);
      texture=ReadImage(local_info);
    }
  /*
    Initialize font info.
  */
  font_height=annotate_info.bounds.height;
  FormatLabel(local_info,montage_info->title,((tile_info.width+
    (border_width << 1))*Min(number_images,tiles_per_column)) >> 1,
    &font_height);
  for (tile=0; tile < number_images; tile++)
    FormatLabel(local_info,image_list[tile]->label,tile_info.width+
      (border_width << 1),&font_height);
  /*
    Determine the number of lines in an image label.
  */
  title_offset=0;
  if (montage_info->title != (char *) NULL)
    title_offset=((font_height*MultilineCensus(montage_info->title)) << 1)+
      (tile_info.y << 1);
  number_lines=0;
  for (tile=0; tile < number_images; tile++)
    if (MultilineCensus(image_list[tile]->label) > (int) number_lines)
      number_lines=MultilineCensus(image_list[tile]->label);
  /*
    Allocate image structure.
  */
  montage_image=AllocateImage(local_info);
  if (montage_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to montage image_list",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  montage_image->scene=1;
  images_per_page=(number_images-1)/(tiles_per_row*tiles_per_column)+1;
  tiles=0;
  total_tiles=number_images;
  for (i=0; i < (int) images_per_page; i++)
  {
    /*
      Determine bounding box.
    */
    tiles_per_page=Min(number_images,tiles_per_row*tiles_per_column);
    x_offset=0;
    y_offset=title_offset;
    max_height=0;
    bounding_box.width=0;
    bounding_box.height=0;
    for (tile=0; tile < tiles_per_page; tile++)
    {
      width=concatenate ? image_list[tile]->columns : tile_info.width;
      x_offset+=width+(tile_info.x+border_width)*2;
      if (x_offset > (int) bounding_box.width)
        bounding_box.width=x_offset;
      if (image_list[tile]->rows > max_height)
        max_height=image_list[tile]->rows;
      if (((tile+1) == tiles_per_page) || (((tile+1) % tiles_per_row) == 0))
        {
          x_offset=0;
          height=concatenate ? max_height : tile_info.height;
          y_offset+=height+(tile_info.y+border_width)*2+(font_height+4)*
            number_lines+(montage_info->shadow ? 4 : 0)+(concatenate ? 0 : 2);
          if (y_offset > (int) bounding_box.height)
            bounding_box.height=y_offset;
          max_height=0;
        }
    }
    /*
      Initialize Image structure.
    */
    (void) strcpy(montage_image->filename,montage_info->filename);
    montage_image->columns=bounding_box.width;
    montage_image->rows=bounding_box.height;
    montage_image->packets=montage_image->columns*montage_image->rows;
    montage_image->pixels=(RunlengthPacket *) AllocateMemory((unsigned int)
      montage_image->packets*sizeof(RunlengthPacket));
    if (montage_image->pixels == (RunlengthPacket *) NULL)
      {
        MagickWarning(ResourceLimitWarning,"Unable to montage image_list",
          "Memory allocation failed");
        DestroyImages(montage_image);
        return((Image *) NULL);
      }
    /*
      Set montage geometry.
    */
    montage_image->montage=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
    count=1;
    for (tile=0; tile < tiles_per_page; tile++)
      count+=Extent(image_list[tile]->filename)+1;
    montage_image->directory=(char *) AllocateMemory(count*sizeof(char));
    if ((montage_image->montage == (char *) NULL) ||
        (montage_image->directory == (char *) NULL))
      {
        MagickWarning(ResourceLimitWarning,"Unable to montage image_list",
          "Memory allocation failed");
        DestroyImages(montage_image);
        return((Image *) NULL);
      }
    x_offset=0;
    y_offset=title_offset;
    FormatString(montage_image->montage,"%dx%d%+d%+d",
      (int) (tile_info.width+(tile_info.x+border_width)*2),
      (int) (tile_info.height+(tile_info.y+border_width)*2+(font_height+4)*
      number_lines+(montage_info->shadow ? 4 : 0)),x_offset,y_offset);
    *montage_image->directory='\0';
    for (tile=0; tile < tiles_per_page; tile++)
    {
      (void) strcat(montage_image->directory,image_list[tile]->filename);
      (void) strcat(montage_image->directory,"\n");
    }
    /*
      Initialize montage image to background color.
    */
    SetImage(montage_image);
    handler=SetMonitorHandler((MonitorHandler) NULL);
    if (texture != (Image *) NULL)
      TextureImage(montage_image,texture);
    if (montage_info->title != (char *) NULL)
      {
        /*
          Annotate composite image with title.
        */
        FormatString(geometry,"%ux%u%+d%+d",montage_image->columns,
          font_height << 1,0,tile_info.y+4);
        (void) CloneString(&annotate_info.geometry,geometry);
        (void) CloneString(&annotate_info.text,montage_info->title);
        AnnotateImage(montage_image,&annotate_info);
      }
    (void) SetMonitorHandler(handler);
    /*
      Copy tile image_list to the composite image.
    */
    x_offset=tile_info.x;
    y_offset=title_offset+tile_info.y;
    max_height=0;
    for (tile=0; tile < tiles_per_page; tile++)
    {
      /*
        Copy this tile to the composite image.
      */
      handler=SetMonitorHandler((MonitorHandler) NULL);
      image=image_list[tile];
      width=concatenate ? image->columns : tile_info.width;
      if (image->rows > max_height)
        max_height=image->rows;
      height=concatenate ? max_height : tile_info.height;
      if (border_width != 0)
        {
          Image
            *bordered_image;

          RectangleInfo
            border_info;

          /*
            Put a border around the image.
          */
          border_info.width=border_width;
          border_info.height=border_width;
          if (montage_info->frame != (char *) NULL)
            {
              border_info.width=(width-image->columns+1) >> 1;
              border_info.height=(height-image->rows+1) >> 1;
            }
          image->orphan=True;
          bordered_image=BorderImage(image,&border_info);
          image->orphan=False;
          if (bordered_image != (Image *) NULL)
            {
              DestroyImage(image);
              image=bordered_image;
            }
        }
      /*
        Gravitate image as specified by the tile gravity.
      */
      switch (montage_info->gravity)
      {
        case NorthWestGravity:
        {
          x=0;
          y=0;
          break;
        }
        case NorthGravity:
        {
          x=((width+(border_width << 1))-image->columns) >> 1;
          y=0;
          break;
        }
        case NorthEastGravity:
        {
          x=(width+(border_width << 1))-image->columns;
          y=0;
          break;
        }
        case WestGravity:
        {
          x=0;
          y=((height+(border_width << 1))-image->rows) >> 1;
          break;
        }
        case ForgetGravity:
        case StaticGravity:
        case CenterGravity:
        default:
        {
          x=((width+(border_width << 1))-image->columns) >> 1;
          y=((height+(border_width << 1))-image->rows) >> 1;
          break;
        }
        case EastGravity:
        {
          x=(width+(border_width << 1))-image->columns;
          y=((height+(border_width << 1))-image->rows) >> 1;
          break;
        }
        case SouthWestGravity:
        {
          x=0;
          y=(height+(border_width << 1))-image->rows;
          break;
        }
        case SouthGravity:
        {
          x=((width+(border_width << 1))-image->columns) >> 1;
          y=(height+(border_width << 1))-image->rows;
          break;
        }
        case SouthEastGravity:
        {
          x=(width+(border_width << 1))-image->columns;
          y=(height+(border_width << 1))-image->rows;
          break;
        }
      }
      if ((montage_info->frame != (char *) NULL) && (bevel_width != 0))
        {
          FrameInfo
            tile_info;

          Image
            *framed_image;

          /*
            Put an ornamental border around this tile.
          */
          tile_info=frame_info;
          tile_info.width=width+(frame_info.width << 1);
          tile_info.height=height+(frame_info.height << 1)+(font_height+4)*
            MultilineCensus(image->label);
          image->orphan=True;
          framed_image=FrameImage(image,&tile_info);
          image->orphan=False;
          if (framed_image != (Image *) NULL)
            {
              DestroyImage(image);
              image=framed_image;
            }
          x=0;
          y=0;
        }
      if (Latin1Compare(image->magick,"NULL") != 0)
        {
          /*
            Composite background image with tile image.
          */
          if (image->matte)
            CompositeImage(montage_image,montage_info->compose,image,x_offset+x,
              y_offset+y);
          else
            CompositeImage(montage_image,ReplaceCompositeOp,image,x_offset+x,
              y_offset+y);
          montage_image->matte=False;
          if (montage_info->shadow)
            {
              register int
                columns,
                rows;

              /*
                Put a shadow under the tile to show depth.
              */
              for (rows=0; rows < ((int) image->rows-4); rows++)
              {
                p=montage_image->pixels+montage_image->columns*
                  (y_offset+y+rows+4)+x_offset+x+image->columns;
                for (columns=0; columns < Min(tile_info.x,4); columns++)
                {
                  if (p >= (montage_image->pixels+montage_image->packets))
                    continue;
                  Modulate(0.0,0.0,-25.0+4*columns,&p->red,&p->green,&p->blue);
                  p++;
                }
              }
              for (rows=0; rows < Min(tile_info.y,4); rows++)
              {
                p=montage_image->pixels+montage_image->columns*
                  (y_offset+y+image->rows+rows)+x_offset+x+4;
                for (columns=0; columns < (int) image->columns; columns++)
                {
                  if (p >= (montage_image->pixels+montage_image->packets))
                    continue;
                  Modulate(0.0,0.0,-25.0+4*rows,&p->red,&p->green,&p->blue);
                  p++;
                }
              }
            }
          if (image->label != (char *) NULL)
            {
              /*
                Annotate composite image tile with label.
              */
              FormatString(geometry,"%ux%u%+d%+d",
                (montage_info->frame ? image->columns : width)-
                (border_width << 1),font_height,(int) (x_offset+border_width),
                (int) (montage_info->frame ? y_offset+height+
                (border_width << 1)-bevel_width-2 : y_offset+tile_info.height+
                (border_width << 1)+(montage_info->shadow ? 4 : 0)+2));
              (void) CloneString(&annotate_info.geometry,geometry);
              (void) CloneString(&annotate_info.text,image->label);
              AnnotateImage(montage_image,&annotate_info);
            }
        }
      x_offset+=width+(tile_info.x+border_width)*2;
      if (((tile+1) == tiles_per_page) || (((tile+1) % tiles_per_row) == 0))
        {
          x_offset=tile_info.x;
          y_offset+=height+(tile_info.y+border_width)*2+(font_height+4)*
            number_lines+(montage_info->shadow ? 4 : 0);
          max_height=0;
        }
      DestroyImage(image);
      (void) SetMonitorHandler(handler);
      ProgressMonitor(MontageImageText,tiles,total_tiles);
      tiles++;
    }
    CondenseImage(montage_image);
    if ((i+1) < (int) images_per_page)
      {
        /*
          Allocate next image structure.
        */
        AllocateNextImage((ImageInfo *) NULL,montage_image);
        if (montage_image->next == (Image *) NULL)
          {
            DestroyImages(montage_image);
            return((Image *) NULL);
          }
        montage_image=montage_image->next;
        image_list+=tiles_per_page;
        number_images-=tiles_per_page;
      }
  }
  if (texture != (Image *) NULL)
    FreeMemory((char *) texture);
  FreeMemory((char *) master_list);
  DestroyImageInfo(local_info);
  while (montage_image->previous != (Image *) NULL)
    montage_image=montage_image->previous;
  return(montage_image);
}
