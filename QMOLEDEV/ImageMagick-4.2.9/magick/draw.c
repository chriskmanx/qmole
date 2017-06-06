/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        DDDD   RRRR    AAA   W   W                           %
%                        D   D  R   R  A   A  W   W                           %
%                        D   D  RRRR   AAAAA  W   W                           %
%                        D   D  R R    A   A  W W W                           %
%                        DDDD   R  R   A   A   W W                            %
%                                                                             %
%                                                                             %
%                     ImageMagick Image Drawing Methods                       %
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
  Define declarations.
*/
#define MatteMatch(color,target,delta) \
  (ColorMatch(color,target,delta) && ((color).index == (target).index))
#define MaxStacksize  (1 << 15)
#define Push(up,left,right,delta) \
  if ((p < (segment_stack+MaxStacksize)) && (((up)+(delta)) >= 0) && \
      (((up)+(delta)) < (int) image->rows)) \
    { \
      p->y1=(up); \
      p->x1=(left); \
      p->x2=(right); \
      p->y2=(delta); \
      p++; \
    }

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o l o r F l o o d f i l l I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ColorFloodfillImage floodfills the designated area with a color.
%  The floodfill algorithm is strongly based on a similar algorithm in
%  "Graphics Gems" by Paul Heckbert.
%
%  The format of the ColorFloodfillImage method is:
%
%      void ColorFloodfillImage(Image *image,const RunlengthPacket *target,
%        Image *tile,const int x_offset,const int y_offset,const PaintMethod method)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o target: A RunlengthPacket structure.  This is the RGB value of the target
%      color.
%
%    o tile: An image representing the image to tile onto the floodplane.
%
%    o x,y: Unsigned integers representing the current location of the pen.
%
%    o method: drawing method of type PrimitiveType: floodfill or fill to
%      border.
%
%
*/
Export void ColorFloodfillImage(Image *image,const RunlengthPacket *target,
  Image *tile,const int x_offset,const int y_offset,const PaintMethod method)
{
  ColorPacket
    color;

  int
    offset,
    skip,
    start,
    x1,
    x2;

  register int
    i,
    x,
    y;

  register RunlengthPacket
    *pixel,
    *q;

  register SegmentInfo
    *p;

  register unsigned char
    *r;

  SegmentInfo
    *segment_stack;

  unsigned char
    *markers;

  /*
    Check boundary conditions.
  */
  assert(image != (Image *) NULL);
  assert(tile != (Image *) NULL);
  if ((x_offset < 0) || (x_offset >= (int) image->columns))
    return;
  if ((y_offset < 0) || (y_offset >= (int) image->rows))
    return;
  if (!UncondenseImage(image))
    return;
  if (!UncondenseImage(tile))
    return;
  /*
    Set floodfill color.
  */
  x=x_offset;
  y=y_offset;
  color.red=tile->pixels[0].red;
  color.green=tile->pixels[0].green;
  color.blue=tile->pixels[0].blue;
  markers=(unsigned char *)
    AllocateMemory(image->rows*image->columns*sizeof(unsigned char));
  if (markers == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to floodfill image",
        "Memory allocation failed");
      return;
    }
  for (i=0; i < (int) (image->rows*image->columns); i++)
    markers[i]=False;
  if (ColorMatch(color,*target,image->fuzz))
    return;
  /*
    Allocate segment stack.
  */
  segment_stack=(SegmentInfo *)
    AllocateMemory(MaxStacksize*sizeof(SegmentInfo));
  if (segment_stack == (SegmentInfo *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to floodfill image",
        "Memory allocation failed");
      if (tile != (Image *) NULL)
        {
          FreeMemory((char *) markers);
          DestroyImage(tile);
        }
      return;
    }
  /*
    Push initial segment on stack.
  */
  image->class=DirectClass;
  start=0;
  p=segment_stack;
  Push(y,x,x,1);
  Push(y+1,x,x,-1);
  while (p > segment_stack)
  {
    /*
      Pop segment off stack.
    */
    p--;
    x1=(int) p->x1;
    x2=(int) p->x2;
    offset=(int) p->y2;
    y=(int) p->y1+offset;
    /*
      Recolor neighboring pixels.
    */
    pixel=PixelOffset(image,x1,y);
    for (x=x1; x >= 0 ; x--)
    {
      if (method == FloodfillMethod)
        {
          if (!ColorMatch(*pixel,*target,image->fuzz))
            break;
        }
      else
        if (ColorMatch(*pixel,*target,image->fuzz) ||
            ColorMatch(*pixel,color,image->fuzz))
          break;
      pixel->red=color.red;
      pixel->green=color.green;
      pixel->blue=color.blue;
      if (markers != (unsigned char *) NULL)
        markers[y*image->columns+x]=True;
      pixel--;
    }
    skip=x >= x1;
    if (!skip)
      {
        start=x+1;
        if (start < x1)
          Push(y,start,x1-1,-offset);
        x=x1+1;
      }
    do
    {
      if (!skip)
        {
          pixel=PixelOffset(image,x,y);
          for ( ; x < (int) image->columns; x++)
          {
            if (method == FloodfillMethod)
              {
                if (!ColorMatch(*pixel,*target,image->fuzz))
                  break;
              }
            else
              if (ColorMatch(*pixel,*target,image->fuzz) ||
                  ColorMatch(*pixel,color,image->fuzz))
                break;
            pixel->red=color.red;
            pixel->green=color.green;
            pixel->blue=color.blue;
            if (markers != (unsigned char *) NULL)
              markers[y*image->columns+x]=True;
            pixel++;
          }
          Push(y,start,x-1,offset);
          if (x > (x2+1))
            Push(y,x2+1,x-1,-offset);
        }
      skip=False;
      pixel=PixelOffset(image,x,y);
      for (x++; x <= x2; x++)
      {
        pixel++;
        if (method == FloodfillMethod)
          {
            if (ColorMatch(*pixel,*target,image->fuzz))
              break;
          }
        else
          if (!ColorMatch(*pixel,*target,image->fuzz) &&
              !ColorMatch(*pixel,color,image->fuzz))
            break;
      }
      start=x;
    } while (x <= x2);
  }
  /*
    Tile image onto floodplane.
  */
  r=markers;
  q=image->pixels;
  for (y=0; y < (int) image->rows; y++)
  {
    for (x=0; x < (int) image->columns; x++)
    {
      if (*r)
        {
          pixel=PixelOffset(tile,x % tile->columns,y % tile->rows);
          if (!tile->matte)
            {
              q->red=pixel->red;
              q->green=pixel->green;
              q->blue=pixel->blue;
            }
          else
            {
              q->red=(Quantum) ((unsigned long) (pixel->red*pixel->index+
                q->red*(Opaque-pixel->index))/Opaque);
              q->green=(Quantum) ((unsigned long) (pixel->green*pixel->index+
                q->green*(Opaque-pixel->index))/Opaque);
              q->blue=(Quantum) ((unsigned long) (pixel->blue*pixel->index+
                q->blue*(Opaque-pixel->index))/Opaque);
              q->index=(unsigned short) ((unsigned long) (pixel->index*
                pixel->index+q->index*(Opaque-pixel->index))/Opaque);
            }
        }
      r++;
      q++;
    }
  }
  FreeMemory((char *) markers);
  FreeMemory((char *) segment_stack);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D r a w I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DrawImage draws a primitive (line, rectangle, ellipse) on the
%  image.
%
%  The format of the DrawImage method is:
%
%      void DrawImage(Image *image,const AnnotateInfo *annotate_info)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o annotate_info: The address of a DrawInfo structure.
%
%
*/
Export void DrawImage(Image *image,const AnnotateInfo *annotate_info)
{
#define DrawImageText  "  Drawing on image...  "

  AnnotateInfo
    *local_info;

  char
    keyword[MaxTextExtent],
    *primitive;

  double
    mid;

  int
    j,
    n,
    y;

  PointInfo
    point,
    target;

  PrimitiveInfo
    *primitive_info;

  PrimitiveType
    primitive_type;

  RunlengthPacket
    pixel;

  SegmentInfo
    bounds;

  register char
    *p;

  register int
    i,
    x;

  register RunlengthPacket
    *q;

  unsigned int
    indirection,
    length,
    number_coordinates;

  unsigned short
    opacity;

  /*
    Ensure the annotation info is valid.
  */
  assert(image != (Image *) NULL);
  assert(annotate_info != (AnnotateInfo *) NULL);
  assert(annotate_info->primitive != (char *) NULL);
  assert(annotate_info->tile != (Image *) NULL);
  if (*annotate_info->primitive == '\0')
    return;
  if (!UncondenseImage(image))
    return;
  local_info=CloneAnnotateInfo(annotate_info->image_info,annotate_info);
  if (!UncondenseImage(local_info->tile))
    {
      DestroyAnnotateInfo(local_info);
      return;
    }
  primitive=local_info->primitive;
  indirection=(*primitive == '@');
  if (indirection)
    {
      FILE
        *file;

      int
        c;

      register char
        *q;

      /*
        Read text from a file.
      */
      file=(FILE *) fopen(primitive+1,"r");
      if (file == (FILE *) NULL)
        {
          MagickWarning(FileOpenWarning,"Unable to read primitive file",
            primitive+1);
          DestroyAnnotateInfo(local_info);
          return;
        }
      length=MaxTextExtent;
      primitive=(char *) AllocateMemory(length);
      q=primitive;
      while (primitive != (char *) NULL)
      {
        c=fgetc(file);
        if (c == '#')
          {
            /*
              Eat comments.
            */
            for (c=fgetc(file); c != EOF; c=fgetc(file))
              if ((c == '\r') || (c == '\n'))
                break;
            continue;
          }
        if (c == EOF)
          break;
        if ((q-primitive+1) >= (int) length)
          {
            *q='\0';
            length<<=1;
            primitive=(char *) ReallocateMemory(primitive,length);
            if (primitive == (char *) NULL)
              break;
            q=primitive+Extent(primitive);
          }
        *q++=(unsigned char) c;
      }
      (void) fclose(file);
      if (primitive == (char *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to draw image",
            "Memory allocation failed");
          DestroyAnnotateInfo(local_info);
          return;
        }
      *q='\0';
    }
  /*
    Allocate primitive info memory.
  */
  number_coordinates=2048;
  primitive_info=(PrimitiveInfo *)
    AllocateMemory(number_coordinates*sizeof(PrimitiveInfo));
  local_info->geometry=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
  local_info->text=(char *) AllocateMemory(Extent(primitive)*sizeof(char));
  if ((primitive_info == (PrimitiveInfo *) NULL) ||
      (local_info->geometry == (char *) NULL) ||
      (local_info->text == (char *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Unable to draw image",
        "Memory allocation failed");
      if (indirection)
        FreeMemory((char *) primitive);
      DestroyAnnotateInfo(local_info);
      return;
    }
  /*
    Parse the primitive attributes.
  */
  primitive_type=UndefinedPrimitive;
  p=primitive;
  bounds.x1=image->columns-1;
  bounds.y1=image->rows-1;
  bounds.x2=0;
  bounds.y2=0;
  for (i=0; *p != '\0'; )
  {
    /*
      Define primitive.
    */
    while (isspace((int) (*p)))
      p++;
    for (x=0; isalpha((int) (*p)); x++)
      keyword[x]=(*p++);
    keyword[x]='\0';
    if (*keyword == '\0')
      break;
    primitive_type=UndefinedPrimitive;
    if (Latin1Compare("Point",keyword) == 0)
      primitive_type=PointPrimitive;
    if (Latin1Compare("Line",keyword) == 0)
      primitive_type=LinePrimitive;
    if (Latin1Compare("Rectangle",keyword) == 0)
      primitive_type=RectanglePrimitive;
    if (Latin1Compare("FillRectangle",keyword) == 0)
      primitive_type=FillRectanglePrimitive;
    if (Latin1Compare("Circle",keyword) == 0)
      primitive_type=CirclePrimitive;
    if (Latin1Compare("FillCircle",keyword) == 0)
      primitive_type=FillCirclePrimitive;
    if (Latin1Compare("Ellipse",keyword) == 0)
      primitive_type=EllipsePrimitive;
    if (Latin1Compare("FillEllipse",keyword) == 0)
      primitive_type=FillEllipsePrimitive;
    if (Latin1Compare("Polygon",keyword) == 0)
      primitive_type=PolygonPrimitive;
    if (Latin1Compare("FillPolygon",keyword) == 0)
      primitive_type=FillPolygonPrimitive;
    if (Latin1Compare("Color",keyword) == 0)
      primitive_type=ColorPrimitive;
    if (Latin1Compare("Matte",keyword) == 0)
      primitive_type=MattePrimitive;
    if (Latin1Compare("Text",keyword) == 0)
      primitive_type=TextPrimitive;
    if (Latin1Compare("Image",keyword) == 0)
      primitive_type=ImagePrimitive;
    if (primitive_type == UndefinedPrimitive)
      break;
    j=i;
    for (x=0; *p != '\0'; x++)
    {
      /*
        Define points.
      */
      while (isspace((int) (*p)))
        p++;
      if (!IsGeometry(p))
        break;
      point.x=0;
      point.y=0;
      n=0;
      (void) sscanf(p,"%lf%lf%n",&point.x,&point.y,&n);
      if (n == 0)
        (void) sscanf(p,"%lf,%lf%n",&point.x,&point.y,&n);
      if (n == 0)
        (void) sscanf(p,"%lf, %lf%n",&point.x,&point.y,&n);
      if (n == 0)
        (void) sscanf(p,"%lf %lf%n",&point.x,&point.y,&n);
      if (n == 0)
        {
          MagickWarning(OptionWarning,
            "Non-conforming drawing primitive definition",p);
          break;
        }
      if (point.x < bounds.x1)
        bounds.x1=point.x;
      if (point.y < bounds.y1)
        bounds.y1=point.y;
      if (point.x > bounds.x2)
        bounds.x2=point.x;
      if (point.y > bounds.y2)
        bounds.y2=point.y;
      primitive_info[i].primitive=primitive_type;
      primitive_info[i].coordinates=0;
      primitive_info[i].x=point.x;
      primitive_info[i].y=point.y;
      primitive_info[i].method=FloodfillMethod;
      p+=n;
      while (isspace((int) (*p)) || (*p == ','))
        p++;
      i++;
      if (i < (int) (number_coordinates-360-1))
        continue;
      number_coordinates<<=1;
      primitive_info=(PrimitiveInfo *) ReallocateMemory(primitive_info,
        number_coordinates*sizeof(PrimitiveInfo));
      if (primitive_info != (PrimitiveInfo *) NULL)
        continue;
      MagickWarning(ResourceLimitWarning,"Unable to draw image",
        "Memory allocation failed");
      if (indirection)
        FreeMemory((char *) primitive);
      DestroyAnnotateInfo(local_info);
      return;
    }
    primitive_info[j].coordinates=x;
    primitive_info[j].method=FloodfillMethod;
    primitive_info[j].text=(char *) NULL;
    switch (primitive_type)
    {
      case PointPrimitive:
      default:
      {
        if (primitive_info[j].coordinates != 1)
          primitive_type=UndefinedPrimitive;
        break;
      }
      case LinePrimitive:
      case RectanglePrimitive:
      case FillRectanglePrimitive:
      {
        if (primitive_info[j].coordinates != 2)
          primitive_type=UndefinedPrimitive;
        break;
      }
      case CirclePrimitive:
      case FillCirclePrimitive:
      {
        double
          radius;

        if (primitive_info[j].coordinates != 2)
          {
            primitive_type=UndefinedPrimitive;
            break;
          }
        /*
          Determine circle bounding box.
        */
        x=(int) (primitive_info[j+1].x-primitive_info[j].x);
        y=(int) (primitive_info[j+1].y-primitive_info[j].y);
        radius=
          sqrt((double) (x*x+y*y))+local_info->image_info->linewidth/2.0+0.5;
        point.x=Max(primitive_info[j].x-radius,0);
        point.y=Max(primitive_info[j].y-radius,0);
        if (point.x < bounds.x1)
          bounds.x1=point.x;
        if (point.y < bounds.y1)
          bounds.y1=point.y;
        point.x=Min(primitive_info[j].x+radius,image->columns-1);
        point.y=Min(primitive_info[j].y+radius,image->rows-1);
        if (point.x > bounds.x2)
          bounds.x2=point.x;
        if (point.y > bounds.y2)
          bounds.y2=point.y;
        break;
      }
      case EllipsePrimitive:
      case FillEllipsePrimitive:
      {
        double
          n;

        PointInfo
          degrees,
          end,
          start;

        if (primitive_info[j].coordinates != 3)
          {
            primitive_type=UndefinedPrimitive;
            break;
          }
        /*
          Arc's are just short segmented polygons.
        */
        primitive_info[j].primitive=PolygonPrimitive;
        if (primitive_type == FillEllipsePrimitive)
          primitive_info[j].primitive=FillPolygonPrimitive;
        start.x=primitive_info[j].x;
        start.y=primitive_info[j].y;
        end.x=primitive_info[j+1].x/2;
        end.y=primitive_info[j+1].y/2;
        degrees.x=primitive_info[j+2].x;
        degrees.y=primitive_info[j+2].y;
        while (degrees.y < degrees.x)
          degrees.y+=360;
        i=j;
        if ((primitive_type == FillEllipsePrimitive) &&
            (fmod(degrees.y-degrees.x,360.0) != 0.0))
          {
            if (start.x < bounds.x1)
              bounds.x1=start.x;
            if (start.y < bounds.y1)
              bounds.y1=start.y;
            if (end.x > bounds.x2)
              bounds.x2=end.x;
            if (end.y > bounds.y2)
              bounds.y2=end.y;
            primitive_info[j].coordinates++;
            i++;
          }
        for (n=(degrees.x+1.0); n <= degrees.y; n+=1.0)
        {
          point.x=cos(DegreesToRadians(fmod(n,360.0)))*end.x+start.x;
          point.y=sin(DegreesToRadians(fmod(n,360.0)))*end.y+start.y;
          if (point.x < bounds.x1)
            bounds.x1=point.x;
          if (point.y < bounds.y1)
            bounds.y1=point.y;
          if (point.x > bounds.x2)
            bounds.x2=point.x;
          if (point.y > bounds.y2)
            bounds.y2=point.y;
          primitive_info[i].coordinates=0;
          primitive_info[i].x=point.x;
          primitive_info[i].y=point.y;
          primitive_info[j].coordinates++;
          i++;
        }
        break;
      }
      case PolygonPrimitive:
      case FillPolygonPrimitive:
      {
        if (primitive_info[j].coordinates < 3)
          primitive_type=UndefinedPrimitive;
        break;
      }
      case ColorPrimitive:
      case MattePrimitive:
      {
        if (primitive_info[j].coordinates != 1)
          {
            primitive_type=UndefinedPrimitive;
            break;
          }
        /*
          Define method.
        */
        while (isspace((int) (*p)))
          p++;
        for (x=0; isalpha((int) (*p)); x++)
          keyword[x]=(*p++);
        keyword[x]='\0';
        if (*keyword == '\0')
          break;
        if (Latin1Compare("point",keyword) == 0)
          primitive_info[j].method=PointMethod;
        else
          if (Latin1Compare("replace",keyword) == 0)
            primitive_info[j].method=ReplaceMethod;
          else
            if (Latin1Compare("floodfill",keyword) == 0)
              primitive_info[j].method=FloodfillMethod;
            else
              if (Latin1Compare("filltoborder",keyword) == 0)
                primitive_info[j].method=FillToBorderMethod;
              else
                if (Latin1Compare("reset",keyword) == 0)
                  primitive_info[j].method=ResetMethod;
                else
                  primitive_type=UndefinedPrimitive;
        while (isspace((int) (*p)))
          p++;
        break;
      }
      case TextPrimitive:
      case ImagePrimitive:
      {
        if (primitive_info[j].coordinates != 1)
          {
            primitive_type=UndefinedPrimitive;
            break;
          }
        if (*p != '\0')
          {
            primitive_info[j].text=p;
            if (*p == '"')
              {
                for (p++; *p != '\0'; p++)
                  if ((*p == '"') && (*(p-1) != '\\'))
                    break;
              }
            else
              if (*p == '\'')
                {
                  for (p++; *p != '\0'; p++)
                    if ((*p == '\'') && (*(p-1) != '\\'))
                      break;
                }
              else
                for (p++;  *p != '\0'; p++)
                  if (isspace((int) *p) && (*(p-1) != '\\'))
                    break;
            if (*p != '\0')
              p++;
          }
        break;
      }
    }
    if (primitive_type == UndefinedPrimitive)
      break;
  }
  primitive_info[i].primitive=UndefinedPrimitive;
  if (primitive_type == UndefinedPrimitive)
    {
      MagickWarning(OptionWarning,
        "Non-conforming drawing primitive definition",keyword);
      FreeMemory((char *) primitive_info);
      if (indirection)
        FreeMemory((char *) primitive);
      DestroyAnnotateInfo(local_info);
      return;
    }
  for (i=0; primitive_info[i].primitive != UndefinedPrimitive; i++)
    if ((primitive_info[i].method == ReplaceMethod) ||
        (primitive_info[i].method == ResetMethod))
      {
        /*
          Replace and reset methods affect the entire image.
        */
        bounds.x1=0;
        bounds.y1=0;
        bounds.x2=image->columns-1;
        bounds.y2=image->rows-1;
      }
  /*
    Account for linewidth.
  */
  mid=local_info->image_info->linewidth/2.0;
  if ((bounds.x1 != bounds.x2) || (bounds.y1 != bounds.y2))
    {
      bounds.x1=Max(bounds.x1-mid,0);
      bounds.y1=Max(bounds.y1-mid,0);
      bounds.x2=Min(bounds.x2+ceil(mid),image->columns-1);
      bounds.y2=Min(bounds.y2+ceil(mid),image->rows-1);
    }
  /*
    Draw the primitive on the image.
  */
  image->class=DirectClass;
  for (y=(int) bounds.y1; y <= (int) bounds.y2; y++)
  {
    target.y=y;
    q=PixelOffset(image,bounds.x1,y);
    for (x=(int) bounds.x1; x <= (int) bounds.x2; x++)
    {
      target.x=x;
      opacity=InsidePrimitive(primitive_info,local_info,&target,image);
      if (!local_info->image_info->antialias)
        if (opacity != Transparent)
          opacity=Opaque;
      if (opacity != Transparent)
        {
          pixel=(*PixelOffset(local_info->tile,x % local_info->tile->columns,
            y % local_info->tile->rows));
          q->red=(Quantum) ((unsigned long)
            (pixel.red*opacity+q->red*(Opaque-opacity))/Opaque);
          q->green=(Quantum) ((unsigned long)
            (pixel.green*opacity+q->green*(Opaque-opacity))/Opaque);
          q->blue=(Quantum) ((unsigned long)
            (pixel.blue*opacity+q->blue*(Opaque-opacity))/Opaque);
          if (local_info->tile->matte)
            {
              q->index=(unsigned short) ((unsigned long)
                (pixel.index*opacity+q->index*(Opaque-opacity))/Opaque);
              q->red=(Quantum) ((unsigned long)
                (pixel.red*pixel.index+q->red*(Opaque-pixel.index))/Opaque);
              q->green=(Quantum) ((unsigned long)
                (pixel.green*pixel.index+q->green*(Opaque-pixel.index))/Opaque);
              q->blue=(Quantum) ((unsigned long)
                (pixel.blue*pixel.index+q->blue*(Opaque-pixel.index))/Opaque);
              q->index=(unsigned short) ((unsigned long) (pixel.index*
                pixel.index+q->index*(Opaque-pixel.index))/Opaque);
            }
        }
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(DrawImageText,y,image->rows);
  }
  /*
    Free resources.
  */
  FreeMemory((char *) primitive_info);
  if (indirection)
    FreeMemory((char *) primitive);
  DestroyAnnotateInfo(local_info);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   I n s i d e P r i m i t i v e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method InsidePrimitive returns the opacity of the pen at the (x,y) position
%  of the image.  The opacity is Opaque if the (x,y) position is within the
%  primitive as defined in primitive_info.  A value less than fully opaque
%  and greater than fully transparent is returned for a primitive edge pixel
%  to allow for anti-aliasing.  Otherwise fully transparent is returned.
%
%  Rick Mabry provided the algorithms for anti-aliased primitives.
%
%  The format of the InsidePrimitive method is:
%
%      unsigned short InsidePrimitive(PrimitiveInfo *primitive_info,
%        const AnnotateInfo *annotate_info,const PointInfo *pixel,Image *image)
%
%  A description of each parameter follows:
%
%    o opacity:  Method InsidePrimitive returns a pen opacity associated with
%      the (x,y) position of the image.
%
%    o primitive_info: Specifies a pointer to a PrimitiveInfo structure.
%
%    o annotate_info: Specifies a pointer to a AnnotateInfo structure.
%
%    o target: PointInfo representing the (x,y) location in the image.
%
%    o image: The address of a structure of type Image.
%
%
*/

static double DistanceToLine(const PointInfo *pixel,const SegmentInfo *line)
{
  register double
    alpha,
    beta,
    dot_product,
    gamma,
    v;

  alpha=pixel->x-line->x1;
  beta=pixel->y-line->y1;
  dot_product=alpha*(line->x2-line->x1)+beta*(line->y2-line->y1);
  if (dot_product <= 0)
    return(alpha*alpha+beta*beta);
  v=(line->x2-line->x1)*(line->x2-line->x1)+
    (line->y2-line->y1)*(line->y2-line->y1);
  gamma=dot_product*dot_product/v;
  if (gamma <= v)
    return(alpha*alpha+beta*beta-gamma);
  alpha=pixel->x-line->x2;
  beta=pixel->y-line->y2;
  return(alpha*alpha+beta*beta);
}

static unsigned short PixelOnLine(const PointInfo *pixel,
  const SegmentInfo *line,const double mid,const unsigned short opacity)
{
  register double
    alpha,
    distance;

  if ((mid == 0) || (opacity == Opaque))
    return(opacity);
  if ((line->x1 == line->x2) && (line->y1 == line->y2))
    return((pixel->x == line->x1) && (pixel->y == line->y1) ? Opaque : opacity);
  distance=DistanceToLine(pixel,line);
  alpha=mid-0.5;
  if (distance <= (alpha*alpha))
    return(Opaque);
  alpha=mid+0.5;
  if (distance <= (alpha*alpha))
    {
      alpha=sqrt(distance)-mid-0.5;
      return((unsigned short) Max((int) opacity,Opaque*alpha*alpha));
    }
  return(opacity);
}

Export unsigned short InsidePrimitive(PrimitiveInfo *primitive_info,
  const AnnotateInfo *annotate_info,const PointInfo *pixel,Image *image)
{
  ColorPacket
    border_color;

  double
    alpha,
    beta,
    distance,
    mid,
    radius;

  register int
    i;

  register PrimitiveInfo
    *p,
    *q;

  register unsigned short
    opacity;

  RunlengthPacket
    target;

  SegmentInfo
    line;

  assert(primitive_info != (PrimitiveInfo *) NULL);
  assert(annotate_info != (AnnotateInfo *) NULL);
  assert(image != (Image *) NULL);
  opacity=Transparent;
  mid=annotate_info->image_info->linewidth/2.0;
  p=primitive_info;
  while (p->primitive != UndefinedPrimitive)
  {
    q=p+p->coordinates-1;
    switch (p->primitive)
    {
      case PointPrimitive:
      default:
      {
        if ((pixel->x == (int) (p->x+0.5)) && (pixel->y == (int) (p->y+0.5)))
          opacity=Opaque;
        break;
      }
      case LinePrimitive:
      {
        line.x1=p->x;
        line.y1=p->y;
        line.x2=q->x;
        line.y2=q->y;
        opacity=PixelOnLine(pixel,&line,mid,opacity);
        break;
      }
      case RectanglePrimitive:
      {
        if (((pixel->x >= (int) (Min(p->x-mid,q->x+mid)+0.5)) &&
             (pixel->x < (int) (Max(p->x-mid,q->x+mid)+0.5)) &&
             (pixel->y >= (int) (Min(p->y-mid,q->y+mid)+0.5)) &&
             (pixel->y < (int) (Max(p->y-mid,q->y+mid)+0.5))) &&
           !((pixel->x >= (int) (Min(p->x+mid,q->x-mid)+0.5)) &&
             (pixel->x < (int) (Max(p->x+mid,q->x-mid)+0.5)) &&
             (pixel->y >= (int) (Min(p->y+mid,q->y-mid)+0.5)) &&
             (pixel->y < (int) (Max(p->y+mid,q->y-mid)+0.5))))
          opacity=Opaque;
        break;
      }
      case FillRectanglePrimitive:
      {
        if ((pixel->x >= (int) (Min(p->x,q->x)+0.5)) &&
            (pixel->x <= (int) (Max(p->x,q->x)+0.5)) &&
            (pixel->y >= (int) (Min(p->y,q->y)+0.5)) &&
            (pixel->y <= (int) (Max(p->y,q->y)+0.5)))
          opacity=Opaque;
        break;
      }
      case CirclePrimitive:
      {
        alpha=p->x-pixel->x;
        beta=p->y-pixel->y;
        distance=sqrt(alpha*alpha+beta*beta);
        alpha=p->x-q->x;
        beta=p->y-q->y;
        radius=sqrt(alpha*alpha+beta*beta);
        beta=fabs(distance-radius);
        if (beta < (mid+0.5))
          {
            if (beta <= (mid-0.5))
              opacity=Opaque;
            else
              {
                alpha=mid-beta+0.5;
                opacity=(unsigned short) Max((int) opacity,Opaque*alpha*alpha);
              }
          }
        break;
      }
      case FillCirclePrimitive:
      {
        alpha=p->x-pixel->x;
        beta=p->y-pixel->y;
        distance=sqrt(alpha*alpha+beta*beta);
        alpha=p->x-q->x;
        beta=p->y-q->y;
        radius=sqrt(alpha*alpha+beta*beta);
        if (distance <= (radius-1.0))
          opacity=Opaque;
        else
          if (distance < (radius+1.0))
            {
              alpha=(radius-distance+1.0)/2.0;
              opacity=(unsigned short) Max((int) opacity,Opaque*alpha*alpha);
            }
        break;
      }
      case PolygonPrimitive:
      {
        unsigned short
          poly_opacity;

        poly_opacity=Transparent;
        for ( ; (p < q) && (poly_opacity != Opaque); p++)
        {
          line.x1=p->x;
          line.y1=p->y;
          line.x2=(p+1)->x;
          line.y2=(p+1)->y;
          poly_opacity=PixelOnLine(pixel,&line,mid,(unsigned short)
            Max(opacity,poly_opacity));
        }
        opacity=Max(opacity,poly_opacity);
        break;
      }
      case FillPolygonPrimitive:
      {
        double
          distance,
          minimum_distance;

        int
          crossing,
          crossings;

        PrimitiveInfo
          *r;

        unsigned short
          poly_opacity;

        r=p;
        crossings=0;
        if ((pixel->y < q->y) != (pixel->y < p->y))
          {
            crossing=pixel->x < q->x;
            if (crossing != (pixel->x < p->x))
              crossings+=pixel->x <
                (q->x-(q->y-pixel->y)*(p->x-q->x)/(p->y-q->y));
            else
              if (crossing)
                crossings++;
          }
        for (p++; p <= q; p++)
        {
          if (pixel->y < (p-1)->y)
            {
              while ((p <= q) && (pixel->y < p->y))
                p++;
              if (p > q)
                break;
              crossing=pixel->x < (p-1)->x;
              if (crossing != (pixel->x < p->x))
                crossings+=pixel->x < ((p-1)->x-((p-1)->y-pixel->y)*
                  (p->x-(p-1)->x)/(p->y-(p-1)->y));
              else
                if (crossing)
                  crossings++;
              continue;
            }
          while ((p <= q) && (pixel->y >= p->y))
            p++;
          if (p > q)
            break;
          crossing=pixel->x < (p-1)->x;
          if (crossing != (pixel->x < p->x))
            crossings+=pixel->x < ((p-1)->x-((p-1)->y-pixel->y)*
              (p->x-(p-1)->x)/(p->y-(p-1)->y));
          else
            if (crossing)
              crossings++;
        }
        /*
          Now find distance to polygon.
        */
        p=r;
        line.x1=p->x;
        line.y1=p->y;
        line.x2=q->x;
        line.y2=q->y;
        minimum_distance=DistanceToLine(pixel,&line);
        for ( ; p < q; p++)
        {
          line.x1=p->x;
          line.y1=p->y;
          line.x2=(p+1)->x;
          line.y2=(p+1)->y;
          distance=DistanceToLine(pixel,&line);
          if (distance < minimum_distance)
            minimum_distance=distance;
        }
        minimum_distance=sqrt(minimum_distance);
        if (crossings & 0x01)
          {
            poly_opacity=Opaque;
            if (minimum_distance < 0.5)
              {
                alpha=0.5+minimum_distance;
                poly_opacity=(unsigned short) (Opaque*alpha*alpha);
              }
            opacity=Max(opacity,poly_opacity);
            break;
          }
        poly_opacity=Transparent;
        if (minimum_distance < 0.5)
          {
            alpha=0.5-minimum_distance;
            poly_opacity=(unsigned short) (Opaque*alpha*alpha);
          }
        opacity=Max(opacity,poly_opacity);
        break;
      }
      case ColorPrimitive:
      {
        switch (p->method)
        {
          case PointMethod:
          default:
          {
            if ((pixel->x != (int) (p->x+0.5)) &&
                (pixel->y != (int) (p->y+0.5)))
              break;
            opacity=Opaque;
            break;
          }
          case ReplaceMethod:
          {
            RunlengthPacket
              color;

            static RunlengthPacket
              target;

            if ((pixel->x == 0) && (pixel->y == 0))
              target=(*PixelOffset(image,p->x,p->y));
            color=(*PixelOffset(image,pixel->x,pixel->y));
            if (ColorMatch(color,target,(int) image->fuzz))
              opacity=Opaque;
            break;
          }
          case FloodfillMethod:
          case FillToBorderMethod:
          {
            if ((pixel->x != (int) (p->x+0.5)) &&
                (pixel->y != (int) (p->y+0.5)))
              break;
            target=(*PixelOffset(image,pixel->x,pixel->y));
            if (p->method == FillToBorderMethod)
              {
                (void) QueryColorDatabase(
                  annotate_info->image_info->border_color,&border_color);
                target.red=XDownScale(border_color.red);
                target.green=XDownScale(border_color.green);
                target.blue=XDownScale(border_color.blue);
              }
            ColorFloodfillImage(image,&target,annotate_info->tile,
              (int) pixel->x,(int) pixel->y,p->method);
            break;
          }
          case ResetMethod:
          {
            opacity=Opaque;
            break;
          }
        }
        break;
      }
      case MattePrimitive:
      {
        if (!image->matte)
          {
            /*
              Initialize matte image.
            */
            image->class=DirectClass;
            image->matte=True;
            for (i=0; i < (int) image->packets; i++)
              image->pixels[i].index=Opaque;
          }
        switch (p->method)
        {
          case PointMethod:
          default:
          {
            if ((pixel->x != (int) (p->x+0.5)) &&
                (pixel->y != (int) (p->y+0.5)))
              break;
            PixelOffset(image,pixel->x,pixel->y)->index=Transparent;
            break;
          }
          case ReplaceMethod:
          {
            RunlengthPacket
              color;

            static RunlengthPacket
              target;

            if ((pixel->x == 0) && (pixel->y == 0))
              target=(*PixelOffset(image,p->x,p->y));
            color=(*PixelOffset(image,pixel->x,pixel->y));
            if (ColorMatch(color,target,image->fuzz))
              PixelOffset(image,pixel->x,pixel->y)->index=Transparent;
            break;
          }
          case FloodfillMethod:
          case FillToBorderMethod:
          {
            if ((pixel->x != (int) (p->x+0.5)) &&
                (pixel->y != (int) (p->y+0.5)))
              break;
            target=(*PixelOffset(image,pixel->x,pixel->y));
            if (p->method == FillToBorderMethod)
              {
                (void) QueryColorDatabase(
                  annotate_info->image_info->border_color,&border_color);
                target.red=XDownScale(border_color.red);
                target.green=XDownScale(border_color.green);
                target.blue=XDownScale(border_color.blue);
              }
            MatteFloodfillImage(image,&target,Transparent,(int) pixel->x,
              (int) pixel->y,p->method);
            break;
          }
          case ResetMethod:
          {
            PixelOffset(image,pixel->x,pixel->y)->index=Opaque;
            break;
          }
        }
        break;
      }
      case TextPrimitive:
      case ImagePrimitive:
      {
        register char
          *r;

        if ((pixel->x != (int) (p->x+0.5)) && (pixel->y != (int) (p->y+0.5)))
          break;
        if (p->text == (char *) NULL)
          break;
        r=p->text;
        if (*r == '"')
          {
            p->text++;
            for (r++; *r != '\0'; r++)
              if ((*r == '"') && (*(r-1) != '\\'))
                break;
          }
        else
          if (*r == '\'')
            {
              p->text++;
              for (r++; *r != '\0'; r++)
                if ((*r == '\'') && (*(r-1) != '\\'))
                  break;
            }
          else
            for (r++;  *r != '\0'; r++)
              if (isspace((int) *r) && (*(r-1) != '\\'))
                break;
        (void) strncpy(annotate_info->text,p->text,r-p->text);
        annotate_info->text[r-p->text]='\0';
        if (p->primitive == TextPrimitive)
          {
            FormatString(annotate_info->geometry,"%+d%+d",(int) p->x,
              (int) p->y);
            AnnotateImage(image,annotate_info);
          }
        else
          {
            Image
              *composite_image;

            ImageInfo
              composite_info;

            GetImageInfo(&composite_info);
            (void) strcpy(composite_info.filename,annotate_info->text);
            composite_image=ReadImage(&composite_info);
            if (composite_image != (Image *) NULL)
              {
                CompositeImage(image,ReplaceCompositeOp,composite_image,
                  (int) p->x,(int) p->y);
                DestroyImage(composite_image);
              }
          }
        break;
      }
    }
    if (opacity == Opaque)
      return(opacity);
    while (p <= q)
      p++;
  }
  return(opacity);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M a t t e F l o o d f i l l I m a g e                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MatteFloodfillImage floodfills the designated area with a matte
%  value.  The floodfill algorithm is strongly based on a similar algorithm in
%  "Graphics Gems" by Paul Heckbert.
%
%  The format of the MatteFloodfillImage method is:
%
%      void MatteFloodfillImage(Image *image,const RunlengthPacket *target,
%        const unsigned int matte,const int x_offset,const int y_offset,
%        const PaintMethod method)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%    o target: A RunlengthPacket structure.  This is the RGB value of the target
%      color.
%
%    o matte: A integer value representing the amount of transparency.
%
%    o x,y: Unsigned integers representing the current location of the pen.
%
%    o method: drawing method of type PrimitiveType: floodfill or fill to
%      border.
%
%
*/
Export void MatteFloodfillImage(Image *image,const RunlengthPacket *target,
  const unsigned int matte,const int x_offset,const int y_offset,
  const PaintMethod method)
{
  int
    offset,
    skip,
    start,
    x1,
    x2;

  register int
    x,
    y;

  register RunlengthPacket
    *pixel;

  register SegmentInfo
    *p;

  SegmentInfo
    *segment_stack;

  /*
    Check boundary conditions.
  */
  assert(image != (Image *) NULL);
  if ((x_offset < 0) || (x_offset >= (int) image->columns))
    return;
  if ((y_offset < 0) || (y_offset >= (int) image->rows))
    return;
  if (target->index == (unsigned short) matte)
    return;
  if (!UncondenseImage(image))
    return;
  pixel=PixelOffset(image,x_offset,y_offset);
  if (pixel->index == (unsigned short) matte)
    return;
  /*
    Allocate segment stack.
  */
  x=x_offset;
  y=y_offset;
  segment_stack=(SegmentInfo *)
    AllocateMemory(MaxStacksize*sizeof(SegmentInfo));
  if (segment_stack == (SegmentInfo *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to recolor image",
        "Memory allocation failed");
      return;
    }
  /*
    Push initial segment on stack.
  */
  image->class=DirectClass;
  if (!image->matte)
    MatteImage(image);
  start=0;
  p=segment_stack;
  Push(y,x,x,1);
  Push(y+1,x,x,-1);
  while (p > segment_stack)
  {
    /*
      Pop segment off stack.
    */
    p--;
    x1=(int) p->x1;
    x2=(int) p->x2;
    offset=(int) p->y2;
    y=(int) p->y1+offset;
    /*
      Recolor neighboring pixels.
    */
    pixel=PixelOffset(image,x1,y);
    for (x=x1; x >= 0 ; x--)
    {
      if (method == FloodfillMethod)
        {
          if (!MatteMatch(*pixel,*target,image->fuzz))
            break;
        }
      else
        if (MatteMatch(*pixel,*target,image->fuzz) ||
            (pixel->index == (unsigned short) matte))
          break;
      pixel->index=(unsigned short) matte;
      pixel--;
    }
    skip=x >= x1;
    if (!skip)
      {
        start=x+1;
        if (start < x1)
          Push(y,start,x1-1,-offset);
        x=x1+1;
      }
    do
    {
      if (!skip)
        {
          pixel=PixelOffset(image,x,y);
          for ( ; x < (int) image->columns; x++)
          {
            if (method == FloodfillMethod)
              {
                if (!MatteMatch(*pixel,*target,image->fuzz))
                  break;
              }
            else
              if (MatteMatch(*pixel,*target,image->fuzz) ||
                  (pixel->index == (unsigned short) matte))
                break;
            pixel->index=(unsigned short) matte;
            pixel++;
          }
          Push(y,start,x-1,offset);
          if (x > (x2+1))
            Push(y,x2+1,x-1,-offset);
        }
      skip=False;
      pixel=PixelOffset(image,x,y);
      for (x++; x <= x2 ; x++)
      {
        pixel++;
        if (method == FloodfillMethod)
          {
            if (MatteMatch(*pixel,*target,image->fuzz))
              break;
          }
        else
          if (!MatteMatch(*pixel,*target,image->fuzz) &&
              (pixel->index != (unsigned short) matte))
            break;
      }
      start=x;
    } while (x <= x2);
  }
  FreeMemory((char *) segment_stack);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%     O p a q u e I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method OpaqueImage changes the color of an opaque pixel to the pen color.
%
%  The format of the OpaqueImage method is:
%
%      void OpaqueImage(Image *image,const char *opaque_color,
%        const char *pen_color)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%    o opaque_color,
%      pen_color: A character string that contain an X11 color string.
%
%
*/
Export void OpaqueImage(Image *image,const char *opaque_color,
  const char *pen_color)
{
#define OpaqueImageText  "  Setting opaque color in the image...  "

  ColorPacket
    target,
    target_color;

  register int
    i;

  unsigned int
    status;

  /*
    Determine RGB values of the opaque color.
  */
  assert(image != (Image *) NULL);
  status=QueryColorDatabase(opaque_color,&target_color);
  if (status == False)
    return;
  target.red=XDownScale(target_color.red);
  target.green=XDownScale(target_color.green);
  target.blue=XDownScale(target_color.blue);
  status=QueryColorDatabase(pen_color,&target_color);
  if (status == False)
    return;
  /*
    Make image color opaque.
  */
  switch (image->class)
  {
    case DirectClass:
    default:
    {
      register RunlengthPacket
        *p;

      /*
        Make DirectClass image opaque.
      */
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        if (ColorMatch(*p,target,image->fuzz))
          {
            p->red=XDownScale(target_color.red);
            p->green=XDownScale(target_color.green);
            p->blue=XDownScale(target_color.blue);
          }
        p++;
        if (QuantumTick(i,image->packets))
          ProgressMonitor(OpaqueImageText,i,image->packets);
      }
      break;
    }
    case PseudoClass:
    {
      register ColorPacket
        *p;

      /*
        Make PseudoClass image opaque.
      */
      p=image->colormap;
      for (i=0; i < (int) image->colors; i++)
      {
        if (ColorMatch(*p,target,image->fuzz))
          {
            p->red=XDownScale(target_color.red);
            p->green=XDownScale(target_color.green);
            p->blue=XDownScale(target_color.blue);
          }
        p++;
        if (QuantumTick(i,image->packets))
          ProgressMonitor(OpaqueImageText,i,image->packets);
      }
      SyncImage(image);
      break;
    }
  }
}
