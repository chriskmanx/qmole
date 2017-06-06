/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            X   X  PPPP   M   M                              %
%                             X X   P   P  MM MM                              %
%                              X    PPPP   M M M                              %
%                             X X   P      M   M                              %
%                            X   X  P      M   M                              %
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
  Constant declaractions.
*/
const ColorlistInfo
  XPMColorlist[235] =
  {
    { "AliceBlue", 240, 248, 255 },
    { "AntiqueWhite", 250, 235, 215 },
    { "Aquamarine", 50, 191, 193 },
    { "Azure", 240, 255, 255 },
    { "Beige", 245, 245, 220 },
    { "Bisque", 255, 228, 196 },
    { "Black", 0, 0, 0 },
    { "BlanchedAlmond", 255, 235, 205 },
    { "Blue", 0, 0, 255 },
    { "BlueViolet", 138, 43, 226 },
    { "Brown", 165, 42, 42 },
    { "burlywood", 222, 184, 135 },
    { "CadetBlue", 95, 146, 158 },
    { "chartreuse", 127, 255, 0 },
    { "chocolate", 210, 105, 30 },
    { "Coral", 255, 114, 86 },
    { "CornflowerBlue", 34, 34, 152 },
    { "cornsilk", 255, 248, 220 },
    { "Cyan", 0, 255, 255 },
    { "DarkGoldenrod", 184, 134, 11 },
    { "DarkGreen", 0, 86, 45 },
    { "DarkKhaki", 189, 183, 107 },
    { "DarkOliveGreen", 85, 86, 47 },
    { "DarkOrange", 255, 140, 0 },
    { "DarkOrchid", 139, 32, 139 },
    { "DarkSalmon", 233, 150, 122 },
    { "DarkSeaGreen", 143, 188, 143 },
    { "DarkSlateBlue", 56, 75, 102 },
    { "DarkSlateGray", 47, 79, 79 },
    { "DarkTurquoise", 0, 166, 166 },
    { "DarkViolet", 148, 0, 211 },
    { "DeepPink", 255, 20, 147 },
    { "DeepSkyBlue", 0, 191, 255 },
    { "DimGray", 84, 84, 84 },
    { "DodgerBlue", 30, 144, 255 },
    { "Firebrick", 142, 35, 35 },
    { "FloralWhite", 255, 250, 240 },
    { "ForestGreen", 80, 159, 105 },
    { "gainsboro", 220, 220, 220 },
    { "GhostWhite", 248, 248, 255 },
    { "Gold", 218, 170, 0 },
    { "Goldenrod", 239, 223, 132 },
    { "Gray", 126, 126, 126 },
    { "Green", 0, 255, 0 },
    { "GreenYellow", 173, 255, 47 },
    { "honeydew", 240, 255, 240 },
    { "HotPink", 255, 105, 180 },
    { "IndianRed", 107, 57, 57 },
    { "ivory", 255, 255, 240 },
    { "Khaki", 179, 179, 126 },
    { "lavender", 230, 230, 250 },
    { "LavenderBlush", 255, 240, 245 },
    { "LawnGreen", 124, 252, 0 },
    { "LemonChiffon", 255, 250, 205 },
    { "LightBlue", 176, 226, 255 },
    { "LightCoral", 240, 128, 128 },
    { "LightCyan", 224, 255, 255 },
    { "LightGoldenrod", 238, 221, 130 },
    { "LightGoldenrodYellow", 250, 250, 210 },
    { "LightGray", 168, 168, 168 },
    { "LightPink", 255, 182, 193 },
    { "LightSalmon", 255, 160, 122 },
    { "LightSeaGreen", 32, 178, 170 },
    { "LightSkyBlue", 135, 206, 250 },
    { "LightSlateBlue", 132, 112, 255 },
    { "LightSlateGray", 119, 136, 153 },
    { "LightSteelBlue", 124, 152, 211 },
    { "LightYellow", 255, 255, 224 },
    { "LimeGreen", 0, 175, 20 },
    { "linen", 250, 240, 230 },
    { "Magenta", 255, 0, 255 },
    { "Maroon", 143, 0, 82 },
    { "MediumAquamarine", 0, 147, 143 },
    { "MediumBlue", 50, 50, 204 },
    { "MediumForestGreen", 50, 129, 75 },
    { "MediumGoldenrod", 209, 193, 102 },
    { "MediumOrchid", 189, 82, 189 },
    { "MediumPurple", 147, 112, 219 },
    { "MediumSeaGreen", 52, 119, 102 },
    { "MediumSlateBlue", 106, 106, 141 },
    { "MediumSpringGreen", 35, 142, 35 },
    { "MediumTurquoise", 0, 210, 210 },
    { "MediumVioletRed", 213, 32, 121 },
    { "MidnightBlue", 47, 47, 100 },
    { "MintCream", 245, 255, 250 },
    { "MistyRose", 255, 228, 225 },
    { "moccasin", 255, 228, 181 },
    { "NavajoWhite", 255, 222, 173 },
    { "Navy", 35, 35, 117 },
    { "NavyBlue", 35, 35, 117 },
    { "OldLace", 253, 245, 230 },
    { "OliveDrab", 107, 142, 35 },
    { "Orange", 255, 135, 0 },
    { "OrangeRed", 255, 69, 0 },
    { "Orchid", 239, 132, 239 },
    { "PaleGoldenrod", 238, 232, 170 },
    { "PaleGreen", 115, 222, 120 },
    { "PaleTurquoise", 175, 238, 238 },
    { "PaleVioletRed", 219, 112, 147 },
    { "PapayaWhip", 255, 239, 213 },
    { "PeachPuff", 255, 218, 185 },
    { "peru", 205, 133, 63 },
    { "Pink", 255, 181, 197 },
    { "Plum", 197, 72, 155 },
    { "PowderBlue", 176, 224, 230 },
    { "purple", 160, 32, 240 },
    { "Red", 255, 0, 0 },
    { "RosyBrown", 188, 143, 143 },
    { "RoyalBlue", 65, 105, 225 },
    { "SaddleBrown", 139, 69, 19 },
    { "Salmon", 233, 150, 122 },
    { "SandyBrown", 244, 164, 96 },
    { "SeaGreen", 82, 149, 132 },
    { "seashell", 255, 245, 238 },
    { "Sienna", 150, 82, 45 },
    { "SkyBlue", 114, 159, 255 },
    { "SlateBlue", 126, 136, 171 },
    { "SlateGray", 112, 128, 144 },
    { "snow", 255, 250, 250 },
    { "SpringGreen", 65, 172, 65 },
    { "SteelBlue", 84, 112, 170 },
    { "Tan", 222, 184, 135 },
    { "Thistle", 216, 191, 216 },
    { "tomato", 255, 99, 71 },
    { "Transparent", 0, 0, 1 },
    { "Turquoise", 25, 204, 223 },
    { "Violet", 156, 62, 206 },
    { "VioletRed", 243, 62, 150 },
    { "Wheat", 245, 222, 179 },
    { "White", 255, 255, 255 },
    { "WhiteSmoke", 245, 245, 245 },
    { "Yellow", 255, 255, 0 },
    { "YellowGreen", 50, 216, 56 },
    { "Gray0", 0, 0, 0 },
    { "Gray1", 3, 3, 3 },
    { "Gray10", 26, 26, 26 },
    { "Gray100", 255, 255, 255 },
    { "Gray11", 28, 28, 28 },
    { "Gray12", 31, 31, 31 },
    { "Gray13", 33, 33, 33 },
    { "Gray14", 36, 36, 36 },
    { "Gray15", 38, 38, 38 },
    { "Gray16", 41, 41, 41 },
    { "Gray17", 43, 43, 43 },
    { "Gray18", 46, 46, 46 },
    { "Gray19", 48, 48, 48 },
    { "Gray2", 5, 5, 5 },
    { "Gray20", 51, 51, 51 },
    { "Gray21", 54, 54, 54 },
    { "Gray22", 56, 56, 56 },
    { "Gray23", 59, 59, 59 },
    { "Gray24", 61, 61, 61 },
    { "Gray25", 64, 64, 64 },
    { "Gray26", 66, 66, 66 },
    { "Gray27", 69, 69, 69 },
    { "Gray28", 71, 71, 71 },
    { "Gray29", 74, 74, 74 },
    { "Gray3", 8, 8, 8 },
    { "Gray30", 77, 77, 77 },
    { "Gray31", 79, 79, 79 },
    { "Gray32", 82, 82, 82 },
    { "Gray33", 84, 84, 84 },
    { "Gray34", 87, 87, 87 },
    { "Gray35", 89, 89, 89 },
    { "Gray36", 92, 92, 92 },
    { "Gray37", 94, 94, 94 },
    { "Gray38", 97, 97, 97 },
    { "Gray39", 99, 99, 99 },
    { "Gray4", 10, 10, 10 },
    { "Gray40", 102, 102, 102 },
    { "Gray41", 105, 105, 105 },
    { "Gray42", 107, 107, 107 },
    { "Gray43", 110, 110, 110 },
    { "Gray44", 112, 112, 112 },
    { "Gray45", 115, 115, 115 },
    { "Gray46", 117, 117, 117 },
    { "Gray47", 120, 120, 120 },
    { "Gray48", 122, 122, 122 },
    { "Gray49", 125, 125, 125 },
    { "Gray5", 13, 13, 13 },
    { "Gray50", 127, 127, 127 },
    { "Gray51", 130, 130, 130 },
    { "Gray52", 133, 133, 133 },
    { "Gray53", 135, 135, 135 },
    { "Gray54", 138, 138, 138 },
    { "Gray55", 140, 140, 140 },
    { "Gray56", 143, 143, 143 },
    { "Gray57", 145, 145, 145 },
    { "Gray58", 148, 148, 148 },
    { "Gray59", 150, 150, 150 },
    { "Gray6", 15, 15, 15 },
    { "Gray60", 153, 153, 153 },
    { "Gray61", 156, 156, 156 },
    { "Gray62", 158, 158, 158 },
    { "Gray63", 161, 161, 161 },
    { "Gray64", 163, 163, 163 },
    { "Gray65", 166, 166, 166 },
    { "Gray66", 168, 168, 168 },
    { "Gray67", 171, 171, 171 },
    { "Gray68", 173, 173, 173 },
    { "Gray69", 176, 176, 176 },
    { "Gray7", 18, 18, 18 },
    { "Gray70", 179, 179, 179 },
    { "Gray71", 181, 181, 181 },
    { "Gray72", 184, 184, 184 },
    { "Gray73", 186, 186, 186 },
    { "Gray74", 189, 189, 189 },
    { "Gray75", 191, 191, 191 },
    { "Gray76", 194, 194, 194 },
    { "Gray77", 196, 196, 196 },
    { "Gray78", 199, 199, 199 },
    { "Gray79", 201, 201, 201 },
    { "Gray8", 20, 20, 20 },
    { "Gray80", 204, 204, 204 },
    { "Gray81", 207, 207, 207 },
    { "Gray82", 209, 209, 209 },
    { "Gray83", 212, 212, 212 },
    { "Gray84", 214, 214, 214 },
    { "Gray85", 217, 217, 217 },
    { "Gray86", 219, 219, 219 },
    { "Gray87", 222, 222, 222 },
    { "Gray88", 224, 224, 224 },
    { "Gray89", 227, 227, 227 },
    { "Gray9", 23, 23, 23 },
    { "Gray90", 229, 229, 229 },
    { "Gray91", 232, 232, 232 },
    { "Gray92", 235, 235, 235 },
    { "Gray93", 237, 237, 237 },
    { "Gray94", 240, 240, 240 },
    { "Gray95", 242, 242, 242 },
    { "Gray96", 245, 245, 245 },
    { "Gray97", 247, 247, 247 },
    { "Gray98", 250, 250, 250 },
    { "Gray99", 252, 252, 252 },
    { (char *) NULL, 0, 0, 0 }
  };

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d X P M I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadXPMImage reads an X11 pixmap image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadXPMImage method is:
%
%      Image *ReadXPMImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadXPMImage returns a pointer to the image after
%      creating it. A null image is returned if there is a memory shortage
%      or if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/

static char *ParseColor(char *data)
{
#define NumberTargets  6

  static const char
    *targets[NumberTargets] = { "c ", "g ", "g4 ", "m ", "b ", "s " };

  register char
     *p,
     *r;

  register const char
     *q;

  register int
    i;

  for (i=0; i < NumberTargets; i++)
  {
    r=data;
    for (q=targets[i]; *r != '\0'; r++)
    {
      if (*r != *q)
        continue;
      if (!isspace((int) (*(r-1))))
        continue;
      p=r;
      for ( ; ; )
      {
        if (*q == '\0')
          return(r);
        if (*p++ != *q++)
          break;
      }
      q=targets[i];
    }
  }
  return((char *) NULL);
}

Export Image *ReadXPMImage(const ImageInfo *image_info)
{
  char
    key[MaxTextExtent],
    target[MaxTextExtent],
    **textlist,
    *xpm_buffer;

  ColorPacket
    color;

  Image
    *image;

  int
    count,
    length,
    x,
    y;

  register char
    *p,
    *q;

  register int
    i,
    j;

  register RunlengthPacket
    *r;

  unsigned int
    status,
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
  status=OpenBlob(image_info,image,"r");
  if (status == False)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  /*
    Read XPM file.
  */
  length=MaxTextExtent;
  xpm_buffer=(char *) AllocateMemory(length*sizeof(char));
  p=xpm_buffer;
  if (xpm_buffer != (char *) NULL)
    while (GetStringBlob(image,p) != (char *) NULL)
    {
      if (*p == '#')
        if ((p == xpm_buffer) || (*(p-1) == '\n'))
          continue;
      if ((*p == '}') && (*(p+1) == ';'))
        break;
      p+=Extent(p);
      if ((p-xpm_buffer+MaxTextExtent+1) < length)
        continue;
      length<<=1;
      xpm_buffer=(char *)
        ReallocateMemory((char *) xpm_buffer,length*sizeof(char));
      if (xpm_buffer == (char *) NULL)
        break;
      p=xpm_buffer+Extent(xpm_buffer);
    }
  if (xpm_buffer == (char *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Remove comments.
  */
  for (p=xpm_buffer; *p != '\0'; p++)
  {
    if ((*p == '"') || (*p == '\''))
      {
        if (*p == '"')
          {
            for (p++; *p != '\0'; p++)
              if ((*p == '"') && (*(p-1) != '\\'))
                break;
          }
        else
          for (p++; *p != '\0'; p++)
            if ((*p == '\'') && (*(p-1) != '\\'))
              break;
        if (*p == '\0')
          break;
        continue;
      }
    if ((*p != '/') || (*(p+1) != '*'))
      continue;
    for (q=p+2; *q != '\0'; q++)
      if ((*q == '*') && (*(q+1) == '/'))
        break;
    (void) strcpy(p,q+2);
  }
  /*
    Remove unquoted characters.
  */
  i=0;
  for (p=xpm_buffer; *p != '\0'; p++)
  {
    if (*p != '"')
      continue;
    for (q=p+1; *q != '\0'; q++)
      if (*q == '"')
        break;
    (void) strncpy(xpm_buffer+i,p+1,q-p-1);
    i+=q-p-1;
    xpm_buffer[i++]='\n';
    p=q+1;
  }
  xpm_buffer[i]='\0';
  textlist=StringToList(xpm_buffer);
  FreeMemory(xpm_buffer);
  if (textlist == (char **) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Read hints.
  */
  image->class=PseudoClass;
  count=sscanf(textlist[0],"%u %u %u %u",&image->columns,&image->rows,
    &image->colors,&width);
  if ((count != 4) || (width > 2) ||
      ((image->columns*image->rows*image->colors) == 0))
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        FreeMemory((char *) textlist[i]);
      FreeMemory((char *) textlist);
      ReaderExit(CorruptImageWarning,"Not a XPM image file",image);
    }
  /*
    Initialize image structure.
  */
  image->colormap=(ColorPacket *)
    AllocateMemory(image->colors*sizeof(ColorPacket));
  if (image->colormap == (ColorPacket *) NULL)
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        FreeMemory((char *) textlist[i]);
      FreeMemory((char *) textlist);
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  /*
    Read image colormap.
  */
  i=1;
  for (j=0; j < (int) image->colors; j++)
  {
    p=textlist[i++];
    if (p == (char *) NULL)
      break;
    image->colormap[j].key[width]='\0';
    (void) strncpy(image->colormap[j].key,p,width);
    /*
      Parse color.
    */
    (void) strcpy(target,"gray");
    q=ParseColor(p+width);
    if (q != (char *) NULL)
      {
        while (!isspace((int) (*q)) && (*q != '\0'))
          q++;
        (void) strcpy(target,q);
        q=ParseColor(target);
        if (q != (char *) NULL)
          *q='\0';
      }
    Strip(target);
    image->colormap[j].flags=Latin1Compare(target,"none") == 0;
    if (image->colormap[j].flags)
      {
        image->class=DirectClass;
        image->matte=True;
        (void) strcpy(target,"black");
      }
    (void) QueryColorDatabase(target,&color);
    image->colormap[j].red=XDownScale(color.red);
    image->colormap[j].green=XDownScale(color.green);
    image->colormap[j].blue=XDownScale(color.blue);
  }
  if (j < (int) image->colors)
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        FreeMemory((char *) textlist[i]);
      FreeMemory((char *) textlist);
      ReaderExit(CorruptImageWarning,"Corrupt XPM image file",image);
    }
  if (image_info->ping)
    {
      CloseBlob(image);
      return(image);
    }
  /*
    Read image pixels.
  */
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        FreeMemory((char *) textlist[i]);
      FreeMemory((char *) textlist);
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  SetImage(image);
  j=0;
  key[width]='\0';
  r=image->pixels;
  for (y=0; y < (int) image->rows; y++)
  {
    p=textlist[i++];
    if (p == (char *) NULL)
      break;
    for (x=0; x < (int) image->columns; x++)
    {
      (void) strncpy(key,p,width);
      if (strcmp(key,image->colormap[j].key) != 0)
        for (j=0; j < (int) (image->colors-1); j++)
          if (strcmp(key,image->colormap[j].key) == 0)
            break;
      r->red=image->colormap[j].red;
      r->green=image->colormap[j].green;
      r->blue=image->colormap[j].blue;
      if (image->class == PseudoClass)
        r->index=(unsigned short) j;
      else
        if (image->colormap[j].flags)
          r->index=Transparent;
        else
          r->index=Opaque;
      r->length=0;
      r++;
      p+=width;
    }
  }
  if (y < (int) image->rows)
    {
      for (i=0; textlist[i] != (char *) NULL; i++)
        FreeMemory((char *) textlist[i]);
      FreeMemory((char *) textlist);
      ReaderExit(CorruptImageWarning,"Corrupt XPM image file",image);
    }
  /*
    Free resources.
  */
  for (i=0; textlist[i] != (char *) NULL; i++)
    FreeMemory((char *) textlist[i]);
  FreeMemory((char *) textlist);
  CondenseImage(image);
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e X P M I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Procedure WriteXPMImage writes an image to a file in the X pixmap format.
%
%  The format of the WriteXPMImage method is:
%
%      unsigned int WriteXPMImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteXPMImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteXPMImage(const ImageInfo *image_info,Image *image)
{
#define MaxCixels  92

  static const char
    Cixel[MaxCixels+1] = " .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjk"
                         "lzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`'][{}|";

  char
    buffer[MaxTextExtent],
    name[MaxTextExtent],
    symbol[MaxTextExtent];

  double
    min_distance;

  int
    j,
    k,
    y;

  long
    mean;

  register double
    distance_squared;

  register int
    distance,
    i,
    runlength,
    x;

  register RunlengthPacket
    *p;

  register const ColorlistInfo
    *q;

  unsigned int
    characters_per_pixel,
    colors,
    status,
    transparent;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  TransformRGBImage(image,RGBColorspace);
  transparent=False;
  if (image->class == PseudoClass)
    colors=image->colors;
  else
    {
      QuantizeInfo
        quantize_info;

      unsigned char
        *matte_image;

      /*
        Convert DirectClass to PseudoClass image.
      */
      matte_image=(unsigned char *) NULL;
      if (image->matte)
        {
          /*
            Map all the transparent pixels.
          */
          if (!UncondenseImage(image))
            return(False);
          matte_image=(unsigned char *)
            AllocateMemory(image->packets*sizeof(unsigned char));
          if (matte_image == (unsigned char *) NULL)
            WriterExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          p=image->pixels;
          for (i=0; i < (int) image->packets; i++)
          {
            matte_image[i]=p->index == Transparent;
            if (matte_image[i])
              transparent=True;
            p++;
          }
        }
      GetQuantizeInfo(&quantize_info);
      quantize_info.dither=image_info->dither;
      (void) QuantizeImage(&quantize_info,image);
      SyncImage(image);
      colors=image->colors;
      if (transparent)
        {
          if (!UncondenseImage(image))
            return(False);
          colors++;
          p=image->pixels;
          for (i=0; i < (int) image->packets; i++)
          {
            if (matte_image[i])
              p->index=image->colors;
            p++;
          }
        }
      if (matte_image != (unsigned char *) NULL)
        FreeMemory((char *) matte_image);
    }
  /*
    Compute the character per pixel.
  */
  characters_per_pixel=1;
  for (k=MaxCixels; (int) colors > k; k*=MaxCixels)
    characters_per_pixel++;
  /*
    XPM header.
  */
  (void) strcpy(buffer,"/* XPM */\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"static char *magick[] = {\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"/* columns rows colors chars-per-pixel */\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"\"%u %u %u %d\",\n",image->columns,
    image->rows,colors,characters_per_pixel);
  (void) WriteBlob(image,strlen(buffer),buffer);
  for (i=0; i < (int) colors; i++)
  {
    ColorPacket
      *p;

    /*
      Define XPM color.
    */
    min_distance=0;
    p=image->colormap+i;
    FormatString(name,HexColorFormat,(unsigned int) p->red,
      (unsigned int) p->green,(unsigned int) p->blue);
    for (q=XPMColorlist; q->name != (char *) NULL; q++)
    {
      mean=(DownScale(p->red)+(int) q->red)/2;
      distance=DownScale(p->red)-(int) q->red;
      distance_squared=(2.0*256.0+mean)*distance*distance/256.0;
      distance=DownScale(p->green)-(int) q->green;
      distance_squared+=4.0*(distance*distance);
      distance=DownScale(p->blue)-(int) q->blue;
      distance_squared+=(3.0*256.0-1.0-mean)*distance*distance/256.0;
      if ((q == XPMColorlist) || (distance_squared <= min_distance))
        {
          min_distance=distance_squared;
          if (min_distance == 0.0)
            (void) strcpy(name,q->name);
        }
    }
    if (transparent)
      if (i == (int) (colors-1))
        (void) strcpy(name,"None");
    /*
      Write XPM color.
    */
    k=i % MaxCixels;
    symbol[0]=Cixel[k];
    for (j=1; j < (int) characters_per_pixel; j++)
    {
      k=((i-k)/MaxCixels) % MaxCixels;
      symbol[j]=Cixel[k];
    }
    symbol[j]='\0';
    (void) sprintf(buffer,"\"%.1024s c %.1024s\",\n",symbol,name);
    (void) WriteBlob(image,strlen(buffer),buffer);
  }
  /*
    Define XPM pixels.
  */
  (void) strcpy(buffer,"/* pixels */\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  p=image->pixels;
  runlength=p->length+1;
  for (y=0; y < (int) image->rows; y++)
  {
    (void) strcpy(buffer,"\"");
    (void) WriteBlob(image,strlen(buffer),buffer);
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      k=p->index % MaxCixels;
      symbol[0]=Cixel[k];
      for (j=1; j < (int) characters_per_pixel; j++)
      {
        k=(((int) p->index-k)/MaxCixels) % MaxCixels;
        symbol[j]=Cixel[k];
      }
      symbol[j]='\0';
      (void) sprintf(buffer,"%.1024s",symbol);
      (void) WriteBlob(image,strlen(buffer),buffer);
    }
    (void) sprintf(buffer,"\"%.1024s\n",
      (y == (int) (image->rows-1) ? "" : ","));
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (QuantumTick(y,image->rows))
      ProgressMonitor(SaveImageText,y,image->rows);
  }
  (void) strcpy(buffer,"};\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  CloseBlob(image);
  return(True);
}
