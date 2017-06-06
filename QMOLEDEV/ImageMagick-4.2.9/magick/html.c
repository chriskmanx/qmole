/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        H   H  TTTTT  M   M  L                               %
%                        H   H    T    MM MM  L                               %
%                        HHHHH    T    M M M  L                               %
%                        H   H    T    M   M  L                               %
%                        H   H    T    M   M  LLLLL                           %
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
%   W r i t e H T M L I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteHTMLImage writes an image in the HTML encoded image format.
%
%  The format of the WriteHTMLImage method is:
%
%      unsigned int WriteHTMLImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteHTMLImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteHTMLImage(const ImageInfo *image_info,Image *image)
{
  char
    buffer[MaxTextExtent],
    filename[MaxTextExtent],
    mapname[MaxTextExtent],
    url[MaxTextExtent];

  Image
    *next;

  ImageInfo
    *local_info;

  int
    x,
    y;

  register char
    *p;

  unsigned int
    height,
    status,
    width;

  /*
    Open image.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  CloseBlob(image);
  TransformRGBImage(image,RGBColorspace);
  *url='\0';
  if ((Latin1Compare(image_info->magick,"FTP") == 0) ||
      (Latin1Compare(image_info->magick,"HTTP") == 0))
    {
      /*
        Extract URL base from filename.
      */
      p=strrchr(image->filename,'/');
      if (p)
        {
          p++;
          (void) strcpy(url,image_info->magick);
          (void) strcat(url,":");
          url[Extent(url)+p-image->filename]='\0';
          (void) strncat(url,image->filename,p-image->filename);
          (void) strcpy(image->filename,p);
        }
    }
  /*
    Refer to image map file.
  */
  (void) strcpy(filename,image->filename);
  AppendImageFormat("map",filename);
  (void) strcpy(mapname,BaseFilename(filename));
  (void) strcpy(image->filename,image_info->filename);
  (void) strcpy(filename,image->filename);
  local_info=CloneImageInfo(image_info);
  if (local_info == (ImageInfo *) NULL)
    WriterExit(FileOpenWarning,"Unable to allocate memory",image);
  local_info->adjoin=True;
  status=True;
  if (Latin1Compare(image_info->magick,"SHTML") != 0)
    {
      /*
        Open output image file.
      */
      status=OpenBlob(image_info,image,WriteBinaryType);
      if (status == False)
        WriterExit(FileOpenWarning,"Unable to open file",image);
      /*
        Write the HTML image file.
      */
      (void) strcpy(buffer,"<html version=\"2.0\">\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(buffer,"<head>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) sprintf(buffer,"<title>%.1024s</title>\n",
        image->label ? image->label : BaseFilename(image->filename));
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(buffer,"</head>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(buffer,"<body>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(buffer,"<center>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) sprintf(buffer,"<h1>%.1024s</h1>\n",image->filename);
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(buffer,"<br><br>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(filename,image->filename);
      AppendImageFormat("gif",filename);
      (void) sprintf(buffer,
        "<img ismap usemap=#%.1024s src=\"%.1024s\" border=0>\n",
        mapname,filename);
      (void) WriteBlob(image,strlen(buffer),buffer);
      /*
        Determine the size and location of each image tile.
      */
      width=image->columns;
      height=image->rows;
      x=0;
      y=0;
      if (image->montage != (char *) NULL)
        (void) ParseGeometry(image->montage,&x,&y,&width,&height);
      /*
        Write an image map.
      */
      (void) sprintf(buffer,"<map name=%.1024s>\n",mapname);
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) sprintf(buffer,"  <area href=""%.1024s""",url);
      (void) WriteBlob(image,strlen(buffer),buffer);
      if (image->directory == (char *) NULL)
        {
          (void) sprintf(buffer,"%.1024s shape=rect coords=0,0,%u,%u>\n",
            image->filename,width-1,height-1);
          (void) WriteBlob(image,strlen(buffer),buffer);
        }
      else
        for (p=image->directory; *p != '\0'; p++)
          if (*p != '\n')
            (void) WriteByte(image,*p);
          else
            {
              (void) sprintf(buffer," shape=rect coords=%d,%d,%d,%d>\n",
                x,y,x+(int) width-1,y+(int) height-1);
              (void) WriteBlob(image,strlen(buffer),buffer);
              if (*(p+1) != '\0')
                {
                  (void) sprintf(buffer,"  <area href=""%.1024s""",url);
                  (void) WriteBlob(image,strlen(buffer),buffer);
                }
              x+=width;
              if (x >= (int) image->columns)
                {
                  x=0;
                  y+=height;
                }
            }
      (void) strcpy(buffer,"</map>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      if (image->montage != (char *) NULL)
        {
          char
            color[MaxTextExtent];

          /*
            Make montage background transparent.
          */
          FormatString(color,HexColorFormat,
            (unsigned int) image->pixels[0].red,
            (unsigned int) image->pixels[0].green,
            (unsigned int) image->pixels[0].blue);
          TransparentImage(image,color);
        }
      (void) strcpy(filename,image->filename);
      (void) strcpy(buffer,"</center>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(buffer,"</body>\n");
      (void) WriteBlob(image,strlen(buffer),buffer);
      (void) strcpy(buffer,"</html>\n");
      status=WriteBlob(image,strlen(buffer),buffer);
      CloseBlob(image);
      /*
        Write the image as transparent GIF.
      */
      (void) strcpy(image->filename,filename);
      AppendImageFormat("gif",image->filename);
      next=image->next;
      image->next=(Image *) NULL;
      status|=WriteGIFImage(local_info,image);
      image->next=next;
      /*
        Determine image map filename.
      */
      (void) strcpy(image->filename,filename);
      for (p=filename+Extent(filename)-1; p > (filename+1); p--)
        if (*p == '.')
          {
            (void) strncpy(image->filename,filename,p-filename);
            image->filename[p-filename]='\0';
            break;
          }
      (void) strcat(image->filename,"_map.shtml");
    }
  /*
    Open image map.
  */
  status=OpenBlob(local_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  DestroyImageInfo(local_info);
  /*
    Determine the size and location of each image tile.
  */
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  if (image->montage != (char *) NULL)
    (void) ParseGeometry(image->montage,&x,&y,&width,&height);
  /*
    Write an image map.
  */
  (void) sprintf(buffer,"<map name=%.1024s>\n",mapname);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"  <area href=""%.1024s""",url);
  (void) WriteBlob(image,strlen(buffer),buffer);
  if (image->directory == (char *) NULL)
    {
      (void) sprintf(buffer,"%.1024s shape=rect coords=0,0,%u,%u>\n",
        image->filename,width-1,height-1);
      (void) WriteBlob(image,strlen(buffer),buffer);
    }
  else
    for (p=image->directory; *p != '\0'; p++)
      if (*p != '\n')
        (void) WriteByte(image,*p);
      else
        {
          (void) sprintf(buffer," shape=rect coords=%d,%d,%d,%d>\n",x,y,
            x+(int) width-1,y+(int) height-1);
          (void) WriteBlob(image,strlen(buffer),buffer);
          if (*(p+1) != '\0')
            {
              (void) sprintf(buffer,"  <area href=""%.1024s""",url);
              (void) WriteBlob(image,strlen(buffer),buffer);
            }
          x+=width;
          if (x >= (int) image->columns)
            {
              x=0;
              y+=height;
            }
        }
  (void) strcpy(buffer,"</map>\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  CloseBlob(image);
  (void) strcpy(image->filename,filename);
  return(status);
}
