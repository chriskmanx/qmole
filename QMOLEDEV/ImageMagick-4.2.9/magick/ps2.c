/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            PPPP   SSSSS  22222                              %
%                            P   P  SS        22                              %
%                            PPPP    SSS    222                               %
%                            P         SS  22                                 %
%                            P      SSSSS  22222                              %
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
#if defined(HasTIFF)
#define CCITTParam  "-1"
#else
#define CCITTParam  "0"
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e P S 2 I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WritePS2Image translates an image to encapsulated Postscript
%  Level II for printing.  If the supplied geometry is null, the image is
%  centered on the Postscript page.  Otherwise, the image is positioned as
%  specified by the geometry.
%
%  The format of the WritePS2Image method is:
%
%      unsigned int WritePS2Image(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status: Method WritePS2Image return True if the image is printed.
%      False is returned if the image file cannot be opened for printing.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image;  returned from
%      ReadImage.
%
%
*/
Export unsigned int WritePS2Image(const ImageInfo *image_info,Image *image)
{
  static const char
    *PostscriptProlog[]=
    {
      "%%%%BeginProlog",
      "%%",
      "%% Display a color image.  The image is displayed in color on",
      "%% Postscript viewers or printers that support color, otherwise",
      "%% it is displayed as grayscale.",
      "%%",
      "/buffer 512 string def",
      "/pixel_stream currentfile /ASCII85Decode filter def",
      "",
      "/DirectClassImage",
      "{",
      "  %%",
      "  %% Display a DirectClass image.",
      "  %%",
      "  colorspace 0 eq",
      "  {",
      "    /DeviceRGB setcolorspace",
      "    <<",
      "      /ImageType 1",
      "      /Width columns",
      "      /Height rows",
      "      /BitsPerComponent 8",
      "      /Decode [0 1 0 1 0 1]",
      "      /ImageMatrix [columns 0 0 rows neg 0 rows]",
      "      compression 0 gt",
      "      { /DataSource pixel_stream }",
      "      { /DataSource pixel_stream /%.1024s filter } ifelse",
      "    >> image",
      "  }",
      "  {",
      "    /DeviceCMYK setcolorspace",
      "    <<",
      "      /ImageType 1",
      "      /Width columns",
      "      /Height rows",
      "      /BitsPerComponent 8",
      "      /Decode [0 1 0 1 0 1 0 1]",
      "      /ImageMatrix [columns 0 0 rows neg 0 rows]",
      "      compression 0 gt",
      "      { /DataSource pixel_stream }",
      "      { /DataSource pixel_stream /%.1024s filter } ifelse",
      "    >> image",
      "  } ifelse",
      "} bind def",
      "",
      "/PseudoClassImage",
      "{",
      "  %%",
      "  %% Display a PseudoClass image.",
      "  %%",
      "  %% Parameters:",
      "  %%   colors: number of colors in the colormap.",
      "  %%",
      "  currentfile buffer readline pop",
      "  token pop /colors exch def pop",
      "  colors 0 eq",
      "  {",
      "    %%",
      "    %% Image is grayscale.",
      "    %%",
      "    /DeviceGray setcolorspace",
      "    <<",
      "      /ImageType 1",
      "      /Width columns",
      "      /Height rows",
      "      /BitsPerComponent 1",
      "      /Decode [0 1]",
      "      /ImageMatrix [columns 0 0 rows neg 0 rows]",
      "      compression 0 gt",
      "      { /DataSource pixel_stream }",
      "      {",
      "        /DataSource pixel_stream",
      "        <<",
      "           /K "CCITTParam,
      "           /Columns columns",
      "           /Rows rows",
      "        >> /CCITTFaxDecode filter",
      "      } ifelse",
      "    >> image",
      "  }",
      "  {",
      "    %%",
      "    %% Parameters:",
      "    %%   colormap: red, green, blue color packets.",
      "    %%",
      "    /colormap colors 3 mul string def",
      "    currentfile colormap readhexstring pop pop",
      "    [ /Indexed /DeviceRGB colors 1 sub colormap ] setcolorspace",
      "    <<",
      "      /ImageType 1",
      "      /Width columns",
      "      /Height rows",
      "      /BitsPerComponent 8",
      "      /Decode [0 255]",
      "      /ImageMatrix [columns 0 0 rows neg 0 rows]",
      "      compression 0 gt",
      "      { /DataSource pixel_stream }",
      "      { /DataSource pixel_stream /%.1024s filter } ifelse",
      "    >> image",
      "  } ifelse",
      "} bind def",
      "",
      "/DisplayImage",
      "{",
      "  %%",
      "  %% Display a DirectClass or PseudoClass image.",
      "  %%",
      "  %% Parameters:",
      "  %%   x & y translation.",
      "  %%   x & y scale.",
      "  %%   label pointsize.",
      "  %%   image label.",
      "  %%   image columns & rows.",
      "  %%   class: 0-DirectClass or 1-PseudoClass.",
      "  %%   colorspace: 0-RGB or 1-CMYK.",
      "  %%   compression: 0-RunlengthEncodedCompression or 1-NoCompression.",
      "  %%   hex color packets.",
      "  %%",
      "  gsave",
      "  currentfile buffer readline pop",
      "  token pop /x exch def",
      "  token pop /y exch def pop",
      "  x y translate",
      "  currentfile buffer readline pop",
      "  token pop /x exch def",
      "  token pop /y exch def pop",
      "  currentfile buffer readline pop",
      "  token pop /pointsize exch def pop",
      "  /Helvetica findfont pointsize scalefont setfont",
      (char *) NULL
    },
    *PostscriptEpilog[]=
    {
      "  x y scale",
      "  currentfile buffer readline pop",
      "  token pop /columns exch def",
      "  token pop /rows exch def pop",
      "  currentfile buffer readline pop",
      "  token pop /class exch def pop",
      "  currentfile buffer readline pop",
      "  token pop /colorspace exch def pop",
      "  currentfile buffer readline pop",
      "  token pop /compression exch def pop",
      "  class 0 gt { PseudoClassImage } { DirectClassImage } ifelse",
      "  grestore",
      (char *) NULL
    };

  char
    buffer[MaxTextExtent],
    date[MaxTextExtent],
    density[MaxTextExtent],
    **labels;

  const char
    **q;

  CompressionType
    compression;

  double
    dx_resolution,
    dy_resolution,
    x_resolution,
    x_scale,
    y_resolution,
    y_scale;

  int
    count,
    status,
    x,
    y;

  register RunlengthPacket
    *p;

  register int
    i,
    j;

  SegmentInfo
    bounding_box;

  time_t
    timer;

  unsigned char
    *pixels;

  unsigned int
    height,
    page,
    scene,
    text_size,
    width;

  unsigned long
    number_packets;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  compression=image_info->compression;
#if defined(HasLZW)
  if (compression == UndefinedCompression)
    compression=LZWCompression;
#endif
  page=1;
  scene=0;
  do
  {
    /*
      Scale image to size of Postscript page.
    */
    text_size=0;
    if (image->label != (char *) NULL)
      text_size=MultilineCensus(image->label)*image_info->pointsize+12;
    width=image->columns;
    height=image->rows;
    x=0;
    y=text_size;
    if (image_info->page != (char *) NULL)
      (void) ParseImageGeometry(image_info->page,&x,&y,&width,&height);
    else
      if (image->page != (char *) NULL)
        (void) ParseImageGeometry(image->page,&x,&y,&width,&height);
      else
        if (Latin1Compare(image_info->magick,"PS2") == 0)
          (void) ParseImageGeometry(PSPageGeometry,&x,&y,&width,&height);
    /*
      Scale relative to dots-per-inch.
    */
    dx_resolution=72.0;
    dy_resolution=72.0;
    x_resolution=72.0;
    (void) strcpy(density,PSDensityGeometry);
    count=sscanf(density,"%lfx%lf",&x_resolution,&y_resolution);
    if (count != 2)
      y_resolution=x_resolution;
    if (image_info->density != (char *) NULL)
      {
        count=sscanf(image_info->density,"%lfx%lf",&x_resolution,&y_resolution);
        if (count != 2)
          y_resolution=x_resolution;
      }
    x_scale=(width*dx_resolution)/x_resolution;
    width=(unsigned int) (x_scale+0.5);
    y_scale=(height*dy_resolution)/y_resolution;
    height=(unsigned int) (y_scale+0.5);
    if (page == 1)
      {
        /*
          Output Postscript header.
        */
        if (Latin1Compare(image_info->magick,"PS2") == 0)
          (void) strcpy(buffer,"%!PS-Adobe-3.0\n");
        else
          (void) strcpy(buffer,"%!PS-Adobe-3.0 EPSF-3.0\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"%%Creator: (ImageMagick)\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) sprintf(buffer,"%%Title: (%.1024s)\n",image->filename);
        (void) WriteBlob(image,strlen(buffer),buffer);
        timer=time((time_t *) NULL);
        (void) localtime(&timer);
        (void) strcpy(date,ctime(&timer));
        date[Extent(date)-1]='\0';
        (void) sprintf(buffer,"%%%%CreationDate: (%.1024s)\n",date);
        (void) WriteBlob(image,strlen(buffer),buffer);
        bounding_box.x1=x;
        bounding_box.y1=y;
        bounding_box.x2=x+width-1;
        bounding_box.y2=y+(height+text_size)-1;
        if (image_info->adjoin && (image->next != (Image *) NULL))
          (void) strcpy(buffer,"%%BoundingBox: (atend)\n");
        else
          (void) sprintf(buffer,"%%%%BoundingBox: %g %g %g %g\n",
            bounding_box.x1,bounding_box.y1,bounding_box.x2,bounding_box.y2);
        (void) WriteBlob(image,strlen(buffer),buffer);
        if (image->label != (char *) NULL)
          {
            (void) strcpy(buffer,
              "%%%%DocumentNeededResources: font Helvetica\n");
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        (void) strcpy(buffer,"%%LanguageLevel: 2\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        if (Latin1Compare(image_info->magick,"PS2") != 0)
          {
            (void) sprintf(buffer,"%%%%Pages: 0\n");
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        else
          {
            (void) strcpy(buffer,"%%Orientation: Portrait\n");
            (void) WriteBlob(image,strlen(buffer),buffer);
            (void) strcpy(buffer,"%%PageOrder: Ascend\n");
            (void) WriteBlob(image,strlen(buffer),buffer);
            if (!image_info->adjoin)
              (void) strcpy(buffer,"%%Pages: 0\n");
            else
              (void) sprintf(buffer,"%%%%Pages: %u\n",GetNumberScenes(image));
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        (void) strcpy(buffer,"%%EndComments\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"\n%%BeginDefaults\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"%%PageOrientation: Portrait\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"%%EndDefaults\n\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        /*
          Output Postscript commands.
        */
        for (q=PostscriptProlog; *q; q++)
        {
          (void) sprintf(buffer,*q,
            compression == ZipCompression ? "FlateDecode" :
            compression == LZWCompression ? "LZWDecode" : "RunLengthDecode");
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) WriteByte(image,'\n');
        }
        for (i=MultilineCensus(image->label)-1; i >= 0; i--)
        {
          (void) strcpy(buffer,"  /label 512 string def\n");
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) strcpy(buffer,"  currentfile label readline pop\n");
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) sprintf(buffer,"  0 y %d add moveto label show pop\n",
            i*image_info->pointsize+12);
          (void) WriteBlob(image,strlen(buffer),buffer);
        }
        for (q=PostscriptEpilog; *q; q++)
          {
            (void) sprintf(buffer,"%.255s\n",*q);
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        if (Latin1Compare(image_info->magick,"PS2") == 0)
          {
            (void) strcpy(buffer,"  showpage\n");
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
        (void) strcpy(buffer,"} bind def\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"%%EndProlog\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    (void) sprintf(buffer,"%%%%Page:  1 %u\n",page++);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"%%%%PageBoundingBox: %d %d %d %d\n",x,y,
      x+(int) width-1,y+(int) (height+text_size)-1);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (x < bounding_box.x1)
      bounding_box.x1=x;
    if (y < bounding_box.y1)
      bounding_box.y1=y;
    if ((x+(int) width-1) > bounding_box.x2)
      bounding_box.x2=x+width-1;
    if ((y+(int) (height+text_size)-1) > bounding_box.y2)
      bounding_box.y2=y+(height+text_size)-1;
    if (image->label != (char *) NULL)
      {
        (void) strcpy(buffer,"%%PageResources: font Helvetica\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (Latin1Compare(image_info->magick,"PS2") != 0)
      {
        (void) strcpy(buffer,"userdict begin\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    (void) strcpy(buffer,"%%BeginData:\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"DisplayImage\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Output image data.
    */
    labels=StringToList(image->label);
    (void) sprintf(buffer,"%d %d\n%g %g\n%u\n",x,y,x_scale,y_scale,
      image_info->pointsize);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (labels != (char **) NULL)
      {
        for (i=0; labels[i] != (char *) NULL; i++)
        {
          (void) sprintf(buffer,"%.1024s \n",labels[i]);
          (void) WriteBlob(image,strlen(buffer),buffer);
          FreeMemory(labels[i]);
        }
        FreeMemory(labels);
      }
    (void) sprintf(buffer,"%u %u\n%u\n%d\n%d\n",image->columns,image->rows,
      IsPseudoClass(image),(int) (image->colorspace == CMYKColorspace),
      (int) (compression == NoCompression));
    (void) WriteBlob(image,strlen(buffer),buffer);
    p=image->pixels;
    if (!IsPseudoClass(image) && !IsGrayImage(image))
      switch (compression)
      {
        case RunlengthEncodedCompression:
        default:
        {
          register unsigned char
            *q;

          /*
            Allocate pixel array.
          */
          number_packets=(image->colorspace == CMYKColorspace ? 4 : 3)*
            image->columns*image->rows;
          pixels=(unsigned char *)
            AllocateMemory(number_packets*sizeof(unsigned char));
          if (pixels == (unsigned char *) NULL)
            WriterExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          /*
            Dump Packbit encoded pixels.
          */
          q=pixels;
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              if (image->matte && (p->index == Transparent))
                {
                  *q++=DownScale(MaxRGB);
                  *q++=DownScale(MaxRGB);
                  *q++=DownScale(MaxRGB);
                }
              else
                if (image->colorspace != CMYKColorspace)
                  {
                    *q++=DownScale(p->red);
                    *q++=DownScale(p->green);
                    *q++=DownScale(p->blue);
                  }
                else
                  {
                    *q++=DownScale(p->red);
                    *q++=DownScale(p->green);
                    *q++=DownScale(p->blue);
                    *q++=DownScale(p->index);
                  }
            }
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
          if (compression == ZipCompression)
            status=
              ZLIBEncodeImage(image,number_packets,image_info->quality,pixels);
          else
            if (compression == LZWCompression)
              status=LZWEncodeImage(image,number_packets,pixels);
            else
              status=PackbitsEncodeImage(image,number_packets,pixels);
          if (!status)
            {
              CloseBlob(image);
              return(False);
            }
          FreeMemory((char *) pixels);
          break;
        }
        case NoCompression:
        {
          /*
            Dump uncompressed DirectColor packets.
          */
          Ascii85Initialize();
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              if (image->matte && (p->index == Transparent))
                {
                  Ascii85Encode(image,DownScale(MaxRGB));
                  Ascii85Encode(image,DownScale(MaxRGB));
                  Ascii85Encode(image,DownScale(MaxRGB));
                }
              else
                if (image->colorspace != CMYKColorspace)
                  {
                    Ascii85Encode(image,DownScale(p->red));
                    Ascii85Encode(image,DownScale(p->green));
                    Ascii85Encode(image,DownScale(p->blue));
                  }
                else
                  {
                    Ascii85Encode(image,DownScale(p->red));
                    Ascii85Encode(image,DownScale(p->green));
                    Ascii85Encode(image,DownScale(p->blue));
                    Ascii85Encode(image,DownScale(p->index));
                  }
            }
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
          Ascii85Flush(image);
          break;
        }
      }
    else
      if (IsFaxImage(image) && (compression != LZWCompression))
        {
          register unsigned char
            bit,
            byte,
            polarity;

          polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
          if (image->colors == 2)
            polarity=
              Intensity(image->colormap[0]) < Intensity(image->colormap[1]);
          bit=0;
          byte=0;
          x=0;
          (void) WriteByte(image,'\0');
          (void) WriteByte(image,'\n');
          switch (compression)
          {
            case RunlengthEncodedCompression:
            default:
            {
              if (Latin1Compare(CCITTParam,"0") == 0)
                (void) HuffmanEncodeImage((ImageInfo *) image_info,image);
              else
                (void) Huffman2DEncodeImage((ImageInfo *) image_info,image);
              break;
            }
            case NoCompression:
            {
              /*
                Dump uncompressed PseudoColor packets.
              */
              Ascii85Initialize();
              for (i=0; i < (int) image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                {
                  byte<<=1;
                  if (p->index == polarity)
                    byte|=0x01;
                  bit++;
                  if (bit == 8)
                    {
                      Ascii85Encode(image,byte);
                      bit=0;
                      byte=0;
                    }
                  x++;
                  if (x == (int) image->columns)
                    {
                      /*
                        Advance to the next scanline.
                      */
                      if (bit != 0)
                        Ascii85Encode(image,byte << (8-bit));
                      if (image->previous == (Image *) NULL)
                        if (QuantumTick(y,image->rows))
                          ProgressMonitor(SaveImageText,y,image->rows);
                      bit=0;
                      byte=0;
                      x=0;
                   }
                }
                p++;
              }
              Ascii85Flush(image);
              break;
            }
          }
        }
      else
        {
          /*
            Dump number of colors and colormap.
          */
          (void) sprintf(buffer,"%u\n",image->colors);
          (void) WriteBlob(image,strlen(buffer),buffer);
          for (i=0; i < (int) image->colors; i++)
          {
            (void) sprintf(buffer,"%02lx%02lx%02lx\n",
              DownScale(image->colormap[i].red),
              DownScale(image->colormap[i].green),
              DownScale(image->colormap[i].blue));
            (void) WriteBlob(image,strlen(buffer),buffer);
          }
          switch (compression)
          {
            case RunlengthEncodedCompression:
            default:
            {
              register unsigned char
                *q;

              /*
                Allocate pixel array.
              */
              number_packets=image->columns*image->rows;
              pixels=(unsigned char *)
                AllocateMemory(number_packets*sizeof(unsigned char));
              if (pixels == (unsigned char *) NULL)
                WriterExit(ResourceLimitWarning,"Memory allocation failed",
                  image);
              /*
                Dump Runlength encoded pixels.
              */
              q=pixels;
              for (i=0; i < (int) image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                  *q++=(unsigned char) p->index;
                p++;
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(i,image->packets))
                    ProgressMonitor(SaveImageText,i,image->packets);
              }
              if (compression == ZipCompression)
                status=ZLIBEncodeImage(image,number_packets,image_info->quality,
                  pixels);
              else
                if (compression == LZWCompression)
                  status=LZWEncodeImage(image,number_packets,pixels);
                else
                  status=PackbitsEncodeImage(image,number_packets,pixels);
              FreeMemory((char *) pixels);
              if (!status)
                {
                  CloseBlob(image);
                  return(False);
                }
              break;
            }
            case NoCompression:
            {
              /*
                Dump uncompressed PseudoColor packets.
              */
              Ascii85Initialize();
              for (i=0; i < (int) image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                  Ascii85Encode(image,(unsigned char) p->index);
                p++;
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(i,image->packets))
                    ProgressMonitor(SaveImageText,i,image->packets);
              }
              Ascii85Flush(image);
              break;
            }
          }
        }
    (void) WriteByte(image,'\n');
    (void) strcpy(buffer,"%%EndData\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (Latin1Compare(image_info->magick,"PS2") != 0)
      {
        (void) strcpy(buffer,"end\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    (void) strcpy(buffer,"%%PageTrailer\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (image->next == (Image *) NULL)
      break;
    image->next->file=image->file;
    image=image->next;
    ProgressMonitor(SaveImagesText,scene++,GetNumberScenes(image));
  } while (image_info->adjoin);
  if (image_info->adjoin)
    while (image->previous != (Image *) NULL)
      image=image->previous;
  (void) strcpy(buffer,"%%Trailer\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  if (page > 1)
    {
      (void) sprintf(buffer,"%%%%BoundingBox: %g %g %g %g\n",
        bounding_box.x1,bounding_box.y1,bounding_box.x2,bounding_box.y2);
      (void) WriteBlob(image,strlen(buffer),buffer);
    }
  (void) strcpy(buffer,"%%EOF\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  CloseBlob(image);
  return(True);
}
