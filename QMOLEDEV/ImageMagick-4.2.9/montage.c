/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%             M   M   OOO   N   N  TTTTT   AAA    GGGG  EEEEE                 %
%             MM MM  O   O  NN  N    T    A   A  G      E                     %
%             M M M  O   O  N N N    T    AAAAA  G  GG  EEE                   %
%             M   M  O   O  N  NN    T    A   A  G   G  E                     %
%             M   M   OOO   N   N    T    A   A   GGGG  EEEEE                 %
%                                                                             %
%                                                                             %
%              Montage Magick Image File Format Image via X11.                %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                              July 1992                                      %
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
%  Montage creates a composite image by combining several separate
%  images.  The images are tiled on the composite image with the name of
%  the image appearing just above the individual tile.
%
%  The composite image is constructed in the following manner.  First,
%  each image specified on the command line, except for the last, is
%  scaled to fit the maximum tile size.  The maximum tile size by default
%  is 256x256.  It can be modified with the -geometry command line
%  argument or X resource.  Note that the maximum tile size need not be a
%  square.  To respect the aspect ratio of each image append ~ to the
%  geometry specification.
%
%  Next the composite image is initialized with the color specified by the
%  -background command line argument or X resource.  The width and height
%  of the composite image is determined by the maximum tile size, the
%  number of tiles per row, the tile border width and height, the image
%  border width, and the label height.  The number of tiles per row specifies
%  how many images are to appear in each row of the composite image.  The
%  default is to have an equal number of images in each row and column of the
%  composite.  This value can be specified with -tiles.  The tile border
%  width and height, and the image border width defaults to the value of the
%  X resource -borderwidth.  It can be changed with the -borderwidth or
%  -geometry command line argument or X resource.  The label height is
%  determined by the font you specify with the -font command line argument or
%  X resource.  If you do not specify a font, a font is chosen that allows
%  the name of the image to fit the maximum width of a tiled area.  The label
%  colors is determined by the -background and -pen command line
%  argument or X resource.  Note, that if the background and pen colors
%  are the same, labels will not appear.
%
%  Finally, each image is set onto the composite image, surrounded by its
%  border color, with its name centered just below it.  The individual images
%  are centered within the width of the tiled area.  The final argument on the
%  command line is the name assigned to the composite image.  The image is
%  written in the MIFF format and may by viewed or printed with `display'.
%
%  The Montage program command syntax is:
%
%  Usage: montage [options ...] file [ [options ...] file ...] file
%
%  Where options include:
%    -adjoin             join images into a single multi-image file
%    -blur factor        apply a filter to blur the image
%    -colors value       preferred number of colors in the image
%    -colorspace type    alternate image colorspace
%    -comment string     annotate image with comment
%    -compose operator   composite operator
%    -compress type      type of image compression
%    -crop geometry      preferred size and location of the cropped image
%    -density geometry   vertical and horizontal density of the image
%    -display server     query fonts from this X server
%    -dispose method     GIF disposal method
%    -dither             apply Floyd/Steinberg error diffusion to image
%    -draw string        annotate the image with a graphic primitive
%    -filter type        use this filter when resizing an image
%    -frame geometry     surround image with an ornamental border
%    -gamma value        level of gamma correction
%    -geometry geometry  preferred tile and border sizes
%    -gravity direction  which direction to gravitate towards
%    -interlace type     None, Line, Plane, or Partition
%    -label name         assign a label to an image
%    -matte              store matte channel if the image has one
%    -mode type          Frame, Unframe, or Concatenate
%    -monochrome         transform image to black and white
%    -page geometry      size and location of an image canvas
%    -pen color          font color
%    -pointsize value    pointsize of Postscript font
%    -quality value      JPEG/MIFF/PNG compression level
%    -rotate degrees     apply Paeth rotation to the image
%    -scene value        image scene number
%    -shadow             add a shadow beneath a tile to simulate depth
%    -sharpen factor     apply a filter to sharpen the image
%    -size geometry      width and height of image
%    -texture filename   name of texture to tile onto the image background
%    -tile geometry      number of tiles per row and column
%    -transparent color  make this color transparent within the image
%    -treedepth value    depth of the color classification tree
%    -verbose            print detailed information about the image
%
%  In addition to those listed above, you can specify these standard X
%  resources as command line options:  -background, -bordercolor -borderwidth,
%  -font, -mattecolor, or -title.
%
%  By default, the image format of `file' is determined by its magic
%  number.  To specify a particular image format, precede the filename
%  with an image format name and a colon (i.e. ps:image) or specify the
%  image type as the filename suffix (i.e. image.ps).  Specify 'file' as
%  '-' for standard input or output.
%
%
*/

/*
  Include declarations.
*/
#if !defined(macintosh)
#include "magick/magick.h"
#include "magick/defines.h"
#else
#include "magick.h"
#include "defines.h"
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   U s a g e                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method Usage displays the program command syntax.
%
%  The format of the Usage method is:
%
%      void Usage(const char *client_name)
%
%    o client_name: a character string representing the name of the client
%      program.
%
%
*/
static void Usage(const char *client_name)
{
  const char
    **p;

  static const char
    *options[]=
    {
      "-adjoin             join images into a single multi-image file",
      "-blur factor        apply a filter to blur the image",
      "-colors value       preferred number of colors in the image",
      "-colorspace type    alternate image colorsapce",
      "-comment string     annotate image with comment",
      "-compose operator   composite operator",
      "-compress type      type of image compression",
      "-crop geometry      preferred size and location of the cropped image",
      "-density geometry   vertical and horizontal density of the image",
      "-display server     query font from this X server",
      "-dispose method     GIF disposal method",
      "-dither             apply Floyd/Steinberg error diffusion to image",
      "-draw string        annotate the image with a graphic primitive",
      "-filter type        use this filter when resizing an image",
      "-frame geometry     surround image with an ornamental border",
      "-gamma value        level of gamma correction",
      "-geometry geometry  preferred tile and border sizes",
      "-gravity direction  which direction to gravitate towards",
      "-interlace type     None, Line, Plane, or Partition",
      "-label name         assign a label to an image",
      "-matte              store matte channel if the image has one",
      "-mode type          Frame, Unframe, or Concatenate",
      "-monochrome         transform image to black and white",
      "-page geometry      size and location of an image canvas",
      "-pen color          font color",
      "-pointsize value    pointsize of Postscript font",
      "-quality value      JPEG/MIFF/PNG compression level",
      "-rotate degrees     apply Paeth rotation to the image",
      "-scene value        image scene number",
      "-shadow             add a shadow beneath a tile to simulate depth",
      "-size geometry      width and height of image",
      "-texture filename   name of texture to tile onto the image background",
      "-tile geometry      number of tiles per row and column",
      "-transparent color  make this color transparent within the image",
      "-treedepth value    depth of the color classification tree",
      "-verbose            print detailed information about the image",
      (char *) NULL
    };

  (void) printf("Version: %.1024s\n",MagickVersion);
  (void) printf("Copyright: %.1024s\n\n",MagickCopyright);
  (void) printf(
    "Usage: %.1024s [-options ...] file [ [-options ...] file ...] file\n",
    client_name);
  (void) printf("\nWhere options include: \n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %.1024s\n",*p);
  (void) printf(
    "\nIn addition to those listed above, you can specify these standard X\n");
  (void) printf(
    "resources as command line options:  -background, -bordercolor,\n");
  (void) printf(
    "-borderwidth, -font, -mattecolor, or -title\n");
  (void) printf(
    "\nBy default, the image format of `file' is determined by its magic\n");
  (void) printf(
    "number.  To specify a particular image format, precede the filename\n");
  (void) printf(
    "with an image format name and a colon (i.e. ps:image) or specify the\n");
  (void) printf(
    "image type as the filename suffix (i.e. image.ps).  Specify 'file' as\n");
  (void) printf("'-' for standard input or output.\n");
  Exit(0);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%    M a i n                                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
int main(int argc,char **argv)
{
  char
    *client_name,
    *option,
    *transparent_color;

  double
    sans;

  Image
    *image,
    *montage_image,
    *next_image,
    *p;

  ImageInfo
    image_info;

  int
    status,
    x;

  MontageInfo
    montage_info;

  QuantizeInfo
    quantize_info;

  register int
    i;

  unsigned int
    first_scene,
    last_scene,
    scene;

  /*
    Initialize command line arguments.
  */
  ReadCommandlLine(argc,&argv);
  client_name=SetClientName(*argv);
  (void) ExpandFilenames(&argc,&argv);
  if (argc < 3)
    Usage(client_name);
  /*
    Set defaults.
  */
  first_scene=0;
  image=(Image *) NULL;
  last_scene=0;
  GetImageInfo(&image_info);
  image_info.coalesce_frames=True;
  GetMontageInfo(&montage_info);
  GetQuantizeInfo(&quantize_info);
  scene=0;
  transparent_color=(char *) NULL;
  /*
    Parse command line.
  */
  for (i=1; i < (argc-1); i++)
  {
    option=argv[i];
    if ((Extent(option) > 1) && ((*option == '-') || (*option == '+')))
      switch (*(option+1))
      {
        case 'a':
        {
          if (strncmp("adjoin",option+1,2) == 0)
            {
              image_info.adjoin=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'b':
        {
          if ((strncmp("background",option+1,5) == 0) ||
              (strncmp("bg",option+1,2) == 0))
            {
              image_info.background_color=(char *) NULL;
              montage_info.background_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing color",option);
                  (void) CloneString(&image_info.background_color,argv[i]);
                  (void) CloneString(&montage_info.background_color,argv[i]);
                }
              break;
            }
          if (strncmp("blur",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing factor",option);
                }
              break;
            }
          if (strncmp("bordercolor",option+1,7) == 0)
            {
              image_info.border_color=(char *) NULL;
              montage_info.border_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing color",option);
                  (void) CloneString(&image_info.border_color,argv[i]);
                  (void) CloneString(&montage_info.border_color,argv[i]);
                }
              break;
            }
          if (strncmp("borderwidth",option+1,7) == 0)
            {
              montage_info.border_width=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing width",option);
                  montage_info.border_width=atoi(argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'c':
        {
          if (strncmp("colors",option+1,7) == 0)
            {
              quantize_info.number_colors=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing colors",option);
                  quantize_info.number_colors=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("colorspace",option+1,7) == 0)
            {
              quantize_info.colorspace=RGBColorspace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  quantize_info.colorspace=UndefinedColorspace;
                  if (Latin1Compare("cmyk",option) == 0)
                    quantize_info.colorspace=CMYKColorspace;
                  if (Latin1Compare("gray",option) == 0)
                    {
                      quantize_info.colorspace=GRAYColorspace;
                      quantize_info.number_colors=256;
                      quantize_info.tree_depth=8;
                    }
                  if (Latin1Compare("ohta",option) == 0)
                    quantize_info.colorspace=OHTAColorspace;
                  if (Latin1Compare("rgb",option) == 0)
                    quantize_info.colorspace=RGBColorspace;
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
                  if (quantize_info.colorspace == UndefinedColorspace)
                    MagickError(OptionError,"Invalid colorspace type",option);
                }
              break;
            }
          if (strncmp("comment",option+1,4) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing comment",option);
                }
              break;
            }
          if (strncmp("compress",option+1,5) == 0)
            {
              image_info.compression=NoCompression;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  image_info.compression=UndefinedCompression;
                  if (Latin1Compare("None",option) == 0)
                    image_info.compression=NoCompression;
                  if (Latin1Compare("BZip",option) == 0)
                    image_info.compression=BZipCompression;
                  if (Latin1Compare("Fax",option) == 0)
                    image_info.compression=FaxCompression;
                  if (Latin1Compare("Group4",option) == 0)
                    image_info.compression=Group4Compression;
                  if (Latin1Compare("JPEG",option) == 0)
                    image_info.compression=JPEGCompression;
                  if (Latin1Compare("LZW",option) == 0)
                    image_info.compression=LZWCompression;
                  if (Latin1Compare("RunlengthEncoded",option) == 0)
                    image_info.compression=RunlengthEncodedCompression;
                  if (Latin1Compare("Zip",option) == 0)
                    image_info.compression=ZipCompression;
                  if (image_info.compression == UndefinedCompression)
                    MagickError(OptionError,"Invalid compression type",option);
                }
              break;
            }
          if (strncmp("compose",option+1,5) == 0)
            {
              montage_info.compose=ReplaceCompositeOp;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  montage_info.compose=UndefinedCompositeOp;
                  if (Latin1Compare("Over",option) == 0)
                    montage_info.compose=OverCompositeOp;
                  if (Latin1Compare("In",option) == 0)
                    montage_info.compose=InCompositeOp;
                  if (Latin1Compare("Out",option) == 0)
                    montage_info.compose=OutCompositeOp;
                  if (Latin1Compare("Atop",option) == 0)
                    montage_info.compose=AtopCompositeOp;
                  if (Latin1Compare("Xor",option) == 0)
                    montage_info.compose=XorCompositeOp;
                  if (Latin1Compare("Plus",option) == 0)
                    montage_info.compose=PlusCompositeOp;
                  if (Latin1Compare("Minus",option) == 0)
                    montage_info.compose=MinusCompositeOp;
                  if (Latin1Compare("Add",option) == 0)
                    montage_info.compose=AddCompositeOp;
                  if (Latin1Compare("Subtract",option) == 0)
                    montage_info.compose=SubtractCompositeOp;
                  if (Latin1Compare("Difference",option) == 0)
                    montage_info.compose=DifferenceCompositeOp;
                  if (Latin1Compare("Bumpmap",option) == 0)
                    montage_info.compose=BumpmapCompositeOp;
                  if (Latin1Compare("Replace",option) == 0)
                    montage_info.compose=ReplaceCompositeOp;
                  if (Latin1Compare("ReplaceRed",option) == 0)
                    montage_info.compose=ReplaceRedCompositeOp;
                  if (Latin1Compare("ReplaceGreen",option) == 0)
                    montage_info.compose=ReplaceGreenCompositeOp;
                  if (Latin1Compare("ReplaceBlue",option) == 0)
                    montage_info.compose=ReplaceBlueCompositeOp;
                  if (Latin1Compare("ReplaceMatte",option) == 0)
                    montage_info.compose=ReplaceMatteCompositeOp;
                  if (montage_info.compose == UndefinedCompositeOp)
                    MagickError(OptionError,"Invalid compose type",option);
                }
              break;
            }
          if (strncmp("crop",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'd':
        {
          if (strncmp("density",option+1,3) == 0)
            {
              image_info.density=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&image_info.density,argv[i]);
                }
              break;
            }
          if (Latin1Compare("display",option+1) == 0)
            {
              image_info.server_name=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing server name",option);
                  (void) CloneString(&image_info.server_name,argv[i]);
                }
              break;
            }
          if (strncmp("dispose",option+1,5) == 0)
            {
              image_info.dispose=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing method",option);
                  image_info.dispose=PostscriptGeometry(argv[i]);
                }
              break;
            }
          if (strncmp("dither",option+1,3) == 0)
            {
              quantize_info.dither=(*option == '-');
              break;
            }
          if (strncmp("draw",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing primitive",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'f':
        {
          if (strncmp("filter",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  FilterType
                    filter;

                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  filter=UndefinedFilter;
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
                  if (filter == UndefinedFilter)
                    MagickError(OptionError,"Invalid filter type",option);
                }
              break;
            }
          if (strncmp("font",option+1,3) == 0)
            {
              image_info.font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing font name",option);
                  (void) CloneString(&image_info.font,argv[i]);
                  (void) CloneString(&montage_info.font,image_info.font);
                }
              break;
            }
          if (strncmp("frame",option+1,2) == 0)
            {
              montage_info.frame=(char *) NULL;
              argv[i]="+sans";
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&montage_info.frame,argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'g':
        {
          if (strncmp("gamma",option+1,2) == 0)
            {
              i++;
              if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                MagickError(OptionError,"Missing value",option);
              break;
            }
          if (strncmp("geometry",option+1,2) == 0)
            {
              montage_info.geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&montage_info.geometry,argv[i]);
                }
              break;
            }
          if (strncmp("gravity",option+1,2) == 0)
            {
              montage_info.gravity=SouthGravity;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  montage_info.gravity=(~0);
                  if (Latin1Compare("Forget",option) == 0)
                    montage_info.gravity=ForgetGravity;
                  if (Latin1Compare("NorthWest",option) == 0)
                    montage_info.gravity=NorthWestGravity;
                  if (Latin1Compare("North",option) == 0)
                    montage_info.gravity=NorthGravity;
                  if (Latin1Compare("NorthEast",option) == 0)
                    montage_info.gravity=NorthEastGravity;
                  if (Latin1Compare("West",option) == 0)
                    montage_info.gravity=WestGravity;
                  if (Latin1Compare("Center",option) == 0)
                    montage_info.gravity=CenterGravity;
                  if (Latin1Compare("East",option) == 0)
                    montage_info.gravity=EastGravity;
                  if (Latin1Compare("SouthWest",option) == 0)
                    montage_info.gravity=SouthWestGravity;
                  if (Latin1Compare("South",option) == 0)
                    montage_info.gravity=SouthGravity;
                  if (Latin1Compare("SouthEast",option) == 0)
                    montage_info.gravity=SouthEastGravity;
                  if (Latin1Compare("Static",option) == 0)
                    montage_info.gravity=StaticGravity;
                  if (montage_info.gravity == (unsigned int) (~0))
                    MagickError(OptionError,"Invalid gravity type",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'h':
        {
          if (strncmp("help",option+1,2) == 0)
            {
              Usage(client_name);
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'i':
        {
          if (strncmp("interlace",option+1,3) == 0)
            {
              image_info.interlace=NoInterlace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  image_info.interlace=UndefinedInterlace;
                  if (Latin1Compare("None",option) == 0)
                    image_info.interlace=NoInterlace;
                  if (Latin1Compare("Line",option) == 0)
                    image_info.interlace=LineInterlace;
                  if (Latin1Compare("Plane",option) == 0)
                    image_info.interlace=PlaneInterlace;
                  if (Latin1Compare("Partition",option) == 0)
                    image_info.interlace=PartitionInterlace;
                  if (image_info.interlace == UndefinedInterlace)
                    MagickError(OptionError,"Invalid interlace type",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'l':
        {
          if (strncmp("label",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing label name",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'm':
        {
          if (Latin1Compare("matte",option+1) == 0)
            break;
          if (strncmp("mattecolor",option+1,6) == 0)
            {
              image_info.matte_color=(char *) NULL;
              montage_info.matte_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing color",option);
                  (void) CloneString(&image_info.matte_color,argv[i]);
                  (void) CloneString(&montage_info.matte_color,argv[i]);
                }
              break;
            }
          if (strncmp("mode",option+1,4) == 0)
            {
              if (*option == '-')
                {
                  MontageMode
                    mode;

                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  mode=UndefinedMode;
                  if (Latin1Compare("frame",option) == 0)
                    {
                      mode=FrameMode;
                      (void) CloneString(&montage_info.frame,"15x15+3+3");
                      montage_info.shadow=True;
                      break;
                    }
                  if (Latin1Compare("unframe",option) == 0)
                    {
                      mode=UnframeMode;
                      montage_info.frame=(char *) NULL;
                      montage_info.shadow=False;
                      montage_info.border_width=0;
                      break;
                    }
                  if (Latin1Compare("concatenate",option) == 0)
                    {
                      mode=ConcatenateMode;
                      montage_info.frame=(char *) NULL;
                      montage_info.shadow=False;
                      (void) CloneString(&montage_info.geometry,"+0+0");
                      montage_info.border_width=0;
                      break;
                    }
                  if (mode == UndefinedMode)
                    MagickError(OptionError,"Invalid mode type",option);
                }
              break;
            }
          if (strncmp("monochrome",option+1,3) == 0)
            {
              image_info.monochrome=(*option == '-');
              if (image_info.monochrome)
                {
                  quantize_info.number_colors=2;
                  quantize_info.tree_depth=8;
                  quantize_info.colorspace=GRAYColorspace;
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'p':
        {
          if (strncmp("page",option+1,3) == 0)
            {
              image_info.page=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing page geometry",option);
                  image_info.page=PostscriptGeometry(argv[i]);
                }
              break;
            }
          if (strncmp("pen",option+1,2) == 0)
            {
              image_info.pen=(char *) NULL;
              montage_info.pen=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing pen color",option);
                  (void) CloneString(&image_info.pen,argv[i]);
                  (void) CloneString(&montage_info.pen,argv[i]);
                }
              break;
            }
          if (strncmp("pointsize",option+1,2) == 0)
            {
              image_info.pointsize=12;
              montage_info.pointsize=12;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing size",option);
                  image_info.pointsize=atoi(argv[i]);
                  montage_info.pointsize=atoi(argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'q':
        {
          if (strncmp("quality",option+1,2) == 0)
            {
              image_info.quality=75;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing quality",option);
                  image_info.quality=atoi(argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'r':
        {
          if (*option == '-')
            {
              i++;
              if ((i == argc) || !IsGeometry(argv[i]))
                MagickError(OptionError,"Missing degrees",option);
            }
          break;
        }
        case 's':
        {
          if (strncmp("scene",option+1,3) == 0)
            {
              first_scene=0;
              last_scene=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing scene number",option);
                  first_scene=atoi(argv[i]);
                  last_scene=first_scene;
                  (void) sscanf(argv[i],"%u-%u",&first_scene,&last_scene);
                }
              break;
            }
          if (strncmp("shadow",option+1,2) == 0)
            {
              montage_info.shadow=(*option == '-');
              break;
            }
          if (strncmp("sharpen",option+1,5) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing factor",option);
                }
              break;
            }
          if (strncmp("size",option+1,2) == 0)
            {
              image_info.size=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&image_info.size,argv[i]);
                }
              break;
            }
          if (strncmp("sharpen",option+1,5) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing factor",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("texture",option+1,5) == 0)
            {
              montage_info.texture=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing filename",option);
                  (void) CloneString(&montage_info.texture,argv[i]);
                }
              break;
            }
          if (strncmp("tile",option+1,3) == 0)
            {
              montage_info.tile=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&montage_info.tile,argv[i]);
                }
              break;
            }
          if (strncmp("title",option+1,3) == 0)
            {
              montage_info.title=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing title",option);
                  (void) CloneString(&montage_info.title,argv[i]);
                }
              break;
            }
          if (strncmp("transparent",option+1,3) == 0)
            {
              transparent_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing color",option);
                  (void) CloneString(&transparent_color,argv[i]);
                }
              break;
            }
          if (strncmp("treedepth",option+1,3) == 0)
            {
              quantize_info.tree_depth=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing depth",option);
                  quantize_info.tree_depth=atoi(argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'v':
        {
          if (strncmp("verbose",option+1,2) == 0)
            {
              image_info.verbose=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case '?':
        {
          Usage(client_name);
          break;
        }
        default:
        {
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
      }
    else
      for (scene=first_scene; scene <= last_scene ; scene++)
      {
        /*
          Option is a file name: begin by reading image from specified file.
        */
        (void) strcpy(image_info.filename,argv[i]);
        if (first_scene != last_scene)
          {
            char
              filename[MaxTextExtent];

            /*
              Form filename for multi-part images.
            */
            FormatString(filename,image_info.filename,scene);
            if (Latin1Compare(filename,image_info.filename) == 0)
              FormatString(filename,"%.1024s.%u",image_info.filename,scene);
            (void) strcpy(image_info.filename,filename);
          }
        image_info.font=montage_info.font;
        image_info.colorspace=quantize_info.colorspace;
        image_info.dither=quantize_info.dither;
        if (image_info.size == (char *) NULL)
          image_info.size=montage_info.geometry;
        next_image=ReadImage(&image_info);
        if (next_image == (Image *) NULL)
          {
            if (*option == '-')
              break;
            else
              continue;
          }
        MogrifyImages(&image_info,i,argv,&next_image);
        if (image == (Image *) NULL)
          image=next_image;
        else
          {
            /*
              Link image into image list.
            */
            for (p=image; p->next != (Image *) NULL; p=p->next);
            next_image->previous=p;
            p->next=next_image;
          }
      }
    }
  if (image == (Image *) NULL)
    MagickError(OptionError,"Missing an image file name",(char *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  /*
    Create composite image.
  */
  (void) strcpy(montage_info.filename,argv[argc-1]);
  montage_image=MontageImages(image,&montage_info);
  if (montage_image == (Image *) NULL)
    MagickError(OptionError,"Missing an image file name",(char *) NULL);
  DestroyImages(image);
  /*
    Write image.
  */
  (void) strcpy(image_info.filename,argv[argc-1]);
  for (p=montage_image; p != (Image *) NULL; p=p->next)
  {
    if (transparent_color != (char *) NULL)
      TransparentImage(p,transparent_color);
    if (quantize_info.number_colors != 0)
      {
        /*
          Reduce the number of colors in the image.
        */
        if ((p->class == DirectClass) ||
            (p->colors > quantize_info.number_colors) ||
            (quantize_info.colorspace == GRAYColorspace))
          (void) QuantizeImage(&quantize_info,p);
        if (image_info.verbose)
          (void) QuantizationError(p);
        SyncImage(p);
      }
    (void) strcpy(p->filename,argv[argc-1]);
  }
  SetImageInfo(&image_info,True);
  for (p=montage_image; p != (Image *) NULL; p=p->next)
  {
    status=WriteImage(&image_info,p);
    if ((status == False) || image_info.adjoin)
      break;
  }
  (void) strcpy(montage_image->magick_filename,argv[argc-1]);
  if (image_info.verbose)
    DescribeImage(montage_image,(FILE *) NULL,False);
  DestroyDelegateInfo();
  Exit(status ? 0 : errno);
  return(False);
}
