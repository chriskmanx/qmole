/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%              M   M   OOO    GGGG  RRRR   IIIII  FFFFF  Y   Y                %
%              MM MM  O   O  G      R   R    I    F       Y Y                 %
%              M M M  O   O  G GG   RRRRR    I    FFF      Y                  %
%              M   M  O   O  G   G  R R      I    F        Y                  %
%              M   M   OOO    GGG   R  R   IIIII  F        Y                  %
%                                                                             %
%                                                                             %
%               Transmogrify an Image or Sequence of Images.                  %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                            December 1992                                    %
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
%  `Mogrify' applies one or more image transforms to an image or sequence of
%   images and overwrites the original image.
%
%  The Mogrify program command syntax is:
%
%  Usage: mogrify [options ...] file [ [options ...] file ...]
%
%  Where options include:
%    -antialias           remove pixel-aliasing
%    -blur factor         apply a filter to blur the image
%    -border geometry     surround image with a border of color
%    -bordercolor color   border color
%    -box color           color for annotation bounding box
%    -charcoal factor     simulate a charcoal drawing
%    -colorize value      colorize the image with the pen color
%    -colors value        preferred number of colors in the image
%    -colorspace type     alternate image colorspace
%    -comment string      annotate image with comment
%    -compress type       type of image compression
%    -contrast            enhance or reduce the image contrast
%    -crop geometry       preferred size and location of the cropped image
%    -cycle amount        cycle the image colormap
%    -delay value         display the next image after pausing
%    -density geometry    vertical and horizontal density of the image
%    -depth value         depth of the image
%    -despeckle           reduce the speckles within an image",
%    -display server      obtain image or font from this X server
%    -dispose method      GIF disposal method
%    -dither              apply Floyd/Steinberg error diffusion to image
%    -draw string         annotate the image with a graphic primitive
%    -edge factor         apply a filter to detect edges in the image
%    -emboss              emboss an image
%    -enhance             apply a digital filter to enhance a noisy image
%    -equalize            perform histogram equalization to an image
%    -filter type         use this filter when resizing an image
%    -flip                flip image in the vertical direction
%    -flop                flop image in the horizontal direction
%    -font name           X11 font for displaying text
%    -format type         image format type
%    -frame geometry      surround image with an ornamental border
%    -fuzz distance       colors within this distance are considered equal
%    -gamma value         level of gamma correction
%    -geometry geometry   perferred size or location of the image
%    -implode amount      implode image pixels about the center
%    -interlace type      None, Line, Plane, or Partition
%    -label name          assign a label to an image
%    -layer type          Red, Green, Blue, Matte
%    -linewidth value     width of line in pixels
%    -loop iterations     add Netscape loop extension to your GIF animation
%    -map filename        transform image colors to match this set of colors
%    -matte               store matte channel if the image has one
%    -modulate value      vary the brightness, saturation, and hue
%    -monochrome          transform image to black and white
%    -negate              replace every pixel with its complementary color 
%    -noise               add or reduce noise in an image
%    -normalize           transform image to span the full range of colors
%    -opaque color        change this color to the pen color
%    -page geometry       size and location of an image canvas
%    -paint radius        simulate an oil painting
%    -pen color           color for annotating or changing opaque color
%    -pointsize value     pointsize of Postscript font
%    -quality value       JPEG/MIFF/PNG compression level
%    -raise value         lighten/darken image edges to create a 3-D effect
%    -region geometry     apply options to a portion of the image
%    -roll geometry       roll an image vertically or horizontally
%    -rotate degrees      apply Paeth rotation to the image
%    -sample geometry     scale image with pixel sampling
%    -scene value         image scene number
%    -seed value          pseudo-random number generator seed value
%    -segment values      segment an image
%    -shade degrees       shade the image using a distant light source
%    -sharpen factor      apply a filter to sharpen the image
%    -shear geometry      slide one edge of the image along the X or Y axis
%    -size geometry       width and height of image
%    -solarize threshold   negate all pixels above the threshold level
%    -spread amount       displace image pixels by a random amount
%    -swirl degrees       swirl image pixels about the center
%    -texture filename    name of texture to tile onto the image background
%    -threshold value     threshold the image
%    -transparent color   make this color transparent within the image
%    -treedepth value     depth of the color classification tree
%    -units type          PixelsPerInch, PixelsPerCentimeter, or Undefined
%    -verbose             print detailed information about the image
%    -view                FlashPix viewing transforms
%    -wave geometry       alter an image along a sine wave
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
      "-antialias           remove pixel-aliasing",
      "-blur factor         apply a filter to blur the image",
      "-border geometry     surround image with a border of color",
      "-bordercolor color   border color",
      "-box color           color for annotation bounding box",
      "-charcoal factor     simulate a charcoal drawing",
      "-colorize value      colorize the image with the pen color",
      "-colors value        preferred number of colors in the image",
      "-colorspace type     alternate image colorspace",
      "-comment string      annotate image with comment",
      "-compress type       type of image compression",
      "-contrast            enhance or reduce the image contrast",
      "-crop geometry       preferred size and location of the cropped image",
      "-cycle amount        cycle the image colormap",
      "-delay value         display the next image after pausing",
      "-density geometry    vertical and horizontal density of the image",
      "-depth value         depth of the image",
      "-despeckle           reduce the speckles within an image",
      "-display server      obtain image or font from this X server",
      "-dispose method      GIF disposal method",
      "-dither              apply Floyd/Steinberg error diffusion to image",
      "-draw string         annotate the image with a graphic primitive",
      "-edge factor         apply a filter to detect edges in the image",
      "-emboss              emboss an image",
      "-enhance             apply a digital filter to enhance a noisy image",
      "-equalize            perform histogram equalization to an image",
      "-filter type         use this filter when resizing an image",
      "-flip                flip image in the vertical direction",
      "-flop                flop image in the horizontal direction",
      "-font name           X11 font for displaying text",
      "-format type         image format type",
      "-frame geometry      surround image with an ornamental border",
      "-fuzz distance       colors within this distance are considered equal",
      "-gamma value         level of gamma correction",
      "-geometry geometry   perferred size or location of the image",
      "-implode amount      implode image pixels about the center",
      "-interlace type      None, Line, Plane, or Partition",
      "-label name          assign a label to an image",
      "-layer type          Red, Green, Blue, Matte",
      "-linewidth value     width of line in pixels",
      "-loop iterations     add Netscape loop extension to your GIF animation",
      "-map filename        transform image colors to match this set of colors",
      "-matte               store matte channel if the image has one",
      "-modulate value      vary the brightness, saturation, and hue",
      "-monochrome          transform image to black and white",
      "-negate              replace every pixel with its complementary color ",
      "-noise               add or reduce noise in an image.",
      "-normalize           transform image to span the full range of colors",
      "-opaque color        change this color to the pen color",
      "-page geometry       size and location of an image canvas",
      "-paint radius        simulate an oil painting",
      "-pen color           color for annotating or changing opaque color",
      "-pointsize value     pointsize of Postscript font",
      "-quality value       JPEG/MIFF/PNG compression level",
      "-raise value         lighten/darken image edges to create a 3-D effect",
      "-region geometry     apply options to a portion of the image",
      "-roll geometry       roll an image vertically or horizontally",
      "-rotate degrees      apply Paeth rotation to the image",
      "-sample geometry     scale image with pixel sampling",
      "-scene number        image scene number",
      "-seed value          pseudo-random number generator seed value",
      "-segment values      segment an image",
      "-shade degrees       shade the image using a distant light source",
      "-sharpen factor      apply a filter to sharpen the image",
      "-shear geometry      slide one edge of the image along the X or Y axis",
      "-size geometry       width and height of image",
      "-solarize threshold   negate all pixels above the threshold level",
      "-spread amount       displace image pixels by a random amount",
      "-swirl degrees       swirl image pixels about the center",
      "-texture filename    name of texture to tile onto the image background",
      "-threshold value     threshold the image",
      "-transparent color   make this color transparent within the image",
      "-treedepth value     depth of the color classification tree",
      "-units type          PixelsPerInch, PixelsPerCentimeter, or Undefined",
      "-verbose             print detailed information about the image",
      "-view                FlashPix viewing transforms",
      "-wave geometry       alter an image along a sine wave",
      (char *) NULL
    };

  (void) printf("Version: %.1024s\n",MagickVersion);
  (void) printf("Copyright: %.1024s\n\n",MagickCopyright);
  (void) printf(
    "Usage: %.1024s [-options ...] file [ [-options ...] file ...]\n",
    client_name);
  (void) printf("\nWhere options include: \n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %.1024s\n",*p);
  (void) printf(
    "\nBy default, the image format of `file' is determined by its magic\n");
  (void) printf(
    "number.  To specify a particular image format, precede the filename\n");
  (void) printf(
    "with an image format name and a colon (i.e. ps:image) or specify the\n");
  (void) printf(
    "image type as the filename suffix (i.e. image.ps).  Specify 'file' as\n");
  (void) printf("'-' for standard input or output.\n");
  ListMagickInfo((FILE *) NULL);
  ListDelegateInfo((FILE *) NULL);
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
    filename[MaxTextExtent],
    *format,
    *option;

  double
    sans;

  ImageInfo
    image_info;

  Image
    *image;

  int
    gravity,
    x;

  register Image
    *p;

  register int
    i;

  unsigned int
    global_colormap,
    scene,
    status;

  /*
    Initialize command line arguments.
  */
  ReadCommandlLine(argc,&argv);
  client_name=SetClientName(*argv);
  (void) ExpandFilenames(&argc,&argv);
  if (argc < 2)
    Usage(client_name);
  /*
    Set defaults.
  */
  format=(char *) NULL;
  global_colormap=False;
  image=(Image *) NULL;
  GetImageInfo(&image_info);
  scene=0;
  status=False;
  /*
    Parse command line.
  */
  for (i=1; i < argc; i++)
  {
    option=argv[i];
    if ((Extent(option) > 1) && ((*option == '-') || (*option == '+')))
      switch (*(option+1))
      {
        case 'a':
        {
          if (strncmp("antialias",option+1,3) == 0)
            {
              image_info.antialias=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'b':
        {
          if (strncmp("background",option+1,5) == 0)
            {
              image_info.background_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing background color",option);
                  (void) CloneString(&image_info.background_color,argv[i]);
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
          if (strncmp("border",option+1,7) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                }
              break;
            }
          if (strncmp("bordercolor",option+1,7) == 0)
            {
              image_info.border_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing border color",option);
                  (void) CloneString(&image_info.border_color,argv[i]);
                }
              break;
            }
          if (strncmp("box",option+1,3) == 0)
            {
              image_info.box=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing box color",option);
                  (void) CloneString(&image_info.box,argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'c':
        {
          if (strncmp("charcoal",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing factor",option);
                }
              break;
            }
          if (strncmp("colorize",option+1,7) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing value",option);
                }
              break;
            }
          if (strncmp("colors",option+1,7) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing colors",option);
                }
              break;
            }
          if (strncmp("colorspace",option+1,7) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  image_info.colorspace=UndefinedColorspace;
                  if (Latin1Compare("cmyk",option) == 0)
                    image_info.colorspace=CMYKColorspace;
                  if (Latin1Compare("gray",option) == 0)
                    image_info.colorspace=GRAYColorspace;
                  if (Latin1Compare("ohta",option) == 0)
                    image_info.colorspace=OHTAColorspace;
                  if (Latin1Compare("rgb",option) == 0)
                    image_info.colorspace=RGBColorspace;
                  if (Latin1Compare("srgb",option) == 0)
                    image_info.colorspace=sRGBColorspace;
                  if (Latin1Compare("transparent",option) == 0)
                    image_info.colorspace=TransparentColorspace;
                  if (Latin1Compare("xyz",option) == 0)
                    image_info.colorspace=XYZColorspace;
                  if (Latin1Compare("ycbcr",option) == 0)
                    image_info.colorspace=YCbCrColorspace;
                  if (Latin1Compare("ycc",option) == 0)
                    image_info.colorspace=YCCColorspace;
                  if (Latin1Compare("yiq",option) == 0)
                    image_info.colorspace=YIQColorspace;
                  if (Latin1Compare("ypbpr",option) == 0)
                    image_info.colorspace=YPbPrColorspace;
                  if (Latin1Compare("yuv",option) == 0)
                    image_info.colorspace=YUVColorspace;
                  if (image_info.colorspace == UndefinedColorspace)
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
          if (strncmp("contrast",option+1,3) == 0)
            break;
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
          if (strncmp("cycle",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing amount",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'd':
        {
          if (strncmp("delay",option+1,3) == 0)
            {
              image_info.delay=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing delay",option);
                  image_info.delay=PostscriptGeometry(argv[i]);
                }
              break;
            }
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
          if (strncmp("depth",option+1,3) == 0)
            {
              image_info.depth=QuantumDepth;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing image depth",option);
                  image_info.depth=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("despeckle",option+1,3) == 0)
            break;
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
              image_info.dither=(*option == '-');
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
        case 'e':
        {
          if (strncmp("edge",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing factor",option);
                }
              break;
            }
          if (strncmp("emboss",option+1,2) == 0)
            break;
          if (strncmp("enhance",option+1,2) == 0)
            break;
          if (strncmp("equalize",option+1,2) == 0)
            break;
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
          if (strncmp("flip",option+1,3) == 0)
            break;
          if (strncmp("flop",option+1,3) == 0)
            break;
          if (strncmp("font",option+1,3) == 0)
            {
              image_info.font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing font name",option);
                  (void) CloneString(&image_info.font,argv[i]);
                }
              break;
            }
          if (strncmp("format",option+1,3) == 0)
            {
              format=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing image type",option);
                  (void) CloneString(&format,argv[i]);
                  (void) strcpy(image_info.filename,format);
                  (void) strcat(image_info.filename,":");
                  SetImageInfo(&image_info,False);
                  if (*image_info.magick == '\0')
                    MagickError(OptionError,"Unrecognized image format",format);
                }
              break;
            }
          if (strncmp("frame",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                }
              break;
            }
          if (strncmp("fuzz",option+1,2) == 0)
            {
              image_info.fuzz=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing distance",option);
                  image_info.fuzz=atoi(argv[i]);
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
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                }
              break;
            }
          if (strncmp("gravity",option+1,2) == 0)
            {
              gravity=ForgetGravity;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
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
                  if (gravity == ForgetGravity)
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
          if (strncmp("implode",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing amount",option);
                }
              break;
            }
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
          if (strncmp("layer",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  LayerType
                    layer;

                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  layer=UndefinedLayer;
                  if (Latin1Compare("Red",option) == 0)
                    layer=RedLayer;
                  if (Latin1Compare("Green",option) == 0)
                    layer=GreenLayer;
                  if (Latin1Compare("Blue",option) == 0)
                    layer=BlueLayer;
                  if (Latin1Compare("Matte",option) == 0)
                    layer=MatteLayer;
                  if (layer == UndefinedLayer)
                    MagickError(OptionError,"Invalid layer type",option);
                }
              break;
            }
          if (strncmp("linewidth",option+1,2) == 0)
            {
              image_info.linewidth=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing size",option);
                  image_info.linewidth=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("loop",option+1,2) == 0)
            {
              image_info.iterations=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing iterations",option);
                  image_info.iterations=PostscriptGeometry(argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'm':
        {
          if (strncmp("map",option+1,3) == 0)
            {
              global_colormap=(*option == '+');
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing file name",option);
                }
              break;
            }
          if (Latin1Compare("matte",option+1) == 0)
            break;
          if (strncmp("mattecolor",option+1,6) == 0)
            {
              image_info.matte_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing matte color",option);
                  (void) CloneString(&image_info.matte_color,argv[i]);
                }
              break;
            }
          if (strncmp("modulate",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing value",option);
                }
              break;
            }
          if (strncmp("monochrome",option+1,4) == 0)
            {
              image_info.monochrome=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'n':
        {
          if (strncmp("negate",option+1,3) == 0)
            break;
         if (strncmp("noise",option+1,3) == 0)
            {
              if (*option == '+')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  if ((Latin1Compare("Uniform",option) != 0) &&
                      (Latin1Compare("Gaussian",option) != 0) &&
                      (Latin1Compare("Multiplicative",option) != 0) &&
                      (Latin1Compare("Impulse",option) != 0) &&
                      (Latin1Compare("Laplacian",option) != 0) &&
                      (Latin1Compare("Poisson",option) != 0))
                    MagickError(OptionError,"Invalid noise type",option);
                }
              break;
            }
          if (strncmp("normalize",option+1,3) == 0)
            break;
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'o':
        {
          if (strncmp("opaque",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing opaque color",option);
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
          if (strncmp("paint",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing radius",option);
                }
              break;
            }
          if (strncmp("pen",option+1,2) == 0)
            {
              image_info.pen=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing pen color",option);
                  (void) CloneString(&image_info.pen,argv[i]);
                }
              break;
            }
          if (strncmp("pointsize",option+1,2) == 0)
            {
              image_info.pointsize=12;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing size",option);
                  image_info.pointsize=atoi(argv[i]);
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
          if (strncmp("raise",option+1,2) == 0)
            {
              i++;
              if ((i == argc) || !sscanf(argv[i],"%d",&x))
                MagickError(OptionError,"Missing bevel width",option);
              break;
            }
          if (strncmp("region",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                }
              break;
            }
          if (strncmp("roll",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                }
              break;
            }
          if (strncmp("rotate",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing degrees",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 's':
        {
          if (strncmp("sample",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                }
              break;
            }
          if (strncmp("scene",option+1,3) == 0)
            {
              scene=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing scene number",option);
                  scene=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("seed",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing value",option);
                }
              srand((unsigned int) atoi(argv[i]));
              break;
            }
          if (strncmp("segment",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing threshold",option);
                }
              break;
            }
          if (strncmp("shade",option+1,5) == 0)
            {
              i++;
              if ((i == argc) || !sscanf(argv[i],"%d",&x))
                MagickError(OptionError,"Missing azimuth",option);
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
          if (strncmp("shear",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing geometry",option);
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
          if (strncmp("solarize",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing threshold",option);
                }
              break;
            }
          if (strncmp("spread",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing amount",option);
                }
              break;
            }
          if (strncmp("swirl",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing degrees",option);
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
              image_info.texture=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing filename",option);
                  (void) CloneString(&image_info.texture,argv[i]);
                }
              break;
            }
          if (strncmp("threshold",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing value",option);
                }
              break;
            }
          if (strncmp("transparent",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing transparent color",option);
                }
              break;
            }
          if (strncmp("treedepth",option+1,3) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing depth",option);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'u':
        {
          if (strncmp("units",option+1,3) == 0)
            {
              image_info.units=UndefinedResolution;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  image_info.units=UndefinedResolution;
                  if (Latin1Compare("PixelsPerInch",option) == 0)
                    image_info.units=PixelsPerInchResolution;
                  if (Latin1Compare("PixelsPerCentimeter",option) == 0)
                    image_info.units=PixelsPerCentimeterResolution;
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
          if (strncmp("view",option+1,3) == 0)
            {
              image_info.view=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing view transform",option);
                  (void) CloneString(&image_info.view,argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'w':
        {
          if (Latin1Compare("wave",option+1) == 0)
            {
              i++;
              if ((i == argc) || !sscanf(argv[i],"%d",&x))
                MagickError(OptionError,"Missing amplitude",option);
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
      {
        /*
          Option is a file name: begin by reading image from specified file.
        */
        (void) strcpy(image_info.filename,argv[i]);
        image=ReadImage(&image_info);
        if (image == (Image *) NULL)
          {
            if (*option == '-')
              break;
            else
              continue;
          }
        if (scene != 0)
          image->scene=scene;
        if (format != (char *) NULL)
          {
            register char
              *p;

            /*
              Modify filename to include a user specified image format.
            */
            p=image->filename+Extent(image->filename)-1;
            while ((*p != '.') && (p > (image->filename+1)))
              p--;
            p++;
            if (Latin1Compare(p,image->magick) == 0)
              (void) strcpy(p,format);
            else
              {
                FormatString(image_info.filename,"%.1024s:%.1024s",format,
                  image->filename);
                (void) strcpy(image->filename,image_info.filename);
              }
          }
        /*
          Transmogrify image as defined by the image processing options.
        */
        MogrifyImages(&image_info,i,argv,&image);
        if (global_colormap)
          (void) MapImages(image,(Image *) NULL,image_info.dither);
        /*
          Write transmogrified image to disk.
        */
        if (format != (char *) NULL)
          AppendImageFormat(format,image->filename);
        else
          if (strcmp(image_info.filename,"-") != 0)
            {
              (void) strcpy(filename,image->filename);
              LocaleFilename(image->filename);
            }
        for (p=image; p != (Image *) NULL; p=p->next)
        {
          (void) strcpy(p->filename,image->filename);
          p->scene=scene++;
        }
        SetImageInfo(&image_info,True);
        for (p=image; p != (Image *) NULL; p=p->next)
        {
          status=WriteImage(&image_info,p);
          if ((status == False) || image_info.adjoin)
            break;
        }
        for (p=image; p != (Image *) NULL; p=p->next)
          scene--;
        if (image_info.verbose)
          DescribeImage(image,(FILE *) NULL,False);
        if ((format == (char *) NULL) && (status != False))
          if (Latin1Compare(image_info.filename,"-") != 0)
            {
              (void) remove(filename);
              (void) rename(image->filename,filename);
            }
        DestroyImage(image);
      }
    }
  if (image == (Image *) NULL)
    MagickError(OptionError,"Missing an image file name",(char *) NULL);
  DestroyDelegateInfo();
  Exit(status ? 0 : errno);
  return(False);
}
