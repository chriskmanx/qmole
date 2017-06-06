/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%             DDDD   IIIII  SSSSS  PPPP   L       AAA   Y   Y                 %
%             D   D    I    SS     P   P  L      A   A   Y Y                  %
%             D   D    I     SSS   PPPP   L      AAAAA    Y                   %
%             D   D    I       SS  P      L      A   A    Y                   %
%             DDDD   IIIII  SSSSS  P      LLLLL  A   A    Y                   %
%                                                                             %
%                                                                             %
%                     Interactively Display an Image.                         %
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
%  Display is a machine architecture independent image processing
%  and display program.  It can display any image in the MIFF format on
%  any workstation display running X.  Display first determines the
%  hardware capabilities of the workstation.  If the number of unique
%  colors in the image is less than or equal to the number the workstation
%  can support, the image is displayed in an X window.  Otherwise the
%  number of colors in the image is first reduced to match the color
%  resolution of the workstation before it is displayed.
%
%  This means that a continuous-tone 24 bits-per-pixel image can display on a
%  8 bit pseudo-color device or monochrome device.  In most instances the
%  reduced color image closely resembles the original.  Alternatively, a
%  monochrome or pseudo-color image can display on a continuous-tone 24
%  bits-per-pixel device.
%
%  The Display program command syntax is:
%
%  Usage: display [options ...] file [ [options ...] file ...]
%
%  Where options include:
%    -backdrop          display image centered on a backdrop
%    -border geometry   surround image with a border of color
%    -colormap type     Shared or Private
%    -colors value      preferred number of colors in the image
%    -colorspace type   alternate image colorspace
%    -comment string    annotate image with comment",
%    -compress type     type of image compression
%    -contrast          enhance or reduce the image contrast
%    -crop geometry     preferred size and location of the cropped image
%    -delay value       display the next image after pausing
%    -density geometry  vertical and horizontal density of the image
%    -despeckle         reduce the speckles within an image
%    -display server    display image to this X server
%    -dispose method    GIF disposal method
%    -dither            apply Floyd/Steinberg error diffusion to image
%    -edge factor       apply a filter to detect edges in the image
%    -enhance           apply a digital filter to enhance a noisy image
%    -filter type       use this filter when resizing an image
%    -flip              flip image in the vertical direction
%    -flop              flop image in the horizontal direction
%    -frame geometry    surround image with an ornamental border
%    -gamma value       level of gamma correction
%    -geometry geometry preferred size and location of the Image window
%    -immutable         displayed image cannot be modified
%    -interlace type    None, Line, Plane, or Partition
%    -label name        assign a label to an image
%    -map type          display image using this Standard Colormap
%    -matte             store matte channel if the image has one
%    -monochrome        transform image to black and white
%    -negate            replace every pixel with its complementary color
%    -page geometry     size and location of an image canvas
%    -quality value     JPEG/MIFF/PNG compression level
%    -raise value       lighten/darken image edges to create a 3-D effect
%    -remote command    execute a command in an remote display process
%    -roll geometry     roll an image vertically or horizontally
%    -rotate degrees    apply Paeth rotation to the image
%    -sample geometry   scale image with pixel sampling
%    -scene value       image scene number
%    -segment value     segment an image
%    -sharpen factor    apply a filter to sharpen the image
%    -size geometry     width and height of image
%    -texture filename  name of texture to tile onto the image background
%    -treedepth value   depth of the color classification tree
%    -update seconds    detect when image file is modified and redisplay
%    -verbose           print detailed information about the image
%    -visual type       display image using this visual type
%    -window id         display image to background of this window
%    -window_group id   exit program when this window id is destroyed
%    -write filename    write image to a file
%
%  In addition to those listed above, you can specify these standard X
%  resources as command line options:  -background, -bordercolor,
%  -borderwidth, -font, -foreground, -iconGeometry, -iconic, -mattecolor,
%  -name, -shared_memory, -usePixmap, or -title.
%
%  By default, the image format of `file' is determined by its magic
%  number.  To specify a particular image format, precede the filename
%  with an image format name and a colon (i.e. ps:image) or specify the
%  image type as the filename suffix (i.e. image.ps).  Specify 'file' as
%  '-' for standard input or output.
%
%  Buttons:
%    1    press to map or unmap the Command widget
%    2    press and drag to magnify a region of an image
%    3    press to load an image from a visual image directory
%
%
*/

/*
  Include declarations.
*/
#if !defined(macintosh)
#include "magick/magick.h"
#include "magick/defines.h"
#include "magick/xwindows.h"
#else
#include "magick.h"
#include "defines.h"
#include "xwindows.h"
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
%  A description of each parameter follows:
%
%    o client_name: a character string representing the name of the client
%      program.
%
*/
static void Usage(const char *client_name)
{
  const char
    **p;

  static const char
    *buttons[]=
    {
      "1    press to map or unmap the Command widget",
      "2    press and drag to magnify a region of an image",
      "3    press to load an image from a visual image directory",
      (char *) NULL
    },
    *options[]=
    {
      "-backdrop          display image centered on a backdrop",
      "-border geometry   surround image with a border of color",
      "-colormap type     Shared or Private",
      "-colors value      preferred number of colors in the image",
      "-colorspace type   alternate image colorspace",
      "-comment string    annotate image with comment",
      "-compress type     type of image compression",
      "-contrast          enhance or reduce the image contrast",
      "-crop geometry     preferred size and location of the cropped image",
      "-delay value       display the next image after pausing",
      "-density geometry  vertical and horizontal density of the image",
      "-despeckle         reduce the speckles within an image",
      "-display server    display image to this X server",
      "-dispose method    GIF disposal method",
      "-dither            apply Floyd/Steinberg error diffusion to image",
      "-edge factor       apply a filter to detect edges in the image",
      "-enhance           apply a digital filter to enhance a noisy image",
      "-filter type       use this filter when resizing an image",
      "-flip              flip image in the vertical direction",
      "-flop              flop image in the horizontal direction",
      "-frame geometry    surround image with an ornamental border",
      "-gamma value       level of gamma correction",
      "-geometry geometry preferred size and location of the Image window",
      "-immutable         displayed image cannot be modified",
      "-interlace type    None, Line, Plane, or Partition",
      "-label name        assign a label to an image",
      "-map type          display image using this Standard Colormap",
      "-matte             store matte channel if the image has one",
      "-monochrome        transform image to black and white",
      "-negate            replace every pixel with its complementary color ",
      "-page geometry     size and location of an image canvas",
      "-quality value     JPEG/MIFF/PNG compression level",
      "-raise value       lighten/darken image edges to create a 3-D effect",
      "-remote command    execute a command in an remote display process",
      "-roll geometry     roll an image vertically or horizontally",
      "-rotate degrees    apply Paeth rotation to the image",
      "-scene value       image scene number",
      "-segment value     segment an image",
      "-sample geometry   scale image with pixel sampling",
      "-sharpen factor    apply a filter to sharpen the image",
      "-size geometry     width and height of image",
      "-texture filename  name of texture to tile onto the image background",
      "-treedepth value   depth of the color classification tree",
      "-update seconds    detect when image file is modified and redisplay",
      "-verbose           print detailed information about the image",
      "-visual type       display image using this visual type",
      "-window id         display image to background of this window",
      "-window_group id   exit program when this window id is destroyed",
      "-write filename    write image to a file",
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
    "\nIn addition to those listed above, you can specify these standard X\n");
  (void) printf(
    "resources as command line options:  -background, -bordercolor,\n");
  (void) printf(
    "-borderwidth, -font, -foreground, -iconGeometry, -iconic, -mattecolor,\n");
  (void) printf("-name, -shared_memory, -usePixmap, or -title.\n");
  (void) printf(
    "\nBy default, the image format of `file' is determined by its magic\n");
  (void) printf(
    "number.  To specify a particular image format, precede the filename\n");
  (void) printf(
    "with an image format name and a colon (i.e. ps:image) or specify the\n");
  (void) printf(
    "image type as the filename suffix (i.e. image.ps).  Specify 'file' as\n");
  (void) printf("'-' for standard input or output.\n");
  (void) printf("\nButtons: \n");
  for (p=buttons; *p != (char *) NULL; p++)
    (void) printf("  %.1024s\n",*p);
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

#if defined(WIN32)
int WINAPI WinMain(HINSTANCE instance,HINSTANCE last,LPSTR command,int state)
{
  char
    **argv;

  int
    argc,
    main(int,char **);

  argv=StringToArgv(command,&argc);
  return(main(argc,argv));
}
#endif

int main(int argc,char **argv)
{
#if defined(HasX11)
  char
    *client_name,
    *option,
    *resource_value,
    *server_name;

  Display
    *display;

  double
    sans;

  Image
    *image,
    *next_image;

  ImageInfo
    *image_info;

  int
    image_number,
    status,
    x;

  QuantizeInfo
    *quantize_info;

  register int
    i,
    j;

  unsigned int
    first_scene,
    *image_marker,
    last_image,
    last_scene,
    scene;

  unsigned long
    state;

  XResourceInfo
    resource_info;

  XrmDatabase
    resource_database;

  /*
    Initialize command line arguments.
  */
  SetNotifyHandlers;
  ReadCommandlLine(argc,&argv);
  client_name=SetClientName(*argv);
  (void) ExpandFilenames(&argc,&argv);
  /*
    Set defaults.
  */
  display=(Display *) NULL;
  first_scene=0;
  image_number=0;
  last_image=0;
  last_scene=0;
  image_marker=(unsigned int *) AllocateMemory((argc+1)*sizeof(unsigned int));
  if (image_marker == (unsigned int *) NULL)
    MagickError(ResourceLimitError,"Unable to display image",
      "Memory allocation failed");
  for (i=0; i <= argc; i++)
    image_marker[i]=argc;
  resource_database=(XrmDatabase) NULL;
  state=0;
  /*
    Check for server name specified on the command line.
  */
  server_name=(char *) NULL;
  for (i=1; i < argc; i++)
  {
    /*
      Check command line for server name.
    */
    option=argv[i];
    if ((Extent(option) == 1) || ((*option != '-') && (*option != '+')))
      continue;
    if (Latin1Compare("display",option+1) == 0)
      {
        /*
          User specified server name.
        */
        i++;
        if (i == argc)
          MagickError(OptionError,"Missing server name",option);
        server_name=argv[i];
        break;
      }
    if (strncmp("help",option+1,2) == 0)
      Usage(client_name);
  }
  /*
    Get user defaults from X resource database.
  */
  display=XOpenDisplay(server_name);
  if (display == (Display *) NULL)
    MagickError(XServerError,"Unable to connect to X server",
      XDisplayName(server_name));
  XSetErrorHandler(XError);
  resource_database=XGetResourceDatabase(display,client_name);
  XGetResourceInfo(resource_database,client_name,&resource_info);
  image_info=resource_info.image_info;
  image_info->coalesce_frames=True;
  quantize_info=resource_info.quantize_info;
  image_info->density=
    XGetResourceInstance(resource_database,client_name,"density",(char *) NULL);
  if (image_info->density == (char *) NULL)
    image_info->density=XGetScreenDensity(display);
  resource_value=
    XGetResourceInstance(resource_database,client_name,"interlace","none");
  image_info->interlace=UndefinedInterlace;
  if (Latin1Compare("None",resource_value) == 0)
    image_info->interlace=NoInterlace;
  if (Latin1Compare("Line",resource_value) == 0)
    image_info->interlace=LineInterlace;
  if (Latin1Compare("Plane",resource_value) == 0)
    image_info->interlace=PlaneInterlace;
  if (Latin1Compare("Partition",resource_value) == 0)
    image_info->interlace=PartitionInterlace;
  if (image_info->interlace == UndefinedInterlace)
    MagickWarning(OptionWarning,"Unrecognized interlace type",resource_value);
  image_info->page=XGetResourceInstance(resource_database,client_name,
    "pageGeometry",(char *) NULL);
  resource_value=
    XGetResourceInstance(resource_database,client_name,"quality","75");
  image_info->quality=atoi(resource_value);
  resource_value=
    XGetResourceInstance(resource_database,client_name,"verbose","False");
  image_info->verbose=IsTrue(resource_value);
  resource_value=
    XGetResourceInstance(resource_database,client_name,"dither","True");
  quantize_info->dither=IsTrue(resource_value);
  /*
    Parse command line.
  */
  for (i=1; ((i <= argc) && !(state & ExitState)); i++)
  {
    if (i < argc)
      option=argv[i];
    else
      if (image_number != 0)
        break;
      else
        if (!isatty(STDIN_FILENO))
          option="-";
        else
          option="logo:Untitled";
    if ((Extent(option) > 1) && ((*option == '-') || (*option == '+')))
      switch (*(option+1))
      {
        case 'b':
        {
          if (strncmp("backdrop",option+1,5) == 0)
            {
              resource_info.backdrop=(*option == '-');
              break;
            }
          if ((strncmp("background",option+1,5) == 0) ||
              (strncmp("bg",option+1,2) == 0))
            {
              resource_info.background_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing color",option);
                  (void) CloneString(&resource_info.background_color,argv[i]);
                  (void) CloneString(&image_info->background_color,argv[i]);
                }
              break;
            }
          if (Latin1Compare("border",option+1) == 0)
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
              resource_info.border_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing color",option);
                  (void) CloneString(&resource_info.border_color,argv[i]);
                }
              break;
            }
          if (strncmp("borderwidth",option+1,7) == 0)
            {
              resource_info.border_width=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing width",option);
                  resource_info.border_width=atoi(argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'c':
        {
          if (strncmp("colormap",option+1,6) == 0)
            {
              resource_info.colormap=PrivateColormap;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  resource_info.colormap=UndefinedColormap;
                  if (Latin1Compare("private",option) == 0)
                    resource_info.colormap=PrivateColormap;
                  if (Latin1Compare("shared",option) == 0)
                    resource_info.colormap=SharedColormap;
                  if (resource_info.colormap == UndefinedColormap)
                    MagickError(OptionError,"Invalid colormap type",option);
                }
              break;
            }
          if (strncmp("colors",option+1,7) == 0)
            {
              quantize_info->number_colors=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing colors",option);
                  quantize_info->number_colors=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("colorspace",option+1,7) == 0)
            {
              quantize_info->colorspace=RGBColorspace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  quantize_info->colorspace=UndefinedColorspace;
                  if (Latin1Compare("cmyk",option) == 0)
                    quantize_info->colorspace=CMYKColorspace;
                  if (Latin1Compare("gray",option) == 0)
                    {
                      quantize_info->colorspace=GRAYColorspace;
                      quantize_info->number_colors=256;
                      quantize_info->tree_depth=8;
                    }
                  if (Latin1Compare("ohta",option) == 0)
                    quantize_info->colorspace=OHTAColorspace;
                  if (Latin1Compare("rgb",option) == 0)
                    quantize_info->colorspace=RGBColorspace;
                  if (Latin1Compare("srgb",option) == 0)
                    quantize_info->colorspace=sRGBColorspace;
                  if (Latin1Compare("transparent",option) == 0)
                    quantize_info->colorspace=TransparentColorspace;
                  if (Latin1Compare("xyz",option) == 0)
                    quantize_info->colorspace=XYZColorspace;
                  if (Latin1Compare("ycbcr",option) == 0)
                    quantize_info->colorspace=YCbCrColorspace;
                  if (Latin1Compare("ycc",option) == 0)
                    quantize_info->colorspace=YCCColorspace;
                  if (Latin1Compare("yiq",option) == 0)
                    quantize_info->colorspace=YIQColorspace;
                  if (Latin1Compare("ypbpr",option) == 0)
                    quantize_info->colorspace=YPbPrColorspace;
                  if (Latin1Compare("yuv",option) == 0)
                    quantize_info->colorspace=YUVColorspace;
                  if (quantize_info->colorspace == UndefinedColorspace)
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
              image_info->compression=NoCompression;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  image_info->compression=UndefinedCompression;
                  if (Latin1Compare("None",option) == 0)
                    image_info->compression=NoCompression;
                  if (Latin1Compare("BZip",option) == 0)
                    image_info->compression=BZipCompression;
                  if (Latin1Compare("Fax",option) == 0)
                    image_info->compression=FaxCompression;
                  if (Latin1Compare("Group4",option) == 0)
                    image_info->compression=Group4Compression;
                  if (Latin1Compare("JPEG",option) == 0)
                    image_info->compression=JPEGCompression;
                  if (Latin1Compare("LZW",option) == 0)
                    image_info->compression=LZWCompression;
                  if (Latin1Compare("RunlengthEncoded",option) == 0)
                    image_info->compression=RunlengthEncodedCompression;
                  if (Latin1Compare("Zip",option) == 0)
                    image_info->compression=ZipCompression;
                  if (image_info->compression == UndefinedCompression)
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
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'd':
        {
          if (strncmp("debug",option+1,3) == 0)
            {
              resource_info.debug=(*option == '-');
              break;
            }
          if (strncmp("delay",option+1,3) == 0)
            {
              resource_info.delay=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing seconds",option);
                  resource_info.delay=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("density",option+1,3) == 0)
            {
              image_info->density=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&image_info->density,argv[i]);
                }
              break;
            }
          if (strncmp("despeckle",option+1,3) == 0)
            break;
          if (Latin1Compare("display",option+1) == 0)
            {
              image_info->server_name=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing server name",option);
                  image_info->server_name=argv[i];
                }
              break;
            }
          if (strncmp("dispose",option+1,5) == 0)
            {
              image_info->dispose=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing method",option);
                  image_info->dispose=PostscriptGeometry(argv[i]);
                }
              break;
            }
          if (strncmp("dither",option+1,3) == 0)
            {
              quantize_info->dither=(*option == '-');
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
          if (strncmp("enhance",option+1,2) == 0)
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
              image_info->font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing font name",option);
                  image_info->font=argv[i];
                }
              if ((image_info->font == (char *) NULL) ||
                  (*image_info->font != '@'))
                resource_info.font=image_info->font;
              break;
            }
          if ((strncmp("foreground",option+1,3) == 0) ||
              (strncmp("fg",option+1,2) == 0))
           {
             resource_info.foreground_color=(char *) NULL;
             if (*option == '-')
               {
                 i++;
                 if (i == argc)
                   MagickError(OptionError,"Missing foreground",option);
                 resource_info.foreground_color=argv[i];
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
              resource_info.image_geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  resource_info.image_geometry=argv[i];
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'h':
        {
          if (strncmp("help",option+1,2) == 0)
            Usage(client_name);
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'i':
        {
          if (strncmp("iconGeometry",option+1,5) == 0)
            {
              resource_info.icon_geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  resource_info.icon_geometry=argv[i];
                }
              break;
            }
          if (strncmp("iconic",option+1,5) == 0)
            {
              resource_info.iconic=(*option == '-');
              break;
            }
          if (strncmp("immutable",option+1,5) == 0)
            {
              resource_info.immutable=(*option == '-');
              break;
            }
          if (strncmp("interlace",option+1,3) == 0)
            {
              image_info->interlace=NoInterlace;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  image_info->interlace=UndefinedInterlace;
                  if (Latin1Compare("None",option) == 0)
                    image_info->interlace=NoInterlace;
                  if (Latin1Compare("Line",option) == 0)
                    image_info->interlace=LineInterlace;
                  if (Latin1Compare("Plane",option) == 0)
                    image_info->interlace=PlaneInterlace;
                  if (Latin1Compare("Partition",option) == 0)
                    image_info->interlace=PartitionInterlace;
                  if (image_info->interlace == UndefinedInterlace)
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
          if (strncmp("magnify",option+1,3) == 0)
            {
              resource_info.magnify=2;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing level",option);
                  resource_info.magnify=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("map",option+1,3) == 0)
            {
              argv[i]="+sans";
              resource_info.map_type=(char *) NULL;
              if (*option == '-')
                {
                  argv[i]="-sans";
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing map type",option);
                  resource_info.map_type=argv[i];
                }
              break;
            }
          if (Latin1Compare("matte",option+1) == 0)
            break;
          if (strncmp("mattecolor",option+1,6) == 0)
            {
              resource_info.matte_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing color",option);
                  resource_info.matte_color=argv[i];
                }
              break;
            }
          if (strncmp("monochrome",option+1,3) == 0)
            {
              image_info->monochrome=(*option == '-');
              if (image_info->monochrome)
                {
                  quantize_info->number_colors=2;
                  quantize_info->tree_depth=8;
                  quantize_info->colorspace=GRAYColorspace;
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'n':
        {
          if (strncmp("name",option+1,2) == 0)
            {
              resource_info.name=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing name",option);
                  resource_info.name=argv[i];
                }
              break;
            }
          if (strncmp("negate",option+1,2) == 0)
            break;
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'p':
        {
          if (strncmp("page",option+1,3) == 0)
            {
              image_info->page=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing page geometry",option);
                  image_info->page=PostscriptGeometry(argv[i]);
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
              image_info->quality=75;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing quality",option);
                  image_info->quality=atoi(argv[i]);
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
          if (strncmp("remote",option+1,3) == 0)
            {
              i++;
              if (i == argc)
                MagickError(OptionError,"Missing command",option);
              XRemoteCommand(display,resource_info.window_id,argv[i]);
              Exit(0);
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
          if (strncmp("shared_memory",option+1,5) == 0)
            {
              resource_info.use_shared_memory=(*option == '-');
              break;
            }
          if (strncmp("size",option+1,2) == 0)
            {
              image_info->size=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  image_info->size=argv[i];
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("text_font",option+1,5) == 0)
            {
              resource_info.text_font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing font name",option);
                  resource_info.text_font=argv[i];
                }
              break;
            }
          if (strncmp("texture",option+1,5) == 0)
            {
              image_info->texture=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing filename",option);
                  image_info->texture=argv[i];
                }
              break;
            }
          if (strncmp("title",option+1,2) == 0)
            {
              resource_info.title=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing title",option);
                  resource_info.title=argv[i];
                }
              break;
            }
          if (strncmp("treedepth",option+1,3) == 0)
            {
              quantize_info->tree_depth=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing depth",option);
                  quantize_info->tree_depth=atoi(argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'u':
        {
          if (strncmp("update",option+1,2) == 0)
            {
              resource_info.update=(*option == '-');
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing seconds",option);
                  resource_info.delay=atoi(argv[i]);
                }
              break;
            }
          if (strncmp("use_pixmap",option+1,2) == 0)
            {
              resource_info.use_pixmap=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'v':
        {
          if (strncmp("verbose",option+1,2) == 0)
            {
              image_info->verbose=(*option == '-');
              break;
            }
          if (strncmp("visual",option+1,3) == 0)
            {
              resource_info.visual_type=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing visual class",option);
                  resource_info.visual_type=argv[i];
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'w':
        {
          if (Latin1Compare("window",option+1) == 0)
            {
              resource_info.window_id=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing id, name, or 'root'",
                      option);
                  resource_info.window_id=argv[i];
                }
              break;
            }
          if (strncmp("window_group",option+1,7) == 0)
            {
              resource_info.window_group=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing id, name, or 'root'",
                      option);
                  resource_info.window_group=argv[i];
                }
              break;
            }
          if (strncmp("write",option+1,2) == 0)
            {
              resource_info.write_filename=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing file name",option);
                  resource_info.write_filename=argv[i];
                  if (IsAccessible(resource_info.write_filename))
                    {
                      char
                        answer[2];

                      (void) fprintf(stderr,"Overwrite %.1024s? ",
                        resource_info.write_filename);
                      (void) fgets(answer,sizeof(answer),stdin);
                      if (!((*answer == 'y') || (*answer == 'Y')))
                        Exit(0);
                    }
                }
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
          Option is a file name.
        */
        for (scene=first_scene; scene <= last_scene ; scene++)
        {
          /*
            Read image.
          */
          (void) strcpy(image_info->filename,option);
          if (first_scene != last_scene)
            {
              char
                filename[MaxTextExtent];

              /*
                Form filename for multi-part images.
              */
              FormatString(filename,image_info->filename,scene);
              if (Latin1Compare(filename,image_info->filename) == 0)
                FormatString(filename,"%.1024s.%u",image_info->filename,scene);
              (void) strcpy(image_info->filename,filename);
            }
          (void) strcpy(image_info->magick,"MIFF");
          image_info->colorspace=quantize_info->colorspace;
          image_info->dither=quantize_info->dither;
          image=ReadImage(image_info);
          if (image == (Image *) NULL)
            {
              if ((i < (argc-1)) || (scene < last_scene))
                continue;
              else
                {
                  state|=ExitState;
                  break;
                }
            }
          do
          {
            /*
              Transmogrify image as defined by the image processing options.
            */
            resource_info.quantum=1;
            MogrifyImage(image_info,i,argv,&image);
            if (first_scene != last_scene)
              image->scene=scene;
            /*
              Display image to X server.
            */
            if (resource_info.window_id != (char *) NULL)
              {
                /*
                  Display image to a specified X window.
                */
                status=XDisplayBackgroundImage(display,&resource_info,image);
                if (status)
                  state|=RetainColorsState;
                if (resource_info.delay == 0)
                  state|=ExitState;
              }
            else
              do
              {
                Image
                  *loaded_image;

                /*
                  Display image to X server.
                */
                loaded_image=
                  XDisplayImage(display,&resource_info,argv,argc,&image,&state);
                if (loaded_image == (Image *) NULL)
                  break;
                while ((loaded_image != (Image *) NULL) &&
                       (!(state & ExitState)))
                {
                  if (loaded_image->montage != (char *) NULL)
                    {
                      /*
                        User selected a visual directory image (montage).
                      */
                      DestroyImages(image);
                      image=loaded_image;
                      break;
                    }
                  MogrifyImage(image_info,i,argv,&loaded_image);
                  if (first_scene != last_scene)
                    image->scene=scene;
                  next_image=XDisplayImage(display,&resource_info,argv,argc,
                    &loaded_image,&state);
                  if ((next_image == (Image *) NULL) &&
                      (loaded_image->next != (Image *) NULL))
                    {
                      DestroyImages(image);
                      image=loaded_image->next;
                      loaded_image=(Image *) NULL;
                    }
                  else
                    {
                      if (loaded_image != image)
                        DestroyImages(loaded_image);
                      loaded_image=next_image;
                    }
                }
              } while (!(state & ExitState));
            if (resource_info.write_filename != (char *) NULL)
              {
                /*
                  Write image.
                */
                (void) strcpy(image->filename,resource_info.write_filename);
                SetImageInfo(image_info,True);
                (void) WriteImage(image_info,image);
              }
            if (image_info->verbose)
              DescribeImage(image,(FILE *) NULL,False);
            /*
              Proceed to next/previous image.
            */
            next_image=image;
            if (state & FormerImageState)
              for (j=0; j < resource_info.quantum; j++)
              {
                next_image=next_image->previous;
                if (next_image == (Image *) NULL)
                  break;
              }
            else
              for (j=0; j < resource_info.quantum; j++)
              {
                next_image=next_image->next;
                if (next_image == (Image *) NULL)
                  break;
              }
            if (next_image != (Image *) NULL)
              image=next_image;
          } while ((next_image != (Image *) NULL) && !(state & ExitState));
          /*
            Free image resources.
          */
          DestroyImages(image);
          if (!(state & FormerImageState))
            {
              last_image=image_number;
              image_marker[i]=image_number++;
            }
          else
            {
              /*
                Proceed to previous image.
              */
              for (i--; i > 0; i--)
                if (image_marker[i] == (image_number-2))
                  break;
              image_number--;
            }
          if (state & ExitState)
            break;
        }
      }
    /*
      Determine if we should proceed to the first image.
    */
    if (image_number < 0)
      {
        if (state & FormerImageState)
          {
            for (i=1; i < (argc-2); i++)
              if (image_marker[i] == last_image)
                break;
            image_number=image_marker[i]+1;
          }
        continue;
      }
    if (image_number > 0)
      if ((i == (argc-1)) || (argc == 1))
        if (!(state & ExitState))
          if (resource_info.window_id == (char *) NULL)
            {
              i=0;
              image_number=0;
            }
  }
  if (state & RetainColorsState)
    {
      XRetainWindowColors(display,XRootWindow(display,XDefaultScreen(display)));
      XSync(display,False);
    }
  DestroyDelegateInfo();
  Exit(0);
#endif
  return(False);
}
