/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%              AAA   N   N  IIIII  M   M   AAA   TTTTT  EEEEE                 %
%             A   A  NN  N    I    MM MM  A   A    T    E                     %
%             AAAAA  N N N    I    M M M  AAAAA    T    EEE                   %
%             A   A  N  NN    I    M   M  A   A    T    E                     %
%             A   A  N   N  IIIII  M   M  A   A    T    EEEEE                 %
%                                                                             %
%                                                                             %
%                 Interactively Animate an Image Sequence.                    %
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
%  Animate displays a sequence of images in the MIFF format on any
%  workstation display running an X server.  Animate first determines the
%  hardware capabilities of the workstation.  If the number of unique
%  colors in an image is less than or equal to the number the workstation
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
%  The Animate program command syntax is:
%
%  Usage: animate [options ...] file [ [options ...] file ...]
%
%  Where options include:
%    -backdrop            display image centered on a backdrop
%    -colormap type       Shared or Private
%    -colors value        preferred number of colors in the image
%    -colorspace type     alternate image colorspace
%    -crop geometry       preferred size and location of the cropped image
%    -delay value         display the next image after pausing
%    -density geometry    vertical and horizontal density of the image
%    -display server      display image to this X server
%    -dither              apply Floyd/Steinberg error diffusion to image
%    -gamma value         level of gamma correction
%    -geometry geometry   preferred size and location of the Image window
%    -interlace type      None, Line, Plane, or Partition
%    -map type            display image using this Standard Colormap
%    -matte               store matte channel if the image has one
%    -monochrome          transform image to black and white
%    -remote command      execute a command in an remote display process
%    -rotate degrees      apply Paeth rotation to the image
%    -scene value         image scene number
%    -size geometry       width and height of image
%    -treedepth value     depth of the color classification tree
%    -verbose             print detailed information about the image
%    -visual type         display image using this visual type
%    -window id           display image to background of this window
%
%  In addition to those listed above, you can specify these standard X
%  resources as command line options:  -background, -bordercolor,
%  -borderwidth, -font, -foreground, -iconGeometry, -iconic, -name,
%  -mattecolor, -shared_memory, or -title.
%
%  By default, the image format of `file' is determined by its magic
%  number.  To specify a particular image format, precede the filename
%  with an image format name and a colon (i.e. ps:image) or specify the
%  image type as the filename suffix (i.e. image.ps).  Specify 'file' as
%  '-' for standard input or output.
%
%  Buttons:
%    Press any button to map or unmap the Command widget.
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
%
*/
static void Usage(const char *client_name)
{
  const char
    **p;

  static const char
    *buttons[]=
    {
      "Press any button to map or unmap the Command widget",
      (char *) NULL
    },
    *options[]=
    {
      "-backdrop            display image centered on a backdrop",
      "-colormap type       Shared or Private",
      "-colors value        preferred number of colors in the image",
      "-colorspace type     alternate image colorspace",
      "-crop geometry       preferred size and location of the cropped image",
      "-delay value         display the next image after pausing",
      "-density geometry    vertical and horizontal density of the image",
      "-display server      display image to this X server",
      "-dither              apply Floyd/Steinberg error diffusion to image",
      "-gamma value         level of gamma correction",
      "-geometry geometry   preferred size and location of the Image window",
      "-interlace type      None, Line, Plane, or Partition",
      "-matte               store matte channel if the image has one",
      "-map type            display image using this Standard Colormap",
      "-monochrome          transform image to black and white",
      "-remote command      execute a command in an remote display process",
      "-rotate degrees      apply Paeth rotation to the image",
      "-scene value         image scene number",
      "-size geometry       width and height of image",
      "-treedepth value     depth of the color classification tree",
      "-verbose             print detailed information about the image",
      "-visual type         display image using this visual type",
      "-window id          display image to background of this window",
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
    "-borderwidth, -font, -foreground, -iconGeometry, -iconic, -name,\n");
  (void) printf("-mattecolor, -shared_memory, or -title.\n");
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
    *loaded_image,
    *next_image,
    *p;

  ImageInfo
    *image_info;

  int
    i,
    x;

  QuantizeInfo
    *quantize_info;

  unsigned int
    first_scene,
    last_scene,
    scene;

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
  image=(Image *) NULL;
  last_scene=0;
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
  image_info->insert_backdrops=True;
  image_info->coalesce_frames=True;
  quantize_info=resource_info.quantize_info;
  resource_info.delay=0;
  resource_info.pause=0;
  resource_value=
    XGetResourceInstance(resource_database,client_name,"delay","0");
  (void) XParseGeometry(resource_value,&x,&x,&resource_info.delay,
    &resource_info.pause);
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
  resource_value=
    XGetResourceInstance(resource_database,client_name,"verbose","False");
  image_info->verbose=IsTrue(resource_value);
  resource_value=
    XGetResourceInstance(resource_database,client_name,"dither","True");
  quantize_info->dither=IsTrue(resource_value);
  /*
    Parse command line.
  */
  for (i=1; i <= argc; i++)
  {
    if (i < argc)
      option=argv[i];
    else
      if (image != (Image *) NULL)
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
          if (strncmp("background",option+1,5) == 0)
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
              resource_info.pause=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing seconds",option);
                  (void) XParseGeometry(argv[i],&x,&x,&resource_info.delay,
                    &resource_info.pause);
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
          if (Latin1Compare("display",option+1) == 0)
            {
              image_info->server_name=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing server name",option);
                  (void) CloneString(&image_info->server_name,argv[i]);
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
        case 'f':
        {
          if (strncmp("font",option+1,3) == 0)
            {
              image_info->font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing font name",option);
                  (void) CloneString(&image_info->font,argv[i]);
                }
              if ((image_info->font == (char *) NULL) ||
                  (*image_info->font != '@'))
                (void) CloneString(&resource_info.font,image_info->font);
              break;
            }
          if (strncmp("foreground",option+1,3) == 0)
            {
              resource_info.foreground_color=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing foreground",option);
                  (void) CloneString(&resource_info.foreground_color,argv[i]);
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
                  (void) CloneString(&resource_info.image_geometry,argv[i]);
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
          if (strncmp("iconGeometry",option+1,5) == 0)
            {
              resource_info.icon_geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&resource_info.icon_geometry,argv[i]);
                }
              break;
            }
          if (strncmp("iconic",option+1,5) == 0)
            {
              resource_info.iconic=(*option == '-');
              break;
            }
          if (strncmp("interlace",option+1,3) == 0)
            {
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
        case 'm':
        {
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
                  (void) CloneString(&resource_info.map_type,argv[i]);
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
                  (void) CloneString(&resource_info.matte_color,argv[i]);
                }
              break;
            }
          if (strncmp("monochrome",option+1,2) == 0)
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
          resource_info.name=(char *) NULL;
          if (*option == '-')
            {
              i++;
              if (i == argc)
                MagickError(OptionError,"Missing name",option);
              (void) CloneString(&resource_info.name,argv[i]);
            }
          break;
        }
        case 'r':
        {
          if (strncmp("remote",option+1,3) == 0)
            {
              i++;
              if (i == argc)
                MagickError(OptionError,"Missing command",option);
              XRemoteCommand(display,resource_info.window_id,argv[i]);
              Exit(0);
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
          if (strncmp("shared_memory",option+1,4) == 0)
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
                  (void) CloneString(&image_info->size,argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("text_font",option+1,3) == 0)
            {
              resource_info.text_font=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing font name",option);
                  (void) CloneString(&resource_info.text_font,argv[i]);
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
                  (void) CloneString(&resource_info.title,argv[i]);
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
                  (void) CloneString(&resource_info.visual_type,argv[i]);
                }
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'w':
        {
          if (strncmp("window",option+1,2) == 0)
            {
              resource_info.window_id=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing id, name, or 'root'",
                      option);
                  (void) CloneString(&resource_info.window_id,argv[i]);
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
                FormatString(filename,"%.1024s[%u]",image_info->filename,scene);
              (void) strcpy(image_info->filename,filename);
            }
          (void) strcpy(image_info->magick,"MIFF");
          image_info->colorspace=quantize_info->colorspace;
          image_info->dither=quantize_info->dither;
          next_image=ReadImage(image_info);
          if (next_image == (Image *) NULL)
            {
              if (*option == '-')
                break;
              else
                continue;
            }
          MogrifyImages(image_info,i,argv,&next_image);
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
  }
  if (image == (Image *) NULL)
    MagickError(OptionError,"Missing an image file name",(char *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  if (resource_info.window_id != (char *) NULL)
    XAnimateBackgroundImage(display,&resource_info,image);
  else
    {
      /*
        Animate image to X server.
      */
      loaded_image=XAnimateImages(display,&resource_info,argv,argc,image);
      DestroyImages(image);
      while (loaded_image != (Image *) NULL)
      {
        image=loaded_image;
        MogrifyImage(image_info,argc-1,argv,&image);
        loaded_image=XAnimateImages(display,&resource_info,argv,argc,image);
        DestroyImages(image);
      }
    }
  DestroyDelegateInfo();
  Exit(0);
#endif
  return(False);
}
