/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                CCCC   OOO   M   M  BBBB   IIIII  N   N  EEEEE               %
%               C      O   O  MM MM  B   B    I    NN  N  E                   %
%               C      O   O  M M M  BBBB     I    N N N  EEE                 %
%               C      O   O  M   M  B   B    I    N  NN  E                   %
%                CCCC   OOO   M   N  BBBB   IIIII  N   N  EEEEE               %
%                                                                             %
%                                                                             %
%                        Digitally combine two images.                        %
%                                                                             %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                               January 1993                                  %
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
%  The combine program syntax is:
%
%  Usage: combine [options ...] image composite [mask] combined
%
%  Where options include:
%    -blend value        blend the two images a given percent
%    -colors value       preferred number of colors in the image
%    -compose operator   composite operator
%    -colorspace type    alternate image colorspace
%    -comment string     annotate image with comment
%    -compress type      type of image compression
%    -density geometry   vertical and horizontal density of the image
%    -displace geometry  shift image pixels as defined by a displacement map
%    -display server     obtain image or font from this X server
%    -dispose method     GIF disposal method
%    -dither             apply Floyd/Steinberg error diffusion to image
%    -font name          X11 font for displaying text
%    -geometry geometry  perferred size or location of the image
%    -gravity type       which direction to gravitate towards
%    -interlace type     None, Line, Plane, or Partition
%    -label name         assign a label to an image
%    -matte              store matte channel if the image has one
%    -monochrome         transform image to black and white
%    -negate             replace every pixel with its complementary color 
%    -page geometry      size and location of an image canvas
%    -quality value      JPEG/MIFF/PNG compression level
%    -scene value        image scene number
%    -size geometry      width and height of image
%    -stegano offset     hide watermark within an image
%    -stereo             combine two image to create a stereo anaglyph
%    -tile               repeat composite operation across image
%    -treedepth value    depth of the color classification tree
%    -verbose            print detailed information about the image
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
%  Procedure Usage displays the program usage;
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
      "-blend value        blend the two images a given percent",
      "-colors value       preferred number of colors in the image",
      "-colorspace type    alternate image colorspace",
      "-comment string     annotate image with comment",
      "-compose operator   composite operator",
      "-compress type      type of image compression",
      "-density geometry   vertical and horizontal density of the image",
      "-displace geometry  shift image pixels as defined by a displacement map",
      "-display server     obtain image or font from this X server",
      "-dispose method     GIF disposal method",
      "-dither             apply Floyd/Steinberg error diffusion to image",
      "-font name          X11 font for displaying text",
      "-geometry geometry  perferred size or location of the image",
      "-gravity type       which direction to gravitate towards",
      "-interlace type     None, Line, Plane, or Partition",
      "-label name         ssign a label to an image",
      "-matte              store matte channel if the image has one",
      "-monochrome         transform image to black and white",
      "-negate             replace every pixel with its complementary color ",
      "-page geometry      size and location of an image canvas",
      "-quality value      JPEG/MIFF/PNG compression level",
      "-scene value        image scene number",
      "-size geometry      width and height of image",
      "-stegano offset     hide watermark within an image",
      "-stereo             combine two image to create a stereo anaglyph",
      "-tile               repeat composite operation across image",
      "-treedepth value    depth of the color classification tree",
      "-verbose            print detailed information about the image",
      (char *) NULL
    };

  (void) printf("Version: %.1024s\n",MagickVersion);
  (void) printf("Copyright: %.1024s\n\n",MagickCopyright);
  (void) printf("Usage: %.1024s [options ...] image composite [mask] combined\n",
    client_name);
  (void) printf("\nWhere options include:\n");
  for (p=options; *p != (char *) NULL; p++)
    (void) printf("  %.1024s\n",*p);
  Exit(0);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  M a i n                                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
int main(int argc,char **argv)
{
#define NotInitialized  (unsigned int) (~0)

  char
    *client_name,
    *displacement_geometry,
    *filename,
    *geometry,
    *option,
    *write_filename;

  CompositeOperator
    compose;

  double
    blend,
    sans;

  Image
    *combined_image,
    *composite_image,
    *image,
    *mask_image;

  ImageInfo
    image_info;

  int
    gravity,
    status,
    x,
    y;

  register int
    i;

  unsigned int
    stegano,
    stereo,
    tile;

  /*
    Initialize command line arguments.
  */
  ReadCommandlLine(argc,&argv);
  client_name=SetClientName(*argv);
  (void) ExpandFilenames(&argc,&argv);
  if (argc < 4)
    Usage(client_name);
  /*
    Set default.
  */
  blend=0.0;
  compose=ReplaceCompositeOp;
  composite_image=(Image *) NULL;
  displacement_geometry=(char *) NULL;
  geometry=(char *) NULL;
  gravity=NorthWestGravity;
  image=(Image *) NULL;
  GetImageInfo(&image_info);
  mask_image=(Image *) NULL;
  stegano=0;
  stereo=False;
  tile=False;
  write_filename=argv[argc-1];
  /*
    Check command syntax.
  */
  filename=(char *) NULL;
  for (i=1; i < (argc-1); i++)
  {
    option=argv[i];
    if ((Extent(option) < 2) || ((*option != '-') && (*option != '+')))
      {
        /*
          Read input images.
        */
        filename=argv[i];
        (void) strcpy(image_info.filename,filename);
        if (image == (Image *) NULL)
          {
            image=ReadImage(&image_info);
            if (image == (Image *) NULL)
              MagickError(OptionError,"Missing an image file name",
                (char *) NULL);
            continue;
          }
        if (mask_image != (Image *) NULL)
          MagickError(OptionError,"input images already specified",filename);
        if (composite_image == (Image *) NULL)
          {
            composite_image=ReadImage(&image_info);
            if (composite_image == (Image *) NULL)
              MagickError(OptionError,"Missing an image file name",
                (char *) NULL);
            continue;
          }
        mask_image=ReadImage(&image_info);
        if (mask_image == (Image *) NULL)
          MagickError(OptionError,"Missing an image file name",(char *) NULL);
      }
    else
      switch(*(option+1))
      {
        case 'b':
        {
          if (strncmp("blend",option+1,3) == 0)
            {
              blend=0.0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing value",option);
                  blend=atof(argv[i]);
                  compose=BlendCompositeOp;
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
              image_info.colorspace=RGBColorspace;
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
          if (strncmp("compose",option+1,5) == 0)
            {
              compose=ReplaceCompositeOp;
              if (*option == '-')
                {
                  i++;
                  if (i == argc)
                    MagickError(OptionError,"Missing type",option);
                  option=argv[i];
                  compose=UndefinedCompositeOp;
                  if (Latin1Compare("Over",option) == 0)
                    compose=OverCompositeOp;
                  if (Latin1Compare("In",option) == 0)
                    compose=InCompositeOp;
                  if (Latin1Compare("Out",option) == 0)
                    compose=OutCompositeOp;
                  if (Latin1Compare("Atop",option) == 0)
                    compose=AtopCompositeOp;
                  if (Latin1Compare("Xor",option) == 0)
                    compose=XorCompositeOp;
                  if (Latin1Compare("Plus",option) == 0)
                    compose=PlusCompositeOp;
                  if (Latin1Compare("Minus",option) == 0)
                    compose=MinusCompositeOp;
                  if (Latin1Compare("Add",option) == 0)
                    compose=AddCompositeOp;
                  if (Latin1Compare("Subtract",option) == 0)
                    compose=SubtractCompositeOp;
                  if (Latin1Compare("Difference",option) == 0)
                    compose=DifferenceCompositeOp;
                  if (Latin1Compare("Bumpmap",option) == 0)
                    compose=BumpmapCompositeOp;
                  if (Latin1Compare("Replace",option) == 0)
                    compose=ReplaceCompositeOp;
                  if (Latin1Compare("ReplaceRed",option) == 0)
                    compose=ReplaceRedCompositeOp;
                  if (Latin1Compare("ReplaceGreen",option) == 0)
                    compose=ReplaceGreenCompositeOp;
                  if (Latin1Compare("ReplaceBlue",option) == 0)
                    compose=ReplaceBlueCompositeOp;
                  if (Latin1Compare("ReplaceMatte",option) == 0)
                    compose=ReplaceMatteCompositeOp;
                  if (compose == UndefinedCompositeOp)
                    MagickError(OptionError,"Invalid compose type",option);
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
          if (Latin1Compare("displace",option+1) == 0)
            {
              displacement_geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%lf",&sans))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&displacement_geometry,argv[i]);
                  compose=DisplaceCompositeOp;
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
              image_info.dither=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'f':
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
        case 'g':
        {
          if (strncmp("geometry",option+1,2) == 0)
            {
              geometry=(char *) NULL;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !IsGeometry(argv[i]))
                    MagickError(OptionError,"Missing geometry",option);
                  (void) CloneString(&geometry,argv[i]);
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
                  if (Latin1Compare("Forget",option) == 0)
                    gravity=ForgetGravity;
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
          if (strncmp("matte",option+1,5) == 0)
            break;
          if (strncmp("monochrome",option+1,2) == 0)
            break;
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 'n':
        {
          if (strncmp("negate",option+1,3) == 0)
            break;
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
        case 's':
        {
          if (strncmp("scene",option+1,2) == 0)
            {
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing scene number",option);
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
          if (strncmp("stegano",option+1,4) == 0)
            {
              stegano=0;
              if (*option == '-')
                {
                  i++;
                  if ((i == argc) || !sscanf(argv[i],"%d",&x))
                    MagickError(OptionError,"Missing offset",option);
                  stegano=atoi(argv[i])+1;
                }
              break;
            }
          if (strncmp("stereo",option+1,4) == 0)
            {
              stereo=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
          break;
        }
        case 't':
        {
          if (strncmp("tile",option+1,2) == 0)
            {
              tile=(*option == '-');
              break;
            }
          MagickError(OptionError,"Unrecognized option",option);
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
  }
  if ((image == (Image *) NULL) || (composite_image == (Image *) NULL))
    Usage(client_name);
  if (mask_image != (Image *) NULL)
    {
      CompositeImage(composite_image,ReplaceMatteCompositeOp,mask_image,0,0);
      DestroyImage(mask_image);
    }
  if (compose == BlendCompositeOp)
    {
      register RunlengthPacket
        *p;

      unsigned short
        index;

      /*
        Create mattes for blending.
      */
      index=(unsigned short)
        (DownScale(MaxRGB)-(((int) DownScale(MaxRGB)*blend)/100));
      image->class=DirectClass;
      image->matte=True;
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        p->index=index;
        p++;
      }
      index=(unsigned short) (DownScale(MaxRGB)-index);
      composite_image->class=DirectClass;
      composite_image->matte=True;
      p=composite_image->pixels;
      for (i=0; i < (int) composite_image->packets; i++)
      {
        p->index=index;
        p++;
      }
    }
  if (compose == DisplaceCompositeOp)
    composite_image->geometry=displacement_geometry;
  /*
    Combine image.
  */
  if (stegano != 0)
    {
      image->offset=stegano-1;
      combined_image=SteganoImage(image,composite_image);
    }
  else
    if (stereo)
      combined_image=StereoImage(image,composite_image);
    else
      if (tile)
        {
          /*
            Tile the composite image.
          */
          for (y=0; y < (int) image->rows; y+=composite_image->rows)
            for (x=0; x < (int) image->columns; x+=composite_image->columns)
              CompositeImage(image,compose,composite_image,x,y);
          combined_image=image;
        }
      else
        {
          unsigned int
            size;

          /*
            Digitally composite image.
          */
          size=0;
          x=0;
          y=0;
          if (geometry != (char *) NULL)
            (void) ParseImageGeometry(geometry,&x,&y,&size,&size);
          switch (gravity)
          {
            case NorthWestGravity:
              break;
            case NorthGravity:
            {
              x+=(image->columns-composite_image->columns) >> 1;
              break;
            }
            case NorthEastGravity:
            {
              x+=image->columns-composite_image->columns;
              break;
            }
            case WestGravity:
            {
              y+=(image->rows-composite_image->rows) >> 1;
              break;
            }
            case ForgetGravity:
            {
              char
                geometry[MaxTextExtent];

              /*
                Stretch composite to the same size as the image.
              */
              FormatString(geometry,"%ux%u+0+0",image->columns,image->rows);
              TransformImage(&composite_image,(char *) NULL,geometry);
              break;
            }
            case StaticGravity:
            case CenterGravity:
            default:
            {
              x+=(image->columns-composite_image->columns) >> 1;
              y+=(image->rows-composite_image->rows) >> 1;
              break;
            }
            case EastGravity:
            {
              x+=image->columns-composite_image->columns;
              y+=(image->rows-composite_image->rows) >> 1;
              break;
            }
            case SouthWestGravity:
            {
              y+=image->rows-composite_image->rows;
              break;
            }
            case SouthGravity:
            {
              x+=(image->columns-composite_image->columns) >> 1;
              y+=image->rows-composite_image->rows;
              break;
            }
            case SouthEastGravity:
            {
              x+=image->columns-composite_image->columns;
              y+=image->rows-composite_image->rows;
              break;
            }
          }
          CompositeImage(image,compose,composite_image,x,y);
          combined_image=image;
        }
  if (combined_image == (Image *) NULL)
    MagickError(OptionError,"Missing an image file name",(char *) NULL);
  /*
    Transmogrify image as defined by the image processing options.
  */
  CommentImage(combined_image,composite_image->comments);
  MogrifyImage(&image_info,argc,argv,&combined_image);
  /*
    Write image.
  */
  (void) strcpy(combined_image->filename,write_filename);
  SetImageInfo(&image_info,True);
  status=WriteImage(&image_info,combined_image);
  if (image_info.verbose)
    DescribeImage(combined_image,(FILE *) NULL,False);
  DestroyDelegateInfo();
  Exit(status ? 0 : errno);
  return(False);
}
