/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%           IIIII  DDDD   EEEEE  N   N  TTTTT  IIIII  FFFFF  Y   Y            %
%             I    D   D  E      NN  N    T      I    F       Y Y             %
%             I    D   D  EEE    N N N    T      I    FFF      Y              %
%             I    D   D  E      N  NN    T      I    F        Y              %
%           IIIII  DDDD   EEEEE  N   N    T    IIIII  F        Y              %
%                                                                             %
%                                                                             %
%               Identify an Image Format and Characteristics.                 %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                            September 1994                                   %
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
%  Identify describes the format and characteristics of one or more image
%  files.  It will also report if an image is incomplete or corrupt.
%
%  The identify program syntax is:
%
%  Usage: identify [options ...] file [ files... ]
%
%  Where options include:
%    -ping           efficiently determine image characteristics
%    -size geometry  width and height of image
%    -verbose        print detailed information about the image
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
      "-ping           efficiently determine image characteristics",
      "-size geometry  width and height of image",
      "-verbose        print detailed information about the image",
      (char *) NULL
    };

  (void) printf("Version: %.1024s\n",MagickVersion);
  (void) printf("Copyright: %.1024s\n\n",MagickCopyright);
  (void) printf("Usage: %.1024s [ options ... ] file [ file ... ]\n",client_name);
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
    *option;

  Image
    *image,
    *p;

  ImageInfo
    image_info;

  int
    number_images;

  register int
    i;

  unsigned int
    count;

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
  count=0;
  GetImageInfo(&image_info);
  number_images=0;
  /*
    Identify an image.
  */
  for (i=1; i < argc; i++)
  {
    option=argv[i];
    if ((Extent(option) >= 2) && ((*option == '-') || (*option == '+')))
      {
        switch(*(option+1))
        {
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
          case 'p':
          {
            if (strncmp("ping",option+1,2) == 0)
              {
                image_info.ping=(*option == '-');
                break;
              }
            MagickError(OptionError,"Unrecognized option",option);
            break;
          }
          case 's':
          {
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
        continue;
      }
    /*
      Identify image.
    */
    (void) strcpy(image_info.filename,argv[i]);
    if (image_info.ping)
      {
        image_info.verbose=True;
        image=PingImage(&image_info);
        number_images++;
        continue;
      }
    image=ReadImage(&image_info);
    if (image == (Image *) NULL)
      continue;
    for (p=image; p != (Image *) NULL; p=p->next)
    {
      if (p->scene == 0)
        p->scene=count++;
      DescribeImage(p,(FILE *) NULL,image_info.verbose);
    }
    DestroyImages(image);
    number_images++;
  }
  if (number_images == 0)
    MagickError(OptionError,"Missing an image file name",(char *) NULL);
  DestroyDelegateInfo();
  Exit(0);
  return(False);
}
