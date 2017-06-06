/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%              PPPP   RRRR   EEEEE  V   V  IIIII  EEEEE  W   W                %
%              P   P  R   R  E      V   V    I    E      W   W                %
%              PPPP   RRRR   EEE    V   V    I    EEE    W   W                %
%              P      R R    E       V V     I    E      W W W                %
%              P      R  R   EEEEE    V    IIIII  EEEEE   W W                 %
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
  Constant declarations.
*/
const char
  *DefaultPreviewGeometry = "204x204+10+10";

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e P R E V I E W I m a g e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WritePREVIEWImage creates several tiles each with a varying
%  stength of an image enhancement function (e.g. gamma).  The image is written
%  in the MIFF format.
%
%  The format of the WritePREVIEWImage method is:
%
%      unsigned int WritePREVIEWImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WritePREVIEWImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WritePREVIEWImage(const ImageInfo *image_info,Image *image)
{
#define NumberTiles  9
#define PreviewImageText  "  Creating image preview...  "

  char
    *commands[NumberTiles+6],
    factor[MaxTextExtent],
    label[MaxTextExtent];

  float
    degrees,
    gamma,
    percentage,
    threshold;

  Image
    *images[NumberTiles],
    *montage_image,
    *preview_image;

  ImageInfo
    *local_info;

  int
    argc,
    x,
    y;

  MonitorHandler
    handler;

  MontageInfo
    montage_info;

  register int
    i;

  unsigned int
    colors,
    height,
    status,
    width;

  /*
    Scale the image to tile size.
  */
  TransformRGBImage(image,RGBColorspace);
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  (void) ParseImageGeometry(DefaultPreviewGeometry,&x,&y,&width,&height);
  image->orphan=True;
  preview_image=ZoomImage(image,width,height);
  image->orphan=False;
  if (preview_image == (Image *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  LabelImage(preview_image,DefaultTileLabel);
  /*
    Apply enhancement at varying strengths.
  */
  local_info=CloneImageInfo(image_info);
  if (local_info == (ImageInfo *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  local_info->quality=0;
  degrees=0;
  gamma=(-0.2f);
  colors=2;
  x=0;
  y=0;
  percentage=0.0;
  threshold=0.0;
  commands[0]=SetClientName((char *) NULL);
  for (i=0; i < NumberTiles; i++)
  {
    images[i]=
      CloneImage(preview_image,preview_image->columns,preview_image->rows,True);
    if (images[i] == (Image *) NULL)
      {
        for (x=0;  x < i; x++)
          DestroyImage(images[x]);
        WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
      }
    argc=1;
    if (i == (NumberTiles >> 1))
      {
        commands[argc++]="-mattecolor";
        commands[argc++]="#dfdfdf";;
        MogrifyImage(local_info,argc,commands,&images[i]);
        continue;
      }
    handler=SetMonitorHandler((MonitorHandler) NULL);
    switch (image_info->preview_type)
    {
      case RotatePreview:
      {
        FormatString(factor,"%.1f",degrees+=45.0);
        FormatString(label,"rotate %.1024s",factor);
        commands[argc++]="-rotate";
        commands[argc++]=factor;
        commands[argc++]="-crop";
        commands[argc++]="0x0";
        break;
      }
      case ShearPreview:
      {
        degrees+=10.0;
        FormatString(factor,"%.1fx%.1f",degrees,2.0*degrees);
        FormatString(label,"shear %.1024s",factor);
        commands[argc++]="-shear";
        commands[argc++]=factor;
        commands[argc++]="-crop";
        commands[argc++]="0x0";
        break;
      }
      case RollPreview:
      {
        x+=preview_image->columns/NumberTiles;
        y+=preview_image->rows/NumberTiles;
        FormatString(factor,"%+d%+d",x,y);
        FormatString(label,"roll %.1024s",factor);
        commands[argc++]="-roll";
        commands[argc++]=factor;
        break;
      }
      case HuePreview:
      {
        FormatString(factor,"0,0,%.1f",percentage);
        FormatString(label,"modulate %.1024s",factor);
        commands[argc++]="-modulate";
        commands[argc++]=factor;
        break;
      }
      case SaturationPreview:
      {
        FormatString(factor,"0,%.1f",percentage);
        FormatString(label,"modulate %.1024s",factor);
        commands[argc++]="-modulate";
        commands[argc++]=factor;
        break;
      }
      case BrightnessPreview:
      {
        FormatString(factor,"%.1f",percentage);
        FormatString(label,"modulate %.1024s",factor);
        commands[argc++]="-modulate";
        commands[argc++]=factor;
        break;
      }
      case GammaPreview:
      {
        FormatString(factor,"%.1f",gamma+=0.4f);
        FormatString(label,"gamma %.1024s",factor);
        commands[argc++]="-gamma";
        commands[argc++]=factor;
        break;
      }
      case SpiffPreview:
      {
        for (x=0; x < i; x++)
          commands[argc++]="-contrast";
        FormatString(label,"-contrast %d",i+1);
        break;
      }
      case DullPreview:
      {
        for (x=0; x < i; x++)
          commands[argc++]="+contrast";
        FormatString(label,"+contrast %d",i+1);
        break;
      }
      case GrayscalePreview:
      {
        FormatString(factor,"%u",colors);
        colors<<=1;
        FormatString(label,"colors %.1024s",factor);
        commands[argc++]="-colorspace";
        commands[argc++]="gray";
        commands[argc++]="-colors";
        commands[argc++]=factor;
        break;
      }
      case QuantizePreview:
      {
        FormatString(factor,"%u",colors<<=1);
        FormatString(label,"colors %.1024s",factor);
        commands[argc++]="-colors";
        commands[argc++]=factor;
        break;
      }
      case DespecklePreview:
      {
        for (x=0; x < i; x++)
          commands[argc++]="-despeckle";
        FormatString(label,"despeckle %d",i+1);
        break;
      }
      case ReduceNoisePreview:
      {
        for (x=0; x < i; x++)
          commands[argc++]="-noise";
        FormatString(label,"noise %d",i+1);
        break;
      }
      case AddNoisePreview:
      {
        switch (x)
        {
          case 0: (void) strcpy(factor,"uniform"); break;
          case 1: (void) strcpy(factor,"Gaussian"); break;
          case 2: (void) strcpy(factor,"multiplicative"); break;
          case 3: (void) strcpy(factor,"impulse"); break;
          case 4: (void) strcpy(factor,"laplacian"); break;
          case 5: (void) strcpy(factor,"Poisson"); break;
          default: (void) strcpy(images[i]->magick,"NULL"); break;
        }
        x++;
        FormatString(label,"+noise %.1024s",factor);
        commands[argc++]="+noise";
        commands[argc++]=factor;
        break;
      }
      case SharpenPreview:
      {
        FormatString(factor,"%.1f",percentage);
        FormatString(label,"sharpen %.1024s",factor);
        commands[argc++]="-sharpen";
        commands[argc++]=factor;
        break;
      }
      case BlurPreview:
      {
        FormatString(factor,"%.1f",percentage);
        FormatString(label,"-blur %.1024s",factor);
        commands[argc++]="-blur";
        commands[argc++]=factor;
        break;
      }
      case ThresholdPreview:
      {
        FormatString(factor,"%d",(int) ((percentage*(MaxRGB+1))/100));
        FormatString(label,"threshold %.1024s",factor);
        commands[argc++]="-threshold";
        commands[argc++]=factor;
        break;
      }
      case EdgeDetectPreview:
      {
        FormatString(factor,"%.1f",percentage);
        FormatString(label,"edge %.1024s",factor);
        commands[argc++]="-edge";
        commands[argc++]=factor;
        break;
      }
      case SpreadPreview:
      {
        FormatString(factor,"%d",i+1);
        FormatString(label,"spread %.1024s",factor);
        commands[argc++]="-spread";
        commands[argc++]=factor;
        break;
      }
      case SolarizePreview:
      {
        FormatString(factor,"%.1f",percentage);
        FormatString(label,"solarize %.1024s",factor);
        commands[argc++]="-solarize";
        commands[argc++]=factor;
        break;
      }
      case ShadePreview:
      {
        if (i == 0)
          {
            (void) strcpy(label,"+shade");
            commands[argc++]="+shade";
            break;
          }
        degrees+=10.0;
        FormatString(factor,"%.1fx%.1f",degrees,degrees);
        FormatString(label,"shade %.1024s",factor);
        commands[argc++]="-shade";
        commands[argc++]=factor;
        break;
      }
      case RaisePreview:
      {
        FormatString(factor,"%d",i+1);
        FormatString(label,"raise %.1024s",factor);
        commands[argc++]="-raise";
        commands[argc++]=factor;
        break;
      }
      case SegmentPreview:
      {
        threshold+=0.4f;
        FormatString(factor,"%.1fx%.1f",threshold,threshold);
        FormatString(label,"segment %.1024s",factor);
        commands[argc++]="-colors";
        commands[argc++]=factor;
        break;
      }
      case SwirlPreview:
      {
        FormatString(factor,"%.1f",degrees+=45.0);
        FormatString(label,"swirl %.1024s",factor);
        commands[argc++]="-swirl";
        commands[argc++]=factor;
        break;
      }
      case ImplodePreview:
      {
        FormatString(factor,"%.1f",percentage);
        FormatString(label,"implode %.1024s",factor);
        commands[argc++]="-implode";
        commands[argc++]=factor;
        break;
      }
      case WavePreview:
      {
        degrees+=5.0;
        FormatString(factor,"%.1fx%.1f",degrees,degrees);
        FormatString(label,"wave %.1024s",factor);
        commands[argc++]="-implode";
        commands[argc++]=factor;
        break;
      }
      case OilPaintPreview:
      {
        FormatString(factor,"%i",i+1);
        FormatString(label,"paint %.1024s",factor);
        commands[argc++]="-paint";
        commands[argc++]=factor;
        break;
      }
      case CharcoalDrawingPreview:
      {
        FormatString(factor,"%.1f",percentage);
        FormatString(label,"charcoal %.1024s",factor);
        commands[argc++]="-charcoal";
        commands[argc++]=factor;
        break;
      }
      case JPEGPreview:
      default:
      {
        local_info->quality=(unsigned int) (percentage+13.0);
        FormatString(factor,"%u",local_info->quality);
        TemporaryFilename(images[i]->filename);
        status=WriteJPEGImage(local_info,images[i]);
        if (status != False)
          {
            Image
              *quality_image;

            (void) strcpy(local_info->filename,images[i]->filename);
            quality_image=ReadImage(local_info);
            (void) remove(images[i]->filename);
            if (quality_image != (Image *) NULL)
              {
                DestroyImage(images[i]);
                images[i]=quality_image;
              }
          }
        FormatString(label,"quality %.1024s\n%ldb ",factor,images[i]->filesize);
        if (images[i]->filesize >= (1 << 24))
          FormatString(label,"quality %.1024s\n%ldmb ",factor,
            images[i]->filesize/1024/1024);
        else
          if (images[i]->filesize >= (1 << 14))
            FormatString(label,"quality %.1024s\n%ldkb ",factor,
              images[i]->filesize/1024);
        break;
      }
    }
    percentage+=12.5;
    commands[argc++]="-label";
    commands[argc++]=label;
    MogrifyImage(local_info,argc,commands,&images[i]);
    (void) SetMonitorHandler(handler);
    ProgressMonitor(PreviewImageText,i,NumberTiles);
  }
  DestroyImageInfo(local_info);
  DestroyImage(preview_image);
  /*
    Create the PCD Overview image.
  */
  for (i=1; i < NumberTiles; i++)
  {
    images[i]->previous=images[i-1];
    images[i-1]->next=images[i];
  }
  GetMontageInfo(&montage_info);
  (void) strcpy(montage_info.filename,image->filename);
  (void) CloneString(&montage_info.geometry,DefaultPreviewGeometry);
  (void) CloneString(&montage_info.tile,"3x3");
  (void) CloneString(&montage_info.font,image_info->font);
  montage_info.pointsize=image_info->pointsize;
  (void) CloneString(&montage_info.frame,DefaultTileFrame);
  montage_info.shadow=True;
  montage_image=MontageImages(*images,&montage_info);
  DestroyMontageInfo(&montage_info);
  for (i=0;  i < i; i++)
    DestroyImage(images[i]);
  if (montage_image == (Image *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  if (montage_image->montage != (char *) NULL)
    {
      /*
        Free image directory.
      */
      FreeMemory(montage_image->montage);
      montage_image->montage=(char *) NULL;
      if (image->directory != (char *) NULL)
        {
          FreeMemory(montage_image->directory);
          montage_image->directory=(char *) NULL;
        }
    }
  status=WriteMIFFImage(image_info,montage_image);
  DestroyImage(montage_image);
  return(status);
}
