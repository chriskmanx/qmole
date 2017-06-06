/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        JJJJJ  PPPP   EEEEE   GGGG                           %
%                          J    P   P  E      G                               %
%                          J    PPPP   EEE    G  GG                           %
%                        J J    P      E      G   G                           %
%                        JJJ    P      EEEEE   GGG                            %
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
% This software is based in part on the work of the Independent JPEG Group.
% See ftp://ftp.uu.net/graphics/jpeg/jpegsrc.v6b.tar.gz for copyright and
% licensing restrictions.  Blob support contributed by Glenn Randers-Pehrson.
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"

/*
  Define declarations.
*/
#define ICC_MARKER  (JPEG_APP0+2)
#define IPTC_MARKER  (JPEG_APP0+13)
#define MaxBufferExtent  8192

#if defined(HasJPEG)
#include <setjmp.h>
#define JPEG_INTERNAL_OPTIONS
#include "jpeglib.h"
#include "jerror.h"

typedef struct _DestinationManager
{
  struct jpeg_destination_mgr
    manager;

  Image
    *image;

  JOCTET
    *buffer;
} DestinationManager;

typedef struct _SourceManager
{
  struct jpeg_source_mgr
    manager;

  Image
    *image;

  JOCTET
    *buffer;

  boolean
    start_of_blob;
} SourceManager;

static Image
  *image;

static jmp_buf
  error_recovery;

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d J P E G I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadJPEGImage reads a JPEG image file and returns it.  It allocates
%  the memory necessary for the new Image structure and returns a pointer to
%  the new image.
%
%  The format of the ReadJPEGImage method is:
%
%      Image *ReadJPEGImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadJPEGImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o filename:  Specifies the name of the jpeg image to read.
%
%
*/

static void EmitMessage(j_common_ptr jpeg_info,int level)
{
  char
    message[JMSG_LENGTH_MAX];

  (jpeg_info->err->format_message)(jpeg_info,message);
  if (level < 0)
    {
      if ((jpeg_info->err->num_warnings == 0) ||
          (jpeg_info->err->trace_level >= 3))
        MagickWarning(DelegateWarning,(char *) message,image->filename);
      jpeg_info->err->num_warnings++;
    }
  else
    if (jpeg_info->err->trace_level >= level)
      MagickWarning(DelegateWarning,(char *) message,image->filename);
}

static boolean FillInputBuffer(j_decompress_ptr cinfo)
{
  SourceManager
    *source;

  source=(SourceManager *) cinfo->src;
  source->manager.bytes_in_buffer=
    ReadBlob(source->image,MaxBufferExtent,(char *) source->buffer);
  if (source->manager.bytes_in_buffer == 0)
    {
      if (source->start_of_blob)
        ERREXIT(cinfo,JERR_INPUT_EMPTY);
      WARNMS(cinfo,JWRN_JPEG_EOF);
      source->buffer[0]=(JOCTET) 0xff;
      source->buffer[1]=(JOCTET) JPEG_EOI;
      source->manager.bytes_in_buffer=2;
    }
  source->manager.next_input_byte=source->buffer;
  source->start_of_blob=FALSE;
  return(TRUE);
}

static unsigned int GetCharacter(j_decompress_ptr jpeg_info)
{
  if (jpeg_info->src->bytes_in_buffer == 0)
    (*jpeg_info->src->fill_input_buffer)(jpeg_info);
  jpeg_info->src->bytes_in_buffer--;
  return(GETJOCTET(*jpeg_info->src->next_input_byte++));
}

static void InitializeSource(j_decompress_ptr cinfo)
{
  SourceManager
    *source;

  source=(SourceManager *) cinfo->src;
  source->start_of_blob=TRUE;
}

static void JPEGErrorHandler(j_common_ptr jpeg_info)
{
  EmitMessage(jpeg_info,0);
  longjmp(error_recovery,1);
}

static boolean ReadColorProfile(j_decompress_ptr jpeg_info)
{
  char
    magick[12];

  long int
    length;

  register unsigned char
    *p;

  register int
    i;

  /*
    Determine length of color profile.
  */
  length=GetCharacter(jpeg_info) << 8;
  length+=GetCharacter(jpeg_info);
  length-=2;
  for (i=0; i < 12; i++)
    magick[i]=GetCharacter(jpeg_info);
  if (Latin1Compare(magick,"ICC_PROFILE") != 0)
    {
      /*
        Not a ICC profile, return.
      */
      for (i=0; i < length-12; i++)
        (void) GetCharacter(jpeg_info);
      return(True);
    }
  (void) GetCharacter(jpeg_info);  /* id */
  (void) GetCharacter(jpeg_info);  /* markers */
  length-=14;
  if (image->color_profile.length == 0)
    image->color_profile.info=(unsigned char *)
      AllocateMemory((unsigned int) length*sizeof(unsigned char));
  else
    image->color_profile.info=(unsigned char *) ReallocateMemory((char *)
      image->color_profile.info,(image->color_profile.length+length)*
      sizeof(unsigned char));
  if (image->color_profile.info == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  /*
    Read color profile.
  */
  p=image->color_profile.info+image->color_profile.length;
  image->color_profile.length+=length;
  while (--length >= 0)
    *p++=GetCharacter(jpeg_info);
  return(True);
}

static boolean ReadComment(j_decompress_ptr jpeg_info)
{
  long int
    length;

  register char
    *p;

  /*
    Determine length of comment.
  */
  length=GetCharacter(jpeg_info) << 8;
  length+=GetCharacter(jpeg_info);
  length-=2;
  if (image->comments != (char *) NULL)
    image->comments=(char *) ReallocateMemory((char *) image->comments,
      (unsigned int) (Extent(image->comments)+length+1)*sizeof(char));
  else
    {
      image->comments=(char *)
        AllocateMemory((unsigned int) (length+1)*sizeof(char));
      if (image->comments != (char *) NULL)
        *image->comments='\0';
    }
  if (image->comments == (char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  /*
    Read comment.
  */
  p=image->comments+Extent(image->comments);
  while (--length >= 0)
    *p++=GetCharacter(jpeg_info);
  *p='\0';
  return(True);
}

static boolean ReadNewsProfile(j_decompress_ptr jpeg_info)
{
  long int
    length;

  register unsigned char
    *p;

  unsigned char
    tag[2];

  /*
    Determine length of IPTC profile.
  */
  length=GetCharacter(jpeg_info) << 8;
  length+=GetCharacter(jpeg_info);
  length-=2;
  for (*tag='\0'; length > 0; )
  {
    *tag=GetCharacter(jpeg_info);
    *(tag+1)=GetCharacter(jpeg_info);
    length-=2;
    if ((*tag == 0x1c) && (*(tag+1) == 0x02))
      break;
  }
  if (length <= 0)
    return(True);
  if (image->iptc_profile.length != 0)
    image->iptc_profile.info=(unsigned char *) ReallocateMemory((char *)
      image->iptc_profile.info,(unsigned int) (image->iptc_profile.length+
      length+2)*sizeof(unsigned char));
  else
    {
      image->iptc_profile.info=(unsigned char *)
        AllocateMemory((unsigned int) (length+2)*sizeof(unsigned char));
      if (image->iptc_profile.info != (unsigned char *) NULL)
        image->iptc_profile.length=0;
    }
  if (image->iptc_profile.info == (unsigned char *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  /*
    Read IPTC profile.
  */
  p=image->iptc_profile.info+image->iptc_profile.length;
  image->iptc_profile.length+=length+2;
  *p++=0x1c;
  *p++=0x02;
  while (--length >= 0)
    *p++=GetCharacter(jpeg_info);
  return(True);
}

static void SkipInputData(j_decompress_ptr cinfo,long number_bytes)
{
  SourceManager
    *source;

  if (number_bytes <= 0)
    return;
  source=(SourceManager *) cinfo->src;
  while (number_bytes > (long) source->manager.bytes_in_buffer)
  {
    number_bytes-=(long) source->manager.bytes_in_buffer;
    (void) FillInputBuffer(cinfo);
  }
  source->manager.next_input_byte+=(size_t) number_bytes;
  source->manager.bytes_in_buffer-=(size_t) number_bytes;
}

static void TerminateSource(j_decompress_ptr cinfo)
{
}

static void JPEGSourceManager(j_decompress_ptr cinfo,Image *image)
{
  SourceManager
    *source;

  if (cinfo->src == (JOCTET) NULL)
    {
      cinfo->src=(struct jpeg_source_mgr *) (*cinfo->mem->alloc_small)
        ((j_common_ptr) cinfo,JPOOL_PERMANENT,sizeof(SourceManager));
      source=(SourceManager *) cinfo->src;
      source->buffer=(JOCTET *) (*cinfo->mem->alloc_small)
        ((j_common_ptr) cinfo,JPOOL_PERMANENT,MaxBufferExtent*sizeof(JOCTET));
    }
  source=(SourceManager *) cinfo->src;
  source->manager.init_source=InitializeSource;
  source->manager.fill_input_buffer=FillInputBuffer;
  source->manager.skip_input_data=SkipInputData;
  source->manager.resync_to_restart=jpeg_resync_to_restart;
  source->manager.term_source=TerminateSource;
  source->manager.bytes_in_buffer=0;
  source->manager.next_input_byte=NULL;
  source->image=image;
}

Export Image *ReadJPEGImage(const ImageInfo *image_info)
{
  int
    x,
    y;

  JSAMPLE
    *jpeg_pixels;

  JSAMPROW
    scanline[1];

  Quantum
    blue,
    green,
    red;

  register int
    i;

  struct jpeg_decompress_struct
    jpeg_info;

  struct jpeg_error_mgr
    jpeg_error;

  register JSAMPLE
    *p;

  register long
    packets;

  register RunlengthPacket
    *q;

  unsigned int
    status;

  unsigned short
    index;

  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  status=OpenBlob(image_info,image,ReadBinaryType);
  if (status == False)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  /*
    Initialize image structure.
  */
  jpeg_info.err=jpeg_std_error(&jpeg_error);
  jpeg_info.err->emit_message=EmitMessage;
  jpeg_info.err->error_exit=JPEGErrorHandler;
  image->pixels=(RunlengthPacket *) NULL;
  jpeg_pixels=(JSAMPLE *) NULL;
  if (setjmp(error_recovery))
    {
      /*
        JPEG image is corrupt.
      */
      if (jpeg_pixels != (JSAMPLE *) NULL)
        FreeMemory((char *) jpeg_pixels);
      jpeg_destroy_decompress(&jpeg_info);
      DestroyImage(image);
      return((Image *) NULL);
    }
  jpeg_create_decompress(&jpeg_info);
  JPEGSourceManager(&jpeg_info,image);
  jpeg_set_marker_processor(&jpeg_info,JPEG_COM,ReadComment);
  jpeg_set_marker_processor(&jpeg_info,ICC_MARKER,ReadColorProfile);
  jpeg_set_marker_processor(&jpeg_info,IPTC_MARKER,ReadNewsProfile);
  (void) jpeg_read_header(&jpeg_info,True);
  if (jpeg_info.out_color_space == JCS_CMYK)
    image->colorspace=CMYKColorspace;
  if (jpeg_info.saw_JFIF_marker)
    {
      if ((jpeg_info.density_unit != 0) || (jpeg_info.X_density != 1.0) ||
          (jpeg_info.Y_density != 1.0))
        {
          /*
            Set image resolution.
          */
          image->x_resolution=jpeg_info.X_density;
          image->y_resolution=jpeg_info.Y_density;
          if (jpeg_info.density_unit == 1)
            image->units=PixelsPerInchResolution;
          if (jpeg_info.density_unit == 2)
            image->units=PixelsPerCentimeterResolution;
        }
    }
  if ((image->columns*image->rows) != 0)
    {
      unsigned long
        scale_factor;

      /*
        Let the JPEG library subsample for us.
      */
      jpeg_calc_output_dimensions(&jpeg_info);
      image->magick_columns=jpeg_info.output_width;
      image->magick_rows=jpeg_info.output_height;
      scale_factor=UpShift(jpeg_info.output_width)/image->columns;
      if (scale_factor > (UpShift(jpeg_info.output_height)/image->rows))
        scale_factor=UpShift(jpeg_info.output_height)/image->rows;
      jpeg_info.scale_denom=DownShift(scale_factor);
      jpeg_calc_output_dimensions(&jpeg_info);
    }
  if (image_info->subrange != 0)
    {
      jpeg_info.scale_denom=DownShift(image_info->subrange);
      jpeg_calc_output_dimensions(&jpeg_info);
    }
#if (JPEG_LIB_VERSION >= 61) && !defined(D_LOSSLESS_SUPPORTED)
  image->interlace=jpeg_info.progressive_mode ? PlaneInterlace : NoInterlace;
#endif
  jpeg_start_decompress(&jpeg_info);
  image->compression=JPEGCompression;
  image->columns=jpeg_info.output_width;
  image->rows=jpeg_info.output_height;
  if (image_info->ping)
    {
      jpeg_destroy_decompress(&jpeg_info);
      CloseBlob(image);
      return(image);
    }
  if (jpeg_info.out_color_space == JCS_GRAYSCALE)
    {
      /*
        Initialize grayscale colormap.
      */
      image->class=PseudoClass;
      image->colors=MaxRGB+1;
      image->colormap=(ColorPacket *)
        AllocateMemory(image->colors*sizeof(ColorPacket));
      if (image->colormap == (ColorPacket *) NULL)
        ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
      for (i=0; i < (int) image->colors; i++)
      {
        image->colormap[i].red=UpScale(i);
        image->colormap[i].green=UpScale(i);
        image->colormap[i].blue=UpScale(i);
      }
    }
  packets=0;
  jpeg_pixels=(JSAMPLE *)
    AllocateMemory(jpeg_info.output_components*image->columns*sizeof(JSAMPLE));
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->columns*image->rows*sizeof(RunlengthPacket));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (jpeg_pixels == (JSAMPLE *) NULL))
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Convert JPEG pixels to runlength-encoded packets.
  */
  red=0;
  green=0;
  blue=0;
  index=0;
  scanline[0]=(JSAMPROW) jpeg_pixels;
  q=image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; y < (int) image->rows; y++)
  {
    (void) jpeg_read_scanlines(&jpeg_info,scanline,1);
    p=jpeg_pixels;
    for (x=0; x < (int) image->columns; x++)
    {
      if (jpeg_info.data_precision > QuantumDepth)
        {
          if (jpeg_info.out_color_space == JCS_GRAYSCALE)
            index=GETJSAMPLE(*p++) >> 4;
          else
            {
              red=(Quantum) (GETJSAMPLE(*p++) >> 4);
              green=(Quantum) (GETJSAMPLE(*p++) >> 4);
              blue=(Quantum) (GETJSAMPLE(*p++) >> 4);
              if (image->colorspace == CMYKColorspace)
                index=(unsigned short) (GETJSAMPLE(*p++) >> 4);
            }
        }
      else
        if (jpeg_info.out_color_space == JCS_GRAYSCALE)
          index=GETJSAMPLE(*p++);
        else
          {
            red=(Quantum) UpScale(GETJSAMPLE(*p++));
            green=(Quantum) UpScale(GETJSAMPLE(*p++));
            blue=(Quantum) UpScale(GETJSAMPLE(*p++));
            if (image->colorspace == CMYKColorspace)
              index=(unsigned short) UpScale(GETJSAMPLE(*p++));
          }
      if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
          (index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (packets != 0)
            q++;
          packets++;
          q->red=red;
          q->green=green;
          q->blue=blue;
          q->index=index;
          q->length=0;
        }
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
  }
  SetRunlengthPackets(image,packets);
  if (image->class == PseudoClass)
    SyncImage(image);
  else
    if (image->colorspace == CMYKColorspace)
      {
        /*
          Correct CMYK levels.
        */
        q=image->pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          q->red=MaxRGB-q->red;
          q->green=MaxRGB-q->green;
          q->blue=MaxRGB-q->blue;
          q->index=MaxRGB-q->index;
          q++;
        }
      }
  /*
    Free jpeg resources.
  */
  (void) jpeg_finish_decompress(&jpeg_info);
  jpeg_destroy_decompress(&jpeg_info);
  FreeMemory((char *) jpeg_pixels);
  CloseBlob(image);
  return(image);
}
#else
Export Image *ReadJPEGImage(const ImageInfo *image_info)
{
  MagickWarning(MissingDelegateWarning,"JPEG library is not available",
    image_info->filename);
  return((Image *) NULL);
}
#endif

#if defined(HasJPEG)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  W r i t e J P E G I m a g e                                                %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteJPEGImage writes a JPEG image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the WriteJPEGImage method is:
%
%      unsigned int WriteJPEGImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method WriteJPEGImage return True if the image is written.
%      False is returned is there is of a memory shortage or if the image
%      file cannot be opened for writing.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o jpeg_image:  A pointer to a Image structure.
%
%
*/

static boolean EmptyOutputBuffer(j_compress_ptr cinfo)
{
  DestinationManager
    *destination;

  destination=(DestinationManager *) cinfo->dest;
  destination->manager.free_in_buffer=WriteBlob(destination->image,
    MaxBufferExtent,(char *) destination->buffer);
  if (destination->manager.free_in_buffer != MaxBufferExtent)
    ERREXIT(cinfo,JERR_FILE_WRITE);
  destination->manager.next_output_byte=destination->buffer;
  return(TRUE);
}

static void InitializeDestination(j_compress_ptr cinfo)
{
  DestinationManager
    *destination;

  destination=(DestinationManager *) cinfo->dest;
  destination->buffer=(JOCTET *) (*cinfo->mem->alloc_small)
    ((j_common_ptr) cinfo,JPOOL_IMAGE,MaxBufferExtent*sizeof(JOCTET));
  destination->manager.next_output_byte=destination->buffer;
  destination->manager.free_in_buffer=MaxBufferExtent;
}

static void JPEGWarningHandler(j_common_ptr jpeg_info,int level)
{
  char
    message[JMSG_LENGTH_MAX];

  (jpeg_info->err->format_message)(jpeg_info,message);
  if (level < 0)
    {
      if ((jpeg_info->err->num_warnings == 0) ||
          (jpeg_info->err->trace_level >= 3))
        MagickWarning(DelegateWarning,(char *) message,(char *) NULL);
      jpeg_info->err->num_warnings++;
    }
  else
    if (jpeg_info->err->trace_level >= level)
      MagickWarning(DelegateWarning,(char *) message,(char *) NULL);
}

static void TerminateDestination(j_compress_ptr cinfo)
{
  DestinationManager
    *destination;

  destination=(DestinationManager *) cinfo->dest;
  if ((MaxBufferExtent-destination->manager.free_in_buffer) > 0)
    {
      unsigned long
        number_bytes;

      number_bytes=WriteBlob(destination->image,MaxBufferExtent-
        destination->manager.free_in_buffer,(char *) destination->buffer);
      if (number_bytes != (MaxBufferExtent-destination->manager.free_in_buffer))
        ERREXIT(cinfo,JERR_FILE_WRITE);
    }
  if (FlushBlob(destination->image))
    ERREXIT(cinfo,JERR_FILE_WRITE);
}

static void WriteColorProfile(j_compress_ptr jpeg_info,Image *image)
{
  register int
    i,
    j;

  unsigned char
    *profile;

  unsigned int
    length;

  /*
    Save color profile as a APP marker.
  */
  for (i=0; i < (int) image->color_profile.length; i+=65519)
  {
    length=Min(image->color_profile.length-i,65519);
    profile=(unsigned char *) AllocateMemory((length+14)*sizeof(unsigned char));
    if (profile == (unsigned char *) NULL)
      break;
    (void) strcpy((char *) profile,"ICC_PROFILE");
    profile[12]=(i/65519)+1;
    profile[13]=(image->color_profile.length/65519)+1;
    for (j=0; j < (int) length; j++)
      profile[j+14]=image->color_profile.info[j];
    jpeg_write_marker(jpeg_info,ICC_MARKER,profile,(unsigned int) length+14);
    FreeMemory((char *) profile);
  }
}

static void WriteNewsProfile(j_compress_ptr jpeg_info,Image *image)
{
  int
    roundup;

  register int
    i,
    j;

  unsigned char
    *profile;

  unsigned int
    length;

  /*
    Save IPTC profile as a APP marker.
  */
  for (i=0; i < (int) image->iptc_profile.length; i+=65500)
  {
    length=Min(image->iptc_profile.length-i,65500);
    roundup=(length & 0x01); /* round up for Photoshop */
    profile=(unsigned char *)
      AllocateMemory((length+roundup+26)*sizeof(unsigned char));
    if (profile == (unsigned char *) NULL)
      break;
    (void) memcpy((char *) profile,"Photoshop 3.0 8BIM\04\04\0\0\0\0",24);
    profile[13]=0x00;
    profile[24]=length >> 8;
    profile[25]=length & 0xff;
    for (j=0; j < (int) length; j++)
      profile[j+26]=image->iptc_profile.info[j];
    if (roundup)
      profile[length+roundup+25]=0;
    jpeg_write_marker(jpeg_info,IPTC_MARKER,profile,(unsigned int)
      length+roundup+26);
    FreeMemory((char *) profile);
  }
}

static void JPEGDestinationManager(j_compress_ptr cinfo,Image * image)
{
  DestinationManager
    *destination;

  if (cinfo->dest == (JOCTET) NULL)
    cinfo->dest=(struct jpeg_destination_mgr *) (*cinfo->mem->alloc_small)
      ((j_common_ptr) cinfo,JPOOL_PERMANENT,sizeof(DestinationManager));
  destination=(DestinationManager *) cinfo->dest;
  destination->manager.init_destination=InitializeDestination;
  destination->manager.empty_output_buffer=EmptyOutputBuffer;
  destination->manager.term_destination=TerminateDestination;
  destination->image=image;
}

Export unsigned int WriteJPEGImage(const ImageInfo *image_info,Image *image)
{
  int
    x,
    y,
    flags,
    sans_offset;

  JSAMPLE
    *jpeg_pixels;

  JSAMPROW
    scanline[1];

  register int
    i,
    j;

  register JSAMPLE
    *q;

  register RunlengthPacket
    *p;

  struct jpeg_compress_struct
    jpeg_info;

  struct jpeg_error_mgr
    jpeg_error;

  unsigned int
    packets,
    status,
    x_resolution,
    y_resolution;

  /*
    Open image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  /*
    Initialize JPEG parameters.
  */
  jpeg_info.err=jpeg_std_error(&jpeg_error);
  jpeg_info.err->emit_message=JPEGWarningHandler;
  jpeg_create_compress(&jpeg_info);
  JPEGDestinationManager(&jpeg_info,image);
  jpeg_info.image_width=image->columns;
  jpeg_info.image_height=image->rows;
  jpeg_info.input_components=3;
  jpeg_info.in_color_space=JCS_RGB;
  if (Latin1Compare(image_info->magick,"JPEG24") != 0)
    if (IsGrayImage(image))
      {
        jpeg_info.input_components=1;
        jpeg_info.in_color_space=JCS_GRAYSCALE;
      }
  if (((image_info->colorspace != UndefinedColorspace) ||
       (image->colorspace != CMYKColorspace)) &&
       (image_info->colorspace != CMYKColorspace))
    TransformRGBImage(image,RGBColorspace);
  else
    {
      jpeg_info.input_components=4;
      jpeg_info.in_color_space=JCS_CMYK;
      if (image->colorspace != CMYKColorspace)
        RGBTransformImage(image,CMYKColorspace);
    }
  jpeg_set_defaults(&jpeg_info);
  flags=NoValue;
  x_resolution=72;
  y_resolution=72;
  if (image_info->density != (char *) NULL)
    flags=ParseGeometry(image_info->density,&sans_offset,&sans_offset,
      &x_resolution,&y_resolution);
  if (flags & WidthValue)
    image->x_resolution=x_resolution;
  if (flags & HeightValue)
    image->y_resolution=y_resolution;
  jpeg_info.density_unit=1;  /* default to DPI */
  if ((image->x_resolution != 0) && (image->y_resolution != 0))
    {
      /*
        Set image resolution.
      */
      jpeg_info.X_density=(short) image->x_resolution;
      jpeg_info.Y_density=(short) image->y_resolution;
      if (image->units == PixelsPerInchResolution)
        jpeg_info.density_unit=1;
      if (image->units == PixelsPerCentimeterResolution)
        jpeg_info.density_unit=2;
    }
  jpeg_set_quality(&jpeg_info,image_info->quality,True);
  jpeg_info.optimize_coding=True;
#if (JPEG_LIB_VERSION >= 61) && !defined(C_LOSSLESS_SUPPORTED)
  if (image_info->interlace != NoInterlace)
    jpeg_simple_progression(&jpeg_info);
#endif
  jpeg_start_compress(&jpeg_info,True);
  if (image->comments != (char *) NULL)
    for (i=0; i < Extent(image->comments); i+=65533)
      jpeg_write_marker(&jpeg_info,JPEG_COM,(unsigned char *) image->comments+i,
        (unsigned int) Min(Extent(image->comments+i),65533));
  if (image->color_profile.length > 0)
   WriteColorProfile(&jpeg_info,image);
  if (image->iptc_profile.length > 0)
    WriteNewsProfile(&jpeg_info,image);
  /*
    Convert MIFF to JPEG raster pixels.
  */
  packets=jpeg_info.input_components*image->columns;
  jpeg_pixels=(JSAMPLE *) AllocateMemory(packets*sizeof(JSAMPLE));
  if (jpeg_pixels == (JSAMPLE *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  x=0;
  y=0;
  p=image->pixels;
  q=jpeg_pixels;
  scanline[0]=(JSAMPROW) jpeg_pixels;
  if ((jpeg_info.data_precision > 8) && (QuantumDepth > 8))
    {
      if (jpeg_info.in_color_space == JCS_GRAYSCALE)
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            *q++=(JSAMPLE) (Intensity(*p) >> 4);
            x++;
            if (x == (int) image->columns)
              {
                (void) jpeg_write_scanlines(&jpeg_info,scanline,1);
                if (QuantumTick(y,image->rows))
                  ProgressMonitor(SaveImageText,y,image->rows);
                q=jpeg_pixels;
                x=0;
                y++;
              }
          }
          p++;
        }
      else
        if (jpeg_info.in_color_space == JCS_RGB)
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              *q++=(JSAMPLE) (p->red >> 4);
              *q++=(JSAMPLE) (p->green >> 4);
              *q++=(JSAMPLE) (p->blue >> 4);
              x++;
              if (x == (int) image->columns)
                {
                  (void) jpeg_write_scanlines(&jpeg_info,scanline,1);
                  if (QuantumTick(y,image->rows))
                    ProgressMonitor(SaveImageText,y,image->rows);
                  q=jpeg_pixels;
                  x=0;
                  y++;
                }
            }
            p++;
            if (QuantumTick(i,image->packets))
              ProgressMonitor(SaveImageText,i,image->packets);
          }
        else
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              /*
                Convert DirectClass packets to contiguous CMYK scanlines.
              */
              *q++=(JSAMPLE) (MaxRGB-(p->red >> 4));
              *q++=(JSAMPLE) (MaxRGB-(p->green >> 4));
              *q++=(JSAMPLE) (MaxRGB-(p->blue >> 4));
              *q++=(JSAMPLE) (MaxRGB-(p->index >> 4));
              x++;
              if (x == (int) image->columns)
                {
                  (void) jpeg_write_scanlines(&jpeg_info,scanline,1);
                  if (QuantumTick(y,image->rows))
                    ProgressMonitor(SaveImageText,y,image->rows);
                  q=jpeg_pixels;
                  x=0;
                  y++;
                }
            }
            p++;
          }
    }
  else
    if (jpeg_info.in_color_space == JCS_GRAYSCALE)
      for (i=0; i < (int) image->packets; i++)
      {
        for (j=0; j <= ((int) p->length); j++)
        {
          *q++=(JSAMPLE) DownScale(Intensity(*p));
          x++;
          if (x == (int) image->columns)
            {
              (void) jpeg_write_scanlines(&jpeg_info,scanline,1);
              if (QuantumTick(y,image->rows))
                ProgressMonitor(SaveImageText,y,image->rows);
              q=jpeg_pixels;
              x=0;
              y++;
            }
        }
        p++;
      }
    else
      if (jpeg_info.in_color_space == JCS_RGB)
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            *q++=(JSAMPLE) DownScale(p->red);
            *q++=(JSAMPLE) DownScale(p->green);
            *q++=(JSAMPLE) DownScale(p->blue);
            x++;
            if (x == (int) image->columns)
              {
                (void) jpeg_write_scanlines(&jpeg_info,scanline,1);
                if (QuantumTick(y,image->rows))
                  ProgressMonitor(SaveImageText,y,image->rows);
                q=jpeg_pixels;
                x=0;
                y++;
              }
          }
          p++;
        }
      else
        for (i=0; i < (int) image->packets; i++)
        {
          for (j=0; j <= ((int) p->length); j++)
          {
            /*
              Convert DirectClass packets to contiguous CMYK scanlines.
            */
            *q++=(JSAMPLE) DownScale(MaxRGB-p->red);
            *q++=(JSAMPLE) DownScale(MaxRGB-p->green);
            *q++=(JSAMPLE) DownScale(MaxRGB-p->blue);
            *q++=(JSAMPLE) DownScale(MaxRGB-p->index);
            x++;
            if (x == (int) image->columns)
              {
                (void) jpeg_write_scanlines(&jpeg_info,scanline,1);
                if (QuantumTick(y,image->rows))
                  ProgressMonitor(SaveImageText,y,image->rows);
                q=jpeg_pixels;
                x=0;
                y++;
              }
          }
          p++;
        }
  jpeg_finish_compress(&jpeg_info);
  /*
    Free memory.
  */
  jpeg_destroy_compress(&jpeg_info);
  FreeMemory((char *) jpeg_pixels);
  CloseBlob(image);
  return(True);
}
#else
Export unsigned int WriteJPEGImage(const ImageInfo *image_info,Image *image)
{
  MagickWarning(MissingDelegateWarning,"JPEG library is not available",
    image->filename);
  return(False);
}
#endif

