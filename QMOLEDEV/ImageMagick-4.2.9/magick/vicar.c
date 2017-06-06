/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                     V   V  IIIII   CCCC   AAA   RRRR                        %
%                     V   V    I    C      A   A  R   R                       %
%                     V   V    I    C      AAAAA  RRRR                        %
%                      V V     I    C      A   A  R R                         %
%                       V    IIIII   CCCC  A   A  R  R                        %
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
%   R e a d V I C A R I m a g e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadVICARImage reads a VICAR image file and returns it.  It
%  allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadVICARImage method is:
%
%      Image *ReadVICARImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image: Method ReadVICARImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or if
%      the image cannot be read.
%
%    o filename: Specifies the name of the image to read.
%
%
*/
Export Image *ReadVICARImage(const ImageInfo *image_info)
{
  char
    keyword[MaxTextExtent],
    value[MaxTextExtent];

  Image
    *image;

  int
    c,
    y;

  long
    count;

  register int
    i,
    x;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  unsigned char
    *vicar_pixels;

  unsigned int
    header_length,
    status,
    value_expected;

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
    Decode image header.
  */
  c=ReadByte(image);
  count=1;
  if (c == EOF)
    {
      DestroyImage(image);
      return((Image *) NULL);
    }
  header_length=0;
  while (isgraph(c) && ((image->columns*image->rows) == 0))
  {
    if (!isalnum(c))
      {
        c=ReadByte(image);
        count++;
      }
    else
      {
        register char
          *p;

        /*
          Determine a keyword and its value.
        */
        p=keyword;
        do
        {
          if ((p-keyword) < (MaxTextExtent-1))
            *p++=(char) c;
          c=ReadByte(image);
          count++;
        } while (isalnum(c) || (c == '_'));
        *p='\0';
        value_expected=False;
        while (isspace(c) || (c == '='))
        {
          if (c == '=')
            value_expected=True;
          c=ReadByte(image);
          count++;
        }
        if (value_expected == False)
          continue;
        p=value;
        while (isalnum(c))
        {
          if ((p-value) < (MaxTextExtent-1))
            *p++=(char) c;
          c=ReadByte(image);
          count++;
        }
        *p='\0';
        /*
          Assign a value to the specified keyword.
        */
        if (Latin1Compare(keyword,"LABEL_RECORDS") == 0)
          header_length=(unsigned int) atoi(value);
        if (Latin1Compare(keyword,"LBLSIZE") == 0)
          header_length=(unsigned int) atoi(value);
        if (Latin1Compare(keyword,"RECORD_BYTES") == 0)
          image->columns=(unsigned int) atoi(value);
        if (Latin1Compare(keyword,"NS") == 0)
          image->columns=(unsigned int) atoi(value);
        if (Latin1Compare(keyword,"LINES") == 0)
          image->rows=(unsigned int) atoi(value);
        if (Latin1Compare(keyword,"NL") == 0)
          image->rows=(unsigned int) atoi(value);
      }
    while (isspace(c))
    {
      c=ReadByte(image);
      count++;
    }
  }
  image->class=PseudoClass;
  image->colors=256;
  if (image_info->ping)
    {
      CloseBlob(image);
      return(image);
    }
  /*
    Read the rest of the header.
  */
  while (count < (int) header_length)
  {
    c=ReadByte(image);
    count++;
  }
  /*
    Verify that required image information is defined.
  */
  if ((image->columns*image->rows) == 0)
    ReaderExit(CorruptImageWarning,"image size is zero",image);
  /*
    Create linear colormap.
  */
  image->colormap=(ColorPacket *)
    AllocateMemory(image->colors*sizeof(ColorPacket));
  if (image->colormap == (ColorPacket *) NULL)
    ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
  for (i=0; i < (int) image->colors; i++)
  {
    image->colormap[i].red=(Quantum) UpScale(i);
    image->colormap[i].green=(Quantum) UpScale(i);
    image->colormap[i].blue=(Quantum) UpScale(i);
  }
  /*
    Initialize image structure.
  */
  image->packets=image->columns*image->rows;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->packets*sizeof(RunlengthPacket));
  vicar_pixels=(unsigned char *)
    AllocateMemory(image->packets*sizeof(unsigned char));
  if ((image->pixels == (RunlengthPacket *) NULL) ||
      (vicar_pixels == (unsigned char *) NULL))
    ReaderExit(CorruptImageWarning,"Unable to read image data",image);
  SetImage(image);
  /*
    Convert VICAR pixels to runlength-encoded packets.
  */
  status=ReadBlob(image,image->packets,(char *) vicar_pixels);
  if (status == False)
    ReaderExit(CorruptImageWarning,"Insufficient image data in file",image);
  /*
    Convert VICAR pixels to runlength-encoded packets.
  */
  p=vicar_pixels;
  q=image->pixels;
  for (y=0; y < (int) image->rows; y++)
  {
    for (x=0; x < (int) image->columns; x++)
    {
      q->index=(unsigned short) *p;
      q->length=0;
      p++;
      q++;
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(LoadImageText,y,image->rows);
  }
  FreeMemory((char *) vicar_pixels);
  SyncImage(image);
  CondenseImage(image);
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e V I C A R I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteVICARImage writes an image in the VICAR rasterfile format.
%  Vicar files contain a text header, followed by one or more planes of binary
%  grayscale image data.  Vicar files are designed to allow many planes to be
%  stacked together to form image cubes.  This method only writes a single
%  grayscale plane.
%
%  Method WriteVICARImage was written contributed by
%  gorelick@esther.la.asu.edu.
%
%  The format of the WriteVICARImage method is:
%
%      unsigned int WriteVICARImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteVICARImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteVICARImage(const ImageInfo *image_info,Image *image)
{
  char
    buffer[MaxTextExtent],
    header[MaxTextExtent],
    label[16];

  int
    label_size,
    x,
    y;

  register int
    i,
    j;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  unsigned char
    *pixels;

  unsigned int
    status;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  TransformRGBImage(image,RGBColorspace);
  /*
    Write header.
  */
  FormatString(header,"LBLSIZE=            FORMAT='BYTE'  TYPE='IMAGE'");
  FormatString(header+Extent(header),"  BUFSIZE=20000  DIM=2  EOL=0");
  FormatString(header+Extent(header),
    "  RECSIZE=%u  ORG='BSQ'  NL=%u  NS=%u  NB=1",image->columns,image->rows,
    image->columns);
  FormatString(header+Extent(header),
    "  N1=0  N2=0  N3=0  N4=0  NBB=0  NLB=0");
  FormatString(header+Extent(header),"  TASK='ImageMagick'");
  /*
    Compute the size of the label.
  */
  label_size=(Extent(header)+image->columns-1)/image->columns*image->columns;
  FormatString(label,"%d",label_size);
  for (i=0 ; i < Extent(label); i++)
    header[i+8]=label[i];
  /*
    Print the header and enough spaces to pad to label size.
  */
  (void) sprintf(buffer, "%-*s",label_size,header);
  (void) WriteBlob(image,strlen(buffer),buffer);
  /*
    Allocate memory for pixels.
  */
  pixels=(unsigned char *)
    AllocateMemory(image->columns*sizeof(RunlengthPacket));
  if (pixels == (unsigned char *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Write VICAR pixels.
  */
  x=0;
  y=0;
  p=image->pixels;
  q=pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    for (j=0; j <= ((int) p->length); j++)
    {
      *q++=DownScale(Intensity(*p));
      x++;
      if (x == (int) image->columns)
        {
          (void) WriteBlob(image,q-pixels,(char *) pixels);
          if (image->previous == (Image *) NULL)
            if (QuantumTick(y,image->rows))
              ProgressMonitor(SaveImageText,y,image->rows);
          q=pixels;
          x=0;
          y++;
        }
    }
    p++;
  }
  FreeMemory((char *) pixels);
  CloseBlob(image);
  return(True);
}
