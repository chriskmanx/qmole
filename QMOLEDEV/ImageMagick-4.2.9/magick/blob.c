/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                         BBBB   L       OOO   BBBB                           %
%                         B   B  L      O   O  B   B                          %
%                         BBBB   L      O   O  BBBB                           %
%                         B   B  L      O   O  B   B                          %
%                         BBBB   LLLLL   OOO   BBBB                           %
%                                                                             %
%                                                                             %
%                    ImageMagick Binary Large OBjectS Methods                 %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                                 July 1999                                   %
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
%   B l o b T o I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method BlobToImage implements direct to memory image formats.  It returns
%  the blob as an image.
%
%  The format of the BlobToImage method is:
%
%      Image *BlobToImage(const ImageInfo *image_info,const char *blob,
%        const unsigned long length)
%
%  A description of each parameter follows:
%
%    o image:  Method BlobToImage returns an image from the supplied blob.
%      If an error occurs NULL is returned.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o blob: The address of a character stream in one of the image formats
%      understood by ImageMagick.
%
%    o length: This unsigned integer reflects the length in bytes of the blob.
%
%
*/
Export Image *BlobToImage(const ImageInfo *image_info,const char *blob,
  const unsigned long length)
{
  FILE
    *file;

  Image
    *image;

  ImageInfo
    *local_info;

  MagickInfo
    *magick_info;

  local_info=CloneImageInfo(image_info);
  local_info->blob.data=(char *) blob;
  local_info->blob.offset=0;
  local_info->blob.length=length;
  SetImageInfo(local_info,False);
  magick_info=(MagickInfo *) GetMagickInfo(local_info->magick);
  if (magick_info == (MagickInfo *) NULL)
    {
      MagickWarning(BlobWarning,"Unrecognized image format",
        local_info->magick);
      DestroyImageInfo(local_info);
      return((Image *) NULL);
    }
  GetBlobInfo(&(local_info->blob));
  if (magick_info->blob_support)
    {
      /*
        Native blob support for this image format.
      */
      *local_info->filename='\0';
      local_info->blob.data=(char *) blob;
      local_info->blob.length=length;
      image=ReadImage(local_info);
      DestroyImageInfo(local_info);
      GetBlobInfo(&(image->blob));
      return(image);
    }
  /*
    Write blob to a temporary file on disk.
  */
  TemporaryFilename(local_info->filename);
  file=fopen(local_info->filename,"w");
  if (file == (FILE *) NULL)
    {
      MagickWarning(BlobWarning,"Unable to convert blob to an image",
        local_info->filename);
      DestroyImageInfo(local_info);
      return((Image *) NULL);
    }
  (void) fwrite(blob,1,length,file);
  (void) fclose(file);
  image=ReadImage(local_info);
  (void) remove(local_info->filename);
  DestroyImageInfo(local_info);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   C l o s e B l o b                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method CloseBlob closes a file associated with the image.  If the
%  filename prefix is '|', the file is a pipe and is closed with PipeClose.
%
%  The format of the CloseBlob method is:
%
%      void CloseBlob(Image *image)
%
%  A description of each parameter follows:
%
%    o image: The address of a structure of type Image.
%
%
*/
Export void CloseBlob(Image *image)
{
  /*
    Close image file.
  */
  assert(image != (Image *) NULL);
  if (image->blob.data != (char *) NULL)
    {
      image->filesize=image->blob.length;
      image->blob.extent=image->blob.length;
      image->blob.data=(char *)
        ReallocateMemory(image->blob.data,image->blob.extent);
      return;
    }
  if (image->file == (FILE *) NULL)
    return;
  (void) FlushBlob(image);
  image->status=ferror(image->file);
  (void) SeekBlob(image,0L,SEEK_END);
  image->filesize=TellBlob(image);
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
  if (image->pipe)
    (void) pclose(image->file);
  else
#endif
    if (!image->exempt)
      (void) fclose(image->file);
  image->file=(FILE *) NULL;
  if (!image->orphan)
    {
      while (image->previous != (Image *) NULL)
        image=image->previous;
      for ( ; image != (Image *) NULL; image=image->next)
        image->file=(FILE *) NULL;
    }
  errno=0;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  E O F B l o b                                                              %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method EOFBlob returns a non-zero value when EOF has been detected reading
%  from a blob or file.
%
%  The format of the EOFBlob method is:
%
%      int EOFBlob(const Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method EOFBlob returns 0 on success; otherwise,  it
%      returns -1 and set errno to indicate the error.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export int EOFBlob(const Image *image)
{
  assert(image != (Image *) NULL);
  if (image->blob.data == (char *) NULL)
    return(feof(image->file));
  return(image->blob.offset > image->blob.length);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  F l u s h B l o b                                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method FlushBlob flushes the datastream if it is a file.
%
%  The format of the FlushBlob method is:
%
%      int FlushBlob(const Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method FlushBlob returns 0 on success; otherwise,  it
%      returns -1 and set errno to indicate the error.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export int FlushBlob(const Image *image)
{
  assert(image != (Image *) NULL);
  if (image->blob.data == (char *) NULL)
    return(fflush(image->file));
  return(0);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t B l o b I n f o                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetBlobInfo initializes the BlobInfo structure.
%
%  The format of the GetBlobInfo method is:
%
%      void GetBlobInfo(BlobInfo *blob_info)
%
%  A description of each parameter follows:
%
%    o blob_info: Specifies a pointer to a BlobInfo structure.
%
%
*/
Export void GetBlobInfo(BlobInfo *blob_info)
{
  assert(blob_info != (BlobInfo *) NULL);
  blob_info->data=(char *) NULL;
  blob_info->offset=0;
  blob_info->length=0;
  blob_info->extent=0;
  blob_info->quantum=BlobQuantum;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   G e t S t r i n g B l o b                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetStringBlob reads characters from a blob or file until a newline
%  character is read or an end-of-file  condition is encountered.
%  from a blob or file.
%
%  The format of the GetStringBlob method is:
%
%      char *GetStringBlob(Image *image,char *string)
%
%  A description of each parameter follows:
%
%    o status:  Method GetStringBlob returns the string on success, otherwise,
%      a null is returned.
%
%    o image: The address of a structure of type Image.
%
%    o string: The address of a character buffer.
%
%
*/
Export char *GetStringBlob(Image *image,char *string)
{
  int
    c;

  register int
    i;

  assert(image != (Image *) NULL);
  if (image->blob.data == (char *) NULL)
    return(fgets((char *) string,MaxTextExtent,image->file));
  for (i=0; i < (MaxTextExtent-1); i++)
  {
    c=ReadByte(image);
    if (c == EOF)
      return((char *) NULL);
    string[i]=c;
    if ((string[i] == '\n') || (string[i] == '\r'))
      break;
  }
  string[i]='\0';
  return(string);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   I m a g e T o B l o b                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ImageToBlob implements direct to memory image formats.  It returns
%  the image as a blob and its length.  The magick member of the Image
%  structure determines the format of the returned blob (GIG, JPEG, PNG, etc.).
%
%  The format of the ImageToBlob method is:
%
%      char *ImageToBlob(const ImageInfo *image_info,Image *image,
%        unsigned long *length)
%
%  A description of each parameter follows:
%
%    o blob:  Method ImageToBlob returns a chunk of memory written in the
%      desired image format (e.g. JPEG, GIF, etc.).  If an error occurs
%      NULL is returned.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
%    o length: This pointer to an unsigned int sets the initial length of the
%      blob.  On return, it reflects the actual length of the blob.
%
%
*/
Export char *ImageToBlob(const ImageInfo *image_info,Image *image,
  unsigned long *length)
{
  char
    *blob,
    filename[MaxTextExtent];

  FILE
    *file;

  ImageInfo
    *local_info;

  MagickInfo
    *magick_info;

  unsigned int
    status;

  local_info=CloneImageInfo(image_info);
  (void) strcpy(local_info->magick,image->magick);
  magick_info=(MagickInfo *) GetMagickInfo(local_info->magick);
  if (magick_info->blob_support)
    {
      /*
        Native blob support for this image format.
      */
      *image->filename='\0';
      local_info->blob.extent=Max((int) *length,image->blob.quantum);
      local_info->blob.data=(char *) AllocateMemory(local_info->blob.extent);
      if (local_info->blob.data == (char *) NULL)
        {
          MagickWarning(BlobWarning,"Unable to create blob",
            "Memory allocation failed");
          return((char *) NULL);
        }
      local_info->blob.offset=0;
      local_info->blob.length=0;
      status=WriteImage(local_info,image);
      DestroyImageInfo(local_info);
      if (status == False)
        {
          MagickWarning(BlobWarning,"Unable to create blob",local_info->magick);
          return((char *) NULL);
        }
      *length=image->blob.length;
      blob=image->blob.data;
      GetBlobInfo(&(image->blob));
      return(blob);
    }
  /*
    Write file to disk in blob image format.
  */
  *length=0;
  local_info=CloneImageInfo(image_info);
  (void) strcpy(filename,image->filename);
  FormatString(image->filename,"%.1024s:%.1024s",image->magick,
    local_info->unique);
  status=WriteImage(local_info,image);
  if (status == False)
    {
      MagickWarning(BlobWarning,"Unable to create blob",image->filename);
      return((char *) NULL);
    }
  /*
    Read image from disk as blob.
  */
  file=fopen(image->filename,"rb");
  (void) remove(image->filename);
  (void) strcpy(image->filename,filename);
  DestroyImageInfo(local_info);
  if (file == (FILE *) NULL)
    {
      MagickWarning(BlobWarning,"Unable to create blob",image->filename);
      return((char *) NULL);
    }
  (void) fseek(file,0L,SEEK_END);
  *length=ftell(file);
  (void) fseek(file,0L,SEEK_SET);
  blob=(char *) AllocateMemory(*length*sizeof(char));
  if (blob == (char *) NULL)
    {
      MagickWarning(BlobWarning,"Unable to create blob",
        "Memory allocation failed");
      return((char *) NULL);
    }
  (void) fread((char *) blob,1,*length,file);
  (void) fclose(file);
  return(blob);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  L S B F i r s t R e a d L o n g                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method LSBFirstReadLong reads a long value as a 32 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstReadLong method is:
%
%      unsigned long LSBFirstReadLong(Image *image)
%
%  A description of each parameter follows.
%
%    o value:  Method LSBFirstReadLong returns an unsigned long read from
%      the file.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned long LSBFirstReadLong(Image *image)
{
  unsigned char
    buffer[4];

  unsigned long
    value;

  assert(image != (Image *) NULL);
  value=ReadBlob(image,4,(char *) buffer);
  if (value == 0)
    return((unsigned long) ~0);
  value=(unsigned long) (buffer[3] << 24);
  value|=(unsigned long) (buffer[2] << 16);
  value|=(unsigned long) (buffer[1] << 8);
  value|=(unsigned long) (buffer[0]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  L S B F i r s t R e a d S h o r t                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method LSBFirstReadShort reads a short value as a 16 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstReadShort method is:
%
%      unsigned short LSBFirstReadShort(Image *image)
%
%  A description of each parameter follows.
%
%    o value:  Method LSBFirstReadShort returns an unsigned short read from
%      the file.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned short LSBFirstReadShort(Image *image)
{
  unsigned char
    buffer[2];

  unsigned short
    value;

  assert(image != (Image *) NULL);
  value=ReadBlob(image,2,(char *) buffer);
  if (value == 0)
    return((unsigned short) ~0);
  value=(unsigned short) (buffer[1] << 8);
  value|=(unsigned short) (buffer[0]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  L S B F i r s t W r i t e L o n g                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method LSBFirstWriteLong writes a long value as a 32 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstWriteLong method is:
%
%      unsigned long LSBFirstWriteLong(Image *image,const unsigned long value)
%
%  A description of each parameter follows.
%
%    o count: Method LSBFirstWriteLong returns the number of unsigned longs
%      written.
%
%    o image: The address of a structure of type Image.
%
%    o value: Specifies the value to write.
%
%
*/
Export unsigned long LSBFirstWriteLong(Image *image,const unsigned long value)
{
  unsigned char
    buffer[4];

  assert(image != (Image *) NULL);
  buffer[0]=(unsigned char) (value);
  buffer[1]=(unsigned char) ((value) >> 8);
  buffer[2]=(unsigned char) ((value) >> 16);
  buffer[3]=(unsigned char) ((value) >> 24);
  return(WriteBlob(image,4,(char *) buffer));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  L S B F i r s t W r i t e S h o r t                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method LSBFirstWriteShort writes a long value as a 16 bit quantity in
%  least-significant byte first order.
%
%  The format of the LSBFirstWriteShort method is:
%
%      unsigned long LSBFirstWriteShort(Image *image,const unsigned short value)
%
%  A description of each parameter follows.
%
%    o count: Method LSBFirstWriteShort returns the number of unsigned longs
%      written.
%
%    o image: The address of a structure of type Image.
%
%    o value:  Specifies the value to write.
%
%
*/
Export unsigned long LSBFirstWriteShort(Image *image,const unsigned short value)
{
  unsigned char
    buffer[2];

  assert(image != (Image *) NULL);
  buffer[0]=(unsigned char) (value);
  buffer[1]=(unsigned char) ((value) >> 8);
  return(WriteBlob(image,2,(char *) buffer));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  M S B F i r s t O r d e r L o n g                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MSBFirstOrderLong converts a least-significant byte first buffer
%  of integers to most-significant byte first.
%
%  The format of the MSBFirstOrderLong method is:
%
%      void MSBFirstOrderLong(register char *p,const unsigned int length)
%
%  A description of each parameter follows.
%
%   o  p:  Specifies a pointer to a buffer of integers.
%
%   o  length:  Specifies the length of the buffer.
%
%
*/
Export void MSBFirstOrderLong(register char *p,const unsigned int length)
{
  register char
    c,
    *q,
    *sp;

  assert(p != (char *) NULL);
  q=p+length;
  while (p < q)
  {
    sp=p+3;
    c=(*sp);
    *sp=(*p);
    *p++=c;
    sp=p+1;
    c=(*sp);
    *sp=(*p);
    *p++=c;
    p+=2;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  M S B F i r s t O r d e r S h o r t                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MSBFirstOrderShort converts a least-significant byte first buffer
%  of integers to most-significant byte first.
%
%  The format of the MSBFirstOrderShort method is:
%
%      void MSBFirstOrderShort(register char *p,const unsigned int length)
%
%  A description of each parameter follows.
%
%   o  p:  Specifies a pointer to a buffer of integers.
%
%   o  length:  Specifies the length of the buffer.
%
%
*/
Export void MSBFirstOrderShort(register char *p,const unsigned int length)
{
  register char
    c,
    *q;

  assert(p != (char *) NULL);
  q=p+length;
  while (p < q)
  {
    c=(*p);
    *p=(*(p+1));
    p++;
    *p++=c;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  M S B F i r s t R e a d S h o r t                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MSBFirstReadShort reads a short value as a 16 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstReadShort method is:
%
%      unsigned short MSBFirstReadShort(Image *image)
%
%  A description of each parameter follows.
%
%    o value:  Method MSBFirstReadShort returns an unsigned short read from
%      the file.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export unsigned short MSBFirstReadShort(Image *image)
{
  unsigned char
    buffer[2];

  unsigned short
    value;

  assert(image != (Image *) NULL);
  value=ReadBlob(image,2,(char *) buffer);
  if (value == 0)
    return((unsigned short) ~0);
  value=(unsigned int) (buffer[0] << 8);
  value|=(unsigned int) (buffer[1]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  M S B F i r s t R e a d L o n g                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MSBFirstReadLong reads a long value as a 32 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstReadLong method is:
%
%      unsigned long MSBFirstReadLong(Image *image)
%
%  A description of each parameter follows.
%
%    o value:  Method MSBFirstReadLong returns an unsigned long read from
%      the file.
%
%    o image: The address of a structure of type Image.
%
%
%
*/
Export unsigned long MSBFirstReadLong(Image *image)
{
  unsigned char
    buffer[4];

  unsigned long
    value;

  assert(image != (Image *) NULL);
  value=ReadBlob(image,4,(char *) buffer);
  if (value == 0)
    return((unsigned long) ~0);
  value=(unsigned int) (buffer[0] << 24);
  value|=(unsigned int) (buffer[1] << 16);
  value|=(unsigned int) (buffer[2] << 8);
  value|=(unsigned int) (buffer[3]);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  M S B F i r s t W r i t e L o n g                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MSBFirstWriteLong writes a long value as a 32 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstWriteLong method is:
%
%      unsigned long MSBFirstWriteLong(Image *image,const unsigned long value)
%
%  A description of each parameter follows.
%
%    o count: Method MSBFirstWriteLong returns the number of unsigned longs
%      written.
%
%    o value:  Specifies the value to write.
%
%    o image: The address of a structure of type Image.
%
%
%
*/
Export unsigned long MSBFirstWriteLong(Image *image,const unsigned long value)
{
  unsigned char
    buffer[4];

  assert(image != (Image *) NULL);
  buffer[0]=(unsigned char) ((value) >> 24);
  buffer[1]=(unsigned char) ((value) >> 16);
  buffer[2]=(unsigned char) ((value) >> 8);
  buffer[3]=(unsigned char) (value);
  return(WriteBlob(image,4,(char *) buffer));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  M S B F i r s t W r i t e S h o r t                                        %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MSBFirstWriteShort writes a long value as a 16 bit quantity in
%  most-significant byte first order.
%
%  The format of the MSBFirstWriteShort method is:
%
%      unsigned long MSBFirstWriteShort(Image *image,const unsigned short value)
%
%  A description of each parameter follows.
%
%   o  value:  Specifies the value to write.
%
%   o  file:  Specifies the file to write the data to.
%
%
*/
Export unsigned long MSBFirstWriteShort(Image *image,const unsigned short value)
{
  unsigned char
    buffer[2];

  assert(image != (Image *) NULL);
  buffer[0]=(unsigned char) ((value) >> 8);
  buffer[1]=(unsigned char) (value);
  return(WriteBlob(image,2,(char *) buffer));
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   O p e n B l o b                                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method OpenBlob open a file associated with the image.  A file name of
%  '-' sets the file to stdin for type 'r' and stdout for type 'w'.  If the
%  filename suffix is '.gz' or '.Z', the image is decompressed for type 'r'
%  and compressed for type 'w'.  If the filename prefix is '|', it is piped
%  to or from a system command.
%
%  The format of the OpenBlob method is:
%
%      unsigned int OpenBlob(const ImageInfo *image_info,Image *image,
%        const char *type)
%
%  A description of each parameter follows:
%
%    o status:  Method OpenBlob returns True if the file is successfully
%      opened otherwise False.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image: The address of a structure of type Image.
%
%    o type: 'r' for reading; 'w' for writing.
%
*/
Export unsigned int OpenBlob(const ImageInfo *image_info,Image *image,
  const char *type)
{
  char
    filename[MaxTextExtent];

  register char
    *p;

  assert(image_info != (ImageInfo *) NULL);
  assert(image != (Image *) NULL);
  assert(type != (char *) NULL);
  if (image_info->blob.data != (char *) NULL)
    {
      image->blob=image_info->blob;
      return(True);
    }
  image->exempt=False;
  if (image_info->file != (FILE *) NULL)
    {
      /*
        Use previously opened filehandle.
      */
      image->file=image_info->file;
      image->exempt=True;
      return(True);
    }
  (void) strcpy(filename,image->filename);
  p=(char *) NULL;
  if (*filename != '|')
    {
      if ((Extent(filename) > 4) &&
          (Latin1Compare(filename+Extent(filename)-4,".pgp") == 0))
        {
          /*
            Decrypt image file with PGP encryption utilities.
          */
          if (*type == 'r')
            p=GetDelegateCommand(image_info,image,"pgp",(char *) NULL);
        }
      else
        if ((Extent(filename) > 4) &&
            (Latin1Compare(filename+Extent(filename)-4,".bz2") == 0))
          {
            /*
              Uncompress/compress image file with BZIP compress utilities.
            */
            if (*type == 'r')
              p=GetDelegateCommand(image_info,image,"bzip",(char *) NULL);
            else
              p=GetDelegateCommand(image_info,image,(char *) NULL,"bzip");
          }
        else
          if ((Extent(filename) > 3) &&
              (Latin1Compare(filename+Extent(filename)-3,".gz") == 0))
            {
              /*
                Uncompress/compress image file with GNU compress utilities.
              */
              if (*type == 'r')
                p=GetDelegateCommand(image_info,image,"zip",(char *) NULL);
              else
                p=GetDelegateCommand(image_info,image,(char *) NULL,"zip");
            }
          else
            if ((Extent(filename) > 2) &&
                (Latin1Compare(filename+Extent(filename)-2,".Z") == 0))
              {
                /*
                  Uncompress/compress image file with UNIX compress utilities.
                */
                if (*type == 'r')
                  p=GetDelegateCommand(image_info,image,"compress",
                    (char *) NULL);
                else
                  p=GetDelegateCommand(image_info,image,(char *) NULL,
                    "compress");
              }
    }
  if (p != (char *) NULL)
    {
      (void) strcpy(filename,p);
      FreeMemory((char *) p);
    }
  /*
    Open image file.
  */
  image->pipe=False;
  if (Latin1Compare(filename,"-") == 0)
    {
      image->file=(*type == 'r') ? stdin : stdout;
      image->exempt=True;
    }
  else
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
    if (*filename == '|')
      {
        char
          mode[MaxTextExtent];

        /*
          Pipe image to or from a system command.
        */
        if (*type == 'w')
          (void) signal(SIGPIPE,SIG_IGN);
        (void) strncpy(mode,type,1);
        mode[1]='\0';
        image->file=(FILE *) popen(filename+1,mode);
        image->pipe=True;
        image->exempt=True;
      }
    else
#endif
      {
        if (*type == 'w')
          {
            /*
              Form filename for multi-part images.
            */
            FormatString(filename,image->filename,image->scene);
            if (!image_info->adjoin)
              if ((image->previous != (Image *) NULL) ||
                  (image->next != (Image *) NULL))
                {
                  if ((Latin1Compare(filename,image->filename) == 0) ||
                      (strchr(filename,'%') != (char *) NULL))
                    FormatString(filename,"%.1024s.%u",image->filename,
                      image->scene);
                  if (image->next != (Image *) NULL)
                    (void) strcpy(image->next->magick,image->magick);
                }
            (void) strcpy(image->filename,filename);
          }
#if defined(macintosh)
        if (*type == 'w')
          SetApplicationType(filename,image_info->magick,'8BIM');
#endif
        image->file=(FILE *) fopen(filename,type);
        if (image->file != (FILE *) NULL)
          {
            (void) SeekBlob(image,0L,SEEK_END);
            image->filesize=TellBlob(image);
            (void) SeekBlob(image,0L,SEEK_SET);
          }
      }
  image->status=False;
  if (*type == 'r')
    {
      image->next=(Image *) NULL;
      image->previous=(Image *) NULL;
    }
  return(image->file != (FILE *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  R e a d B l o b                                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadBlob reads data from the blob or image file and returns it.  It
%  returns the number of bytes read.
%
%  The format of the ReadBlob method is:
%
%      unsigned long ReadBlob(Image *image,const unsigned long number_bytes,
%        char *data)
%
%  A description of each parameter follows:
%
%    o count:  Method ReadBlob returns the number of items read.
%
%    o image: The address of a structure of type Image.
%
%    o number_bytes:  Specifies an integer representing the number of bytes
%      to read from the file.
%
%    o data:  Specifies an area to place the information requested from
%      the file.
%
%
*/
Export unsigned long ReadBlob(Image *image,const unsigned long number_bytes,
  char *data)
{
  register int
    i;

  unsigned long
    count,
    offset;

  assert(image != (Image *) NULL);
  assert(data != (char *) NULL);
  if (image->blob.data != (char *) NULL)
    {
      /*
        Read bytes from blob.
      */
      offset=Min(number_bytes,(unsigned long)
        (image->blob.length-image->blob.offset));
      if (number_bytes > 0)
        (void) memcpy(data,image->blob.data+image->blob.offset,offset);
      image->blob.offset+=offset;
      return(offset);
    }
  /*
    Read bytes from a file handle.
  */
  offset=0;
  for (i=number_bytes; i > 0; i-=count)
  {
    count=fread(data+offset,1,number_bytes,image->file);
    if (count <= 0)
      break;
    offset+=count;
  }
  return(offset);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  R e a d B y t e                                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadByte reads a single byte from the image file and returns it.
%
%  The format of the ReadByte method is:
%
%      int ReadByte(Image *image)
%
%  A description of each parameter follows.
%
%    o value:  Method ReadByte returns an integer read from the file.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export int ReadByte(Image *image)
{
  int
    count;

  unsigned char
    value;

  assert(image != (Image *) NULL);
  count=ReadBlob(image,1,(char *) &value);
  if (count == 0)
    return(EOF);
  return(value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  R e a d D a t a B l o c k                                                  %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadBlobBlock reads data from the image file and returns it.  The
%  amount of data is determined by first reading a count byte.  The number
%  or bytes read is returned.
%
%  The format of the ReadBlobBlock method is:
%
%      unsigned long ReadBlobBlock(Image *image,char *data)
%
%  A description of each parameter follows:
%
%    o count:  Method ReadBlobBlock returns the number of bytes read.
%
%    o image: The address of a structure of type Image.
%
%    o data:  Specifies an area to place the information requested from
%      the file.
%
%
*/
Export unsigned long ReadBlobBlock(Image *image,char *data)
{
  unsigned char
    block_count;

  unsigned long
    count;

  assert(image != (Image *) NULL);
  assert(data != (char *) NULL);
  count=ReadBlob(image,1,(char *) &block_count);
  if (count == 0)
    return(0);
  count=ReadBlob(image,(unsigned long) block_count,data);
  return(count);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  S e e k B l o b                                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SeekBlob sets the offset in bytes from the beginning of a blob or
%  file.
%
%  The format of the SeekBlob method is:
%
%      int SeekBlob(Image *image,const long offset,const int whence)
%
%  A description of each parameter follows:
%
%    o status:  Method SeekBlob returns 0 on success; otherwise,  it
%      returned -1 and set errno to indicate the error.
%
%    o image: The address of a structure of type Image.
%
%    o offset:  Specifies an integer representing the offset in bytes.
%
%    o whence:  Specifies an integer representing how the offset is
%      treated relative to the beginning of the blob as follows:
%
%        SEEK_SET  Set position equal to offset bytes.
%        SEEK_CUR  Set position to current location plus offset.
%        SEEK_END  Set position to EOF plus offset.
%
%
*/
Export int SeekBlob(Image *image,const long offset,const int whence)
{
  assert(image != (Image *) NULL);
  if (image->blob.data == (char *) NULL)
    return(fseek(image->file,offset,whence));
  switch(whence)
  {
    case SEEK_SET:
    default:
    {
      if (offset < 0)
        return(-1);
      if (offset >= image->blob.length)
        return(-1);
      image->blob.offset=offset;
      break;
    }
    case SEEK_CUR:
    {
      if ((image->blob.offset+offset) < 0)
        return(-1);
      if ((image->blob.offset+offset) >= (long) image->blob.length)
        return(-1);
      image->blob.offset+=offset;
      break;
    }
    case SEEK_END:
    {
      if ((image->blob.offset+image->blob.length+offset) < 0)
        return(-1);
      if ((image->blob.offset+image->blob.length+offset) >= image->blob.length)
        return(-1);
      image->blob.offset+=image->blob.length+offset;
      break;
    }
  }
  return(0);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  S e t B l o b Q u a n t u m                                                %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SetBlobQuantum set the current value of the blob quantum.  This
%  is the size in bytes to add to a blob when writing to a blob exceeds its
%  current length.
%
%  The format of the SetBlobQuantum method is:
%
%      void SetBlobQuantum(BlobInfo *blob_info,const unsigned long quantum)
%
%  A description of each parameter follows:
%
%    o blob_info:  A pointer to a BlobInfo structure.
%
%    o quantum: An unsigned long that reflects the number of bytes to
%      increase a blob.
%
%
*/
Export void SetBlobQuantum(BlobInfo *blob_info,const unsigned long quantum)
{
  assert(blob_info != (BlobInfo *) NULL);
  blob_info->quantum=quantum;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  T e l l B l o b                                                            %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method TellBlob obtains the current value of the blob or file position.
%
%  The format of the TellBlob method is:
%
%      int TellBlob(const Image *image)
%
%  A description of each parameter follows:
%
%    o status:  Method TellBlob returns 0 on success; otherwise,  it
%      returned -1 and set errno to indicate the error.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export int TellBlob(const Image *image)
{
  assert(image != (Image *) NULL);
  if (image->blob.data == (char *) NULL)
    return(ftell(image->file));
  return(image->blob.offset);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  W r i t e B l o b                                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteBlob writes data to a blob or image file.  It returns the
%  number of items written.
%
%  The format of the WriteBlob method is:
%
%      unsigned long WriteBlob(Image *image,const unsigned long number_bytes,
%        const char *data)
%
%  A description of each parameter follows:
%
%    o status:  Method WriteBlob returns True if all the data requested
%      is obtained without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%    o size:  Specifies an integer representing the length of an
%      individual item to be written to the file.
%
%    o number_items:  Specifies an integer representing the number of items
%      to write to the file.
%
%    o data:  The address of the data to write to the blob or file.
%
%
*/
Export unsigned long WriteBlob(Image *image,const unsigned long number_bytes,
  const char *data)
{
  unsigned long
    count;

  assert(image != (Image *) NULL);
  assert(data != (const char *) NULL);
  if (image->blob.data == (char *) NULL)
    {
      count=(long) fwrite((char *) data,1,number_bytes,image->file);
      return(count);
    }
  if (number_bytes > (unsigned long) (image->blob.extent-image->blob.offset))
    {
      image->blob.extent+=number_bytes+image->blob.quantum;
      image->blob.data=(char *)
        ReallocateMemory(image->blob.data,image->blob.extent);
      if (image->blob.data == (char *) NULL)
        {
          image->blob.extent=0;
          return(0);
        }
    }
  memcpy(image->blob.data+image->blob.offset,data,number_bytes);
  image->blob.offset+=number_bytes;
  if (image->blob.offset > image->blob.length)
    image->blob.length=image->blob.offset;
  return(number_bytes);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  W r i t e B y t e                                                          %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteByte write an integer to a file.  It returns the number of
%  bytes written (either 0 or 1);
%
%  The format of the WriteByte method is:
%
%      unsigned long WriteByte(Image *image,const char value)
%
%  A description of each parameter follows.
%
%    o count:  Method WriteByte returns the number of bytes written.
%
%    o image: The address of a structure of type Image.
%
%    o value: Specifies the value to write.
%
%
*/
Export unsigned long WriteByte(Image *image,const char value)
{
  unsigned long
    count;

  assert(image != (Image *) NULL);
  count=WriteBlob(image,1,(char *) &value);
  return(count);
}
