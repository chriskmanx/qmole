/*
  Image Compression/Decompression Methods.
*/
#ifndef _BLOBS_H
#define _BLOBS_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#if !defined(BlobQuantum)
#define BlobQuantum  65535
#endif

/*
  Blob methods.
*/
extern Export char
  *GetStringBlob(Image *,char *),
  *ImageToBlob(const ImageInfo *,Image *,unsigned long *);

extern Export Image
  *BlobToImage(const ImageInfo *,const char *,const unsigned long);

extern Export int
  EOFBlob(const Image *),
  FlushBlob(const Image *),
  ReadByte(Image *),
  SeekBlob(Image *,const long,const int),
  TellBlob(const Image *image);

extern Export unsigned int
  OpenBlob(const ImageInfo *,Image *,const char *);

extern Export unsigned long
  LSBFirstReadLong(Image *),
  LSBFirstWriteLong(Image *,const unsigned long),
  LSBFirstWriteShort(Image *,const unsigned short),
  MSBFirstReadLong(Image *),
  MSBFirstWriteLong(Image *,const unsigned long),
  MSBFirstWriteShort(Image *,const unsigned short),
  ReadBlob(Image *,const unsigned long,char *),
  ReadBlobBlock(Image *,char *),
  WriteBlob(Image *,const unsigned long,const char *),
  WriteByte(Image *,const char value);

extern Export unsigned short
  LSBFirstReadShort(Image *),
  MSBFirstReadShort(Image *);

extern Export void
  CloseBlob(Image *),
  MSBFirstOrderLong(char *,const unsigned int),
  MSBFirstOrderShort(char *,const unsigned int),
  GetBlobInfo(BlobInfo *),
  SetBlobQuantum(BlobInfo *,const unsigned long);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
