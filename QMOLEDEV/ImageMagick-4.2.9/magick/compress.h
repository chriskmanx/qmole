/*
  Image Compression/Decompression Methods.
*/
#ifndef _COMPRESS_H
#define _COMPRESS_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

extern Export unsigned int
  HuffmanDecodeImage(Image *),
  HuffmanEncodeImage(const ImageInfo *,Image *),
  Huffman2DEncodeImage(ImageInfo *,Image *),
  LZWEncodeImage(Image *,const unsigned int,unsigned char *),
  PackbitsEncodeImage(Image *,const unsigned int,unsigned char *),
  RunlengthDecodeImage(Image *),
  RunlengthEncodeImage(Image *),
  ZLIBEncodeImage(Image *,const unsigned long,const unsigned int,
    unsigned char *);

extern Export void
  Ascii85Encode(Image *,const unsigned int),
  Ascii85Flush(Image *),
  Ascii85Initialize(void),
  SetRunlengthEncoder(RunlengthPacket *),
  SetRunlengthPackets(Image *,const unsigned long);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
