/*
  Methods to Reduce the Number of Unique Colors in an Image.
*/
#ifndef _QUANTIZE_H
#define _QUANTIZE_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#define ErrorQueueLength  16
#define MaxTreeDepth  8
#define NodesInAList  2048

/*
  Typedef declarations.
*/
typedef struct _ErrorPacket
{
  long
    red,
    green,
    blue;
} ErrorPacket;

typedef struct _QuantizeInfo
{
  unsigned int
    number_colors,
    tree_depth,
    dither;

  ColorspaceType
    colorspace;
} QuantizeInfo;

/*
  Constant declarations.
*/
extern const long
  MaxColormapSize;

/*
  Quantization utilities methods.
*/
extern Export QuantizeInfo
  *CloneQuantizeInfo(const QuantizeInfo *);

extern Export unsigned int
  MapImage(Image *,const Image *,const unsigned int),
  MapImages(Image *,const Image *,const unsigned int),
  QuantizationError(Image *),
  QuantizeImage(const QuantizeInfo *,Image *),
  QuantizeImages(const QuantizeInfo *,Image *),
  QueryColorName(const ColorPacket *,char *),
  SegmentImage(Image *,const ColorspaceType,const unsigned int,const double,
    const double);

extern Export unsigned long
  GetNumberColors(const Image *,FILE *);

extern Export void
  DestroyQuantizeInfo(QuantizeInfo *),
  GetQuantizeInfo(QuantizeInfo *);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
