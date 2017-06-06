/*
  Graphic Gems - Graphic Support Methods.
*/
#ifndef _GEMS_H
#define _GEMS_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/*
  Graphic gems define declarations.
*/
extern Export ColorPacket
  InterpolateColor(Image *,const double,const double);

extern Export Quantum
  GenerateNoise(const Quantum,const NoiseType);

extern Export unsigned short
  InsidePrimitive(PrimitiveInfo *,const AnnotateInfo *,const PointInfo *,
    Image *);

extern Export void
  Contrast(const int,Quantum *,Quantum *,Quantum *),
  HSLTransform(const double,const double,const double,Quantum *,Quantum *,
    Quantum *),
  Hull(const int,const int,const int,const unsigned int,const unsigned int,
    Quantum *,Quantum *),
  Modulate(double,double,double,Quantum *,Quantum *,Quantum *),
  TransformHSL(const Quantum,const Quantum,const Quantum,double *,double *,
    double *),
  Upsample(const unsigned int,const unsigned int,const unsigned int,
    unsigned char *);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
