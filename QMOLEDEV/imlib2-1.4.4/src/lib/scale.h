#ifndef __SCALE
#define __SCALE 1

typedef struct _imlib_scale_info ImlibScaleInfo;

__hidden ImlibScaleInfo *
__imlib_CalcScaleInfo(ImlibImage *im, int sw, int sh, int dw, int dh, char aa);
__hidden ImlibScaleInfo *
__imlib_FreeScaleInfo(ImlibScaleInfo *isi);
__hidden void
__imlib_ScaleSampleRGBA(ImlibScaleInfo *isi, DATA32 *dest, int dxx, int dyy,
			int dx, int dy, int dw, int dh, int dow);
__hidden void
__imlib_ScaleAARGBA(ImlibScaleInfo *isi, DATA32 *dest, int dxx, int dyy,
		    int dx, int dy, int dw, int dh, int dow, int sow);
__hidden void
__imlib_ScaleAARGB(ImlibScaleInfo *isi, DATA32 *dest, int dxx, int dyy,
		   int dx, int dy, int dw, int dh, int dow, int sow);
__hidden void
__imlib_Scale_mmx_AARGBA(ImlibScaleInfo *isi, DATA32 *dest, int dxx, int dyy,
			 int dx, int dy, int dw, int dh, int dow, int sow);
#endif
