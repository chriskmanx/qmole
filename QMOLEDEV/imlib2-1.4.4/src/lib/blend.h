#ifndef __BLEND
#define __BLEND 1

#ifndef WORDS_BIGENDIAN

#define A_VAL(p) ((DATA8 *)(p))[3]
#define R_VAL(p) ((DATA8 *)(p))[2]
#define G_VAL(p) ((DATA8 *)(p))[1]
#define B_VAL(p) ((DATA8 *)(p))[0]

#else

#define A_VAL(p) ((DATA8 *)(p))[0]
#define R_VAL(p) ((DATA8 *)(p))[1]
#define G_VAL(p) ((DATA8 *)(p))[2]
#define B_VAL(p) ((DATA8 *)(p))[3]

#endif

/* FIXME: endian dependant */
#define READ_RGB(p, r, g, b)  \
   (r) = R_VAL(p); \
   (g) = G_VAL(p); \
   (b) = B_VAL(p);

#define READ_ALPHA(p, a) \
   (a) = A_VAL(p);

#define READ_RGBA(p, r, g, b, a) \
   (r) = R_VAL(p); \
   (g) = G_VAL(p); \
   (b) = B_VAL(p); \
   (a) = A_VAL(p);

#define WRITE_RGB(p, r, g, b) \
   R_VAL(p) = (r); \
   G_VAL(p) = (g); \
   B_VAL(p) = (b);

#define WRITE_RGB_PRESERVE_ALPHA(p, r, g, b) \
   WRITE_RGB(p, r, g, b)

#define WRITE_RGBA(p, r, g, b, a) \
   R_VAL(p) = (r); \
   G_VAL(p) = (g); \
   B_VAL(p) = (b); \
   A_VAL(p) = (a);

#define INTERSECTS(x, y, w, h, xx, yy, ww, hh) \
   ((x < (xx + ww)) && \
       (y < (yy + hh)) && \
       ((x + w) > xx) && \
       ((y + h) > yy))

#define CLIP_TO(_x, _y, _w, _h, _cx, _cy, _cw, _ch) \
{ \
if (INTERSECTS(_x, _y, _w, _h, _cx, _cy, _cw, _ch)) \
   { \
         if (_x < _cx) \
	{ \
	           _w += _x - _cx; \
	           _x = _cx; \
	           if (_w < 0) _w = 0; \
	} \
         if ((_x + _w) > (_cx + _cw)) \
	     _w = _cx + _cw - _x; \
         if (_y < _cy) \
	{ \
	           _h += _y - _cy; \
	           _y = _cy; \
	           if (_h < 0) _h = 0; \
	} \
         if ((_y + _h) > (_cy + _ch)) \
	     _h = _cy + _ch - _y; \
   } \
else \
   { \
      _w = 0; _h = 0; \
   } \
}
   
/*
 * 1) Basic Saturation - 8 bit unsigned
 *
 * The add, subtract, and reshade operations generate new color values that may
 * be out of range for an unsigned 8 bit quantity.  Therefore, we will want to
 * saturate the values into the range [0, 255].  Any value < 0 will become 0,
 * and any value > 255 will become 255.  Or simply:
 *
 *   saturated = (value < 0) ? 0 : ((value > 255) ? 255 : value)
 *
 * Of course the above isn't the most efficient means of saturating.  Sometimes
 * due to the nature of a calculation, we know we only need to saturate from
 * above (> 255) or just from below (< 0).  Or simply:
 *
 *   saturated = (value < 0)   ?   0 : value
 *   saturated = (value > 255) ? 255 : value
 *
 * 2) Alternate Forms of Saturation
 *
 * The methods of saturation described above use testing/branching operations,
 * which are not necessarily efficient on all platforms.  There are other means
 * of performing saturation using just simple arithmetic operations
 * (+, -, >>, <<, ~).  A discussion of these saturation techniques follows.
 *
 * A) Saturation in the range [0, 512), or "from above".
 *
 * Assuming we have an integral value in the range [0, 512), the following
 * formula evaluates to either 0, or 255:
 *
 *    (value & 255) - ((value & 256) >> 8)
 *
 * This is easy to show.  Notice that if the value is in the range [0, 256)
 * the 9th bit is 0, and we get (0 - 0), which is 0.   And if the value is in
 * the range [256, 512) the 9th bit is 1, and we get (256 - 1), which is 255.
 *
 * Now, using the above information and the fact that assigning an integer to
 * an 8 bit unsigned value will truncate to the lower 8 bits of the integer,
 * the following properly saturates:
 *
 *    8bit_value = value | (value & 256) - ((value & 256) >> 8)
 *
 * To prove this to yourself, just think about what the lower 8 bits look like
 * in the ranges [0, 256) and [256, 512).  In particular, notice that the value
 * in the range [0, 256) are unchanged, and values in the range [256, 512)
 * always give you 255.  Just what we want!
 *
 * B) Saturation in the range (-256, 256), or "from below".
 *
 * Assuming we have an integral value in the range (-256, 256), the following
 * formula evaluates to either 0, or -1:
 *
 *   ~(value >> 8)
 *
 * Here's why.  If the value is in the range [0, 256), then shifting right by
 * 8 bits gives us all 0 bits, or 0.  And thus inverting the bits gives all
 * 1 bits, which is -1.  If the value is in the range (-256, 0), then the 9th
 * bit and higher bits are all 1.  So, when we shift right by 8 bits (with
 * signed extension), we get a value with all 1 bits.  Which when inverted is
 * all 0 bits, or 0.
 *
 * Now, using the above information the following properly saturates:
 *
 *    8bit_value = value & (~(value >> 8))
 *
 * To prove this to yourself, noticed that values in the range (-256, 0) will
 * always be AND'd with 0, and thus map to 0.   Further, values in the range
 * [0, 256) will always be AND'd with a value that is all 1 bits, and thus
 * be unchanged.  Just what we want!
 *
 * C) Saturation in the range (-256, 512), or "from above and below".
 *
 * The short of it is the following works:
 *
 *    8bit_value = (tmp | ((tmp & 256) - ((tmp & 256) >> 8))) & (~(tmp >> 9))
 *
 * We leave it to the reader to prove.  Looks very similar to the techniques
 * used above, eh? :)
 */

/* Saturate values in the range [0, 512) */
#define SATURATE_UPPER(nc, v) \
   tmp = (v);                 \
   nc = (tmp | (-(tmp >> 8)));

/* Saturate values in the range (-256, 256) */
#define SATURATE_LOWER(nc, v) \
   tmp = (v);                 \
   nc = tmp & (~(tmp >> 8));

/* Saturate values in the range (-256, 512) */
#define SATURATE_BOTH(nc, v) \
   tmp = (v);                \
   nc = (tmp | (-(tmp >> 8))) & (~(tmp >> 9));

/*
 * 1) Operations
 *
 * There are 4 operations supported:
 *
 *    Copy, Add, Subtract, and Reshade
 *
 * For each operation there are 3 different variations that can be made:
 *
 *   a) Use "blend" or "copy" in the calculations?  A "blend" uses the alpha
 *      value of the source pixel to lighten the source pixel values.  Where
 *      as "copy" ignores the alpha value and uses the raw source pixel values.
 *   b) Include source alpha in the calculation for new destination alpha?
 *      If source alpha is not used, then destination alpha is preserved.
 *      If source alpha is used, a "copy" sets the new alpha to the source
 *      alpha, and a "blend" increases it by a factor given by the product
 *      of the source alpha with one minus the destination alpha.
 *   c) Should the source pixels be passed through a color modifier before the
 *      calculations are performed?
 *
 * All together we have 4*2*2*2 = 32 combinations.
 *
 * 2) Copy operation
 *
 * The "copy" version of this operation copies the source image onto the
 * destination image.
 *
 * The "blend" version of this operation blends the source image color 'c' with
 * the destination image color 'cc' using 'a' (in the range [0, 1]) according
 * to the following formula.  Also notice that saturation is not needed for
 * this calculation, the output is in the range [0, 255]:
 *
 *    nc = c * alpha + (1 - alpha) * cc
 *       = c * alpha - cc * alpha + cc
 *       = (c - cc) * alpha + cc;
 *
 * A discussion of how we're calculating this value follows:
 *
 * We're using 'a', an integer, in the range [0, 255] for alpha (and for 'c'
 * and 'cc', BTW).  Therefore, we need to slightly modify the equation to take
 * that into account.  To get into the range [0, 255] we need to divide 'a'
 * by 255:
 *
 *    nc = ((c - cc) * a) / 255 + cc
 *
 * Notice that it is faster to divide by 256 (bit shifting), however without a
 * fudge factor 'x' to balance things this isn't horribly accurate.  So, let's
 * solve for 'x'.  The equality is:
 *
 *    ((c - cc) * a) / 256 + cc + x = ((c - cc) * a) / 255 + cc
 *
 * The 'cc' terms cancel, and multiply both sides by 255*256 to remove the
 * fractions:
 *
 *    ((c - cc) * a) * 255 + 255 * 256 * x = ((c - cc) * a) * 256
 *
 * Get the 'x' term alone:
 *
 *    255 * 256 * x = ((c - cc) * a)
 *
 * Divide both sides by 255 * 256 to solve for 'x':
 *
 *    x = ((c - cc) * a) / (255 * 256)
 *
 * And putting 'x' back into the equation we get:
 *
 *    nc = ((c - cc) * a) / 256 + cc + ((c - cc) * a) / (255 * 256)
 *
 * And if we let 'tmp' represent the value '(c - cc) * a', and do a little
 * regrouping we get:
 *
 *    nc = tmp / 256 + tmp / (255 * 256) + cc
 *       = (tmp + tmp / 255) / 256 + cc
 *
 * We'll be using integer arithmetic, and over the range of values tmp takes
 * (in [-255*255, 255*255]) the term tmp/(255*256) is pretty much the same as
 * tmp/(256*256).  So we get:
 *
 *    nc = (tmp + tmp / 256) / 256 + cc
 *
 * And because the division of the sum uses integer arithmetic, it always
 * rounds up/down even if that isn't the "best" choice.  If we add .5 to the
 * sum, we can get standard rounding:  Like so:
 *
 *    nc = (tmp + tmp / 256 + 128) / 256 + cc
 *
 * 3) Add operation
 *
 * The "copy" version of this operation sums the source image pixel values
 * with the destination image pixel values, saturating at 255 (from above).
 *
 * The "blend" version of this operation sums the source image pixel values,
 * after taking into account alpha transparency (e.g. a percentage), with the
 * destination image pixel values, saturating at 255 (from above).
 *
 * 4) Subtract operation
 *
 * This operation is the same as the Add operation, except the source values
 * are subtracted from the destination values (instead of added).  Further,
 * the result must be saturated at 0 (from below).
 *
 * 5) Reshade operation
 *
 * This operation uses the source image color values to lighten/darken color
 * values in the destination image using the following formula:
 *
 *    nc = cc + ((c - middle_value) * 2 * alpha)
 *
 * Recall our pixel color and alpha values are in the range [0, 255].  So, the
 * "blend" version of this operation can be calculated as:
 *
 *    nc = cc + ((c - 127) * 2 * (a / 255))
 *
 * And in an integer arithmetic friendly form is:
 *
 *    nc = cc + (((c - 127) * a) >> 7)
 *
 * The "copy" version of this operation treats alpha as 1.0 (or a/255), and in
 * integer arithmetic friendly form is:
 *
 *    nc = cc + ((c - 127) << 1)
 *
 * Notice the color values created by this operation are in the range
 * (-256, 512), and thus must be saturated at 0 and 255 (from above and below).
 *
 * For all the operations, when the "blend" version involves computing new
 * destination alpha values via the use of some source alpha, we have that:
 *
 *    nalpha = alpha + ((255 - alpha) * (a / 255))
 *
 * We can use the previous argument for approximating division by 255, and
 * calculate this by:
 *
 *    tmp = (255 - alpha) * a;
 *    nalpha = alpha + ((tmp + (tmp >> 8) + 0x80) >> 8);
 *
 * This is again in the range [0, 255], so no saturation is needed.
 */

#define BLEND_COLOR(a, nc, c, cc) \
tmp = ((c) - (cc)) * (a); \
nc = (cc) + ((tmp + (tmp >> 8) + 0x80) >> 8);

#define ADD_COLOR_WITH_ALPHA(a, nc, c, cc) \
tmp = (c) * (a); \
tmp = (cc) + ((tmp + (tmp >> 8) + 0x80) >> 8); \
nc = (tmp | (-(tmp >> 8)));

#define ADD_COLOR(nc, c, cc) \
tmp = (cc) + (c); \
nc = (tmp | (-(tmp >> 8)));

#define SUB_COLOR_WITH_ALPHA(a, nc, c, cc) \
tmp = (c) * (a); \
tmp = (cc) - ((tmp + (tmp >> 8) + 0x80) >> 8); \
nc = (tmp & (~(tmp >> 8)));

#define SUB_COLOR(nc, c, cc) \
tmp = (cc) - (c); \
nc = (tmp & (~(tmp >> 8)));

#define RESHADE_COLOR_WITH_ALPHA(a, nc, c, cc) \
tmp = (cc) + ((((c) - 127) * (a)) >> 7); \
nc = (tmp | (-(tmp >> 8))) & (~(tmp >> 9));

#define RESHADE_COLOR(nc, c, cc) \
tmp = (cc) + (((c) - 127) << 1); \
nc = (tmp | (-(tmp >> 8))) & (~(tmp >> 9));

extern int pow_lut_initialized;
extern DATA8 pow_lut[256][256];

#define BLEND_DST_ALPHA(r1, g1, b1, a1, dest) \
{ DATA8 _aa; \
_aa = pow_lut[a1][A_VAL(dest)]; \
BLEND_COLOR(a1, A_VAL(dest), 255, A_VAL(dest)); \
BLEND_COLOR(_aa, R_VAL(dest), r1, R_VAL(dest)); \
BLEND_COLOR(_aa, G_VAL(dest), g1, G_VAL(dest)); \
BLEND_COLOR(_aa, B_VAL(dest), b1, B_VAL(dest)); \
}

#define BLEND(r1, g1, b1, a1, dest) \
BLEND_COLOR(a1, R_VAL(dest), r1, R_VAL(dest)); \
BLEND_COLOR(a1, G_VAL(dest), g1, G_VAL(dest)); \
BLEND_COLOR(a1, B_VAL(dest), b1, B_VAL(dest));

#define BLEND_ADD(r1, g1, b1, a1, dest) \
ADD_COLOR_WITH_ALPHA(a1, R_VAL(dest), r1, R_VAL(dest)); \
ADD_COLOR_WITH_ALPHA(a1, G_VAL(dest), g1, G_VAL(dest)); \
ADD_COLOR_WITH_ALPHA(a1, B_VAL(dest), b1, B_VAL(dest));

#define BLEND_SUB(r1, g1, b1, a1, dest) \
SUB_COLOR_WITH_ALPHA(a1, R_VAL(dest), r1, R_VAL(dest)); \
SUB_COLOR_WITH_ALPHA(a1, G_VAL(dest), g1, G_VAL(dest)); \
SUB_COLOR_WITH_ALPHA(a1, B_VAL(dest), b1, B_VAL(dest));

#define BLEND_RE(r1, g1, b1, a1, dest) \
RESHADE_COLOR_WITH_ALPHA(a1, R_VAL(dest), r1, R_VAL(dest)); \
RESHADE_COLOR_WITH_ALPHA(a1, G_VAL(dest), g1, G_VAL(dest)); \
RESHADE_COLOR_WITH_ALPHA(a1, B_VAL(dest), b1, B_VAL(dest));

enum _imlibop
{
   OP_COPY,
   OP_ADD,
   OP_SUBTRACT,
   OP_RESHADE
};

typedef enum _imlibop ImlibOp;

typedef void (*ImlibBlendFunction)(DATA32*, int, DATA32*, int, int, int,
				   ImlibColorModifier *);

__hidden ImlibBlendFunction
__imlib_GetBlendFunction(ImlibOp op, char merge_alpha, char blend, char rgb_src,
			 ImlibColorModifier * cm);
__hidden void
__imlib_BlendImageToImage(ImlibImage *im_src, ImlibImage *im_dst,
                          char aa, char blend, char merge_alpha,
                          int ssx, int ssy, int ssw, int ssh,
                          int ddx, int ddy, int ddw, int ddh,
			  ImlibColorModifier *cm, ImlibOp op,
			  int clx, int cly, int clw, int clh);
__hidden void
__imlib_BlendRGBAToData(DATA32 *src, int src_w, int src_h, DATA32 *dst,
			int dst_w, int dst_h, int sx, int sy, int dx, int dy,
			int w, int h, char blend, char merge_alpha,
			ImlibColorModifier *cm, ImlibOp op, char rgb_src);
__hidden void
__imlib_build_pow_lut(void);

#ifdef DO_MMX_ASM
void
__imlib_mmx_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
			       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				   int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				 int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
					int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);

void
__imlib_mmx_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
			       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
			       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				   int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				   int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				 int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_add_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
					int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
					int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_subtract_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_mmx_reshade_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
#elif DO_AMD64_ASM
void
__imlib_amd64_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
			       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				   int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				 int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
					int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_blend_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_blend_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_copy_rgba_to_rgb(DATA32 *src, int sw, DATA32 *dst,
				     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_copy_rgba_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_copy_rgb_to_rgba(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);


void
__imlib_amd64_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
			       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
			       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
                              int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
			     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				   int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				   int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				 int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_add_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				  int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
					int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
					int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_subtract_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_blend_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_blend_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_blend_rgb_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_blend_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				       int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_copy_rgba_to_rgb_cmod(DATA32 *src, int sw, DATA32 *dst,
				     int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_copy_rgba_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
void
__imlib_amd64_reshade_copy_rgb_to_rgba_cmod(DATA32 *src, int sw, DATA32 *dst,
				      int dw, int w, int h, ImlibColorModifier *cm);
#endif
#endif
