#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "span.h"
#include "updates.h"
#include "rgbadraw.h"

static void
__imlib_Rectangle_DrawToData(int x, int y, int rw, int rh, DATA32 color,
                             DATA32 * dst, int dstw, int clx, int cly, int clw,
                             int clh, ImlibOp op, char dst_alpha, char blend)
{
   ImlibPointDrawFunction pfunc;
   ImlibSpanDrawFunction sfunc;
   int                 x0, y0, x1, y1, len;
   DATA32             *p;

   if (A_VAL(&color) == 0xff)
      blend = 0;

   sfunc = __imlib_GetSpanDrawFunction(op, dst_alpha, blend);
   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   if (!pfunc || !sfunc)
      return;

   dst += (dstw * cly) + clx;
   x -= clx;
   y -= cly;

   x0 = x;
   x1 = x + rw - 1;

   if (x0 < 0)
      x0 = 0;
   if (x1 >= clw)
      x1 = clw - 1;

   if (y >= 0)
     {
        p = dst + (dstw * y) + x0;
        len = x1 - x0 + 1;
        sfunc(color, p, len);
     }
   if ((y + rh) <= clh)
     {
        p = dst + (dstw * (y + rh - 1)) + x0;
        len = x1 - x0 + 1;
        sfunc(color, p, len);
     }

   y0 = y + 1;
   y1 = y + rh - 2;

   if (y0 < 0)
      y0 = 0;
   if (y1 >= clh)
      y1 = clh - 1;

   len = y1 - y0 + 1;
   if (len <= 0)
      return;
   y1 = len;

   if (x >= 0)
     {
        p = dst + (dstw * y0) + x;
        while (len--)
          {
             pfunc(color, p);
             p += dstw;
          }
     }
   if ((x + rw) <= clw)
     {
        len = y1;
        p = dst + (dstw * y0) + x + rw - 1;
        while (len--)
          {
             pfunc(color, p);
             p += dstw;
          }
     }
}

static void
__imlib_Rectangle_FillToData(int x, int y, int rw, int rh, DATA32 color,
                             DATA32 * dst, int dstw, int clx, int cly, int clw,
                             int clh, ImlibOp op, char dst_alpha, char blend)
{
   ImlibSpanDrawFunction sfunc;
   DATA32             *p;

   if (A_VAL(&color) == 0xff)
      blend = 0;
   sfunc = __imlib_GetSpanDrawFunction(op, dst_alpha, blend);
   if (!sfunc)
      return;

   dst += (dstw * cly) + clx;
   x -= clx;
   y -= cly;

   CLIP_RECT_TO_RECT(x, y, rw, rh, 0, 0, clw, clh);
   if ((rw < 1) || (rh < 1))
      return;

   p = dst + (dstw * y) + x;
   while (rh--)
     {
        sfunc(color, p, rw);
        p += dstw;
     }
}

void
__imlib_Rectangle_DrawToImage(int x, int y, int w, int h, DATA32 color,
                              ImlibImage * im, int clx, int cly, int clw,
                              int clh, ImlibOp op, char blend)
{
   if ((w < 1) || (h < 1) || (clw < 0))
      return;
   if ((w == 1) || (h == 1))
     {
        (void)__imlib_Line_DrawToImage(x, y, x + w - 1, y + h - 1, color,
                                       im, clx, cly, clw, clh, op, blend, 0, 0);
        return;
     }
   if (blend && (!A_VAL(&color)))
      return;

   if (clw == 0)
     {
        clw = im->w;
        clx = 0;
        clh = im->h;
        cly = 0;
     }

   CLIP_RECT_TO_RECT(clx, cly, clw, clh, 0, 0, im->w, im->h);
   if ((clw < 1) || (clh < 1))
      return;

   CLIP_RECT_TO_RECT(clx, cly, clw, clh, x, y, w, h);
   if ((clw < 1) || (clh < 1))
      return;

   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   __imlib_Rectangle_DrawToData(x, y, w, h, color,
                                im->data, im->w, clx, cly, clw, clh,
                                op, IMAGE_HAS_ALPHA(im), blend);
}

void
__imlib_Rectangle_FillToImage(int x, int y, int w, int h, DATA32 color,
                              ImlibImage * im, int clx, int cly, int clw,
                              int clh, ImlibOp op, char blend)
{
   if ((w < 1) || (h < 1) || (clw < 0))
      return;
   if ((w == 1) || (h == 1))
     {
        (void)__imlib_Line_DrawToImage(x, y, x + w - 1, y + h - 1, color,
                                       im, clx, cly, clw, clh, op, blend, 0, 0);
        return;
     }
   if (blend && (!A_VAL(&color)))
      return;

   if (clw == 0)
     {
        clw = im->w;
        clx = 0;
        clh = im->h;
        cly = 0;
     }

   CLIP_RECT_TO_RECT(clx, cly, clw, clh, 0, 0, im->w, im->h);
   if ((clw < 1) || (clh < 1))
      return;

   CLIP_RECT_TO_RECT(clx, cly, clw, clh, x, y, w, h);
   if ((clw < 1) || (clh < 1))
      return;

   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   __imlib_Rectangle_FillToData(x, y, w, h, color,
                                im->data, im->w, clx, cly, clw, clh,
                                op, IMAGE_HAS_ALPHA(im), blend);
}
