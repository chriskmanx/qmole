#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "span.h"
#include "updates.h"
#include "rgbadraw.h"

static void
__imlib_Ellipse_DrawToData(int xc, int yc, int a, int b, DATA32 color,
                           DATA32 * dst, int dstw, int clx, int cly, int clw,
                           int clh, ImlibOp op, char dst_alpha, char blend)
{
   ImlibPointDrawFunction pfunc;
   int                 xx, yy, x, y, prev_x, prev_y, ty, by, lx, rx;
   DATA32              a2, b2, *tp, *bp;
   DATA64              dx, dy;

   if (A_VAL(&color) == 0xff)
      blend = 0;
   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   if (!pfunc)
      return;

   xc -= clx;
   yc -= cly;
   dst += (dstw * cly) + clx;

   a2 = a * a;
   b2 = b * b;

   yy = b << 16;
   prev_y = b;

   dx = a2 * b;
   dy = 0;

   ty = yc - b - 1;
   by = yc + b;
   lx = xc - 1;
   rx = xc;

   tp = dst + (dstw * ty) + lx;
   bp = dst + (dstw * by) + lx;

   while (dy < dx)
     {
        int                 len;

        y = yy >> 16;
        y += ((yy - (y << 16)) >> 15);

        if (prev_y != y)
          {
             prev_y = y;
             dx -= a2;
             ty++;
             by--;
             tp += dstw;
             bp -= dstw;
          }

        len = rx - lx;

        if (IN_RANGE(lx, ty, clw, clh))
           pfunc(color, tp);
        if (IN_RANGE(rx, ty, clw, clh))
           pfunc(color, tp + len);
        if (IN_RANGE(lx, by, clw, clh))
           pfunc(color, bp);
        if (IN_RANGE(rx, by, clw, clh))
           pfunc(color, bp + len);

        dy += b2;
        yy -= ((dy << 16) / dx);
        lx--;
        rx++;
        tp--;
        bp--;

        if ((lx < 0) && (rx > clw))
           return;
        if ((ty > clh) || (by < 0))
           return;
     }

   xx = yy;
   prev_x = xx >> 16;

   dx = dy;

   ty++;
   by--;

   tp += dstw;
   bp -= dstw;

   while (ty < yc)
     {
        int                 len;

        x = xx >> 16;
        x += ((xx - (x << 16)) >> 15);

        if (prev_x != x)
          {
             prev_x = x;
             dy += b2;
             lx--;
             rx++;
             tp--;
             bp--;
          }

        len = rx - lx;

        if (IN_RANGE(lx, ty, clw, clh))
           pfunc(color, tp);
        if (IN_RANGE(rx, ty, clw, clh))
           pfunc(color, tp + len);
        if (IN_RANGE(lx, by, clw, clh))
           pfunc(color, bp);
        if (IN_RANGE(rx, by, clw, clh))
           pfunc(color, bp + len);

        dx -= a2;
        xx += ((dx << 16) / dy);
        ty++;
        by--;
        tp += dstw;
        bp -= dstw;

        if ((lx < 0) && (rx > clw))
           return;
        if ((ty > clh) || (by < 0))
           return;
     }
}

static void
__imlib_Ellipse_DrawToData_AA(int xc, int yc, int a, int b, DATA32 color,
                              DATA32 * dst, int dstw, int clx, int cly, int clw,
                              int clh, ImlibOp op, char dst_alpha, char blend)
{
   ImlibPointDrawFunction pfunc;
   int                 xx, yy, x, y, prev_x, prev_y, ty, by, lx, rx;
   DATA32              a2, b2, col0, col1, *tp, *bp;
   DATA64              dx, dy;
   DATA8               ca = A_VAL(&color);

   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   if (!pfunc)
      return;

   xc -= clx;
   yc -= cly;
   dst += (dstw * cly) + clx;

   col0 = col1 = color;
   a2 = a * a;
   b2 = b * b;

   yy = b << 16;
   prev_y = b;

   dx = a2 * b;
   dy = 0;

   ty = yc - b - 2;
   by = yc + b + 1;
   lx = xc - 1;
   rx = xc;

   tp = dst + (dstw * ty) + lx;
   bp = dst + (dstw * by) + lx;

   while (dy < dx)
     {
        int                 len;
        DATA32              tmp;

        y = yy >> 16;

        if (prev_y != y)
          {
             prev_y = y;
             dx -= a2;
             ty++;
             by--;
             tp += dstw;
             bp -= dstw;
          }

        A_VAL(&col1) = (yy - (y << 16)) >> 8;
        A_VAL(&col0) = 255 - A_VAL(&col1);

        if (ca < 255)
          {
             MULT(A_VAL(&col0), ca, A_VAL(&col0), tmp);
             MULT(A_VAL(&col1), ca, A_VAL(&col1), tmp);
          }

        len = rx - lx;

        if (IN_RANGE(lx, ty, clw, clh))
           pfunc(col1, tp);
        if (IN_RANGE(lx, ty + 1, clw, clh))
           pfunc(col0, tp + dstw);

        if (IN_RANGE(rx, ty + 1, clw, clh))
           pfunc(col0, tp + dstw + len);
        if (IN_RANGE(rx, ty, clw, clh))
           pfunc(col1, tp + len);

        if (IN_RANGE(lx, by, clw, clh))
           pfunc(col1, bp);
        if (IN_RANGE(lx, by - 1, clw, clh))
           pfunc(col0, bp - dstw);

        if (IN_RANGE(rx, by - 1, clw, clh))
           pfunc(col0, bp - dstw + len);
        if (IN_RANGE(rx, by, clw, clh))
           pfunc(col1, bp + len);

        dy += b2;
        yy -= ((dy << 16) / dx);
        lx--;
        rx++;
        tp--;
        bp--;

        if ((lx < 0) && (rx > clw))
           return;
        if ((ty > clh) || (by < 0))
           return;
     }

   y = yy >> 16;
   xx = yy;
   prev_x = y;

   dx = dy;

   ty++;
   by--;

   tp += dstw;
   bp -= dstw;

   while (ty < yc)
     {
        int                 len;
        DATA32              tmp;

        x = xx >> 16;

        if (prev_x != x)
          {
             prev_x = x;
             dy += b2;
             lx--;
             rx++;
             tp--;
             bp--;
          }

        A_VAL(&col1) = (xx - (x << 16)) >> 8;
        A_VAL(&col0) = 255 - A_VAL(&col1);

        if (ca < 255)
          {
             MULT(A_VAL(&col0), ca, A_VAL(&col0), tmp);
             MULT(A_VAL(&col1), ca, A_VAL(&col1), tmp);
          }

        len = rx - lx;

        if (IN_RANGE(lx, ty, clw, clh))
           pfunc(col1, tp);
        if (IN_RANGE(lx + 1, ty, clw, clh) && (x != y))
           pfunc(col0, tp + 1);

        if (IN_RANGE(rx - 1, ty, clw, clh) && (x != y))
           pfunc(col0, tp + len - 1);
        if (IN_RANGE(rx, ty, clw, clh))
           pfunc(col1, tp + len);

        if (IN_RANGE(lx, by, clw, clh))
           pfunc(col1, bp);
        if (IN_RANGE(lx + 1, by, clw, clh) && (x != y))
           pfunc(col0, bp + 1);

        if (IN_RANGE(rx - 1, by, clw, clh) && (x != y))
           pfunc(col0, bp + len - 1);
        if (IN_RANGE(rx, by, clw, clh))
           pfunc(col1, bp + len);

        dx -= a2;
        xx += ((dx << 16) / dy);
        ty++;
        by--;
        tp += dstw;
        bp -= dstw;

        if ((lx < 0) && (rx > clw))
           return;
        if ((ty > clh) || (by < 0))
           return;
     }
}

static void
__imlib_Ellipse_FillToData(int xc, int yc, int a, int b, DATA32 color,
                           DATA32 * dst, int dstw, int clx, int cly, int clw,
                           int clh, ImlibOp op, char dst_alpha, char blend)
{
   ImlibPointDrawFunction pfunc;
   ImlibSpanDrawFunction sfunc;
   int                 xx, yy, x, y, prev_x, prev_y, ty, by, lx, rx;
   DATA32              a2, b2, *tp, *bp;
   DATA64              dx, dy;

   if (A_VAL(&color) == 0xff)
      blend = 0;
   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   sfunc = __imlib_GetSpanDrawFunction(op, dst_alpha, blend);
   if ((!sfunc) || (!pfunc))
      return;

   xc -= clx;
   yc -= cly;
   dst += (dstw * cly) + clx;

   a2 = a * a;
   b2 = b * b;

   yy = b << 16;
   prev_y = b;

   dx = a2 * b;
   dy = 0;

   ty = yc - b - 1;
   by = yc + b;
   lx = xc - 1;
   rx = xc;

   tp = dst + (dstw * ty) + lx;
   bp = dst + (dstw * by) + lx;

   while (dy < dx)
     {
        int                 len;
        DATA32             *tpp, *bpp;

        y = yy >> 16;
        y += ((yy - (y << 16)) >> 15);

        if (prev_y != y)
          {
             prev_y = y;
             dx -= a2;
             ty++;
             by--;
             tp += dstw;
             bp -= dstw;

             tpp = tp + 1;
             bpp = bp + 1;
             len = rx;
             if (len > clw)
                len = clw;
             len -= (lx + 1);
             if (lx < -1)
               {
                  len += (lx + 1);
                  tpp -= (lx + 1);
                  bpp -= (lx + 1);
               }

             if (((unsigned)(ty) < clh) && (len > 0))
                sfunc(color, tpp, len);
             if (((unsigned)(by) < clh) && (len > 0))
                sfunc(color, bpp, len);
          }

        len = rx - lx;

        if (IN_RANGE(lx, ty, clw, clh))
           pfunc(color, tp);
        if (IN_RANGE(rx, ty, clw, clh))
           pfunc(color, tp + len);
        if (IN_RANGE(lx, by, clw, clh))
           pfunc(color, bp);
        if (IN_RANGE(rx, by, clw, clh))
           pfunc(color, bp + len);

        dy += b2;
        yy -= ((dy << 16) / dx);
        lx--;
        rx++;
        tp--;
        bp--;

        if ((ty > clh) || (by < 0))
           return;
     }

   xx = yy;
   prev_x = xx >> 16;

   dx = dy;

   ty++;
   by--;

   tp += dstw;
   bp -= dstw;

   while (ty < yc)
     {
        int                 len;
        DATA32             *tpp, *bpp;

        x = xx >> 16;
        x += ((xx - (x << 16)) >> 15);

        if (prev_x != x)
          {
             prev_x = x;
             dy += b2;
             lx--;
             rx++;
             tp--;
             bp--;
          }

        tpp = tp;
        bpp = bp;
        len = rx + 1;
        if (len > clw)
           len = clw;
        len -= lx;
        if (lx < 0)
          {
             len += lx;
             tpp -= lx;
             bpp -= lx;
          }

        if (((unsigned)(ty) < clh) && (len > 0))
           sfunc(color, tpp, len);
        if (((unsigned)(by) < clh) && (len > 0))
           sfunc(color, bpp, len);

        dx -= a2;
        xx += ((dx << 16) / dy);
        ty++;
        by--;
        tp += dstw;
        bp -= dstw;

        if ((ty > clh) || (by < 0))
           return;
     }
}

static void
__imlib_Ellipse_FillToData_AA(int xc, int yc, int a, int b, DATA32 color,
                              DATA32 * dst, int dstw, int clx, int cly, int clw,
                              int clh, ImlibOp op, char dst_alpha, char blend)
{
   ImlibPointDrawFunction pfunc;
   ImlibSpanDrawFunction sfunc;
   int                 xx, yy, x, y, prev_x, prev_y, ty, by, lx, rx;
   DATA32              a2, b2, col1, *tp, *bp;
   DATA64              dx, dy;
   DATA8               ca = A_VAL(&color);

   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   if (ca == 0xff)
      blend = 0;
   sfunc = __imlib_GetSpanDrawFunction(op, dst_alpha, blend);
   if ((!pfunc) || (!sfunc))
      return;

   xc -= clx;
   yc -= cly;
   dst += (dstw * cly) + clx;

   col1 = color;
   a2 = a * a;
   b2 = b * b;

   yy = b << 16;
   prev_y = b;

   dx = a2 * b;
   dy = 0;

   ty = yc - b - 2;
   by = yc + b + 1;
   lx = xc - 1;
   rx = xc;

   tp = dst + (dstw * ty) + lx;
   bp = dst + (dstw * by) + lx;

   while (dy < dx)
     {
        int                 len;
        DATA32              tmp, *tpp, *bpp;

        y = yy >> 16;

        if (prev_y != y)
          {
             prev_y = y;
             dx -= a2;
             ty++;
             by--;
             tp += dstw;
             bp -= dstw;

             tpp = tp + 1;
             bpp = bp + 1;
             len = rx;
             if (len > clw)
                len = clw;
             len -= (lx + 1);
             if (lx < -1)
               {
                  len += (lx + 1);
                  tpp -= (lx + 1);
                  bpp -= (lx + 1);
               }

             if (((unsigned)(ty) < clh) && (len > 0))
                sfunc(color, tpp, len);
             if (((unsigned)(by) < clh) && (len > 0))
                sfunc(color, bpp, len);
          }

        A_VAL(&col1) = ((yy - (y << 16)) >> 8);
        if (ca < 255)
           MULT(A_VAL(&col1), ca, A_VAL(&col1), tmp);

        len = rx - lx;

        if (IN_RANGE(lx, ty, clw, clh))
           pfunc(col1, tp);
        if (IN_RANGE(rx, ty, clw, clh))
           pfunc(col1, tp + len);
        if (IN_RANGE(lx, by, clw, clh))
           pfunc(col1, bp);
        if (IN_RANGE(rx, by, clw, clh))
           pfunc(col1, bp + len);

        dy += b2;
        yy -= ((dy << 16) / dx);
        lx--;
        rx++;
        tp--;
        bp--;

        if ((ty > clh) || (by < 0))
           return;
     }

   y = yy >> 16;
   xx = yy;
   prev_x = y;

   dx = dy;

   ty++;
   by--;

   tp += dstw;
   bp -= dstw;

   while (ty < yc)
     {
        int                 len;
        DATA32              tmp, *tpp, *bpp;

        x = xx >> 16;

        if (prev_x != x)
          {
             prev_x = x;
             dy += b2;
             lx--;
             rx++;
             tp--;
             bp--;
          }

        tpp = tp + 1;
        bpp = bp + 1;
        len = rx;
        if (len > clw)
           len = clw;
        len -= (lx + 1);
        if (lx < -1)
          {
             len += (lx + 1);
             tpp -= (lx + 1);
             bpp -= (lx + 1);
          }

        if (((unsigned)(ty) < clh) && (len > 0))
           sfunc(color, tpp, len);
        if (((unsigned)(by) < clh) && (len > 0))
           sfunc(color, bpp, len);

        A_VAL(&col1) = ((xx - (x << 16)) >> 8);
        if (ca < 255)
           MULT(A_VAL(&col1), ca, A_VAL(&col1), tmp);

        len = rx - lx;

        if (IN_RANGE(lx, ty, clw, clh))
           pfunc(col1, tp);
        if (IN_RANGE(rx, ty, clw, clh))
           pfunc(col1, tp + len);
        if (IN_RANGE(lx, by, clw, clh))
           pfunc(col1, bp);
        if (IN_RANGE(rx, by, clw, clh))
           pfunc(col1, bp + len);

        dx -= a2;
        xx += ((dx << 16) / dy);
        ty++;
        by--;
        tp += dstw;
        bp -= dstw;

        if ((ty > clh) || (by < 0))
           return;
     }
}

void
__imlib_Ellipse_DrawToImage(int xc, int yc, int a, int b, DATA32 color,
                            ImlibImage * im, int clx, int cly, int clw, int clh,
                            ImlibOp op, char blend, char anti_alias)
{
   int                 x, y, w, h;

   if ((a == 0) || (b == 0))
     {
        (void)__imlib_Line_DrawToImage(xc - a, yc - b, xc + a, yc + b, color,
                                       im, clx, cly, clw, clh,
                                       op, blend, anti_alias, 0);
        return;
     }
   if (blend && (!A_VAL(&color)))
      return;
   if (clw < 0)
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

   if (a < 0)
      a = -a;
   if (b < 0)
      b = -b;
   if (a > 65535)
      a = 65535;
   if (b > 65535)
      b = 65535;

   w = 2 * (a + 1);
   h = 2 * (b + 1);
   x = xc - a - 1;
   y = yc - b - 1;
   if (anti_alias)
     {
        w += 2;
        h += 2;
        x--;
        y--;
     }

   CLIP_RECT_TO_RECT(x, y, w, h, clx, cly, clw, clh);
   if ((w < 1) || (h < 1))
      return;

   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   if (anti_alias)
      __imlib_Ellipse_DrawToData_AA(xc, yc, a, b, color,
                                    im->data, im->w, clx, cly, clw, clh,
                                    op, IMAGE_HAS_ALPHA(im), blend);
   else
      __imlib_Ellipse_DrawToData(xc, yc, a, b, color,
                                 im->data, im->w, clx, cly, clw, clh,
                                 op, IMAGE_HAS_ALPHA(im), blend);
}

void
__imlib_Ellipse_FillToImage(int xc, int yc, int a, int b, DATA32 color,
                            ImlibImage * im, int clx, int cly, int clw, int clh,
                            ImlibOp op, char blend, char anti_alias)
{
   int                 x, y, w, h;

   if ((a == 0) || (b == 0))
     {
        (void)__imlib_Line_DrawToImage(xc - a, yc - b, xc + a, yc + b, color,
                                       im, clx, cly, clw, clh,
                                       op, blend, anti_alias, 0);
        return;
     }
   if (blend && (!A_VAL(&color)))
      return;
   if (clw < 0)
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

   if (a < 0)
      a = -a;
   if (b < 0)
      b = -b;
   if (a > 65535)
      a = 65535;
   if (b > 65535)
      b = 65535;

   w = 2 * (a + 1);
   h = 2 * (b + 1);
   x = xc - a - 1;
   y = yc - b - 1;
   if (anti_alias)
     {
        w += 2;
        h += 2;
        x--;
        y--;
     }

   CLIP_RECT_TO_RECT(x, y, w, h, clx, cly, clw, clh);
   if ((w < 1) || (h < 1))
      return;

   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   if (anti_alias)
      __imlib_Ellipse_FillToData_AA(xc, yc, a, b, color,
                                    im->data, im->w, clx, cly, clw, clh,
                                    op, IMAGE_HAS_ALPHA(im), blend);
   else
      __imlib_Ellipse_FillToData(xc, yc, a, b, color,
                                 im->data, im->w, clx, cly, clw, clh,
                                 op, IMAGE_HAS_ALPHA(im), blend);
}
