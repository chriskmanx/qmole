#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "scale.h"

#include <assert.h>

/*\ NB: If you change this, don't forget asm_scale.S \*/
struct _imlib_scale_info {
   int                *xpoints;
   DATA32            **ypoints;
   int                *xapoints, *yapoints;
   int                 xup_yup;
   DATA32             *pix_assert;
};

#define RGBA_COMPOSE(r, g, b, a)  ((a) << 24) | ((r) << 16) | ((g) << 8) | (b)
#define INV_XAP                   (256 - xapoints[x])
#define XAP                       (xapoints[x])
#define INV_YAP                   (256 - yapoints[dyy + y])
#define YAP                       (yapoints[dyy + y])

static DATA32     **
__imlib_CalcYPoints(DATA32 * src, int sw, int sh, int dh, int b1, int b2)
{
   DATA32            **p;
   int                 i, j = 0;
   int                 val, inc, rv = 0;

   if (dh < 0)
     {
        dh = -dh;
        rv = 1;
     }
   p = malloc((dh + 1) * sizeof(DATA32 *));
   if (dh < (b1 + b2))
     {
        if (dh < b1)
          {
             b1 = dh;
             b2 = 0;
          }
        else
           b2 = dh - b1;
     }
   val = 0;
   inc = 1 << 16;
   for (i = 0; i < b1; i++)
     {
        p[j++] = src + ((val >> 16) * sw);
        val += inc;
     }
   if (dh > (b1 + b2))
     {
        val = (b1 << 16);
        inc = ((sh - b1 - b2) << 16) / (dh - b1 - b2);
        for (i = 0; i < (dh - b1 - b2); i++)
          {
             p[j++] = src + ((val >> 16) * sw);
             val += inc;
          }
     }
   val = (sh - b2) << 16;
   inc = 1 << 16;
   for (i = 0; i <= b2; i++)
     {
        p[j++] = src + ((val >> 16) * sw);
        val += inc;
     }
   if (rv)
      for (i = dh / 2; --i >= 0;)
        {
           DATA32             *tmp = p[i];

           p[i] = p[dh - i - 1];
           p[dh - i - 1] = tmp;
        }
   return p;
}

static int         *
__imlib_CalcXPoints(int sw, int dw, int b1, int b2)
{
   int                *p, i, j = 0;
   int                 val, inc, rv = 0;

   if (dw < 0)
     {
        dw = -dw;
        rv = 1;
     }
   p = malloc((dw + 1) * sizeof(int));
   if (dw < (b1 + b2))
     {
        if (dw < b1)
          {
             b1 = dw;
             b2 = 0;
          }
        else
           b2 = dw - b1;
     }
   val = 0;
   inc = 1 << 16;
   for (i = 0; i < b1; i++)
     {
        p[j++] = (val >> 16);
        val += inc;
     }
   if (dw > (b1 + b2))
     {
        val = (b1 << 16);
        inc = ((sw - b1 - b2) << 16) / (dw - b1 - b2);
        for (i = 0; i < (dw - b1 - b2); i++)
          {
             p[j++] = (val >> 16);
             val += inc;
          }
     }
   val = (sw - b2) << 16;
   inc = 1 << 16;
   for (i = 0; i <= b2; i++)
     {
        p[j++] = (val >> 16);
        val += inc;
     }
   if (rv)
      for (i = dw / 2; --i >= 0;)
        {
           int                 tmp = p[i];

           p[i] = p[dw - i - 1];
           p[dw - i - 1] = tmp;
        }
   return p;
}

static int         *
__imlib_CalcApoints(int s, int d, int b1, int b2, int up)
{
   int                *p, i, j = 0, rv = 0;

   if (d < 0)
     {
        rv = 1;
        d = -d;
     }
   p = malloc(d * sizeof(int));
   if (d < (b1 + b2))
     {
        if (d < b1)
          {
             b1 = d;
             b2 = 0;
          }
        else
           b2 = d - b1;
     }
   /* scaling up */
   if (up)
     {
        int                 val, inc;

        for (i = 0; i < b1; i++)
           p[j++] = 0;
        if (d > (b1 + b2))
          {
             int                 ss, dd;

             ss = s - b1 - b2;
             dd = d - b1 - b2;
             val = 0;
             inc = (ss << 16) / dd;
             for (i = 0; i < dd; i++)
               {
                  p[j++] = (val >> 8) - ((val >> 8) & 0xffffff00);
                  if (((val >> 16) + b1) >= (s - 1))
                     p[j - 1] = 0;
                  val += inc;
               }
          }
        for (i = 0; i < b2; i++)
           p[j++] = 0;
     }
   /* scaling down */
   else
     {
        int                 val, inc;

        for (i = 0; i < b1; i++)
           p[j++] = (1 << (16 + 14)) + (1 << 14);
        if (d > (b1 + b2))
          {
             int                 ss, dd, ap, Cp;

             ss = s - b1 - b2;
             dd = d - b1 - b2;
             val = 0;
             inc = (ss << 16) / dd;
             Cp = ((dd << 14) / ss) + 1;
             for (i = 0; i < dd; i++)
               {
                  ap = ((0x100 - ((val >> 8) & 0xff)) * Cp) >> 8;
                  p[j] = ap | (Cp << 16);
                  j++;
                  val += inc;
               }
          }
        for (i = 0; i < b2; i++)
           p[j++] = (1 << (16 + 14)) + (1 << 14);
     }
   if (rv)
     {
        for (i = d / 2; --i >= 0;)
          {
             int                 tmp = p[i];

             p[i] = p[d - i - 1];
             p[d - i - 1] = tmp;
          }
     }
   return p;
}

ImlibScaleInfo     *
__imlib_FreeScaleInfo(ImlibScaleInfo * isi)
{
   if (isi)
     {
        free(isi->xpoints);
        free(isi->ypoints);
        free(isi->xapoints);
        free(isi->yapoints);
        free(isi);
     }
   return NULL;
}

ImlibScaleInfo     *
__imlib_CalcScaleInfo(ImlibImage * im, int sw, int sh, int dw, int dh, char aa)
{
   ImlibScaleInfo     *isi;
   int                 scw, sch;

   scw = dw * im->w / sw;
   sch = dh * im->h / sh;

   isi = malloc(sizeof(ImlibScaleInfo));
   if (!isi)
      return NULL;
   memset(isi, 0, sizeof(ImlibScaleInfo));

   isi->pix_assert = im->data + im->w * im->h;

   isi->xup_yup = (abs(dw) >= sw) + ((abs(dh) >= sh) << 1);

   isi->xpoints = __imlib_CalcXPoints(im->w, scw,
                                      im->border.left, im->border.right);
   if (!isi->xpoints)
      return __imlib_FreeScaleInfo(isi);
   isi->ypoints = __imlib_CalcYPoints(im->data, im->w, im->h, sch,
                                      im->border.top, im->border.bottom);
   if (!isi->ypoints)
      return __imlib_FreeScaleInfo(isi);
   if (aa)
     {
        isi->xapoints = __imlib_CalcApoints(im->w, scw, im->border.left,
                                            im->border.right, isi->xup_yup & 1);
        if (!isi->xapoints)
           return __imlib_FreeScaleInfo(isi);
        isi->yapoints = __imlib_CalcApoints(im->h, sch, im->border.top,
                                            im->border.bottom,
                                            isi->xup_yup & 2);
        if (!isi->yapoints)
           return __imlib_FreeScaleInfo(isi);
     }
   return isi;
}

/* scale by pixel sampling only */
void
__imlib_ScaleSampleRGBA(ImlibScaleInfo * isi, DATA32 * dest, int dxx, int dyy,
                        int dx, int dy, int dw, int dh, int dow)
{
   DATA32             *sptr, *dptr;
   int                 x, y, end;
   DATA32            **ypoints = isi->ypoints;
   int                *xpoints = isi->xpoints;

   /* whats the last pixel ont he line so we stop there */
   end = dxx + dw;
   /* go through every scanline in the output buffer */
   for (y = 0; y < dh; y++)
     {
        /* get the pointer to the start of the destination scanline */
        dptr = dest + dx + ((y + dy) * dow);
        /* calculate the source line we'll scan from */
        sptr = ypoints[dyy + y];
        /* go thru the scanline and copy across */
        for (x = dxx; x < end; x++)
           *dptr++ = sptr[xpoints[x]];
     }
}

/* FIXME: NEED to optimise ScaleAARGBA - currently its "ok" but needs work*/

/* scale by area sampling */
void
__imlib_ScaleAARGBA(ImlibScaleInfo * isi, DATA32 * dest, int dxx, int dyy,
                    int dx, int dy, int dw, int dh, int dow, int sow)
{
   DATA32             *sptr, *dptr;
   int                 x, y, end;
   DATA32            **ypoints = isi->ypoints;
   int                *xpoints = isi->xpoints;
   int                *xapoints = isi->xapoints;
   int                *yapoints = isi->yapoints;

   end = dxx + dw;
   /* scaling up both ways */
   if (isi->xup_yup == 3)
     {
        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];
             if (YAP > 0)
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r, g, b, a;
                       int                 rr, gg, bb, aa;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_XAP;
                            g = G_VAL(pix) * INV_XAP;
                            b = B_VAL(pix) * INV_XAP;
                            a = A_VAL(pix) * INV_XAP;
                            pix++;
                            r += R_VAL(pix) * XAP;
                            g += G_VAL(pix) * XAP;
                            b += B_VAL(pix) * XAP;
                            a += A_VAL(pix) * XAP;
                            pix += sow;
                            rr = R_VAL(pix) * XAP;
                            gg = G_VAL(pix) * XAP;
                            bb = B_VAL(pix) * XAP;
                            aa = A_VAL(pix) * XAP;
                            pix--;
                            rr += R_VAL(pix) * INV_XAP;
                            gg += G_VAL(pix) * INV_XAP;
                            bb += B_VAL(pix) * INV_XAP;
                            aa += A_VAL(pix) * INV_XAP;
                            r = ((rr * YAP) + (r * INV_YAP)) >> 16;
                            g = ((gg * YAP) + (g * INV_YAP)) >> 16;
                            b = ((bb * YAP) + (b * INV_YAP)) >> 16;
                            a = ((aa * YAP) + (a * INV_YAP)) >> 16;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                       else
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_YAP;
                            g = G_VAL(pix) * INV_YAP;
                            b = B_VAL(pix) * INV_YAP;
                            a = A_VAL(pix) * INV_YAP;
                            pix += sow;
                            r += R_VAL(pix) * YAP;
                            g += G_VAL(pix) * YAP;
                            b += B_VAL(pix) * YAP;
                            a += A_VAL(pix) * YAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            a >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                    }
               }
             else
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r, g, b, a;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_XAP;
                            g = G_VAL(pix) * INV_XAP;
                            b = B_VAL(pix) * INV_XAP;
                            a = A_VAL(pix) * INV_XAP;
                            pix++;
                            r += R_VAL(pix) * XAP;
                            g += G_VAL(pix) * XAP;
                            b += B_VAL(pix) * XAP;
                            a += A_VAL(pix) * XAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            a >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                       else
                          *dptr++ = sptr[xpoints[x]];
                    }
               }
          }
     }
   /* if we're scaling down vertically */
   else if (isi->xup_yup == 1)
#ifndef OLD_SCALE_DOWN
     {
        /*\ 'Correct' version, with math units prepared for MMXification \ */
        int                 Cy, j;
        DATA32             *pix;
        int                 r, g, b, a, rr, gg, bb, aa;
        int                 yap;

        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             Cy = YAP >> 16;
             yap = YAP & 0xffff;

             dptr = dest + dx + ((y + dy) * dow);
             for (x = dxx; x < end; x++)
               {
                  pix = ypoints[dyy + y] + xpoints[x];
                  r = (R_VAL(pix) * yap) >> 10;
                  g = (G_VAL(pix) * yap) >> 10;
                  b = (B_VAL(pix) * yap) >> 10;
                  a = (A_VAL(pix) * yap) >> 10;
                  for (j = (1 << 14) - yap; j > Cy; j -= Cy)
                    {
                       pix += sow;
                       r += (R_VAL(pix) * Cy) >> 10;
                       g += (G_VAL(pix) * Cy) >> 10;
                       b += (B_VAL(pix) * Cy) >> 10;
                       a += (A_VAL(pix) * Cy) >> 10;
                    }
                  if (j > 0)
                    {
                       pix += sow;
                       r += (R_VAL(pix) * j) >> 10;
                       g += (G_VAL(pix) * j) >> 10;
                       b += (B_VAL(pix) * j) >> 10;
                       a += (A_VAL(pix) * j) >> 10;
                    }
                  assert(pix < isi->pix_assert);
                  if (XAP > 0)
                    {
                       pix = ypoints[dyy + y] + xpoints[x] + 1;
                       rr = (R_VAL(pix) * yap) >> 10;
                       gg = (G_VAL(pix) * yap) >> 10;
                       bb = (B_VAL(pix) * yap) >> 10;
                       aa = (A_VAL(pix) * yap) >> 10;
                       for (j = (1 << 14) - yap; j > Cy; j -= Cy)
                         {
                            pix += sow;
                            rr += (R_VAL(pix) * Cy) >> 10;
                            gg += (G_VAL(pix) * Cy) >> 10;
                            bb += (B_VAL(pix) * Cy) >> 10;
                            aa += (A_VAL(pix) * Cy) >> 10;
                         }
                       if (j > 0)
                         {
                            pix += sow;
                            rr += (R_VAL(pix) * j) >> 10;
                            gg += (G_VAL(pix) * j) >> 10;
                            bb += (B_VAL(pix) * j) >> 10;
                            aa += (A_VAL(pix) * j) >> 10;
                         }
                       assert(pix < isi->pix_assert);
                       r = r * INV_XAP;
                       g = g * INV_XAP;
                       b = b * INV_XAP;
                       a = a * INV_XAP;
                       r = (r + ((rr * XAP))) >> 12;
                       g = (g + ((gg * XAP))) >> 12;
                       b = (b + ((bb * XAP))) >> 12;
                       a = (a + ((aa * XAP))) >> 12;
                    }
                  else
                    {
                       r >>= 4;
                       g >>= 4;
                       b >>= 4;
                       a >>= 4;
                    }
                  *dptr = RGBA_COMPOSE(r, g, b, a);
                  dptr++;
               }
          }
     }
#else
     {
        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             int                 yap;

             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];

             yap = (ypoints[dyy + y + 1] - ypoints[dyy + y]) / sow;
             if (yap > 1)
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0, a = 0;
                       int                 rr = 0, gg = 0, bb = 0, aa = 0;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = sptr + xpoints[x];
                            for (j = 0; j < yap; j++)
                              {
                                 r += R_VAL(pix);
                                 g += G_VAL(pix);
                                 b += B_VAL(pix);
                                 a += A_VAL(pix);
                                 rr += R_VAL(pix + 1);
                                 gg += G_VAL(pix + 1);
                                 bb += B_VAL(pix + 1);
                                 aa += A_VAL(pix + 1);
                                 pix += sow;
                              }
                            r = r * INV_XAP / yap;
                            g = g * INV_XAP / yap;
                            b = b * INV_XAP / yap;
                            a = a * INV_XAP / yap;
                            r = (r + ((rr * XAP) / yap)) >> 8;
                            g = (g + ((gg * XAP) / yap)) >> 8;
                            b = (b + ((bb * XAP) / yap)) >> 8;
                            a = (a + ((aa * XAP) / yap)) >> 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                       else
                         {
                            pix = sptr + xpoints[x];
                            for (j = 0; j < yap; j++)
                              {
                                 r += R_VAL(pix);
                                 g += G_VAL(pix);
                                 b += B_VAL(pix);
                                 a += A_VAL(pix);
                                 pix += sow;
                              }
                            r /= yap;
                            g /= yap;
                            b /= yap;
                            a /= yap;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                    }
               }
             else
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0, a = 0;
                       int                 count;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_XAP;
                            g = G_VAL(pix) * INV_XAP;
                            b = B_VAL(pix) * INV_XAP;
                            a = A_VAL(pix) * INV_XAP;
                            pix++;
                            r += R_VAL(pix) * XAP;
                            g += G_VAL(pix) * XAP;
                            b += B_VAL(pix) * XAP;
                            a += A_VAL(pix) * XAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            a >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                       else
                          *dptr++ = sptr[xpoints[x]];
                    }
               }
          }
     }
#endif
   /* if we're scaling down horizontally */
   else if (isi->xup_yup == 2)
#ifndef OLD_SCALE_DOWN
     {
        /*\ 'Correct' version, with math units prepared for MMXification \ */
        int                 Cx, j;
        DATA32             *pix;
        int                 r, g, b, a, rr, gg, bb, aa;
        int                 xap;

        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             dptr = dest + dx + ((y + dy) * dow);
             for (x = dxx; x < end; x++)
               {
                  Cx = XAP >> 16;
                  xap = XAP & 0xffff;

                  pix = ypoints[dyy + y] + xpoints[x];
                  r = (R_VAL(pix) * xap) >> 10;
                  g = (G_VAL(pix) * xap) >> 10;
                  b = (B_VAL(pix) * xap) >> 10;
                  a = (A_VAL(pix) * xap) >> 10;
                  for (j = (1 << 14) - xap; j > Cx; j -= Cx)
                    {
                       pix++;
                       r += (R_VAL(pix) * Cx) >> 10;
                       g += (G_VAL(pix) * Cx) >> 10;
                       b += (B_VAL(pix) * Cx) >> 10;
                       a += (A_VAL(pix) * Cx) >> 10;
                    }
                  if (j > 0)
                    {
                       pix++;
                       r += (R_VAL(pix) * j) >> 10;
                       g += (G_VAL(pix) * j) >> 10;
                       b += (B_VAL(pix) * j) >> 10;
                       a += (A_VAL(pix) * j) >> 10;
                    }
                  assert(pix < isi->pix_assert);
                  if (YAP > 0)
                    {
                       pix = ypoints[dyy + y] + xpoints[x] + sow;
                       rr = (R_VAL(pix) * xap) >> 10;
                       gg = (G_VAL(pix) * xap) >> 10;
                       bb = (B_VAL(pix) * xap) >> 10;
                       aa = (A_VAL(pix) * xap) >> 10;
                       for (j = (1 << 14) - xap; j > Cx; j -= Cx)
                         {
                            pix++;
                            rr += (R_VAL(pix) * Cx) >> 10;
                            gg += (G_VAL(pix) * Cx) >> 10;
                            bb += (B_VAL(pix) * Cx) >> 10;
                            aa += (A_VAL(pix) * Cx) >> 10;
                         }
                       if (j > 0)
                         {
                            pix++;
                            rr += (R_VAL(pix) * j) >> 10;
                            gg += (G_VAL(pix) * j) >> 10;
                            bb += (B_VAL(pix) * j) >> 10;
                            aa += (A_VAL(pix) * j) >> 10;
                         }
                       assert(pix < isi->pix_assert);
                       r = r * INV_YAP;
                       g = g * INV_YAP;
                       b = b * INV_YAP;
                       a = a * INV_YAP;
                       r = (r + ((rr * YAP))) >> 12;
                       g = (g + ((gg * YAP))) >> 12;
                       b = (b + ((bb * YAP))) >> 12;
                       a = (a + ((aa * YAP))) >> 12;
                    }
                  else
                    {
                       r >>= 4;
                       g >>= 4;
                       b >>= 4;
                       a >>= 4;
                    }
                  *dptr = RGBA_COMPOSE(r, g, b, a);
                  dptr++;
               }
          }
     }
#else
     {
        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];
             if (YAP > 0)
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0, a = 0;
                       int                 rr = 0, gg = 0, bb = 0, aa = 0;
                       int                 xap;
                       DATA32             *pix;

                       xap = xpoints[x + 1] - xpoints[x];
                       if (xap > 1)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            for (i = 0; i < xap; i++)
                              {
                                 r += R_VAL(pix + i);
                                 g += G_VAL(pix + i);
                                 b += B_VAL(pix + i);
                                 a += A_VAL(pix + i);
                              }
                            r = r * INV_YAP / xap;
                            g = g * INV_YAP / xap;
                            b = b * INV_YAP / xap;
                            a = a * INV_YAP / xap;
                            pix = ypoints[dyy + y] + xpoints[x] + sow;
                            for (i = 0; i < xap; i++)
                              {
                                 rr += R_VAL(pix + i);
                                 gg += G_VAL(pix + i);
                                 bb += B_VAL(pix + i);
                                 aa += A_VAL(pix + i);
                              }
                            r = (r + ((rr * YAP) / xap)) >> 8;
                            g = (g + ((gg * YAP) / xap)) >> 8;
                            b = (b + ((bb * YAP) / xap)) >> 8;
                            a = (a + ((aa * YAP) / xap)) >> 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                       else
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_YAP;
                            g = G_VAL(pix) * INV_YAP;
                            b = B_VAL(pix) * INV_YAP;
                            a = A_VAL(pix) * INV_YAP;
                            pix += sow;
                            r += R_VAL(pix) * YAP;
                            g += G_VAL(pix) * YAP;
                            b += B_VAL(pix) * YAP;
                            a += A_VAL(pix) * YAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            a >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                    }
               }
             else
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0, a = 0;
                       int                 xap;
                       DATA32             *pix;

                       xap = xpoints[x + 1] - xpoints[x];
                       if (xap > 1)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            for (i = 0; i < xap; i++)
                              {
                                 r += R_VAL(pix + i);
                                 g += G_VAL(pix + i);
                                 b += B_VAL(pix + i);
                                 a += A_VAL(pix + i);
                              }
                            r /= xap;
                            g /= xap;
                            b /= xap;
                            a /= xap;
                            *dptr++ = RGBA_COMPOSE(r, g, b, a);
                         }
                       else
                          *dptr++ = sptr[xpoints[x]];
                    }
               }
          }
     }
#endif
   /* if we're scaling down horizontally & vertically */
   else
#ifndef OLD_SCALE_DOWN
     {
        /*\ 'Correct' version, with math units prepared for MMXification:
         * |*|  The operation 'b = (b * c) >> 16' translates to pmulhw,
         * |*|  so the operation 'b = (b * c) >> d' would translate to
         * |*|  psllw (16 - d), %mmb; pmulh %mmc, %mmb
         * \ */
        int                 Cx, Cy, i, j;
        DATA32             *pix;
        int                 a, r, g, b, ax, rx, gx, bx;
        int                 xap, yap;

        for (y = 0; y < dh; y++)
          {
             Cy = YAP >> 16;
             yap = YAP & 0xffff;

             dptr = dest + dx + ((y + dy) * dow);
             for (x = dxx; x < end; x++)
               {
                  Cx = XAP >> 16;
                  xap = XAP & 0xffff;

                  sptr = ypoints[dyy + y] + xpoints[x];
                  pix = sptr;
                  sptr += sow;
                  rx = (R_VAL(pix) * xap) >> 9;
                  gx = (G_VAL(pix) * xap) >> 9;
                  bx = (B_VAL(pix) * xap) >> 9;
                  ax = (A_VAL(pix) * xap) >> 9;
                  pix++;
                  for (i = (1 << 14) - xap; i > Cx; i -= Cx)
                    {
                       rx += (R_VAL(pix) * Cx) >> 9;
                       gx += (G_VAL(pix) * Cx) >> 9;
                       bx += (B_VAL(pix) * Cx) >> 9;
                       ax += (A_VAL(pix) * Cx) >> 9;
                       pix++;
                    }
                  if (i > 0)
                    {
                       rx += (R_VAL(pix) * i) >> 9;
                       gx += (G_VAL(pix) * i) >> 9;
                       bx += (B_VAL(pix) * i) >> 9;
                       ax += (A_VAL(pix) * i) >> 9;
                    }

                  r = (rx * yap) >> 14;
                  g = (gx * yap) >> 14;
                  b = (bx * yap) >> 14;
                  a = (ax * yap) >> 14;

                  for (j = (1 << 14) - yap; j > Cy; j -= Cy)
                    {
                       pix = sptr;
                       sptr += sow;
                       rx = (R_VAL(pix) * xap) >> 9;
                       gx = (G_VAL(pix) * xap) >> 9;
                       bx = (B_VAL(pix) * xap) >> 9;
                       ax = (A_VAL(pix) * xap) >> 9;
                       pix++;
                       for (i = (1 << 14) - xap; i > Cx; i -= Cx)
                         {
                            rx += (R_VAL(pix) * Cx) >> 9;
                            gx += (G_VAL(pix) * Cx) >> 9;
                            bx += (B_VAL(pix) * Cx) >> 9;
                            ax += (A_VAL(pix) * Cx) >> 9;
                            pix++;
                         }
                       if (i > 0)
                         {
                            rx += (R_VAL(pix) * i) >> 9;
                            gx += (G_VAL(pix) * i) >> 9;
                            bx += (B_VAL(pix) * i) >> 9;
                            ax += (A_VAL(pix) * i) >> 9;
                         }

                       r += (rx * Cy) >> 14;
                       g += (gx * Cy) >> 14;
                       b += (bx * Cy) >> 14;
                       a += (ax * Cy) >> 14;
                    }
                  if (j > 0)
                    {
                       pix = sptr;
                       sptr += sow;
                       rx = (R_VAL(pix) * xap) >> 9;
                       gx = (G_VAL(pix) * xap) >> 9;
                       bx = (B_VAL(pix) * xap) >> 9;
                       ax = (A_VAL(pix) * xap) >> 9;
                       pix++;
                       for (i = (1 << 14) - xap; i > Cx; i -= Cx)
                         {
                            rx += (R_VAL(pix) * Cx) >> 9;
                            gx += (G_VAL(pix) * Cx) >> 9;
                            bx += (B_VAL(pix) * Cx) >> 9;
                            ax += (A_VAL(pix) * Cx) >> 9;
                            pix++;
                         }
                       if (i > 0)
                         {
                            rx += (R_VAL(pix) * i) >> 9;
                            gx += (G_VAL(pix) * i) >> 9;
                            bx += (B_VAL(pix) * i) >> 9;
                            ax += (A_VAL(pix) * i) >> 9;
                         }

                       r += (rx * j) >> 14;
                       g += (gx * j) >> 14;
                       b += (bx * j) >> 14;
                       a += (ax * j) >> 14;
                    }

                  R_VAL(dptr) = r >> 5;
                  G_VAL(dptr) = g >> 5;
                  B_VAL(dptr) = b >> 5;
                  A_VAL(dptr) = a >> 5;
                  dptr++;
               }
          }
     }
#else
     {
        int                 count;
        DATA32             *pix;
        int                 a, r, g, b;

        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             int                 yap =
                (ypoints[dyy + y + 1] - ypoints[dyy + y]) / sow;
             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];
             for (x = dxx; x < end; x++)
               {
                  int                 xap = xpoints[x + 1] - xpoints[x];

                  if ((xap > 1) || (yap > 1))
                    {
                       r = 0;
                       g = 0;
                       b = 0;
                       pix = ypoints[dyy + y] + xpoints[x];
                       for (j = yap; --j >= 0;)
                         {
                            for (i = xap; --i >= 0;)
                              {
                                 r += R_VAL(pix + i);
                                 g += G_VAL(pix + i);
                                 b += B_VAL(pix + i);
                                 a += A_VAL(pix + i);
                              }
                            pix += sow;
                         }
                       count = xap * yap;
                       R_VAL(dptr) = r / count;
                       G_VAL(dptr) = g / count;
                       B_VAL(dptr) = b / count;
                       A_VAL(dptr) = a / count;
                       dptr++;
                    }
                  else
                     *dptr++ = sptr[xpoints[x]];
               }
          }
     }
#endif
}

/* scale by area sampling - IGNORE the ALPHA byte*/
void
__imlib_ScaleAARGB(ImlibScaleInfo * isi, DATA32 * dest, int dxx, int dyy,
                   int dx, int dy, int dw, int dh, int dow, int sow)
{
   DATA32             *sptr, *dptr;
   int                 x, y, end;
   DATA32            **ypoints = isi->ypoints;
   int                *xpoints = isi->xpoints;
   int                *xapoints = isi->xapoints;
   int                *yapoints = isi->yapoints;

   end = dxx + dw;
   /* scaling up both ways */
   if (isi->xup_yup == 3)
     {
        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];
             if (YAP > 0)
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0;
                       int                 rr = 0, gg = 0, bb = 0;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_XAP;
                            g = G_VAL(pix) * INV_XAP;
                            b = B_VAL(pix) * INV_XAP;
                            pix++;
                            r += R_VAL(pix) * XAP;
                            g += G_VAL(pix) * XAP;
                            b += B_VAL(pix) * XAP;
                            pix += sow;
                            rr = R_VAL(pix) * XAP;
                            gg = G_VAL(pix) * XAP;
                            bb = B_VAL(pix) * XAP;
                            pix--;
                            rr += R_VAL(pix) * INV_XAP;
                            gg += G_VAL(pix) * INV_XAP;
                            bb += B_VAL(pix) * INV_XAP;
                            r = ((rr * YAP) + (r * INV_YAP)) >> 16;
                            g = ((gg * YAP) + (g * INV_YAP)) >> 16;
                            b = ((bb * YAP) + (b * INV_YAP)) >> 16;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                       else
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_YAP;
                            g = G_VAL(pix) * INV_YAP;
                            b = B_VAL(pix) * INV_YAP;
                            pix += sow;
                            r += R_VAL(pix) * YAP;
                            g += G_VAL(pix) * YAP;
                            b += B_VAL(pix) * YAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                    }
               }
             else
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_XAP;
                            g = G_VAL(pix) * INV_XAP;
                            b = B_VAL(pix) * INV_XAP;
                            pix++;
                            r += R_VAL(pix) * XAP;
                            g += G_VAL(pix) * XAP;
                            b += B_VAL(pix) * XAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                       else
                          *dptr++ = sptr[xpoints[x]];
                    }
               }
          }
     }
   /* if we're scaling down vertically */
   else if (isi->xup_yup == 1)
#ifndef OLD_SCALE_DOWN
     {
        /*\ 'Correct' version, with math units prepared for MMXification \ */
        int                 Cy, j;
        DATA32             *pix;
        int                 r, g, b, rr, gg, bb;
        int                 yap;

        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             Cy = YAP >> 16;
             yap = YAP & 0xffff;

             dptr = dest + dx + ((y + dy) * dow);
             for (x = dxx; x < end; x++)
               {
                  pix = ypoints[dyy + y] + xpoints[x];
                  r = (R_VAL(pix) * yap) >> 10;
                  g = (G_VAL(pix) * yap) >> 10;
                  b = (B_VAL(pix) * yap) >> 10;
                  pix += sow;
                  for (j = (1 << 14) - yap; j > Cy; j -= Cy)
                    {
                       r += (R_VAL(pix) * Cy) >> 10;
                       g += (G_VAL(pix) * Cy) >> 10;
                       b += (B_VAL(pix) * Cy) >> 10;
                       pix += sow;
                    }
                  if (j > 0)
                    {
                       r += (R_VAL(pix) * j) >> 10;
                       g += (G_VAL(pix) * j) >> 10;
                       b += (B_VAL(pix) * j) >> 10;
                    }
                  if (XAP > 0)
                    {
                       pix = ypoints[dyy + y] + xpoints[x] + 1;
                       rr = (R_VAL(pix) * yap) >> 10;
                       gg = (G_VAL(pix) * yap) >> 10;
                       bb = (B_VAL(pix) * yap) >> 10;
                       pix += sow;
                       for (j = (1 << 14) - yap; j > Cy; j -= Cy)
                         {
                            rr += (R_VAL(pix) * Cy) >> 10;
                            gg += (G_VAL(pix) * Cy) >> 10;
                            bb += (B_VAL(pix) * Cy) >> 10;
                            pix += sow;
                         }
                       if (j > 0)
                         {
                            rr += (R_VAL(pix) * j) >> 10;
                            gg += (G_VAL(pix) * j) >> 10;
                            bb += (B_VAL(pix) * j) >> 10;
                         }
                       r = r * INV_XAP;
                       g = g * INV_XAP;
                       b = b * INV_XAP;
                       r = (r + ((rr * XAP))) >> 12;
                       g = (g + ((gg * XAP))) >> 12;
                       b = (b + ((bb * XAP))) >> 12;
                    }
                  else
                    {
                       r >>= 4;
                       g >>= 4;
                       b >>= 4;
                    }
                  *dptr = RGBA_COMPOSE(r, g, b, 0xff);
                  dptr++;
               }
          }
     }
#else
     {
        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             int                 yap;

             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];

             yap = (ypoints[dyy + y + 1] - ypoints[dyy + y]) / sow;
             if (yap > 1)
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0;
                       int                 rr = 0, gg = 0, bb = 0;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = sptr + xpoints[x];
                            for (j = 0; j < yap; j++)
                              {
                                 r += R_VAL(pix);
                                 g += G_VAL(pix);
                                 b += B_VAL(pix);
                                 rr += R_VAL(pix + 1);
                                 gg += G_VAL(pix + 1);
                                 bb += B_VAL(pix + 1);
                                 pix += sow;
                              }
                            r = r * INV_XAP / yap;
                            g = g * INV_XAP / yap;
                            b = b * INV_XAP / yap;
                            r = (r + ((rr * XAP) / yap)) >> 8;
                            g = (g + ((gg * XAP) / yap)) >> 8;
                            b = (b + ((bb * XAP) / yap)) >> 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                       else
                         {
                            pix = sptr + xpoints[x];
                            for (j = 0; j < yap; j++)
                              {
                                 r += R_VAL(pix);
                                 g += G_VAL(pix);
                                 b += B_VAL(pix);
                                 pix += sow;
                              }
                            r /= yap;
                            g /= yap;
                            b /= yap;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                    }
               }
             else
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0;
                       DATA32             *pix;

                       if (XAP > 0)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_XAP;
                            g = G_VAL(pix) * INV_XAP;
                            b = B_VAL(pix) * INV_XAP;
                            pix++;
                            r += R_VAL(pix) * XAP;
                            g += G_VAL(pix) * XAP;
                            b += B_VAL(pix) * XAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                       else
                          *dptr++ = sptr[xpoints[x]];
                    }
               }
          }
     }
#endif
   /* if we're scaling down horizontally */
   else if (isi->xup_yup == 2)
#ifndef OLD_SCALE_DOWN
     {
        /*\ 'Correct' version, with math units prepared for MMXification \ */
        int                 Cx, j;
        DATA32             *pix;
        int                 r, g, b, rr, gg, bb;
        int                 xap;

        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             dptr = dest + dx + ((y + dy) * dow);
             for (x = dxx; x < end; x++)
               {
                  Cx = XAP >> 16;
                  xap = XAP & 0xffff;

                  pix = ypoints[dyy + y] + xpoints[x];
                  r = (R_VAL(pix) * xap) >> 10;
                  g = (G_VAL(pix) * xap) >> 10;
                  b = (B_VAL(pix) * xap) >> 10;
                  pix++;
                  for (j = (1 << 14) - xap; j > Cx; j -= Cx)
                    {
                       r += (R_VAL(pix) * Cx) >> 10;
                       g += (G_VAL(pix) * Cx) >> 10;
                       b += (B_VAL(pix) * Cx) >> 10;
                       pix++;
                    }
                  if (j > 0)
                    {
                       r += (R_VAL(pix) * j) >> 10;
                       g += (G_VAL(pix) * j) >> 10;
                       b += (B_VAL(pix) * j) >> 10;
                    }
                  if (YAP > 0)
                    {
                       pix = ypoints[dyy + y] + xpoints[x] + sow;
                       rr = (R_VAL(pix) * xap) >> 10;
                       gg = (G_VAL(pix) * xap) >> 10;
                       bb = (B_VAL(pix) * xap) >> 10;
                       pix++;
                       for (j = (1 << 14) - xap; j > Cx; j -= Cx)
                         {
                            rr += (R_VAL(pix) * Cx) >> 10;
                            gg += (G_VAL(pix) * Cx) >> 10;
                            bb += (B_VAL(pix) * Cx) >> 10;
                            pix++;
                         }
                       if (j > 0)
                         {
                            rr += (R_VAL(pix) * j) >> 10;
                            gg += (G_VAL(pix) * j) >> 10;
                            bb += (B_VAL(pix) * j) >> 10;
                         }
                       r = r * INV_YAP;
                       g = g * INV_YAP;
                       b = b * INV_YAP;
                       r = (r + ((rr * YAP))) >> 12;
                       g = (g + ((gg * YAP))) >> 12;
                       b = (b + ((bb * YAP))) >> 12;
                    }
                  else
                    {
                       r >>= 4;
                       g >>= 4;
                       b >>= 4;
                    }
                  *dptr = RGBA_COMPOSE(r, g, b, 0xff);
                  dptr++;
               }
          }
     }
#else
     {
        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];
             if (YAP > 0)
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0;
                       int                 rr = 0, gg = 0, bb = 0;
                       int                 xap;
                       DATA32             *pix;

                       xap = xpoints[x + 1] - xpoints[x];
                       if (xap > 1)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            for (i = 0; i < xap; i++)
                              {
                                 r += R_VAL(pix + i);
                                 g += G_VAL(pix + i);
                                 b += B_VAL(pix + i);
                              }
                            r = r * INV_YAP / xap;
                            g = g * INV_YAP / xap;
                            b = b * INV_YAP / xap;
                            pix = ypoints[dyy + y] + xpoints[x] + sow;
                            for (i = 0; i < xap; i++)
                              {
                                 rr += R_VAL(pix + i);
                                 gg += G_VAL(pix + i);
                                 bb += B_VAL(pix + i);
                              }
                            r = (r + ((rr * YAP) / xap)) >> 8;
                            g = (g + ((gg * YAP) / xap)) >> 8;
                            b = (b + ((bb * YAP) / xap)) >> 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                       else
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            r = R_VAL(pix) * INV_YAP;
                            g = G_VAL(pix) * INV_YAP;
                            b = B_VAL(pix) * INV_YAP;
                            pix += sow;
                            r += R_VAL(pix) * YAP;
                            g += G_VAL(pix) * YAP;
                            b += B_VAL(pix) * YAP;
                            r >>= 8;
                            g >>= 8;
                            b >>= 8;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                    }
               }
             else
               {
                  for (x = dxx; x < end; x++)
                    {
                       int                 r = 0, g = 0, b = 0;
                       int                 xap;
                       DATA32             *pix;

                       xap = xpoints[x + 1] - xpoints[x];
                       if (xap > 1)
                         {
                            pix = ypoints[dyy + y] + xpoints[x];
                            for (i = 0; i < xap; i++)
                              {
                                 r += R_VAL(pix + i);
                                 g += G_VAL(pix + i);
                                 b += B_VAL(pix + i);
                              }
                            r /= xap;
                            g /= xap;
                            b /= xap;
                            *dptr++ = RGBA_COMPOSE(r, g, b, 0xff);
                         }
                       else
                          *dptr++ = sptr[xpoints[x]];
                    }
               }
          }
     }
#endif
   /* fully optimized (i think) - onyl change of algorithm can help */
   /* if we're scaling down horizontally & vertically */
   else
#ifndef OLD_SCALE_DOWN
     {
        /*\ 'Correct' version, with math units prepared for MMXification \ */
        int                 Cx, Cy, i, j;
        DATA32             *pix;
        int                 r, g, b, rx, gx, bx;
        int                 xap, yap;

        for (y = 0; y < dh; y++)
          {
             Cy = YAP >> 16;
             yap = YAP & 0xffff;

             dptr = dest + dx + ((y + dy) * dow);
             for (x = dxx; x < end; x++)
               {
                  Cx = XAP >> 16;
                  xap = XAP & 0xffff;

                  sptr = ypoints[dyy + y] + xpoints[x];
                  pix = sptr;
                  sptr += sow;
                  rx = (R_VAL(pix) * xap) >> 9;
                  gx = (G_VAL(pix) * xap) >> 9;
                  bx = (B_VAL(pix) * xap) >> 9;
                  pix++;
                  for (i = (1 << 14) - xap; i > Cx; i -= Cx)
                    {
                       rx += (R_VAL(pix) * Cx) >> 9;
                       gx += (G_VAL(pix) * Cx) >> 9;
                       bx += (B_VAL(pix) * Cx) >> 9;
                       pix++;
                    }
                  if (i > 0)
                    {
                       rx += (R_VAL(pix) * i) >> 9;
                       gx += (G_VAL(pix) * i) >> 9;
                       bx += (B_VAL(pix) * i) >> 9;
                    }

                  r = (rx * yap) >> 14;
                  g = (gx * yap) >> 14;
                  b = (bx * yap) >> 14;

                  for (j = (1 << 14) - yap; j > Cy; j -= Cy)
                    {
                       pix = sptr;
                       sptr += sow;
                       rx = (R_VAL(pix) * xap) >> 9;
                       gx = (G_VAL(pix) * xap) >> 9;
                       bx = (B_VAL(pix) * xap) >> 9;
                       pix++;
                       for (i = (1 << 14) - xap; i > Cx; i -= Cx)
                         {
                            rx += (R_VAL(pix) * Cx) >> 9;
                            gx += (G_VAL(pix) * Cx) >> 9;
                            bx += (B_VAL(pix) * Cx) >> 9;
                            pix++;
                         }
                       if (i > 0)
                         {
                            rx += (R_VAL(pix) * i) >> 9;
                            gx += (G_VAL(pix) * i) >> 9;
                            bx += (B_VAL(pix) * i) >> 9;
                         }

                       r += (rx * Cy) >> 14;
                       g += (gx * Cy) >> 14;
                       b += (bx * Cy) >> 14;
                    }
                  if (j > 0)
                    {
                       pix = sptr;
                       sptr += sow;
                       rx = (R_VAL(pix) * xap) >> 9;
                       gx = (G_VAL(pix) * xap) >> 9;
                       bx = (B_VAL(pix) * xap) >> 9;
                       pix++;
                       for (i = (1 << 14) - xap; i > Cx; i -= Cx)
                         {
                            rx += (R_VAL(pix) * Cx) >> 9;
                            gx += (G_VAL(pix) * Cx) >> 9;
                            bx += (B_VAL(pix) * Cx) >> 9;
                            pix++;
                         }
                       if (i > 0)
                         {
                            rx += (R_VAL(pix) * i) >> 9;
                            gx += (G_VAL(pix) * i) >> 9;
                            bx += (B_VAL(pix) * i) >> 9;
                         }

                       r += (rx * j) >> 14;
                       g += (gx * j) >> 14;
                       b += (bx * j) >> 14;
                    }

                  R_VAL(dptr) = r >> 5;
                  G_VAL(dptr) = g >> 5;
                  B_VAL(dptr) = b >> 5;
                  dptr++;
               }
          }
     }
#else
     {
        int                 count;
        DATA32             *pix;
        int                 r, g, b;

        /* go through every scanline in the output buffer */
        for (y = 0; y < dh; y++)
          {
             int                 yap =
                (ypoints[dyy + y + 1] - ypoints[dyy + y]) / sow;
             /* calculate the source line we'll scan from */
             dptr = dest + dx + ((y + dy) * dow);
             sptr = ypoints[dyy + y];
             for (x = dxx; x < end; x++)
               {
                  int                 xap = xpoints[x + 1] - xpoints[x];

                  if ((xap > 1) || (yap > 1))
                    {
                       r = 0;
                       g = 0;
                       b = 0;
                       pix = sptr + xpoints[x];
                       for (j = yap; --j >= 0;)
                         {
                            for (i = xap; --i >= 0;)
                              {
                                 r += R_VAL(pix + i);
                                 g += G_VAL(pix + i);
                                 b += B_VAL(pix + i);
                              }
                            pix += sow;
                         }
                       count = xap * yap;
                       R_VAL(dptr) = r / count;
                       G_VAL(dptr) = g / count;
                       B_VAL(dptr) = b / count;
                       dptr++;
                    }
                  else
                     *dptr++ = sptr[xpoints[x]];
               }
          }
     }
#endif
}
