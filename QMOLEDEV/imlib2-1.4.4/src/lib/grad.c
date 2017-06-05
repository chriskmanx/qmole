#include "common.h"
#include "colormod.h"
#include "file.h"
#include "loaderpath.h"
#include <math.h>
#include "image.h"
#include "blend.h"
#include "grad.h"
#include "color_helpers.h"

ImlibRange         *
__imlib_CreateRange(void)
{
   ImlibRange         *rg = NULL;

   rg = malloc(sizeof(ImlibRange));
   rg->color = NULL;
   return rg;
}

void
__imlib_FreeRange(ImlibRange * rg)
{
   ImlibRangeColor    *p, *pp;

   p = rg->color;
   while (p)
     {
        pp = p;
        p = p->next;
        free(pp);
     }
   free(rg);
}

void
__imlib_AddRangeColor(ImlibRange * rg, DATA8 r, DATA8 g, DATA8 b, DATA8 a,
                      int dist)
{
   ImlibRangeColor    *p, *rc;

   if (dist < 1)
      dist = 1;
   if (!rg->color)
      dist = 0;

   rc = malloc(sizeof(ImlibRangeColor));
   rc->red = r;
   rc->green = g;
   rc->blue = b;
   rc->alpha = a;
   rc->distance = 0;
   rc->next = NULL;

   p = rg->color;
   if (p)
     {
        while (p)
          {
             if (!p->next)
               {
                  p->distance = dist;
                  p->next = rc;
                  p = NULL;
               }
             else
                p = p->next;
          }
     }
   else
      rg->color = rc;
}

DATA32             *
__imlib_MapRange(ImlibRange * rg, int len)
{
   ImlibRangeColor    *p;
   DATA32             *map, *pmap, v, vv;
   int                 r, g, b, a, rr, gg, bb, aa, i, l, ll, v1, v2, inc, j;

   if (!rg->color)
      return NULL;
   if (!rg->color->next)
      return NULL;
   ll = 1;
   for (p = rg->color; p; p = p->next)
      ll += p->distance;
   map = malloc(len * sizeof(DATA32));
   pmap = malloc(ll * sizeof(DATA32));
   i = 0;
   for (p = rg->color; p; p = p->next)
     {
        if (p->next)
          {
             for (j = 0; j < p->distance; j++)
               {
                  v1 = (j << 16) / p->distance;
                  v2 = 65536 - v1;
                  r = ((p->red * v2) + (p->next->red * v1)) >> 16;
                  g = ((p->green * v2) + (p->next->green * v1)) >> 16;
                  b = ((p->blue * v2) + (p->next->blue * v1)) >> 16;
                  a = ((p->alpha * v2) + (p->next->alpha * v1)) >> 16;
                  pmap[i++] = (a << 24) | (r << 16) | (g << 8) | b;
               }
          }
        else
          {
             r = p->red;
             g = p->green;
             b = p->blue;
             a = p->alpha;
             pmap[i++] = (a << 24) | (r << 16) | (g << 8) | b;
          }
     }
   inc = ((ll - 1) << 16) / (len - 1);
   l = 0;
   for (i = 0; i < len; i++)
     {
        v = pmap[l >> 16];
        if ((l >> 16) < ll)
           vv = pmap[(l >> 16) + 1];
        else
           vv = pmap[(l >> 16)];
        v1 = l - ((l >> 16) << 16);
        v2 = 65536 - v1;
        b = ((v)) & 0xff;
        g = ((v) >> 8) & 0xff;
        r = ((v) >> 16) & 0xff;
        a = ((v) >> 24) & 0xff;
        bb = ((vv)) & 0xff;
        gg = ((vv) >> 8) & 0xff;
        rr = ((vv) >> 16) & 0xff;
        aa = ((vv) >> 24) & 0xff;
        r = ((r * v2) + (rr * v1)) >> 16;
        g = ((g * v2) + (gg * v1)) >> 16;
        b = ((b * v2) + (bb * v1)) >> 16;
        a = ((a * v2) + (aa * v1)) >> 16;
        map[i] = (a << 24) | (r << 16) | (g << 8) | b;
        l += inc;
     }
   free(pmap);
   return map;
}

DATA32             *
__imlib_MapHsvaRange(ImlibRange * rg, int len)
{
   ImlibRangeColor    *p;
   DATA32             *map, *pmap, k, kk;
   int                 r, g, b, a, rr, gg, bb, aa, i, l, ll, inc, j;
   float               h1, s1, v1, h2, s2, v2, h, s, v, k1, k2;

   if (!rg->color)
      return NULL;
   if (!rg->color->next)
      return NULL;
   ll = 1;
   for (p = rg->color; p; p = p->next)
      ll += p->distance;
   map = malloc(len * sizeof(DATA32));
   pmap = malloc(ll * sizeof(DATA32));
   i = 0;
   for (p = rg->color; p; p = p->next)
     {
        if (p->next)
          {
             for (j = 0; j < p->distance; j++)
               {
                  k1 = (j << 16) / (float)p->distance;
                  k2 = 65536 - k1;
                  r = p->red;
                  rr = p->next->red;
                  g = p->green;
                  gg = p->next->green;
                  b = p->blue;
                  bb = p->next->blue;
                  __imlib_rgb_to_hsv(r, g, b, &h1, &s1, &v1);
                  __imlib_rgb_to_hsv(rr, gg, bb, &h2, &s2, &v2);
                  h = ((h1 * k2) + (h2 * k1)) / 65536.0;
                  s = ((s1 * k2) + (s2 * k1)) / 65536.0;
                  v = ((v1 * k2) + (v2 * k1)) / 65536.0;
                  __imlib_hsv_to_rgb(h, s, v, &r, &g, &b);
                  a = (unsigned long int)((p->alpha * k2) +
                                          (p->next->alpha * k1)) >> 16;
                  pmap[i++] = (a << 24) | (r << 16) | (g << 8) | b;
               }
          }
        else
          {
             r = p->red;
             g = p->green;
             b = p->blue;
             a = p->alpha;
             pmap[i++] = (a << 24) | (r << 16) | (g << 8) | b;
          }
     }
   inc = ((ll - 1) << 16) / (len - 1);
   l = 0;
   for (i = 0; i < len; i++)
     {
        k = pmap[l >> 16];
        if ((l >> 16) < ll)
           kk = pmap[(l >> 16) + 1];
        else
           kk = pmap[(l >> 16)];
        k1 = l - (float)((l >> 16) << 16);
        k2 = 65536 - k1;
        b = ((k)) & 0xff;
        g = ((k) >> 8) & 0xff;
        r = ((k) >> 16) & 0xff;
        a = ((k) >> 24) & 0xff;
        bb = ((kk)) & 0xff;
        gg = ((kk) >> 8) & 0xff;
        rr = ((kk) >> 16) & 0xff;
        aa = ((kk) >> 24) & 0xff;
        __imlib_rgb_to_hsv(r, g, b, &h1, &s1, &v1);
        __imlib_rgb_to_hsv(rr, gg, bb, &h2, &s2, &v2);
        h = ((h1 * k2) + (h2 * k1)) / 65536.0;
        s = ((s1 * k2) + (s2 * k1)) / 65536.0;
        v = ((v1 * k2) + (v2 * k1)) / 65536.0;
        __imlib_hsv_to_rgb(h, s, v, &r, &g, &b);
        a = (unsigned long int)((a * k2) + (aa * k1)) >> 16;
        map[i] = (a << 24) | (r << 16) | (g << 8) | b;
        l += inc;
     }
   free(pmap);
   return map;
}

void
__imlib_DrawGradient(ImlibImage * im, int x, int y, int w, int h,
                     ImlibRange * rg, double angle, ImlibOp op,
                     int clx, int cly, int clw, int clh)
{
   DATA32             *map, *p;
   int                *hlut, *vlut, len = 0, xx, yy, xoff = 0, yoff =
      0, ww, hh, jump;
   int                 tmp, i, divw, divh;
   DATA8               r, g, b, a;

   ww = w;
   hh = h;
   if (x < 0)
     {
        w += x;
        xoff = -x;
        x = 0;
     }
   if (w <= 0)
      return;
   if ((x + w) > im->w)
      w = (im->w - x);
   if (w <= 0)
      return;
   if (y < 0)
     {
        h += y;
        yoff = -y;
        y = 0;
     }
   if (h <= 0)
      return;
   if ((y + h) > im->h)
      h = (im->h - y);
   if (h <= 0)
      return;
   if (clw)
     {
        int                 px, py;

        CLIP_TO(clx, cly, clw, clh, 0, 0, im->w, im->h);
        px = x;
        py = y;
        CLIP_TO(x, y, w, h, clx, cly, clw, clh);
        if ((w < 1) || (h < 1))
           return;
        xoff += (x - px);
        yoff += (y - py);
     }

   hlut = malloc(sizeof(int) * ww);
   vlut = malloc(sizeof(int) * hh);
   if (ww > hh)
      len = ww * 16;
   else
      len = hh * 16;
   map = __imlib_MapRange(rg, len);
   if (!map)
      return;

   xx = (int)(32 * sin(((angle + 180) * 2 * 3.141592654) / 360));
   yy = -(int)(32 * cos(((angle + 180) * 2 * 3.141592654) / 360));
   divw = ((ww - 1) << 5);
   divh = ((hh - 1) << 5);
   if (divw < 1)
      divw = 1;
   if (divh < 1)
      divh = 1;
   if (xx < 0)
     {
        for (i = 0; i < ww; i++)
           hlut[i] = (-xx * (ww - 1 - i) * len) / divw;
     }
   else
     {
        for (i = 0; i < ww; i++)
           hlut[i] = (xx * i * len) / divw;
     }
   if (yy < 0)
     {
        for (i = 0; i < hh; i++)
           vlut[i] = (-yy * (hh - 1 - i) * len) / divh;
     }
   else
     {
        for (i = 0; i < hh; i++)
           vlut[i] = (yy * i * len) / divh;
     }
   jump = im->w - w;

   p = im->data + (y * im->w) + x;
   switch (op)
     {
     case OP_COPY:
        if (IMAGE_HAS_ALPHA(im))
          {
             __imlib_build_pow_lut();
             for (yy = 0; yy < h; yy++)
               {
                  for (xx = 0; xx < w; xx++)
                    {
                       i = vlut[yoff + yy] + hlut[xoff + xx];
                       if (i < 0)
                          i = 0;
                       else if (i >= len)
                          i = len - 1;
                       READ_RGBA(&(map[i]), r, g, b, a);
                       BLEND_DST_ALPHA(r, g, b, a, p);
                       p++;
                    }
                  p += jump;
               }
          }
        else
          {
             for (yy = 0; yy < h; yy++)
               {
                  for (xx = 0; xx < w; xx++)
                    {
                       i = vlut[yoff + yy] + hlut[xoff + xx];
                       if (i < 0)
                          i = 0;
                       else if (i >= len)
                          i = len - 1;
                       READ_RGBA(&(map[i]), r, g, b, a);
                       BLEND(r, g, b, a, p);
                       p++;
                    }
                  p += jump;
               }
          }
        break;
     case OP_ADD:
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  i = vlut[yoff + yy] + hlut[xoff + xx];
                  if (i < 0)
                     i = 0;
                  else if (i >= len)
                     i = len - 1;
                  READ_RGBA(&(map[i]), r, g, b, a);
                  BLEND_SUB(r, g, b, a, p);
                  p++;
               }
             p += jump;
          }
        break;
     case OP_SUBTRACT:
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  i = vlut[yoff + yy] + hlut[xoff + xx];
                  if (i < 0)
                     i = 0;
                  else if (i >= len)
                     i = len - 1;
                  READ_RGBA(&(map[i]), r, g, b, a);
                  BLEND_SUB(r, g, b, a, p);
                  p++;
               }
             p += jump;
          }
        break;
     case OP_RESHADE:
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  i = vlut[yoff + yy] + hlut[xoff + xx];
                  if (i < 0)
                     i = 0;
                  else if (i >= len)
                     i = len - 1;
                  READ_RGBA(&(map[i]), r, g, b, a);
                  BLEND_RE(r, g, b, a, p);
                  p++;
               }
             p += jump;
          }
        break;
     default:
        break;
     }

   free(vlut);
   free(hlut);
   free(map);
}

void
__imlib_DrawHsvaGradient(ImlibImage * im, int x, int y, int w, int h,
                         ImlibRange * rg, double angle, ImlibOp op,
                         int clx, int cly, int clw, int clh)
{
   DATA32             *map, *p;
   int                *hlut, *vlut, len = 0, xx, yy, xoff = 0, yoff =
      0, ww, hh, jump;
   int                 tmp, i, divw, divh;
   DATA8               r, g, b, a;

   ww = w;
   hh = h;
   if (x < 0)
     {
        w += x;
        xoff = -x;
        x = 0;
     }
   if (w <= 0)
      return;
   if ((x + w) > im->w)
      w = (im->w - x);
   if (w <= 0)
      return;
   if (y < 0)
     {
        h += y;
        yoff = -y;
        y = 0;
     }
   if (h <= 0)
      return;
   if ((y + h) > im->h)
      h = (im->h - y);
   if (h <= 0)
      return;
   if (clw)
     {
        int                 px, py;

        CLIP_TO(clx, cly, clw, clh, 0, 0, im->w, im->h);
        px = x;
        py = y;
        CLIP_TO(x, y, w, h, clx, cly, clw, clh);
        if ((w < 1) || (h < 1))
           return;
        xoff += (x - px);
        yoff += (y - py);
     }

   hlut = malloc(sizeof(int) * ww);
   vlut = malloc(sizeof(int) * hh);
   if (ww > hh)
      len = ww * 16;
   else
      len = hh * 16;
   map = __imlib_MapHsvaRange(rg, len);
   if (!map)
      return;

   xx = (int)(32 * sin(((angle + 180) * 2 * 3.141592654) / 360));
   yy = -(int)(32 * cos(((angle + 180) * 2 * 3.141592654) / 360));
   divw = ((ww - 1) << 5);
   divh = ((hh - 1) << 5);
   if (divw < 1)
      divw = 1;
   if (divh < 1)
      divh = 1;
   if (xx < 0)
     {
        for (i = 0; i < ww; i++)
           hlut[i] = (-xx * (ww - 1 - i) * len) / divw;
     }
   else
     {
        for (i = 0; i < ww; i++)
           hlut[i] = (xx * i * len) / divw;
     }
   if (yy < 0)
     {
        for (i = 0; i < hh; i++)
           vlut[i] = (-yy * (hh - 1 - i) * len) / divh;
     }
   else
     {
        for (i = 0; i < hh; i++)
           vlut[i] = (yy * i * len) / divh;
     }
   jump = im->w - w;

   p = im->data + (y * im->w) + x;
   switch (op)
     {
     case OP_COPY:
        if (IMAGE_HAS_ALPHA(im))
          {
             __imlib_build_pow_lut();
             for (yy = 0; yy < h; yy++)
               {
                  for (xx = 0; xx < w; xx++)
                    {
                       i = vlut[yoff + yy] + hlut[xoff + xx];
                       if (i < 0)
                          i = 0;
                       else if (i >= len)
                          i = len - 1;
                       READ_RGBA(&(map[i]), r, g, b, a);
                       BLEND_DST_ALPHA(r, g, b, a, p);
                       p++;
                    }
                  p += jump;
               }
          }
        else
          {
             for (yy = 0; yy < h; yy++)
               {
                  for (xx = 0; xx < w; xx++)
                    {
                       i = vlut[yoff + yy] + hlut[xoff + xx];
                       if (i < 0)
                          i = 0;
                       else if (i >= len)
                          i = len - 1;
                       READ_RGBA(&(map[i]), r, g, b, a);
                       BLEND(r, g, b, a, p);
                       p++;
                    }
                  p += jump;
               }
          }
        break;
     case OP_ADD:
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  i = vlut[yoff + yy] + hlut[xoff + xx];
                  if (i < 0)
                     i = 0;
                  else if (i >= len)
                     i = len - 1;
                  READ_RGBA(&(map[i]), r, g, b, a);
                  BLEND_SUB(r, g, b, a, p);
                  p++;
               }
             p += jump;
          }
        break;
     case OP_SUBTRACT:
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  i = vlut[yoff + yy] + hlut[xoff + xx];
                  if (i < 0)
                     i = 0;
                  else if (i >= len)
                     i = len - 1;
                  READ_RGBA(&(map[i]), r, g, b, a);
                  BLEND_SUB(r, g, b, a, p);
                  p++;
               }
             p += jump;
          }
        break;
     case OP_RESHADE:
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  i = vlut[yoff + yy] + hlut[xoff + xx];
                  if (i < 0)
                     i = 0;
                  else if (i >= len)
                     i = len - 1;
                  READ_RGBA(&(map[i]), r, g, b, a);
                  BLEND_RE(r, g, b, a, p);
                  p++;
               }
             p += jump;
          }
        break;
     default:
        break;
     }

   free(vlut);
   free(hlut);
   free(map);
}
