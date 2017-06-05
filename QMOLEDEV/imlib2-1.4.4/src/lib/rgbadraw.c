#include "common.h"
#include <math.h>
#include "colormod.h"
#include "image.h"
#include "scale.h"
#include "blend.h"
#include "updates.h"
#include "rgbadraw.h"

void
__imlib_FlipImageHoriz(ImlibImage * im)
{
   DATA32             *p1, *p2, tmp;
   int                 x, y;

   for (y = 0; y < im->h; y++)
     {
        p1 = im->data + (y * im->w);
        p2 = im->data + ((y + 1) * im->w) - 1;
        for (x = 0; x < (im->w >> 1); x++)
          {
             tmp = *p1;
             *p1 = *p2;
             *p2 = tmp;
             p1++;
             p2--;
          }
     }
   x = im->border.left;
   im->border.left = im->border.right;
   im->border.right = x;
}

void
__imlib_FlipImageVert(ImlibImage * im)
{
   DATA32             *p1, *p2, tmp;
   int                 x, y;

   for (y = 0; y < (im->h >> 1); y++)
     {
        p1 = im->data + (y * im->w);
        p2 = im->data + ((im->h - 1 - y) * im->w);
        for (x = 0; x < im->w; x++)
          {
             tmp = *p1;
             *p1 = *p2;
             *p2 = tmp;
             p1++;
             p2++;
          }
     }
   x = im->border.top;
   im->border.top = im->border.bottom;
   im->border.bottom = x;
}

void
__imlib_FlipImageBoth(ImlibImage * im)
{
   DATA32             *p1, *p2, tmp;
   int                 x;

   p1 = im->data;
   p2 = im->data + (im->h * im->w) - 1;
   for (x = (im->w * im->h) / 2; --x >= 0;)
     {
        tmp = *p1;
        *p1 = *p2;
        *p2 = tmp;
        p1++;
        p2--;
     }
   x = im->border.top;
   im->border.top = im->border.bottom;
   im->border.bottom = x;
   x = im->border.left;
   im->border.left = im->border.right;
   im->border.right = x;
}

/*\ Directions (source is right/down):
|*| 0 = down/right (flip over ul-dr diagonal)
|*| 1 = down/left  (rotate 90 degrees clockwise)
|*| 2 = up/right   (rotate 90 degrees counterclockwise)
|*| 3 = up/left    (flip over ur-ll diagonal)
\*/
void
__imlib_FlipImageDiagonal(ImlibImage * im, int direction)
{
   DATA32             *data, *to, *from;
   int                 x, y, w, hw, tmp;

   data = malloc(im->w * im->h * sizeof(DATA32));
   from = im->data;
   w = im->h;
   im->h = im->w;
   im->w = w;
   hw = w * im->h;
   switch (direction)
     {
     default:
     case 0:                   /*\ DOWN_RIGHT \ */
        tmp = im->border.top;
        im->border.top = im->border.left;
        im->border.left = tmp;
        tmp = im->border.bottom;
        im->border.bottom = im->border.right;
        im->border.right = tmp;
        to = data;
        hw = -hw + 1;
        break;
     case 1:                   /*\ DOWN_LEFT \ */
        tmp = im->border.top;
        im->border.top = im->border.left;
        im->border.left = im->border.bottom;
        im->border.bottom = im->border.right;
        im->border.right = tmp;
        to = data + w - 1;
        hw = -hw - 1;
        break;
     case 2:                   /*\ UP_RIGHT \ */
        tmp = im->border.top;
        im->border.top = im->border.right;
        im->border.right = im->border.bottom;
        im->border.bottom = im->border.left;
        im->border.left = tmp;
        to = data + hw - w;
        w = -w;
        hw = hw + 1;
        break;
     case 3:                   /*\ UP_LEFT \ */
        tmp = im->border.top;
        im->border.top = im->border.right;
        im->border.right = tmp;
        tmp = im->border.bottom;
        im->border.bottom = im->border.left;
        im->border.left = tmp;
        to = data + hw - 1;
        w = -w;
        hw = hw - 1;
        break;
     }
   from = im->data;
   for (x = im->w; --x >= 0;)
     {
        for (y = im->h; --y >= 0;)
          {
             *to = *from;
             from++;
             to += w;
          }
        to += hw;
     }
   free(im->data);
   im->data = data;
}

void
__imlib_BlurImage(ImlibImage * im, int rad)
{
   DATA32             *p1, *p2, *data;
   int                 x, y, mx, my, mw, mh, mt, xx, yy;
   int                 a, r, g, b;
   int                *as, *rs, *gs, *bs;

   if (rad < 1)
      return;
   data = malloc(im->w * im->h * sizeof(DATA32));
   as = malloc(sizeof(int) * im->w);
   rs = malloc(sizeof(int) * im->w);
   gs = malloc(sizeof(int) * im->w);
   bs = malloc(sizeof(int) * im->w);

   for (y = 0; y < im->h; y++)
     {
        my = y - rad;
        mh = (rad << 1) + 1;
        if (my < 0)
          {
             mh += my;
             my = 0;
          }
        if ((my + mh) > im->h)
           mh = im->h - my;

        p1 = data + (y * im->w);
        memset(as, 0, im->w * sizeof(int));
        memset(rs, 0, im->w * sizeof(int));
        memset(gs, 0, im->w * sizeof(int));
        memset(bs, 0, im->w * sizeof(int));

        for (yy = 0; yy < mh; yy++)
          {
             p2 = im->data + ((yy + my) * im->w);
             for (x = 0; x < im->w; x++)
               {
                  as[x] += (*p2 >> 24) & 0xff;
                  rs[x] += (*p2 >> 16) & 0xff;
                  gs[x] += (*p2 >> 8) & 0xff;
                  bs[x] += *p2 & 0xff;
                  p2++;
               }
          }
        if (im->w > ((rad << 1) + 1))
          {
             for (x = 0; x < im->w; x++)
               {
                  a = 0;
                  r = 0;
                  g = 0;
                  b = 0;
                  mx = x - rad;
                  mw = (rad << 1) + 1;
                  if (mx < 0)
                    {
                       mw += mx;
                       mx = 0;
                    }
                  if ((mx + mw) > im->w)
                     mw = im->w - mx;
                  mt = mw * mh;
                  for (xx = mx; xx < (mw + mx); xx++)
                    {
                       a += as[xx];
                       r += rs[xx];
                       g += gs[xx];
                       b += bs[xx];
                    }
                  a = a / mt;
                  r = r / mt;
                  g = g / mt;
                  b = b / mt;
                  *p1 = (a << 24) | (r << 16) | (g << 8) | b;
                  p1++;
               }
          }
        else
          {
          }
     }
   free(as);
   free(rs);
   free(gs);
   free(bs);
   free(im->data);
   im->data = data;
}

void
__imlib_SharpenImage(ImlibImage * im, int rad)
{
   DATA32             *data, *p1, *p2;
   int                 a, r, g, b, x, y;

   data = malloc(im->w * im->h * sizeof(DATA32));
   if (rad == 0)
      return;
   else
     {
        int                 mul, mul2, tot;

        mul = (rad * 4) + 1;
        mul2 = rad;
        tot = mul - (mul2 * 4);
        for (y = 1; y < (im->h - 1); y++)
          {
             p1 = im->data + 1 + (y * im->w);
             p2 = data + 1 + (y * im->w);
             for (x = 1; x < (im->w - 1); x++)
               {
                  b = (int)((p1[0]) & 0xff) * 5;
                  g = (int)((p1[0] >> 8) & 0xff) * 5;
                  r = (int)((p1[0] >> 16) & 0xff) * 5;
                  a = (int)((p1[0] >> 24) & 0xff) * 5;
                  b -= (int)((p1[-1]) & 0xff);
                  g -= (int)((p1[-1] >> 8) & 0xff);
                  r -= (int)((p1[-1] >> 16) & 0xff);
                  a -= (int)((p1[-1] >> 24) & 0xff);
                  b -= (int)((p1[1]) & 0xff);
                  g -= (int)((p1[1] >> 8) & 0xff);
                  r -= (int)((p1[1] >> 16) & 0xff);
                  a -= (int)((p1[1] >> 24) & 0xff);
                  b -= (int)((p1[-im->w]) & 0xff);
                  g -= (int)((p1[-im->w] >> 8) & 0xff);
                  r -= (int)((p1[-im->w] >> 16) & 0xff);
                  a -= (int)((p1[-im->w] >> 24) & 0xff);
                  b -= (int)((p1[im->w]) & 0xff);
                  g -= (int)((p1[im->w] >> 8) & 0xff);
                  r -= (int)((p1[im->w] >> 16) & 0xff);
                  a -= (int)((p1[im->w] >> 24) & 0xff);

                  a = (a & ((~a) >> 16));
                  a = ((a | ((a & 256) - ((a & 256) >> 8))));
                  r = (r & ((~r) >> 16));
                  r = ((r | ((r & 256) - ((r & 256) >> 8))));
                  g = (g & ((~g) >> 16));
                  g = ((g | ((g & 256) - ((g & 256) >> 8))));
                  b = (b & ((~b) >> 16));
                  b = ((b | ((b & 256) - ((b & 256) >> 8))));

                  *p2 = (a << 24) | (r << 16) | (g << 8) | b;
                  p2++;
                  p1++;
               }
          }
     }
   free(im->data);
   im->data = data;
}

void
__imlib_TileImageHoriz(ImlibImage * im)
{
   DATA32             *p1, *p2, *p3, *p, *data;
   int                 x, y, per, tmp, na, nr, ng, nb, mix, a, r, g, b, aa, rr,
      gg, bb;

   data = malloc(im->w * im->h * sizeof(DATA32));
   p1 = im->data;
   p = data;
   for (y = 0; y < im->h; y++)
     {
        p2 = p1 + (im->w >> 1);
        p3 = p1;
        per = (im->w >> 1);
        for (x = 0; x < (im->w >> 1); x++)
          {
             mix = (x * 255) / per;
             b = (*p1) & 0xff;
             g = (*p1 >> 8) & 0xff;
             r = (*p1 >> 16) & 0xff;
             a = (*p1 >> 24) & 0xff;

             bb = (*p2) & 0xff;
             gg = (*p2 >> 8) & 0xff;
             rr = (*p2 >> 16) & 0xff;
             aa = (*p2 >> 24) & 0xff;

             tmp = (r - rr) * mix;
             nr = rr + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (g - gg) * mix;
             ng = gg + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (b - bb) * mix;
             nb = bb + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (a - aa) * mix;
             na = aa + ((tmp + (tmp >> 8) + 0x80) >> 8);
             *p = (na << 24) | (nr << 16) | (ng << 8) | nb;
             p++;
             p1++;
             p2++;
          }
        p2 = p3;
        per = (im->w - (im->w >> 1));
        for (; x < im->w; x++)
          {
             mix = ((im->w - 1 - x) * 255) / per;
             b = (*p1) & 0xff;
             g = (*p1 >> 8) & 0xff;
             r = (*p1 >> 16) & 0xff;
             a = (*p1 >> 24) & 0xff;

             bb = (*p2) & 0xff;
             gg = (*p2 >> 8) & 0xff;
             rr = (*p2 >> 16) & 0xff;
             aa = (*p2 >> 24) & 0xff;

             tmp = (r - rr) * mix;
             nr = rr + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (g - gg) * mix;
             ng = gg + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (b - bb) * mix;
             nb = bb + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (a - aa) * mix;
             na = aa + ((tmp + (tmp >> 8) + 0x80) >> 8);
             *p = (na << 24) | (nr << 16) | (ng << 8) | nb;
             p++;
             p1++;
             p2++;
          }
     }
   free(im->data);
   im->data = data;
}

void
__imlib_TileImageVert(ImlibImage * im)
{
   DATA32             *p1, *p2, *p, *data;
   int                 x, y, tmp, na, nr, ng, nb, mix, a, r, g, b, aa, rr, gg,
      bb;

   data = malloc(im->w * im->h * sizeof(DATA32));
   p = data;
   for (y = 0; y < im->h; y++)
     {
        p1 = im->data + (y * im->w);
        if (y < (im->h >> 1))
          {
             p2 = im->data + ((y + (im->h >> 1)) * im->w);
             mix = (y * 255) / (im->h >> 1);
          }
        else
          {
             p2 = im->data + ((y - (im->h >> 1)) * im->w);
             mix = ((im->h - y) * 255) / (im->h - (im->h >> 1));
          }
        for (x = 0; x < im->w; x++)
          {
             b = (*p1) & 0xff;
             g = (*p1 >> 8) & 0xff;
             r = (*p1 >> 16) & 0xff;
             a = (*p1 >> 24) & 0xff;

             bb = (*p2) & 0xff;
             gg = (*p2 >> 8) & 0xff;
             rr = (*p2 >> 16) & 0xff;
             aa = (*p2 >> 24) & 0xff;

             tmp = (r - rr) * mix;
             nr = rr + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (g - gg) * mix;
             ng = gg + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (b - bb) * mix;
             nb = bb + ((tmp + (tmp >> 8) + 0x80) >> 8);
             tmp = (a - aa) * mix;
             na = aa + ((tmp + (tmp >> 8) + 0x80) >> 8);
             *p = (na << 24) | (nr << 16) | (ng << 8) | nb;
             p++;
             p1++;
             p2++;
          }
     }
   free(im->data);
   im->data = data;
}

void
__imlib_copy_image_data(ImlibImage * im, int x, int y, int w, int h, int nx,
                        int ny)
{
   int                 xx, yy, jump;
   DATA32             *p1, *p2;

   /* clip horizontal co-ordinates so that both dest and src fit inside */
   /* the image */
   if (x < 0)
     {
        w += x;
        nx -= x;
        x = 0;
     }
   if (w <= 0)
      return;
   if (nx < 0)
     {
        w += nx;
        x -= nx;
        nx = 0;
     }
   if (w <= 0)
      return;
   if ((x + w) > im->w)
      w = (im->w - x);
   if (w <= 0)
      return;
   if ((nx + w) > im->w)
      w = (im->w - nx);
   if (w <= 0)
      return;
   /* clip vertical co-ordinates so that both dest and src fit inside */
   /* the image */
   if (y < 0)
     {
        h += y;
        ny -= y;
        y = 0;
     }
   if (h <= 0)
      return;
   if (ny < 0)
     {
        h += ny;
        y -= ny;
        ny = 0;
     }
   if (h <= 0)
      return;
   if ((y + h) > im->h)
      h = (im->h - y);
   if (h <= 0)
      return;
   if ((ny + h) > im->h)
      h = (im->h - ny);
   if (h <= 0)
      return;

   /* figure out what our source and destnation start pointers are */
   p1 = im->data + (y * im->w) + x;
   p2 = im->data + (ny * im->w) + nx;
   /* the pointer jump between lines */
   jump = (im->w - w);
   /* dest < src address - we can copy forwards */
   if (p2 < p1)
     {
        /* work our way thru the array */
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  *p2 = *p1;
                  p1++;
                  p2++;
               }
             p1 += jump;
             p2 += jump;
          }
     }
   /* dst > src - we must copy backwards */
   else
     {
        /* new pointers to start working at (bottom-right of rect) */
        p1 = im->data + ((y + h - 1) * im->w) + x + w - 1;
        p2 = im->data + ((ny + h - 1) * im->w) + nx + w - 1;
        /* work our way thru the array */
        for (yy = 0; yy < h; yy++)
          {
             for (xx = 0; xx < w; xx++)
               {
                  *p2 = *p1;
                  p1--;
                  p2--;
               }
             p1 -= jump;
             p2 -= jump;
          }
     }
}

void
__imlib_copy_alpha_data(ImlibImage * src, ImlibImage * dst, int x, int y,
                        int w, int h, int nx, int ny)
{
   int                 xx, yy, jump, jump2;
   DATA32             *p1, *p2;

   /* clip horizontal co-ordinates so that both dest and src fit inside */
   /* the image */
   if (x < 0)
     {
        w += x;
        nx -= x;
        x = 0;
     }
   if (w <= 0)
      return;
   if (nx < 0)
     {
        w += nx;
        x -= nx;
        nx = 0;
     }
   if (w <= 0)
      return;
   if ((x + w) > src->w)
      w = (src->w - x);
   if (w <= 0)
      return;
   if ((nx + w) > dst->w)
      w = (dst->w - nx);
   if (w <= 0)
      return;
   /* clip vertical co-ordinates so that both dest and src fit inside */
   /* the image */
   if (y < 0)
     {
        h += y;
        ny -= y;
        y = 0;
     }
   if (h <= 0)
      return;
   if (ny < 0)
     {
        h += ny;
        y -= ny;
        ny = 0;
     }
   if (h <= 0)
      return;
   if ((y + h) > src->h)
      h = (src->h - y);
   if (h <= 0)
      return;
   if ((ny + h) > dst->h)
      h = (dst->h - ny);
   if (h <= 0)
      return;

   /* figure out what our source and destnation start pointers are */
   p1 = src->data + (y * src->w) + x;
   p2 = dst->data + (ny * dst->w) + nx;
   /* the pointer jump between lines */
   jump = (src->w - w);
   jump2 = (dst->w - w);

    /* work our way thru the array */
    for (yy = 0; yy < h; yy++)
      {
         for (xx = 0; xx < w; xx++)
           {
              *p2 = (*p1 & 0xff000000) | (*p2 & 0x00ffffff);
              p1++;
              p2++;
           }
         p1 += jump;
         p2 += jump2;
      }
}
