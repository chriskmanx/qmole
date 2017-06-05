#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "filter.h"

/*\ Create and return an empty filter struct \*/
ImlibFilter        *
__imlib_CreateFilter(int size)
{
   ImlibFilter        *fil;

   fil = malloc(sizeof(ImlibFilter));
   if (size > 0)
     {
        fil->alpha.pixels = malloc(size * sizeof(ImlibFilterPixel));
        fil->red.pixels = malloc(size * sizeof(ImlibFilterPixel));
        fil->green.pixels = malloc(size * sizeof(ImlibFilterPixel));
        fil->blue.pixels = malloc(size * sizeof(ImlibFilterPixel));
        fil->alpha.size = size;
        fil->red.size = size;
        fil->green.size = size;
        fil->blue.size = size;
     }
   else
     {
        fil->alpha.pixels = 0;
        fil->red.pixels = 0;
        fil->green.pixels = 0;
        fil->blue.pixels = 0;
        fil->alpha.size = 0;
        fil->red.size = 0;
        fil->green.size = 0;
        fil->blue.size = 0;
     }
   fil->alpha.entries = 0;
   fil->red.entries = 0;
   fil->green.entries = 0;
   fil->blue.entries = 0;
   fil->alpha.div = 0;
   fil->red.div = 0;
   fil->green.div = 0;
   fil->blue.div = 0;
   fil->alpha.cons = 0;
   fil->red.cons = 0;
   fil->green.cons = 0;
   fil->blue.cons = 0;
   return fil;
}

/*\ Free a filter struct \*/
void
__imlib_FreeFilter(ImlibFilter * fil)
{
   free(fil->alpha.pixels);
   free(fil->red.pixels);
   free(fil->green.pixels);
   free(fil->blue.pixels);
   free(fil);
}

void
__imlib_FilterSetColor(ImlibFilterColor * fil, int x, int y,
                       int a, int r, int g, int b)
{
   int                 i;
   ImlibFilterPixel   *pix = fil->pixels;

   /*\ Look for an entry matching (x, y) \ */
   for (i = fil->entries; --i >= 0;)
     {
        if ((pix[i].xoff == x) && (pix[i].yoff == y))
           break;
     }
   /*\ If all zero, remove the found entry \ */
   if (!(a | r | g | b))
     {
        if (i >= 0)
          {
             while (i < fil->entries)
               {
                  pix[i] = pix[i + 1];
               }
             fil->entries--;
          }
        return;
     }
   /*\ No match, then make a new entry \ */
   if (i < 0)
      i = fil->entries;
   if (i >= fil->size)
     {
        fil->size += 4;
        pix = realloc(pix, (fil->size * sizeof(ImlibFilterPixel)));
        if (!pix)
           return;
        fil->pixels = pix;
     }
   if (i >= fil->entries)
      fil->entries = i + 1;
   pix[i].xoff = x;
   pix[i].yoff = y;
   pix[i].a = a;
   pix[i].r = r;
   pix[i].g = g;
   pix[i].b = b;
}

/*\ Set the divisors manually \*/
void
__imlib_FilterDivisors(ImlibFilter * fil, int a, int r, int g, int b)
{
   fil->alpha.div = a;
   fil->red.div = r;
   fil->green.div = g;
   fil->blue.div = b;
}

/*\ Set the constants \*/
void
__imlib_FilterConstants(ImlibFilter * fil, int a, int r, int g, int b)
{
   fil->alpha.cons = a;
   fil->red.cons = r;
   fil->green.cons = g;
   fil->blue.cons = b;
}

static int
__imlib_FilterCalcDiv(ImlibFilterColor * fil)
{
   int                 i, ret;
   ImlibFilterPixel   *pix;

   if (fil->div)
      return fil->div;
   ret = 0;
   pix = fil->pixels;
   for (i = fil->entries; --i >= 0;)
     {
        ret += pix->a + pix->r + pix->g + pix->b;
        pix++;
     }
   return ret;
}

static int
__imlib_FilterGet(ImlibFilterColor * fil, DATA32 * data,
                  int w, int h, int x, int y)
{
   int                 i, off, ret;
   ImlibFilterPixel   *pix;
   DATA32             *p;

   ret = fil->cons;
   pix = fil->pixels;
   for (i = fil->entries; --i >= 0;)
     {
        off = x + pix->xoff;
        if (off < 0)
           off = 0;
        if (off >= w)
           off = w - 1;
        p = data + off;
        off = y + pix->yoff;
        if (off < 0)
           off = 0;
        if (off >= h)
           off = h - 1;
        p += off * w;
        ret += A_VAL(p) * pix->a + R_VAL(p) * pix->r +
           G_VAL(p) * pix->g + B_VAL(p) * pix->b;
        pix++;
     }
   return ret;
}

/*\ Correct saturation from [-32768, 32767] to [0, 255] \*/
#define SATURATE(x) ((((x) | (!((x) >> 8) - 1)) & (~((x) >> 31))) & 0xff)

/*\ Filter an image with the a, r, g, b filters in fil
|*|  NB: This is currently not very optimal, and could probably be improved
\*/
void
__imlib_FilterImage(ImlibImage * im, ImlibFilter * fil)
{
   int                 x, y, a, r, g, b, ad, rd, gd, bd;
   DATA32             *data, *p1, *p2;

   data = malloc(im->w * im->h * sizeof(DATA32));
   if (!data)
      return;

   ad = __imlib_FilterCalcDiv(&fil->alpha);
   rd = __imlib_FilterCalcDiv(&fil->red);
   gd = __imlib_FilterCalcDiv(&fil->green);
   bd = __imlib_FilterCalcDiv(&fil->blue);

   p1 = im->data;
   p2 = data;

   for (y = 0; y < im->h; y++)
     {
        for (x = 0; x < im->w; x++)
          {
             *p2 = *p1;
             if (ad)
               {
                  a = __imlib_FilterGet(&fil->alpha, im->data, im->w, im->h, x,
                                        y);
                  a /= ad;
                  A_VAL(p2) = SATURATE(a);
               }
             if (rd)
               {
                  r = __imlib_FilterGet(&fil->red, im->data, im->w, im->h, x,
                                        y);
                  r /= rd;
                  R_VAL(p2) = SATURATE(r);
               }
             if (gd)
               {
                  g = __imlib_FilterGet(&fil->green, im->data, im->w, im->h, x,
                                        y);
                  g /= gd;
                  G_VAL(p2) = SATURATE(g);
               }
             if (bd)
               {
                  b = __imlib_FilterGet(&fil->blue, im->data, im->w, im->h, x,
                                        y);
                  b /= bd;
                  B_VAL(p2) = SATURATE(b);
               }
             p1++;
             p2++;
          }
     }
   free(im->data);
   im->data = data;
}
