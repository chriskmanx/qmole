#include "common.h"
#ifdef BUILD_X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "color.h"

DATA8               _pal_type = 0;
DATA16              _max_colors = 256;

int
__imlib_XActualDepth(Display * d, Visual * v)
{
   XVisualInfo         xvi, *xvir;
   int                 depth = 0, num;

   xvi.visual = v;
   xvi.visualid = XVisualIDFromVisual(v);
   xvir = XGetVisualInfo(d, VisualIDMask, &xvi, &num);
   if (xvir)
     {
        depth = xvir[0].depth;
        if ((depth == 16) &&
            ((xvir->red_mask | xvir->green_mask | xvir->blue_mask) == 0x7fff))
           depth = 15;
        XFree(xvir);
     }
   return depth;
}

Visual             *
__imlib_BestVisual(Display * d, int screen, int *depth_return)
{
   XVisualInfo         xvi, *xvir;
   int                 j, i, num, maxd = 0;
   Visual             *v = NULL;
   const int           visprefs[] = {
      PseudoColor, TrueColor, DirectColor, StaticColor, GrayScale, StaticGray
   };

   xvi.screen = screen;
   maxd = 0;
   for (j = 0; j < 6; j++)
     {
        xvi.class = visprefs[j];
        xvir = XGetVisualInfo(d, VisualScreenMask | VisualClassMask,
                              &xvi, &num);
        if (xvir)
          {
             for (i = 0; i < num; i++)
               {
                  if ((xvir[i].depth > 1) &&
                      (xvir[i].depth >= maxd) && (xvi.class == PseudoColor))
                    {
                       maxd = xvir[i].depth;
                       v = xvir[i].visual;
                    }
                  else if ((xvir[i].depth > maxd) && (xvir[i].depth <= 24))
                    {
                       maxd = xvir[i].depth;
                       v = xvir[i].visual;
                    }
               }
             XFree(xvir);
          }
     }
   if (depth_return)
      *depth_return = maxd;
   return v;
}

DATA8              *
__imlib_AllocColorTable(Display * d, Colormap cmap, DATA8 * type_return,
                        Visual * v)
{
   DATA8              *color_lut = NULL;

   if (v->bits_per_rgb > 1)
     {
        if ((_max_colors >= 256)
            && (color_lut = __imlib_AllocColors332(d, cmap, v)))
          {
             *type_return = _pal_type;
             return color_lut;
          }
        if ((_max_colors >= 216)
            && (color_lut = __imlib_AllocColors666(d, cmap, v)))
          {
             *type_return = _pal_type;
             return color_lut;
          }
        if ((_max_colors >= 128)
            && (color_lut = __imlib_AllocColors232(d, cmap, v)))
          {
             *type_return = _pal_type;
             return color_lut;
          }
        if ((_max_colors >= 64)
            && (color_lut = __imlib_AllocColors222(d, cmap, v)))
          {
             *type_return = _pal_type;
             return color_lut;
          }
        if ((_max_colors >= 32)
            && (color_lut = __imlib_AllocColors221(d, cmap, v)))
          {
             *type_return = _pal_type;
             return color_lut;
          }
        if ((_max_colors >= 16)
            && (color_lut = __imlib_AllocColors121(d, cmap, v)))
          {
             *type_return = _pal_type;
             return color_lut;
          }
     }
   if ((_max_colors >= 8) && (color_lut = __imlib_AllocColors111(d, cmap, v)))
     {
        *type_return = _pal_type;
        return color_lut;
     }
   color_lut = __imlib_AllocColors1(d, cmap, v);
   *type_return = _pal_type;
   return color_lut;
}

DATA8              *
__imlib_AllocColors332(Display * d, Colormap cmap, Visual * v)
{
   int                 r, g, b, i;
   DATA8              *color_lut;
   int                 sig_mask = 0;

   for (i = 0; i < v->bits_per_rgb; i++)
      sig_mask |= (0x1 << i);
   sig_mask <<= (16 - v->bits_per_rgb);
   i = 0;
   color_lut = malloc(256 * sizeof(DATA8));
   for (r = 0; r < 8; r++)
     {
        for (g = 0; g < 8; g++)
          {
             for (b = 0; b < 4; b++)
               {
                  XColor              xcl;
                  XColor              xcl_in;
                  int                 val;
                  Status              ret;

                  val = (r << 6) | (r << 3) | (r);
                  xcl.red = (unsigned short)((val << 7) | (val >> 2));
                  val = (g << 6) | (g << 3) | (g);
                  xcl.green = (unsigned short)((val << 7) | (val >> 2));
                  val = (b << 6) | (b << 4) | (b << 2) | (b);
                  xcl.blue = (unsigned short)((val << 8) | (val));
                  xcl_in = xcl;
                  ret = XAllocColor(d, cmap, &xcl);
                  if ((ret == Success) ||
                      ((xcl_in.red & sig_mask) != (xcl.red & sig_mask)) ||
                      ((xcl_in.green & sig_mask) != (xcl.green & sig_mask)) ||
                      ((xcl_in.blue & sig_mask) != (xcl.blue & sig_mask)))
                    {
                       unsigned long       pixels[256];
                       int                 j;

                       if (i > 0)
                         {
                            for (j = 0; j < i; j++)
                               pixels[j] = (unsigned long)color_lut[j];
                            XFreeColors(d, cmap, pixels, i, 0);
                         }
                       free(color_lut);
                       return NULL;
                    }
                  color_lut[i] = xcl.pixel;
                  i++;
               }
          }
     }
   _pal_type = 0;
   return color_lut;
}

DATA8              *
__imlib_AllocColors666(Display * d, Colormap cmap, Visual * v)
{
   int                 r, g, b, i;
   DATA8              *color_lut;
   int                 sig_mask = 0;

   for (i = 0; i < v->bits_per_rgb; i++)
      sig_mask |= (0x1 << i);
   sig_mask <<= (16 - v->bits_per_rgb);
   i = 0;
   color_lut = malloc(256 * sizeof(DATA8));
   for (r = 0; r < 6; r++)
     {
        for (g = 0; g < 6; g++)
          {
             for (b = 0; b < 6; b++)
               {
                  XColor              xcl;
                  XColor              xcl_in;
                  int                 val;
                  Status              ret;

                  val = (int)((((double)r) / 5.0) * 65535);
                  xcl.red = (unsigned short)(val);
                  val = (int)((((double)g) / 5.0) * 65535);
                  xcl.green = (unsigned short)(val);
                  val = (int)((((double)b) / 5.0) * 65535);
                  xcl.blue = (unsigned short)(val);
                  xcl_in = xcl;
                  ret = XAllocColor(d, cmap, &xcl);
                  if ((ret == Success) ||
                      ((xcl_in.red & sig_mask) != (xcl.red & sig_mask)) ||
                      ((xcl_in.green & sig_mask) != (xcl.green & sig_mask)) ||
                      ((xcl_in.blue & sig_mask) != (xcl.blue & sig_mask)))
                    {
                       unsigned long       pixels[256];
                       int                 j;

                       if (i > 0)
                         {
                            for (j = 0; j < i; j++)
                               pixels[j] = (unsigned long)color_lut[j];
                            XFreeColors(d, cmap, pixels, i, 0);
                         }
                       free(color_lut);
                       return NULL;
                    }
                  color_lut[i] = xcl.pixel;
                  i++;
               }
          }
     }
   _pal_type = 7;
   return color_lut;
}

DATA8              *
__imlib_AllocColors232(Display * d, Colormap cmap, Visual * v)
{
   int                 r, g, b, i;
   DATA8              *color_lut;
   int                 sig_mask = 0;

   for (i = 0; i < v->bits_per_rgb; i++)
      sig_mask |= (0x1 << i);
   sig_mask <<= (16 - v->bits_per_rgb);
   i = 0;
   color_lut = malloc(128 * sizeof(DATA8));
   for (r = 0; r < 4; r++)
     {
        for (g = 0; g < 8; g++)
          {
             for (b = 0; b < 4; b++)
               {
                  XColor              xcl;
                  XColor              xcl_in;
                  int                 val;
                  Status              ret;

                  val = (r << 6) | (r << 4) | (r << 2) | (r);
                  xcl.red = (unsigned short)((val << 8) | (val));
                  val = (g << 6) | (g << 3) | (g);
                  xcl.green = (unsigned short)((val << 7) | (val >> 2));
                  val = (b << 6) | (b << 4) | (b << 2) | (b);
                  xcl.blue = (unsigned short)((val << 8) | (val));
                  xcl_in = xcl;
                  ret = XAllocColor(d, cmap, &xcl);
                  if ((ret == Success) ||
                      ((xcl_in.red & sig_mask) != (xcl.red & sig_mask)) ||
                      ((xcl_in.green & sig_mask) != (xcl.green & sig_mask)) ||
                      ((xcl_in.blue & sig_mask) != (xcl.blue & sig_mask)))
                    {
                       unsigned long       pixels[256];
                       int                 j;

                       if (i > 0)
                         {
                            for (j = 0; j < i; j++)
                               pixels[j] = (unsigned long)color_lut[j];
                            XFreeColors(d, cmap, pixels, i, 0);
                         }
                       free(color_lut);
                       return NULL;
                    }
                  color_lut[i] = xcl.pixel;
                  i++;
               }
          }
     }
   _pal_type = 1;
   return color_lut;
}

DATA8              *
__imlib_AllocColors222(Display * d, Colormap cmap, Visual * v)
{
   int                 r, g, b, i;
   DATA8              *color_lut;
   int                 sig_mask = 0;

   for (i = 0; i < v->bits_per_rgb; i++)
      sig_mask |= (0x1 << i);
   sig_mask <<= (16 - v->bits_per_rgb);
   i = 0;
   color_lut = malloc(64 * sizeof(DATA8));
   for (r = 0; r < 4; r++)
     {
        for (g = 0; g < 4; g++)
          {
             for (b = 0; b < 4; b++)
               {
                  XColor              xcl;
                  XColor              xcl_in;
                  int                 val;
                  Status              ret;

                  val = (r << 6) | (r << 4) | (r << 2) | (r);
                  xcl.red = (unsigned short)((val << 8) | (val));
                  val = (g << 6) | (g << 4) | (g << 2) | (g);
                  xcl.green = (unsigned short)((val << 8) | (val));
                  val = (b << 6) | (b << 4) | (b << 2) | (b);
                  xcl.blue = (unsigned short)((val << 8) | (val));
                  xcl_in = xcl;
                  ret = XAllocColor(d, cmap, &xcl);
                  if ((ret == Success) ||
                      ((xcl_in.red & sig_mask) != (xcl.red & sig_mask)) ||
                      ((xcl_in.green & sig_mask) != (xcl.green & sig_mask)) ||
                      ((xcl_in.blue & sig_mask) != (xcl.blue & sig_mask)))
                    {
                       unsigned long       pixels[256];
                       int                 j;

                       if (i > 0)
                         {
                            for (j = 0; j < i; j++)
                               pixels[j] = (unsigned long)color_lut[j];
                            XFreeColors(d, cmap, pixels, i, 0);
                         }
                       free(color_lut);
                       return NULL;
                    }
                  color_lut[i] = xcl.pixel;
                  i++;
               }
          }
     }
   _pal_type = 2;
   return color_lut;
}

DATA8              *
__imlib_AllocColors221(Display * d, Colormap cmap, Visual * v)
{
   int                 r, g, b, i;
   DATA8              *color_lut;
   int                 sig_mask = 0;

   for (i = 0; i < v->bits_per_rgb; i++)
      sig_mask |= (0x1 << i);
   sig_mask <<= (16 - v->bits_per_rgb);
   i = 0;
   color_lut = malloc(32 * sizeof(DATA8));
   for (r = 0; r < 4; r++)
     {
        for (g = 0; g < 4; g++)
          {
             for (b = 0; b < 2; b++)
               {
                  XColor              xcl;
                  XColor              xcl_in;
                  int                 val;
                  Status              ret;

                  val = (r << 6) | (r << 4) | (r << 2) | (r);
                  xcl.red = (unsigned short)((val << 8) | (val));
                  val = (g << 6) | (g << 4) | (g << 2) | (g);
                  xcl.green = (unsigned short)((val << 8) | (val));
                  val = (b << 7) | (b << 6) | (b << 5) | (b << 4) |
                     (b << 3) | (b << 2) | (b << 1) | (b);
                  xcl.blue = (unsigned short)((val << 8) | (val));
                  xcl_in = xcl;
                  ret = XAllocColor(d, cmap, &xcl);
                  if ((ret == Success) ||
                      ((xcl_in.red & sig_mask) != (xcl.red & sig_mask)) ||
                      ((xcl_in.green & sig_mask) != (xcl.green & sig_mask)) ||
                      ((xcl_in.blue & sig_mask) != (xcl.blue & sig_mask)))
                    {
                       unsigned long       pixels[256];
                       int                 j;

                       if (i > 0)
                         {
                            for (j = 0; j < i; j++)
                               pixels[j] = (unsigned long)color_lut[j];
                            XFreeColors(d, cmap, pixels, i, 0);
                         }
                       free(color_lut);
                       return NULL;
                    }
                  color_lut[i] = xcl.pixel;
                  i++;
               }
          }
     }
   _pal_type = 3;
   return color_lut;
}

DATA8              *
__imlib_AllocColors121(Display * d, Colormap cmap, Visual * v)
{
   int                 r, g, b, i;
   DATA8              *color_lut;
   int                 sig_mask = 0;

   for (i = 0; i < v->bits_per_rgb; i++)
      sig_mask |= (0x1 << i);
   sig_mask <<= (16 - v->bits_per_rgb);
   i = 0;
   color_lut = malloc(16 * sizeof(DATA8));
   for (r = 0; r < 2; r++)
     {
        for (g = 0; g < 4; g++)
          {
             for (b = 0; b < 2; b++)
               {
                  XColor              xcl;
                  XColor              xcl_in;
                  int                 val;
                  Status              ret;

                  val = (r << 7) | (r << 6) | (r << 5) | (r << 4) |
                     (r << 3) | (r << 2) | (r << 1) | (r);
                  xcl.red = (unsigned short)((val << 8) | (val));
                  val = (g << 6) | (g << 4) | (g << 2) | (g);
                  xcl.green = (unsigned short)((val << 8) | (val));
                  val = (b << 7) | (b << 6) | (b << 5) | (b << 4) |
                     (b << 3) | (b << 2) | (b << 1) | (b);
                  xcl.blue = (unsigned short)((val << 8) | (val));
                  xcl_in = xcl;
                  ret = XAllocColor(d, cmap, &xcl);
                  if ((ret == Success) ||
                      ((xcl_in.red & sig_mask) != (xcl.red & sig_mask)) ||
                      ((xcl_in.green & sig_mask) != (xcl.green & sig_mask)) ||
                      ((xcl_in.blue & sig_mask) != (xcl.blue & sig_mask)))
                    {
                       unsigned long       pixels[256];
                       int                 j;

                       if (i > 0)
                         {
                            for (j = 0; j < i; j++)
                               pixels[j] = (unsigned long)color_lut[j];
                            XFreeColors(d, cmap, pixels, i, 0);
                         }
                       free(color_lut);
                       return NULL;
                    }
                  color_lut[i] = xcl.pixel;
                  i++;
               }
          }
     }
   _pal_type = 4;
   return color_lut;
}

DATA8              *
__imlib_AllocColors111(Display * d, Colormap cmap, Visual * v)
{
   int                 r, g, b, i;
   DATA8              *color_lut;
   int                 sig_mask = 0;

   for (i = 0; i < v->bits_per_rgb; i++)
      sig_mask |= (0x1 << i);
   sig_mask <<= (16 - v->bits_per_rgb);
   i = 0;
   color_lut = malloc(8 * sizeof(DATA8));
   for (r = 0; r < 2; r++)
     {
        for (g = 0; g < 2; g++)
          {
             for (b = 0; b < 2; b++)
               {
                  XColor              xcl;
                  XColor              xcl_in;
                  int                 val;
                  Status              ret;

                  val = (r << 7) | (r << 6) | (r << 5) | (r << 4) |
                     (r << 3) | (r << 2) | (r << 1) | (r);
                  xcl.red = (unsigned short)((val << 8) | (val));
                  val = (g << 7) | (g << 6) | (g << 5) | (g << 4) |
                     (g << 3) | (g << 2) | (g << 1) | (g);
                  xcl.green = (unsigned short)((val << 8) | (val));
                  val = (b << 7) | (b << 6) | (b << 5) | (b << 4) |
                     (b << 3) | (b << 2) | (b << 1) | (b);
                  xcl.blue = (unsigned short)((val << 8) | (val));
                  xcl_in = xcl;
                  ret = XAllocColor(d, cmap, &xcl);
                  if ((ret == Success) ||
                      ((xcl_in.red & sig_mask) != (xcl.red & sig_mask)) ||
                      ((xcl_in.green & sig_mask) != (xcl.green & sig_mask)) ||
                      ((xcl_in.blue & sig_mask) != (xcl.blue & sig_mask)))
                    {
                       unsigned long       pixels[256];
                       int                 j;

                       if (i > 0)
                         {
                            for (j = 0; j < i; j++)
                               pixels[j] = (unsigned long)color_lut[j];
                            XFreeColors(d, cmap, pixels, i, 0);
                         }
                       free(color_lut);
                       return NULL;
                    }
                  color_lut[i] = xcl.pixel;
                  i++;
               }
          }
     }
   _pal_type = 5;
   return color_lut;
}

DATA8              *
__imlib_AllocColors1(Display * d, Colormap cmap, Visual * v)
{
   XColor              xcl;
   DATA8              *color_lut;

   color_lut = malloc(2 * sizeof(DATA8));
   xcl.red = (unsigned short)(0x0000);
   xcl.green = (unsigned short)(0x0000);
   xcl.blue = (unsigned short)(0x0000);
   XAllocColor(d, cmap, &xcl);
   color_lut[0] = xcl.pixel;
   xcl.red = (unsigned short)(0xffff);
   xcl.green = (unsigned short)(0xffff);
   xcl.blue = (unsigned short)(0xffff);
   XAllocColor(d, cmap, &xcl);
   color_lut[1] = xcl.pixel;
   _pal_type = 6;
   return color_lut;
}

#endif /* BUILD_X11 */
