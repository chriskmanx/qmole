#include "common.h"
#ifdef BUILD_X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XShm.h>
#include "colormod.h"
#include "image.h"
#include "scale.h"
#include "ximage.h"
#include "context.h"
#include "rgba.h"
#include "color.h"
#include "grab.h"
#include "blend.h"
#include "rend.h"
#include "rotate.h"

/* The maximum pixmap dimension is 65535. */
/* However, for now, use 46340 (46340^2 < 2^31) to avoid buffer overflow issues. */
#define X_MAX_DIM 46340

/* size of the lines per segment we scale / render at a time */
#define LINESIZE 16

DATA32
__imlib_RenderGetPixel(Display * d, Drawable w, Visual * v, Colormap cm,
                       int depth, DATA8 r, DATA8 g, DATA8 b)
{
   Context            *ct;

   ct = __imlib_GetContext(d, v, cm, depth);

   if (ct->palette)
     {
        switch (ct->palette_type)
          {
          case 0:              /* 332 */
             return ct->palette[((r >> 0) & 0xe0) |
                                ((g >> 3) & 0x1b) | ((b >> 6) & 0x02)];
             break;
          case 1:              /* 232 */
             return ct->palette[((r >> 0) & 0xe0) |
                                ((g >> 3) & 0x1b) | ((b >> 6) & 0x02)];
             break;
          case 2:              /* 222 */
             return ct->palette[((r >> 0) & 0xe0) |
                                ((g >> 3) & 0x1b) | ((b >> 6) & 0x02)];
             break;
          case 3:              /* 221 */
             return ct->palette[((r >> 0) & 0xe0) |
                                ((g >> 3) & 0x1b) | ((b >> 6) & 0x02)];
             break;
          case 4:              /* 121 */
             return ct->palette[((r >> 0) & 0xe0) |
                                ((g >> 3) & 0x1b) | ((b >> 6) & 0x02)];
             break;
          case 5:              /* 111 */
             return ct->palette[((r >> 0) & 0xe0) |
                                ((g >> 3) & 0x1b) | ((b >> 6) & 0x02)];
             break;
          case 6:              /* 1 */
             return ct->palette[((r >> 0) & 0xe0) |
                                ((g >> 3) & 0x1b) | ((b >> 6) & 0x02)];
             break;
          case 7:              /* 666 */
             return ct->palette[((int)(((double)r / 255) * 5.0) * 36) +
                                ((int)(((double)g / 255) * 5.0) * 6) +
                                ((int)(((double)b / 255) * 5.0))];
             break;
          default:
             return 0;
          }
     }
   else
     {
        unsigned int        rm, gm, bm;
        int                 i, rshift = 0, gshift = 0, bshift = 0;
        DATA32              val;

        rm = v->red_mask;
        gm = v->green_mask;
        bm = v->blue_mask;
        if ((rm == 0xf800) && (gm == 0x7e0) && (bm == 0x1f))    /* 565 */
          {
             return (((r << 8) & 0xf800) |
                     ((g << 3) & 0x07e0) | ((b >> 3) & 0x001f));
          }
        if ((rm == 0xff0000) && (gm == 0xff00) && (bm == 0xff)) /* 888 */
          {
             return (((r << 16) & 0xff0000) |
                     ((g << 8) & 0x00ff00) | ((r) & 0x0000ff));
          }
        if ((rm == 0x7c00) && (gm == 0x3e0) && (bm == 0x1f))    /* 555 */
          {
             return (((r << 7) & 0x7c00) |
                     ((g << 2) & 0x03e0) | ((b >> 3) & 0x001f));
          }
        for (i = 31; i >= 0; i--)
          {
             if (rm >= (1 << i))
               {
                  rshift = i - 7;
                  break;
               }
          }
        for (i = 31; i >= 0; i--)
          {
             if (gm >= (1 << i))
               {
                  gshift = i - 7;
                  break;
               }
          }
        for (i = 31; i >= 0; i--)
          {
             if (bm >= (1 << i))
               {
                  bshift = i - 7;
                  break;
               }
          }
        if (rshift >= 0)
           val = ((r << rshift) & rm);
        else
           val = ((r >> (-rshift)) & rm);
        if (gshift >= 0)
           val |= ((g << gshift) & gm);
        else
           val |= ((g >> (-gshift)) & gm);
        if (bshift >= 0)
           val |= ((b << bshift) & bm);
        else
           val |= ((b >> (-bshift)) & bm);
        return val;
     }
   return 0;
}

__hidden void
                    __imlib_generic_render(DATA32 * src, int jump, int w, int h, int dx, int dy,
                                           XImage * xim, Visual * v,
                                           Context * ct);

void
__imlib_generic_render(DATA32 * src, int jump, int w, int h, int dx, int dy,
                       XImage * xim, Visual * v, Context * ct)
{
   unsigned int        x, y, r, g, b, val, hh;
   unsigned int        rmask, gmask, bmask;
   int                 i, rshift, gshift, bshift;
   const DATA8         _dither_88[8][8] = {
      {0, 32, 8, 40, 2, 34, 10, 42},
      {48, 16, 56, 24, 50, 18, 58, 26},
      {12, 44, 4, 36, 14, 46, 6, 38},
      {60, 28, 52, 20, 62, 30, 54, 22},
      {3, 35, 11, 43, 1, 33, 9, 41},
      {51, 19, 59, 27, 49, 17, 57, 25},
      {15, 47, 7, 39, 13, 45, 5, 37},
      {63, 31, 55, 23, 61, 29, 53, 21}
   };

   if (xim->depth == 1)
     {
        hh = dy + h;
        for (y = dy; y < hh; y++)
          {
             for (x = dx; x < w; x++)
               {
                  r = R_VAL(src);
                  g = G_VAL(src);
                  b = B_VAL(src);
                  val = (R_VAL(src) + G_VAL(src) + B_VAL(src)) / 12;
                  if (val > _dither_88[x & 0x3][y & 0x3])
                     val = ct->palette[1];
                  else
                     val = ct->palette[0];
                  XPutPixel(xim, x, y, val);
                  src++;
               }
          }
        return;
     }
   rmask = xim->red_mask;
   gmask = xim->green_mask;
   bmask = xim->blue_mask;
   rshift = 0;
   gshift = 0;
   bshift = 0;
   for (i = 31; i >= 0; i--)
     {
        if (rmask >= (1 << i))
          {
             rshift = i - 7;
             break;
          }
     }
   for (i = 31; i >= 0; i--)
     {
        if (gmask >= (1 << i))
          {
             gshift = i - 7;
             break;
          }
     }
   for (i = 31; i >= 0; i--)
     {
        if (bmask >= (1 << i))
          {
             bshift = i - 7;
             break;
          }
     }
   hh = dy + h;
   for (y = dy; y < hh; y++)
     {
        for (x = dx; x < w; x++)
          {
             r = R_VAL(src);
             if (rshift >= 0)
                val = ((r << rshift) & rmask);
             else
                val = ((r >> (-rshift)) & rmask);
             g = G_VAL(src);
             if (gshift >= 0)
                val |= ((g << gshift) & gmask);
             else
                val |= ((g >> (-gshift)) & gmask);
             b = B_VAL(src);
             if (bshift >= 0)
                val |= ((b << bshift) & bmask);
             else
                val |= ((b >> (-bshift)) & bmask);
             XPutPixel(xim, x, y, val);
             src++;
          }
     }
}

static Display     *disp = NULL;
static GC           gc = NULL;
static GC           gcm = NULL;
static int          last_depth = 0;

void
__imlib_RenderDisconnect(Display * d)
{
   if (d != disp)
      return;
   disp = NULL;
   gc = gcm = NULL;
   last_depth = 0;
}

void
__imlib_RenderImage(Display * d, ImlibImage * im,
                    Drawable w, Drawable m,
                    Visual * v, Colormap cm, int depth,
                    int sx, int sy, int sw, int sh,
                    int dx, int dy, int dw, int dh,
                    char antialias, char hiq, char blend, char dither_mask,
                    int mat, ImlibColorModifier * cmod, ImlibOp op)
{
   XImage             *xim = NULL, *mxim = NULL;
   Context            *ct;
   DATA32             *buf = NULL, *pointer = NULL, *back = NULL;
   int                 y, h, hh, jump;
   XGCValues           gcv;
   ImlibScaleInfo     *scaleinfo = NULL;
   int                 psx, psy, psw, psh;
   char                shm = 0;
   ImlibRGBAFunction   rgbaer;
   ImlibMaskFunction   masker = NULL;
   ImlibBlendFunction  blender = NULL;
   int                 do_mmx;

   blender = __imlib_GetBlendFunction(op, 1, 0,
                                      (!(im->flags & F_HAS_ALPHA)), NULL);

   /* dont do anything if we have a 0 widht or height image to render */
   if ((dw == 0) || (dh == 0))
      return;
   /* if the input rect size < 0 dont render either */
   if ((sw <= 0) || (sh <= 0))
      return;
   /* if the output is too big (8k arbitary limit here) dont bother */
   if ((abs(dw) > X_MAX_DIM) || (abs(dh) > X_MAX_DIM))
      return;
   /* clip the source rect to be within the actual image */
   psx = sx;
   psy = sy;
   psw = sw;
   psh = sh;
   CLIP(sx, sy, sw, sh, 0, 0, im->w, im->h);
   /* clip output coords to clipped input coords */
   if (psx != sx)
      dx = (dx * sx) / psx;
   if (psy != sy)
      dy = (dy * sy) / psy;
   if (psw != sw)
      dw = (dw * sw) / psw;
   if (psh != sh)
      dh = (dh * sh) / psh;
   /* do a second check to see if we now have invalid coords */
   /* dont do anything if we have a 0 widht or height image to render */
   if ((dw == 0) || (dh == 0))
      return;
   /* if the input rect size < 0 dont render either */
   if ((sw <= 0) || (sh <= 0))
      return;
   /* if the output is too big (8k arbitary limit here) dont bother */
   if ((abs(dw) > X_MAX_DIM) || (abs(dh) > X_MAX_DIM))
      return;
   /* if we are scaling the image at all make a scaling buffer */
   if (!((sw == dw) && (sh == dh)))
     {
        scaleinfo = __imlib_CalcScaleInfo(im, sw, sh, dw, dh, antialias);
        if (!scaleinfo)
           return;
     }
   /* Sign not needed anymore */
   dw = abs(dw);
   dh = abs(dh);
   ct = __imlib_GetContext(d, v, cm, depth);
   __imlib_RGBASetupContext(ct);
   if ((blend) && (IMAGE_HAS_ALPHA(im)))
     {
        back = malloc(dw * dh * sizeof(DATA32));
        if (!__imlib_GrabDrawableToRGBA
            (back, 0, 0, dw, dh, d, w, 0, v, cm, depth, dx, dy, dw, dh, 0, 1))
          {
             free(back);
             back = NULL;
          }
     }
   /* get a new XImage - or get one from the cached list */
   xim = __imlib_ProduceXImage(d, v, depth, dw, dh, &shm);
   if (!xim)
     {
        __imlib_FreeScaleInfo(scaleinfo);
        if (back)
           free(back);
        return;
     }
   if (m)
     {
        mxim = __imlib_ProduceXImage(d, v, 1, dw, dh, &shm);
        if (!mxim)
          {
             __imlib_ConsumeXImage(d, xim);
             __imlib_FreeScaleInfo(scaleinfo);
             if (back)
                free(back);
             return;
          }
        memset(mxim->data, 0, mxim->bytes_per_line * mxim->height);
     }
   /* if we are scaling the image at all make a scaling buffer */
   if (scaleinfo)
     {
        /* allocate a buffer to render scaled RGBA data into */
        buf = malloc(dw * LINESIZE * sizeof(DATA32));
        if (!buf)
          {
             __imlib_ConsumeXImage(d, xim);
             if (m)
                __imlib_ConsumeXImage(d, mxim);
             __imlib_FreeScaleInfo(scaleinfo);
             if (back)
                free(back);
             return;
          }
     }
   /* setup h */
   h = dh;
   /* scale in LINESIZE Y chunks and convert to depth */
   /* Get rgba and mask functions for XImage rendering */
   rgbaer = __imlib_GetRGBAFunction(xim->bits_per_pixel,
                                    v->red_mask, v->green_mask, v->blue_mask,
                                    hiq, ct->palette_type);
   if (m)
      masker = __imlib_GetMaskFunction(dither_mask);
#ifdef DO_MMX_ASM
   do_mmx = __imlib_get_cpuid() & CPUID_MMX;
#endif
   for (y = 0; y < dh; y += LINESIZE)
     {
        hh = LINESIZE;
        if (h < LINESIZE)
           hh = h;
        /* if we're scaling it */
        if (scaleinfo)
          {
             /* scale the imagedata for this LINESIZE lines chunk of image data */
             if (antialias)
               {
#ifdef DO_MMX_ASM
                  if (do_mmx)
                     __imlib_Scale_mmx_AARGBA(scaleinfo, buf,
                                              ((sx * dw) / sw),
                                              ((sy * dh) / sh) + y,
                                              0, 0, dw, hh, dw, im->w);
                  else
#endif
                  if (IMAGE_HAS_ALPHA(im))
                     __imlib_ScaleAARGBA(scaleinfo, buf, ((sx * dw) / sw),
                                         ((sy * dh) / sh) + y,
                                         0, 0, dw, hh, dw, im->w);
                  else
                     __imlib_ScaleAARGB(scaleinfo, buf, ((sx * dw) / sw),
                                        ((sy * dh) / sh) + y,
                                        0, 0, dw, hh, dw, im->w);
               }
             else
                __imlib_ScaleSampleRGBA(scaleinfo, buf, ((sx * dw) / sw),
                                        ((sy * dh) / sh) + y, 0, 0, dw, hh, dw);
             jump = 0;
             pointer = buf;
             if (cmod)
                __imlib_DataCmodApply(buf, dw, hh, 0, NULL, cmod);
          }
        else
          {
             if (cmod)
               {
                  if (!buf)
                     buf = malloc(im->w * LINESIZE * sizeof(DATA32));
                  if (!buf)
                    {
                       __imlib_ConsumeXImage(d, xim);
                       if (m)
                          __imlib_ConsumeXImage(d, mxim);
                       __imlib_FreeScaleInfo(scaleinfo);
                       if (back)
                          free(back);
                       return;
                    }
                  memcpy(buf, im->data + ((y + sy) * im->w),
                         im->w * hh * sizeof(DATA32));
                  __imlib_DataCmodApply(buf, dw, hh, im->w - dw, NULL, cmod);
                  pointer = buf + sx;
                  jump = im->w - sw;
               }
             else
               {
                  jump = im->w - sw;
                  pointer = im->data + ((y + sy) * im->w) + sx;
               }
          }
        /* if we have a back buffer - we're blending to the bg */
        if (back)
          {
             blender(pointer, jump + dw, back + (y * dw), dw, dw, hh, NULL);
             pointer = back + (y * dw);
             jump = 0;
          }
        /* once scaled... convert chunk to bit depth into XImage bufer */
        if (rgbaer)
           rgbaer(pointer, jump,
                  ((DATA8 *) xim->data) + (y * (xim->bytes_per_line)),
                  xim->bytes_per_line, dw, hh, dx, dy + y);
        else
           __imlib_generic_render(pointer, jump, dw, hh, 0, y, xim, v, ct);
        if (m)
           masker(pointer, jump,
                  ((DATA8 *) mxim->data) + (y * (mxim->bytes_per_line)),
                  mxim->bytes_per_line, dw, hh, dx, dy + y, mat);
        h -= LINESIZE;
     }
   /* free up our buffers and poit tables */
   if (buf)
      free(buf);
   if (scaleinfo)
      __imlib_FreeScaleInfo(scaleinfo);
   if (back)
      free(back);
   /* if we changed diplays or depth since last time... free old gc */
   if ((gc) && ((last_depth != depth) || (disp != d)))
     {
        XFreeGC(disp, gc);
        gc = 0;
     }
   /* if we didnt have a gc... create it */
   if (!gc)
     {
        disp = d;
        last_depth = depth;
        gcv.graphics_exposures = False;
        gc = XCreateGC(d, w, GCGraphicsExposures, &gcv);
     }
   if (m)
     {
        /* if we changed diplays since last time... free old gc */
        if ((gcm) && (disp != d))
          {
             XFreeGC(disp, gcm);
             gcm = 0;
          }
        if (!gcm)
          {
             gcv.graphics_exposures = False;
             gcm = XCreateGC(d, m, GCGraphicsExposures, &gcv);
          }
        /* write the mask */
        if (shm)
           /* write shm XImage */
           XShmPutImage(d, m, gcm, mxim, 0, 0, dx, dy, dw, dh, False);
        /* write regular XImage */
        else
           XPutImage(d, m, gcm, mxim, 0, 0, dx, dy, dw, dh);
     }
   /* write the image */
   if (shm)
      /* write shm XImage */
      XShmPutImage(d, w, gc, xim, 0, 0, dx, dy, dw, dh, False);
   /* write regular XImage */
   else
      XPutImage(d, w, gc, xim, 0, 0, dx, dy, dw, dh);
   /* free the XImage and put onto our free list */
   /* wait for the write to be done */
   if (shm)
      XSync(d, False);
   __imlib_ConsumeXImage(d, xim);
   if (m)
      __imlib_ConsumeXImage(d, mxim);
}

void
__imlib_RenderImageSkewed(Display * d, ImlibImage * im, Drawable w, Drawable m,
                          Visual * v, Colormap cm, int depth,
                          int sx, int sy, int sw, int sh, int dx, int dy,
                          int hsx, int hsy, int vsx, int vsy,
                          char antialias, char hiq, char blend,
                          char dither_mask, int mat, ImlibColorModifier * cmod,
                          ImlibOp op)
{
   Context            *ct;
   int                 dx1, dy1, dx2, dy2, dw, dh, tsx, tsy;
   ImlibImage         *back;

   dx1 = dx2 = dx;
   dy1 = dy2 = dy;

   if (hsx < 0)
      dx1 += hsx;
   else
      dx2 += hsx;
   if (hsy < 0)
      dy1 += hsy;
   else
      dy2 += hsy;
   tsx = vsx;
   tsy = vsy;
   if (!tsx && !tsy)
     {
        tsy = (hsx * im->h) / im->w;
        tsx = -(hsy * im->h) / im->w;
     }
   if (tsx < 0)
      dx1 += tsx;
   else
      dx2 += tsx;
   if (tsy < 0)
      dy1 += tsy;
   else
      dy2 += tsy;

   if ((dx2 < 0) || (dy2 < 0))
      return;

   dw = dx2 - dx1;
   dh = dy2 - dy1;

   if ((dw <= 0) || (dh <= 0))
      return;

   if (dx1 < 0)
     {
        dw += dx1;
        dx1 = 0;
     }
   if (dy1 < 0)
     {
        dh += dy1;
        dy1 = 0;
     }

   ct = __imlib_GetContext(d, v, cm, depth);

   back = __imlib_CreateImage(dw, dh, NULL);
   back->data = malloc(dw * dh * sizeof(DATA32));
   memset(back->data, 0, dw * dh * sizeof(DATA32));
   __imlib_GrabDrawableToRGBA(back->data, 0, 0, dw, dh, d, w, 0, v, cm,
                              depth, dx1, dy1, dw, dh, 0, 1);

   __imlib_BlendImageToImageSkewed(im, back, antialias, 1, 0, sx, sy, sw, sh,
                                   dx - dx1, dy - dy1, hsx, hsy, vsx, vsy,
                                   cmod, op, 0, 0, 0, 0);

   __imlib_RenderImage(d, back, w, m, v, cm, depth, 0, 0, dw, dh,
                       dx1, dy1, dw, dh, 0, hiq, 0, dither_mask, mat, 0,
                       OP_COPY);
   __imlib_FreeImage(back);
}

#endif /* BUILD_X11 */
