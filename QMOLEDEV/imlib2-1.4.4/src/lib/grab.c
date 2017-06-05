#include "common.h"
#ifdef BUILD_X11
#include <X11/Xlib.h>
#include <X11/extensions/XShm.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "grab.h"
#include "ximage.h"

static char         _x_err = 0;
static DATA8        rtab[256], gtab[256], btab[256];

static void
Tmp_HandleXError(Display * d, XErrorEvent * ev)
{
   d = NULL;
   ev = NULL;
   _x_err = 1;
}

void
__imlib_GrabXImageToRGBA(DATA32 * data, int ox, int oy, int ow, int oh,
                         Display * d, XImage * xim, XImage * mxim, Visual * v,
                         int depth, int x, int y, int w, int h, char grab)
{
   int                 inx, iny;
   DATA32             *src, *ptr;
   int                 pixel;
   int                 origx, origy;
   int                 bgr = 0;

   if (!data)
      return;

   if (grab)
      XGrabServer(d);           /* This may prevent the image to be changed under our feet */
   origx = x;
   origy = y;

   if (v->blue_mask > v->red_mask)
      bgr = 1;

   if (origx < 0)
      inx = -origx;
   else
      inx = ox;
   if (origy < 0)
      iny = -origy;
   else
      iny = oy;
   /* go thru the XImage and convert */
   if ((depth == 24) && (xim->bits_per_pixel == 32))
      depth = 25;               /* fake depth meaning 24 bit in 32 bpp ximage */
   /* data needs swapping */
#define SWAP32(x) (x) = \
   ((((int)(x) & 0x000000ff ) << 24) |\
       (((int)(x) & 0x0000ff00 ) << 8) |\
       (((int)(x) & 0x00ff0000 ) >> 8) |\
       (((int)(x) & 0xff000000 ) >> 24))
#define SWAP16(x) (x) = \
   ((((short)(x) & 0x00ff ) << 8) |\
       (((short)(x) & 0xff00 ) >> 8))

#ifdef WORDS_BIGENDIAN
   if (xim->bitmap_bit_order == LSBFirst)
#else
   if (xim->bitmap_bit_order == MSBFirst)
#endif
     {
        switch (depth)
          {
          case 0:
          case 1:
          case 2:
          case 3:
          case 4:
          case 5:
          case 6:
          case 7:
          case 8:
             break;
          case 15:
          case 16:
             for (y = 0; y < h; y++)
               {
                  unsigned short     *tmp;

                  tmp =
                     (unsigned short *)(xim->data + (xim->bytes_per_line * y));
                  for (x = 0; x < w; x++)
                    {
                       SWAP16(*tmp);
                       tmp++;
                    }
               }
          case 24:
          case 25:
          case 32:
             for (y = 0; y < h; y++)
               {
                  unsigned int       *tmp;

                  tmp = (unsigned int *)(xim->data + (xim->bytes_per_line * y));
                  for (x = 0; x < w; x++)
                    {
                       SWAP32(*tmp);
                       tmp++;
                    }
               }
             break;
          default:
             break;
          }
     }
   switch (depth)
     {
     case 0:
     case 1:
     case 2:
     case 3:
     case 4:
     case 5:
     case 6:
     case 7:
     case 8:
        if (mxim)
          {
             for (y = 0; y < h; y++)
               {
                  ptr = data + ((y + iny) * ow) + inx;
                  for (x = 0; x < w; x++)
                    {
                       pixel = XGetPixel(xim, x, y);
                       pixel = (btab[pixel & 0xff]) |
                          (gtab[pixel & 0xff] << 8) |
                          (rtab[pixel & 0xff] << 16);
                       if (XGetPixel(mxim, x, y))
                          pixel |= 0xff000000;
                       *ptr++ = pixel;
                    }
               }
          }
        else
          {
             for (y = 0; y < h; y++)
               {
                  ptr = data + ((y + iny) * ow) + inx;
                  for (x = 0; x < w; x++)
                    {
                       pixel = XGetPixel(xim, x, y);
                       *ptr++ = 0xff000000 |
                          (btab[pixel & 0xff]) |
                          (gtab[pixel & 0xff] << 8) |
                          (rtab[pixel & 0xff] << 16);
                    }
               }
          }
        break;
     case 16:
#undef MP
#undef RMSK
#undef GMSK
#undef BMSK
#undef R1SH
#undef G1SH
#undef B1SH
#undef R2SH
#undef G2SH
#undef B2SH
#undef P1
#undef P2
#define MP(x, y) ((XGetPixel(mxim, (x), (y))) ? 0xff000000 : 0)
#define RMSK  0xf80000
#define GMSK  0x00fc00
#define BMSK  0x0000f8
#define R1SH(p)  ((p) << 8)
#define G1SH(p)  ((p) << 5)
#define B1SH(p)  ((p) << 3)
#define R2SH(p)  ((p) >> 8)
#define G2SH(p)  ((p) >> 11)
#define B2SH(p)  ((p) >> 13)
#define P1(p) (R1SH(p) & RMSK) | (G1SH(p) & GMSK) | (B1SH(p) & BMSK)
#define P2(p) (R2SH(p) & RMSK) | (G2SH(p) & GMSK) | (B2SH(p) & BMSK)
        if (mxim)
          {
             for (y = 0; y < h; y++)
               {
                  src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                  ptr = data + ((y + iny) * ow) + inx;
                  for (x = 0; x < (w - 1); x += 2)
                    {
#ifdef WORDS_BIGENDIAN
                       *ptr++ = MP(x + 1, y) | P2(*src);
                       *ptr++ = MP(x, y) | P1(*src);
#else
                       *ptr++ = MP(x, y) | P1(*src);
                       *ptr++ = MP(x + 1, y) | P2(*src);
#endif
                       src++;
                    }
                  if (x == (w - 1))
                    {
                       pixel = XGetPixel(xim, x, y);
                       *ptr++ = MP(x, y) | P1(pixel);
                    }
               }
          }
#undef MP
#define MP(x, y) (0xff000000)
        else
          {
             for (y = 0; y < h; y++)
               {
                  src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                  ptr = data + ((y + iny) * ow) + inx;
                  for (x = 0; x < (w - 1); x += 2)
                    {
#ifdef WORDS_BIGENDIAN
                       *ptr++ = MP(x + 1, y) | P2(*src);
                       *ptr++ = MP(x, y) | P1(*src);
#else
                       *ptr++ = MP(x, y) | P1(*src);
                       *ptr++ = MP(x + 1, y) | P2(*src);
#endif
                       src++;
                    }
                  if (x == (w - 1))
                    {
                       pixel = XGetPixel(xim, x, y);
                       *ptr++ = MP(x, y) | P1(pixel);
                    }
               }
          }
        break;
     case 15:
#undef MP
#undef RMSK
#undef GMSK
#undef BMSK
#undef R1SH
#undef G1SH
#undef B1SH
#undef R2SH
#undef G2SH
#undef B2SH
#undef P1
#undef P2
#define MP(x, y) ((XGetPixel(mxim, (x), (y))) ? 0xff000000 : 0)
#define RMSK  0xf80000
#define GMSK  0x00f800
#define BMSK  0x0000f8
#define R1SH(p)  ((p) << 9)
#define G1SH(p)  ((p) << 6)
#define B1SH(p)  ((p) << 3)
#define R2SH(p)  ((p) >> 7)
#define G2SH(p)  ((p) >> 10)
#define B2SH(p)  ((p) >> 13)
#define P1(p) (R1SH(p) & RMSK) | (G1SH(p) & GMSK) | (B1SH(p) & BMSK)
#define P2(p) (R2SH(p) & RMSK) | (G2SH(p) & GMSK) | (B2SH(p) & BMSK)
        if (mxim)
          {
             for (y = 0; y < h; y++)
               {
                  src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                  ptr = data + ((y + iny) * ow) + inx;
                  for (x = 0; x < (w - 1); x += 2)
                    {
#ifdef WORDS_BIGENDIAN
                       *ptr++ = MP(x + 1, y) | P2(*src);
                       *ptr++ = MP(x, y) | P1(*src);
#else
                       *ptr++ = MP(x, y) | P1(*src);
                       *ptr++ = MP(x + 1, y) | P2(*src);
#endif
                       src++;
                    }
                  if (x == (w - 1))
                    {
                       pixel = XGetPixel(xim, x, y);
                       *ptr++ = MP(x, y) | P1(pixel);
                    }
               }
          }
#undef MP
#define MP(x, y) (0xff000000)
        else
          {
             for (y = 0; y < h; y++)
               {
                  src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                  ptr = data + ((y + iny) * ow) + inx;
                  for (x = 0; x < (w - 1); x += 2)
                    {
#ifdef WORDS_BIGENDIAN
                       *ptr++ = MP(x + 1, y) | P2(*src);
                       *ptr++ = MP(x, y) | P1(*src);
#else
                       *ptr++ = MP(x, y) | P1(*src);
                       *ptr++ = MP(x + 1, y) | P2(*src);
#endif
                       src++;
                    }
                  if (x == (w - 1))
                    {
                       pixel = XGetPixel(xim, x, y);
                       *ptr++ = MP(x, y) | P1(pixel);
                    }
               }
          }
        break;
     case 24:
        if (bgr)
          {
             if (mxim)
               {
                  for (y = 0; y < h; y++)
                    {
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = XGetPixel(xim, x, y);
                            pixel = ((pixel << 16) & 0xff0000) |
                               ((pixel) & 0x00ff00) |
                               ((pixel >> 16) & 0x0000ff);
                            if (XGetPixel(mxim, x, y))
                               pixel |= 0xff000000;
                            *ptr++ = pixel;
                         }
                    }
               }
             else
               {
                  for (y = 0; y < h; y++)
                    {
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = XGetPixel(xim, x, y);
                            *ptr++ = 0xff000000 |
                               ((pixel << 16) & 0xff0000) |
                               ((pixel) & 0x00ff00) |
                               ((pixel >> 16) & 0x0000ff);
                         }
                    }
               }
          }
        else
          {
             if (mxim)
               {
                  for (y = 0; y < h; y++)
                    {
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = XGetPixel(xim, x, y) & 0x00ffffff;
                            if (XGetPixel(mxim, x, y))
                               pixel |= 0xff000000;
                            *ptr++ = pixel;
                         }
                    }
               }
             else
               {
                  for (y = 0; y < h; y++)
                    {
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = XGetPixel(xim, x, y);
                            *ptr++ = 0xff000000 | (pixel & 0x00ffffff);
                         }
                    }
               }
          }
        break;
     case 25:
        if (bgr)
          {
             if (mxim)
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = ((*src << 16) & 0xff0000) |
                               ((*src) & 0x00ff00) | ((*src >> 16) & 0x0000ff);
                            if (XGetPixel(mxim, x, y))
                               pixel |= 0xff000000;
                            *ptr++ = pixel;
                            src++;
                         }
                    }
               }
             else
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            *ptr++ = 0xff000000 |
                               ((*src << 16) & 0xff0000) |
                               ((*src) & 0x00ff00) | ((*src >> 16) & 0x0000ff);
                            src++;
                         }
                    }
               }
          }
        else
          {
             if (mxim)
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = (*src) & 0x00ffffff;
                            if (XGetPixel(mxim, x, y))
                               pixel |= 0xff000000;
                            *ptr++ = pixel;
                            src++;
                         }
                    }
               }
             else
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            *ptr++ = 0xff000000 | ((*src) & 0x00ffffff);
                            src++;
                         }
                    }
               }
          }
        break;
     case 32:
        if (bgr)
          {
             if (mxim)
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = SWAP32(*src);
                            if (!XGetPixel(mxim, x, y))
                               pixel &= 0x00ffffff;
                            *ptr++ = pixel;
                            src++;
                         }
                    }
               }
             else
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            *ptr++ = SWAP32(*src);
                            src++;
                         }
                    }
               }
          }
        else
          {
             if (mxim)
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            pixel = *src++;
                            if (!XGetPixel(mxim, x, y))
                               pixel &= 0x00ffffff;
                            *ptr++ = pixel;
                         }
                    }
               }
             else
               {
                  for (y = 0; y < h; y++)
                    {
                       src = (DATA32 *) (xim->data + (xim->bytes_per_line * y));
                       ptr = data + ((y + iny) * ow) + inx;
                       for (x = 0; x < w; x++)
                         {
                            *ptr++ = *src++;
                         }
                    }
               }
          }
        break;
     default:
        break;
     }

   if (grab)
      XUngrabServer(d);
}

char
__imlib_GrabDrawableToRGBA(DATA32 * data, int ox, int oy, int ow, int oh,
                           Display * d, Drawable p, Pixmap m, Visual * v,
                           Colormap cm, int depth, int x, int y,
                           int w, int h, char *pdomask, char grab)
{
   XErrorHandler       prev_erh = NULL;
   XWindowAttributes   xatt, ratt;
   char                is_pixmap = 0, created_mask = 0, is_shm = 0, is_mshm = 0;
   char                domask;
   int                 i;
   int                 src_x, src_y, src_w, src_h, origw, origh;
   int                 width, height, clipx, clipy;
   XShmSegmentInfo     shminfo, mshminfo;
   XImage             *xim, *mxim;
   XColor              cols[256];

   domask = (pdomask) ? *pdomask : 0;
   /* FIXME: oh isnt used - i wonder if there's a bug looming... */
   oh = 0;
   origw = w;
   origh = h;
   if (grab)
      XGrabServer(d);
   XSync(d, False);
   prev_erh = XSetErrorHandler((XErrorHandler) Tmp_HandleXError);
   _x_err = 0;
   /* lets see if its a pixmap or not */
   XGetWindowAttributes(d, p, &xatt);
   XSync(d, False);
   if (_x_err)
      is_pixmap = 1;
   /* reset our error handler */
   XSetErrorHandler((XErrorHandler) prev_erh);
   if (is_pixmap)
     {
        Window              dw;

        XGetGeometry(d, p, &dw, &src_x, &src_y,
                     (unsigned int *)&src_w, (unsigned int *)&src_h,
                     (unsigned int *)&src_x, (unsigned int *)&xatt.depth);
        src_x = 0;
        src_y = 0;
     }
   else
     {
        Window              dw;

        XGetWindowAttributes(d, xatt.root, &ratt);
        XTranslateCoordinates(d, p, xatt.root, 0, 0, &src_x, &src_y, &dw);
        src_w = xatt.width;
        src_h = xatt.height;
        if ((xatt.map_state != IsViewable) && (xatt.backing_store == NotUseful))
          {
             if (grab)
                XUngrabServer(d);
             return 0;
          }
     }

   /* clip to the drawable tree and screen */
   clipx = 0;
   clipy = 0;
   width = src_w - x;
   height = src_h - y;
   if (width > w)
      width = w;
   if (height > h)
      height = h;

   if (!is_pixmap)
     {
        if ((src_x + x + width) > ratt.width)
           width = ratt.width - (src_x + x);
        if ((src_y + y + height) > ratt.height)
           height = ratt.height - (src_y + y);
     }
   if (x < 0)
     {
        clipx = -x;
        width += x;
        x = 0;
     }
   if (y < 0)
     {
        clipy = -y;
        height += y;
        y = 0;
     }
   if (!is_pixmap)
     {
        if ((src_x + x) < 0)
          {
             clipx -= (src_x + x);
             width += (src_x + x);
             x = -src_x;
          }
        if ((src_y + y) < 0)
          {
             clipy -= (src_y + y);
             height += (src_y + y);
             y = -src_y;
          }
     }
   if ((width <= 0) || (height <= 0))
     {
        if (grab)
           XUngrabServer(d);
        return 0;
     }
   w = width;
   h = height;
   if ((!is_pixmap) && (domask) && (!m))
     {
        int                 ord, rect_no = 0;
        XRectangle         *r = NULL;

        r = XShapeGetRectangles(d, p, ShapeBounding, &rect_no, &ord);
        if (r)
          {
             if (!((rect_no == 1) &&
                   (r[0].x == 0) && (r[0].y == 0) &&
                   (r[0].width == xatt.width) && (r[0].height == xatt.height)))
               {
                  XGCValues           gcv;
                  GC                  gc;

                  created_mask = 1;
                  m = XCreatePixmap(d, p, w, h, 1);
                  gcv.foreground = 0;
                  gc = XCreateGC(d, m, GCForeground, &gcv);
                  XFillRectangle(d, m, gc, 0, 0, w, h);
                  XSetForeground(d, gc, 1);
                  for (i = 0; i < rect_no; i++)
                     XFillRectangle(d, m, gc,
                                    r[i].x - x, r[i].y - y,
                                    r[i].width, r[i].height);
                  XFreeGC(d, gc);
               }
             XFree(r);
          }
     }

   /* Create an Ximage (shared or not) */
   if (x_does_shm < 0)
      __imlib_ShmCheck(d);

   xim = NULL;
   if (x_does_shm)
     {
        xim = __imlib_ShmGetXImage(d, v, p, xatt.depth, x, y, w, h, &shminfo);
        is_shm = xim != NULL;
     }
   if (!xim)
      xim = XGetImage(d, p, x, y, w, h, 0xffffffff, ZPixmap);
   if (!xim)
     {
        if (grab)
           XUngrabServer(d);
        return 0;
     }

   mxim = NULL;
   if ((m) && (domask))
     {
        mxim = __imlib_ShmGetXImage(d, v, m, 1, 0, 0, w, h, &mshminfo);
        is_mshm = mxim != NULL;
        if (!mxim)
           mxim = XGetImage(d, m, 0, 0, w, h, 0xffffffff, ZPixmap);
     }

   if ((is_shm) || (is_mshm))
     {
        XSync(d, False);
        if (grab)
           XUngrabServer(d);
        XSync(d, False);
     }
   else if (grab)
      XUngrabServer(d);

   if ((xatt.depth == 1) && (!cm) && (is_pixmap))
     {
        rtab[0] = 255;
        gtab[0] = 255;
        btab[0] = 255;
        rtab[1] = 0;
        gtab[1] = 0;
        btab[1] = 0;
     }
   else if (xatt.depth <= 8)
     {
        if (!cm)
          {
             if (is_pixmap)
               {
                  cm = DefaultColormap(d, DefaultScreen(d));
               }
             else
               {
                  cm = xatt.colormap;
                  if (cm == None)
                     cm = ratt.colormap;
               }
          }

        for (i = 0; i < (1 << xatt.depth); i++)
          {
             cols[i].pixel = i;
             cols[i].flags = DoRed | DoGreen | DoBlue;
          }
        XQueryColors(d, cm, cols, 1 << xatt.depth);
        for (i = 0; i < (1 << xatt.depth); i++)
          {
             rtab[i] = cols[i].red >> 8;
             gtab[i] = cols[i].green >> 8;
             btab[i] = cols[i].blue >> 8;
          }
     }
   __imlib_GrabXImageToRGBA(data, ox + clipx, oy + clipy, ow, oh,
                            d, xim, mxim, v, xatt.depth, x, y, w, h, 0);

   /* destroy the Ximage */
   if (is_shm)
      __imlib_ShmDetach(d, &shminfo);
   XDestroyImage(xim);
   if (mxim)
     {
        if (is_mshm)
           __imlib_ShmDetach(d, &mshminfo);
        XDestroyImage(mxim);
     }
   if (created_mask)
      XFreePixmap(d, m);

   if (pdomask)
     {
        /* Set domask according to whether or not we have useful alpha data */
        if (xatt.depth == 32)
           *pdomask = 1;
        else if (!m)
           *pdomask = 0;
     }

   return 1;
}

#endif /* BUILD_X11 */
