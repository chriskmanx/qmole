#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "span.h"
#include "updates.h"
#include "rgbadraw.h"

#define EXCHANGE_POINTS(x0, y0, x1, y1)  \
do {                                     \
	int _tmp = y0;                   \
                                         \
	y0 = y1;                         \
	y1 = _tmp;                       \
                                         \
	_tmp = x0;                       \
	x0 = x1;                         \
	x1 = _tmp;                       \
} while (0)

ImlibUpdate        *
__imlib_Point_DrawToImage(int x, int y, DATA32 color,
                          ImlibImage * im, int clx, int cly, int clw, int clh,
                          ImlibOp op, char blend, char make_updates)
{
   ImlibPointDrawFunction pfunc;

   if (blend && (!A_VAL(&color)))
      return NULL;
   if (!IN_RANGE(x, y, im->w, im->h))
      return NULL;
   if (clw == 0)
     {
        clw = im->w;
        clx = 0;
        clh = im->h;
        cly = 0;
     }
   if (!IN_RECT(x, y, clx, cly, clw, clh))
      return NULL;

   if (A_VAL(&color) == 0xff)
      blend = 0;
   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   pfunc = __imlib_GetPointDrawFunction(op, IMAGE_HAS_ALPHA(im), blend);
   if (pfunc)
      pfunc(color, im->data + (im->w * y) + x);
   if (make_updates)
      return __imlib_AddUpdate(NULL, x, y, 1, 1);
   return NULL;
}

static int
__imlib_SimpleLine_DrawToData(int x0, int y0, int x1, int y1, DATA32 color,
                              DATA32 * dst, int dstw, int clx, int cly, int clw,
                              int clh, int *cl_x0, int *cl_y0, int *cl_x1,
                              int *cl_y1, ImlibOp op, char dst_alpha,
                              char blend)
{
   ImlibPointDrawFunction pfunc;
   ImlibSpanDrawFunction sfunc;
   int                 dx, dy, len, lx, ty, rx, by;
   DATA32             *p;

   if (A_VAL(&color) == 0xff)
      blend = 0;

   if (y0 > y1)
      EXCHANGE_POINTS(x0, y0, x1, y1);

   dx = x1 - x0;
   dy = y1 - y0;

   lx = clx;
   rx = clx + clw - 1;
   ty = cly;
   by = cly + clh - 1;

   if ((x0 < lx) && (x1 < lx))
      return 0;
   if ((x0 > rx) && (x1 > rx))
      return 0;
   if ((y0 > by) || (y1 < ty))
      return 0;

   if (dy == 0)
     {
        sfunc = __imlib_GetSpanDrawFunction(op, dst_alpha, blend);
        if (!sfunc)
           return 0;

        if (dx < 0)
          {
             int                 tmp = x1;

             x1 = x0;
             x0 = tmp;
          }

        if (x0 < lx)
           x0 = lx;
        if (x1 > rx)
           x1 = rx;

        len = x1 - x0 + 1;

        p = dst + (dstw * y0) + x0;

        sfunc(color, p, len);

        *cl_x0 = x0;
        *cl_y0 = y0;
        *cl_x1 = x1;
        *cl_y1 = y1;

        return 1;
     }

   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   if (!pfunc)
      return 0;

   if (dx == 0)
     {
        if (y0 < ty)
           y0 = ty;
        if (y1 > by)
           y1 = by;

        len = y1 - y0 + 1;

        p = dst + (dstw * y0) + x0;

        while (len--)
          {
             pfunc(color, p);
             p += dstw;
          }

        *cl_x0 = x0;
        *cl_y0 = y0;
        *cl_x1 = x1;
        *cl_y1 = y1;

        return 1;
     }

   if ((dy == dx) || (dy == -dx))
     {
        int                 p0_in, p1_in;

        p0_in = (IN_RECT(x0, y0, clx, cly, clw, clh) ? 1 : 0);
        p1_in = (IN_RECT(x1, y1, clx, cly, clw, clh) ? 1 : 0);

        if (dx > 0)
          {
             if (!p0_in)
               {
                  x0 = x0 + (ty - y0);
                  y0 = ty;
                  if (x0 > rx)
                     return 0;
                  if (x0 < lx)
                    {
                       y0 = y0 + (lx - x0);
                       x0 = lx;
                       if ((y0 < ty) || (y0 > by))
                          return 0;
                    }
               }
             if (!p1_in)
               {
                  x1 = x0 + (by - y0);
                  y1 = by;
                  if (x1 < lx)
                     return 0;
                  if (x1 > rx)
                    {
                       y1 = y0 + (rx - x0);
                       x1 = rx;
                       if ((y1 < ty) || (y1 > by))
                          return 0;
                    }
               }
          }
        else
          {
             if (!p0_in)
               {
                  x0 = x0 - (ty - y0);
                  y0 = ty;
                  if (x0 < lx)
                     return 0;
                  if (x0 > rx)
                    {
                       y0 = y0 - (rx - x0);
                       x0 = rx;
                       if ((y0 < ty) || (y0 > by))
                          return 0;
                    }
               }
             if (!p1_in)
               {
                  x1 = x0 - (by - y0);
                  y1 = by;
                  if (x1 > rx)
                     return 0;
                  if (x1 < lx)
                    {
                       y1 = y0 - (lx - x0);
                       x1 = lx;
                       if ((y1 < ty) || (y1 > by))
                          return 0;
                    }
               }
          }

        len = y1 - y0 + 1;

        p = dst + (dstw * y0) + x0;
        if (dx > 0)
           dstw++;
        else
           dstw--;

        while (len--)
          {
             pfunc(color, p);
             p += dstw;
          }
     }

   *cl_x0 = x0;
   *cl_y0 = y0;
   *cl_x1 = x1;
   *cl_y1 = y1;

   return 1;
}

#define SETUP_LINE_SHALLOW() \
do { \
	if (x0 > x1)                                         \
	  {                                                  \
	   EXCHANGE_POINTS(x0, y0, x1, y1);                  \
	   dx = -dx;                                         \
	   dy = -dy;                                         \
	  }                                                  \
                                                             \
	px = x0;                                             \
	py = y0;                                             \
                                                             \
	p0_in = (IN_RANGE(x0 ,y0 , clw, clh) ? 1 : 0);       \
	p1_in = (IN_RANGE(x1 ,y1 , clw, clh) ? 1 : 0);       \
                                                             \
	dely = 1;                                            \
	dh = dstw;                                           \
	if (dy < 0)                                          \
	  {                                                  \
	   dely = -1;                                        \
	   dh = -dstw;                                       \
	  }                                                  \
                                                             \
	dyy = (dy << 16) / dx;                               \
                                                             \
	if (!p0_in)                                          \
	  {                                                  \
	   dxx = (dx << 16) / dy;                            \
	   if (px < 0)                                       \
	     {                                               \
		x = -px;  px = 0;                            \
		yy = x * dyy;                                \
		y = yy >> 16;                                \
	        if (!a_a)                                    \
                   y += (yy - (y << 16)) >> 15;              \
		py += y;                                     \
		if ((dely > 0) && (py >= clh))               \
		   return 0;                                 \
		else if ((dely < 0) && (py < -1))            \
                   return 0;                                 \
	     }                                               \
                                                             \
	   y = 0;                                            \
	   if ((dely > 0) && (py < -1))                      \
		y = (-1 - py);                               \
	   else if ((dely < 0) && (py >= clh))               \
		y = (clh - 1 - py);                          \
                                                             \
	   xx = y * dxx;                                     \
	   x = xx >> 16;                                     \
	   if (!a_a)                                         \
              x += (xx - (x << 16)) >> 15;                   \
	   px += x;                                          \
	   if (px >= clw) return 0;                          \
                                                             \
	   yy = x * dyy;                                     \
	   y = yy >> 16;                                     \
	   if (!a_a)                                         \
              y += (yy - (y << 16)) >> 15;                   \
	   py += y;                                          \
	   if ((dely > 0) && (py >= clh))                    \
		 return 0;                                   \
	   else if ((dely < 0) && (py < -1))                 \
		 return 0;                                   \
	  }                                                  \
                                                             \
	p = dst + (dstw * py) + px;                          \
                                                             \
	x = px - x0;                                         \
	yy = x * dyy;                                        \
	prev_y = (yy >> 16);                                 \
                                                             \
	rx = MIN(x1 + 1, clw);                               \
	by = clh - 1;                                        \
} while (0)

#define SETUP_LINE_STEEP() \
do { \
   if (y0 > y1)                                              \
     {                                                       \
	EXCHANGE_POINTS(x0, y0, x1, y1);                     \
	dx = -dx;                                            \
	dy = -dy;                                            \
     }                                                       \
                                                             \
   px = x0;                                                  \
   py = y0;                                                  \
                                                             \
   p0_in = (IN_RANGE(x0 ,y0 , clw, clh) ? 1 : 0);            \
   p1_in = (IN_RANGE(x1 ,y1 , clw, clh) ? 1 : 0);            \
                                                             \
   delx = 1;                                                 \
   if (dx < 0)                                               \
	delx = -1;                                           \
                                                             \
   dxx = (dx << 16) / dy;                                    \
                                                             \
   if (!p0_in)                                               \
     {                                                       \
	dyy = (dy << 16) / dx;                               \
                                                             \
	if (py < 0)                                          \
	  {                                                  \
	   y = -py;  py = 0;                                 \
	   xx = y * dxx;                                     \
	   x = xx >> 16;                                     \
	   if (!a_a)                                         \
              x += (xx - (x << 16)) >> 15;                   \
	   px += x;                                          \
	   if ((delx > 0) && (px >= clw))                    \
		return 0;                                    \
	   else if ((delx < 0) && (px < -1))                 \
		return 0;                                    \
	  }                                                  \
                                                             \
	x = 0;                                               \
	if ((delx > 0) && (px < -1))                         \
	   x = (-1 - px);                                    \
	else if ((delx < 0) && (px >= clw))                  \
	   x = (clw - 1 - px);                               \
                                                             \
	yy = x * dyy;                                        \
	y = yy >> 16;                                        \
	if (!a_a)                                            \
           y += (yy - (y << 16)) >> 15;                      \
	py += y;                                             \
	if (py >= clh) return 0;                             \
                                                             \
	xx = y * dxx;                                        \
	x = xx >> 16;                                        \
        if (!a_a)                                            \
           x += (xx - (x << 16)) >> 15;                      \
	px += x;                                             \
	if ((delx > 0) && (px >= clw))                       \
	   return 0;                                         \
	else if ((delx < 0) && (px < -1))                    \
	   return 0;                                         \
     }                                                       \
                                                             \
   p = dst + (dstw * py) + px;                               \
                                                             \
   y = py - y0;                                              \
   xx = y * dxx;                                             \
   prev_x = (xx >> 16);                                      \
                                                             \
   by = MIN(y1 + 1, clh);                                    \
   rx = clw - 1;                                             \
} while (0)

static int
__imlib_Line_DrawToData(int x0, int y0, int x1, int y1, DATA32 color,
                        DATA32 * dst, int dstw, int clx, int cly, int clw,
                        int clh, int *cl_x0, int *cl_y0, int *cl_x1, int *cl_y1,
                        ImlibOp op, char dst_alpha, char blend)
{
   ImlibPointDrawFunction pfunc;
   int                 px, py, x, y, prev_x, prev_y;
   int                 dx, dy, rx, by, p0_in, p1_in, dh, a_a = 0;
   int                 delx, dely, xx, yy, dxx, dyy;
   DATA32             *p;

   dx = x1 - x0;
   dy = y1 - y0;

   if ((dx == 0) || (dy == 0) || (dx == dy) || (dx == -dy))
      return __imlib_SimpleLine_DrawToData(x0, y0, x1, y1, color,
                                           dst, dstw, clx, cly, clw, clh,
                                           cl_x0, cl_y0, cl_x1, cl_y1,
                                           op, dst_alpha, blend);

   if (A_VAL(&color) == 0xff)
      blend = 0;

   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   if (!pfunc)
      return 0;

   dst += (dstw * cly) + clx;
   x0 -= clx;
   y0 -= cly;
   x1 -= clx;
   y1 -= cly;

   /* shallow: x-parametric */
   if (abs(dy) < abs(dx))
     {
        SETUP_LINE_SHALLOW();

        *cl_x0 = px + clx;
        *cl_y0 = py + cly;

        while (px < rx)
          {
             y = (yy >> 16);
             y += ((yy - (y << 16)) >> 15);
             if (prev_y != y)
               {
                  prev_y = y;
                  p += dh;
                  py += dely;
               }

             if (!p1_in)
               {
                  if ((py < 0) && (dely < 0))
                     break;
                  if ((py > by) && (dely > 0))
                     break;
               }

             if (IN_RANGE(px, py, clw, clh))
                pfunc(color, p);

             yy += dyy;
             px++;
             p++;
          }

        *cl_x1 = px + clx;
        *cl_y1 = py + cly;

        return 1;
     }

   /* steep: y-parametric */

   SETUP_LINE_STEEP();

   *cl_x0 = px + clx;
   *cl_y0 = py + cly;

   while (py < by)
     {
        x = (xx >> 16);
        x += ((xx - (x << 16)) >> 15);
        if (prev_x != x)
          {
             prev_x = x;
             px += delx;
             p += delx;
          }

        if (!p1_in)
          {
             if ((px < 0) && (delx < 0))
                break;
             if ((px > rx) && (delx > 0))
                break;
          }

        if (IN_RANGE(px, py, clw, clh))
           pfunc(color, p);

        xx += dxx;
        py++;
        p += dstw;
     }

   *cl_x1 = px + clx;
   *cl_y1 = py + cly;

   return 1;
}

static int
__imlib_Line_DrawToData_AA(int x0, int y0, int x1, int y1, DATA32 color,
                           DATA32 * dst, int dstw, int clx, int cly, int clw,
                           int clh, int *cl_x0, int *cl_y0, int *cl_x1,
                           int *cl_y1, ImlibOp op, char dst_alpha, char blend)
{
   ImlibPointDrawFunction pfunc;
   int                 px, py, x, y, prev_x, prev_y;
   int                 dx, dy, rx, by, p0_in, p1_in, dh, a_a = 1;
   int                 delx, dely, xx, yy, dxx, dyy;
   DATA32             *p;
   DATA8               ca = A_VAL(&color);

   dx = x1 - x0;
   dy = y1 - y0;

   if ((dx == 0) || (dy == 0) || (dx == dy) || (dx == -dy))
      return __imlib_SimpleLine_DrawToData(x0, y0, x1, y1, color,
                                           dst, dstw, clx, cly, clw, clh,
                                           cl_x0, cl_y0, cl_x1, cl_y1,
                                           op, dst_alpha, blend);

   pfunc = __imlib_GetPointDrawFunction(op, dst_alpha, blend);
   if (!pfunc)
      return 0;

   dst += (dstw * cly) + clx;
   x0 -= clx;
   y0 -= cly;
   x1 -= clx;
   y1 -= cly;

   /* shallow: x-parametric */
   if (abs(dy) < abs(dx))
     {
        SETUP_LINE_SHALLOW();

        *cl_x0 = px + clx;
        *cl_y0 = py + cly;

        while (px < rx)
          {
             DATA32              tmp;
             DATA8               aa;

             y = (yy >> 16);
             if (prev_y != y)
               {
                  prev_y = y;
                  p += dh;
                  py += dely;
               }

             if (!p1_in)
               {
                  if ((py < -1) && (dely < 0))
                     break;
                  if ((py > by) && (dely > 0))
                     break;
               }

             if ((unsigned)(px) < clw)
               {
                  aa = (yy - (y << 16)) >> 8;

                  A_VAL(&color) = 255 - aa;
                  if (ca < 255)
                     MULT(A_VAL(&color), ca, A_VAL(&color), tmp);

                  if ((unsigned)(py) < clh)
                     pfunc(color, p);

                  if ((unsigned)(py + 1) < clh)
                    {
                       A_VAL(&color) = aa;
                       if (ca < 255)
                          MULT(A_VAL(&color), ca, A_VAL(&color), tmp);
                       pfunc(color, p + dstw);
                    }
               }

             yy += dyy;
             px++;
             p++;
          }

        *cl_x1 = px + clx;
        *cl_y1 = py + cly;

        return 1;
     }

   /* steep: y-parametric */

   SETUP_LINE_STEEP();

   *cl_x0 = px + clx;
   *cl_y0 = py + cly;

   while (py < by)
     {
        DATA32              tmp;
        DATA8               aa;

        x = (xx >> 16);
        if (prev_x != x)
          {
             prev_x = x;
             px += delx;
             p += delx;
          }

        if (!p1_in)
          {
             if ((px < -1) && (delx < 0))
                break;
             if ((px > rx) && (delx > 0))
                break;
          }

        if ((unsigned)(py) < clh)
          {
             aa = (xx - (x << 16)) >> 8;

             A_VAL(&color) = 255 - aa;
             if (ca < 255)
                MULT(A_VAL(&color), ca, A_VAL(&color), tmp);

             if ((unsigned)(px) < clw)
                pfunc(color, p);

             if ((unsigned)(px + 1) < clw)
               {
                  A_VAL(&color) = aa;
                  if (ca < 255)
                     MULT(A_VAL(&color), ca, A_VAL(&color), tmp);
                  pfunc(color, p + 1);
               }
          }

        xx += dxx;
        py++;
        p += dstw;
     }

   *cl_x1 = px + clx;
   *cl_y1 = py + cly;

   return 1;
}

ImlibUpdate        *
__imlib_Line_DrawToImage(int x0, int y0, int x1, int y1, DATA32 color,
                         ImlibImage * im, int clx, int cly, int clw, int clh,
                         ImlibOp op, char blend, char anti_alias,
                         char make_updates)
{
   int                 cl_x0, cl_y0, cl_x1, cl_y1, drew;

   if ((x0 == x1) && (y0 == y1))
      return __imlib_Point_DrawToImage(x0, y0, color,
                                       im, clx, cly, clw, clh,
                                       op, blend, make_updates);

   if (blend && (!A_VAL(&color)))
      return NULL;
   if (clw < 0)
      return NULL;

   if (clw == 0)
     {
        clw = im->w;
        clx = 0;
        clh = im->h;
        cly = 0;
     }

   CLIP_RECT_TO_RECT(clx, cly, clw, clh, 0, 0, im->w, im->h);
   if ((clw < 1) || (clh < 1))
      return NULL;

   if ((x0 < clx) && (x1 < clx))
      return NULL;
   if ((x0 >= (clx + clw)) && (x1 >= (clx + clw)))
      return NULL;
   if ((y0 < cly) && (y1 < cly))
      return NULL;
   if ((y0 >= (cly + clh)) && (y1 >= (cly + clh)))
      return NULL;

   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   if (anti_alias)
      drew = __imlib_Line_DrawToData_AA(x0, y0, x1, y1, color,
                                        im->data, im->w, clx, cly, clw, clh,
                                        &cl_x0, &cl_y0, &cl_x1, &cl_y1,
                                        op, IMAGE_HAS_ALPHA(im), blend);
   else
      drew = __imlib_Line_DrawToData(x0, y0, x1, y1, color,
                                     im->data, im->w, clx, cly, clw, clh,
                                     &cl_x0, &cl_y0, &cl_x1, &cl_y1,
                                     op, IMAGE_HAS_ALPHA(im), blend);

   if (drew && make_updates)
     {
        int                 mi, ma, dx, dy, w, h;

        mi = MIN(cl_x0, cl_x1);
        ma = MAX(cl_x0, cl_x1);
        cl_x0 = mi;
        cl_x1 = ma;
        dx = cl_x1 - cl_x0;

        mi = MIN(cl_y0, cl_y1);
        ma = MAX(cl_y0, cl_y1);
        cl_y0 = mi;
        cl_y1 = ma;
        dy = cl_y1 - cl_y0;

        w = dx + 1;
        h = dy + 1;

        if (anti_alias)
          {
             if (((cl_x1 + 1) < (clx + clw)) && (dy > dx))
                w++;
             if (((cl_y1 + 1) < (cly + clh)) && (dx > dy))
                h++;
          }

        CLIP_RECT_TO_RECT(cl_x0, cl_y0, w, h, clx, cly, clw, clh);
        if ((w < 1) || (h < 1))
           return NULL;

        return __imlib_AddUpdate(NULL, cl_x0, cl_y0, w, h);
     }

   return NULL;
}
