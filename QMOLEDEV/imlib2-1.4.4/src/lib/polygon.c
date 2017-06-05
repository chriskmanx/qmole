#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "span.h"
#include "updates.h"
#include "rgbadraw.h"

ImlibPoly
__imlib_polygon_new()
{
   ImlibPoly           poly;

   poly = malloc(sizeof(_ImlibPoly));
   if (!poly)
      return NULL;
   memset(poly, 0, sizeof(_ImlibPoly));
   return poly;
}

void
__imlib_polygon_add_point(ImlibPoly poly, int x, int y)
{
   if (!poly->points)
     {
        poly->points = (ImlibPoint *) malloc(sizeof(ImlibPoint));
        if (!poly->points)
           return;

        poly->pointcount++;
        poly->lx = poly->rx = x;
        poly->ty = poly->by = y;
     }
   else
     {
        poly->pointcount++;
        poly->points =
           (ImlibPoint *) realloc(poly->points,
                                  (poly->pointcount * sizeof(ImlibPoint)));

        if (!poly->points)
          {
             poly->pointcount = 0;
             return;
          }

        if (x < poly->lx)
           poly->lx = x;
        if (poly->rx < x)
           poly->rx = x;
        if (y < poly->ty)
           poly->ty = y;
        if (poly->by < y)
           poly->by = y;
     }

   poly->points[poly->pointcount - 1].x = x;
   poly->points[poly->pointcount - 1].y = y;
}

void
__imlib_polygon_free(ImlibPoly poly)
{
   if (poly->points)
      free(poly->points);
   free(poly);
}

#define TRUE 1
#define FALSE 0

/* Check if p lies on segment [ s1, s2 ] given that
      it lies on the line defined by s1 and s2. */
#define __imlib_point_inside_segment(p_x, p_y, s1_x, s1_y, s2_x,s2_y) \
(s1_y != s2_y) ? (p_y <= MAX(s1_y, s2_y) && p_y >= MIN(s1_y, s2_y)) : (p_x <= MAX(s1_x, s2_x) && p_x >= MIN(s1_x, s2_x))

#define __imlib_point_on_segment(p_x, p_y, s1_x, s1_y, s2_x, s2_y) \
__imlib_segments_intersect(p_x, p_y, p_x, p_y, s1_x, s1_y, s2_x, s2_y)

void
__imlib_polygon_get_bounds(ImlibPoly poly, int *px1, int *py1, int *px2,
                           int *py2)
{
   if (!poly || !poly->points || (poly->pointcount < 1))
      return;

   if (px1)
      *px1 = poly->lx;
   if (py1)
      *py1 = poly->ty;
   if (px2)
      *px2 = poly->rx;
   if (py2)
      *py2 = poly->by;
}

static double
__imlib_point_delta_from_line(int p_x, int p_y, int s1_x, int s1_y, int s2_x,
                              int s2_y)
{
   if (s2_x - s1_x == 0.0)
      return p_x - s1_x;
   else
     {
        double              m = (double)(s2_y - s1_y) / (double)(s2_x - s1_x);

        return (p_y - s1_y - (double)(p_x - s1_x) * m);
     }
}

static unsigned char
__imlib_segments_intersect(int r1_x, int r1_y, int r2_x, int r2_y, int s1_x,
                           int s1_y, int s2_x, int s2_y)
{
   double              testS1R =
      __imlib_point_delta_from_line(s1_x, s1_y, r1_x, r1_y, r2_x, r2_y);
   double              testS2R =
      __imlib_point_delta_from_line(s2_x, s2_y, r1_x, r1_y, r2_x, r2_y);
   double              testR1S =
      __imlib_point_delta_from_line(r1_x, r1_y, s1_x, s1_y, s2_x, s2_y);
   double              testR2S =
      __imlib_point_delta_from_line(r2_x, r2_y, s1_x, s1_y, s2_x, s2_y);

   /* check if segments are collinear */
   if (testS1R == 0.0 && testS2R == 0.0)
     {
        if (__imlib_point_inside_segment(s1_x, s1_y, r1_x, r1_y, r2_x, r2_y)
            || __imlib_point_inside_segment(s2_x, s2_y, r1_x, r1_y, r2_x, r2_y)
            || __imlib_point_inside_segment(r1_x, r1_y, s1_x, s1_y, s2_x, s2_y)
            || __imlib_point_inside_segment(r2_x, r2_y, s1_x, s1_y, s2_x, s2_y))
           return TRUE;
        else
           return FALSE;
     }

   if (testS1R * testS2R <= 0.0 && testR1S * testR2S <= 0.0)
      return TRUE;
   else
      return FALSE;
}

unsigned char
__imlib_polygon_contains_point(ImlibPoly poly, int x, int y)
{
   int                 count = 0;
   int                 start = 0;
   int                 ysave = 0;       /* initial value arbitrary */
   int                 cx, nx, out_x, out_y, i, n;
   int                 curr_x, curr_y, next_x, next_y;

   /* find a vertex of poly that does not lie on the test line */
   while (start < poly->pointcount && poly->points[start].y == y)
      start++;
   /* if one doesn't exist we will use point on segment test
    * and can start with vertex 0 anyway */
   cx = start % poly->pointcount;

   out_x = poly->points[0].x;
   out_y = y;

   for (i = 1; i < poly->pointcount; i++)
     {
        out_x = MAX(out_x, poly->points[i].x);
     }
   out_x++;                     /* out now guaranteed to be outside poly */

   for (n = 0; n < poly->pointcount; n++)
     {
        nx = (cx + 1) % poly->pointcount;

        curr_x = poly->points[cx].x;
        curr_y = poly->points[cx].y;
        next_x = poly->points[nx].x;
        next_y = poly->points[nx].y;

        if (__imlib_point_on_segment(x, y, curr_x, curr_y, next_x, next_y))
           return TRUE;

        /* ignore horizontal segments from this point on */
        if (poly->points[cx].y != poly->points[nx].y)
          {
             if (__imlib_segments_intersect
                 (curr_x, curr_y, next_x, next_y, x, y, out_x, out_y))
               {
                  count++;

                  if (__imlib_point_on_segment
                      (next_x, next_y, x, y, out_x, out_y))
                    {
                       /* current seg intersects test seg @ 2nd vtx
                        * reset ysave */
                       ysave = curr_y;
                    }
                  if (__imlib_point_on_segment
                      (curr_x, curr_y, x, y, out_x, out_y)
                      && ((ysave < y) != (next_y < y)))
                    {
                       /* current seg xsects test seg @ 1st vtx and
                        * ysave on opposite side of test line from
                        * curr seg 2nd vtx;
                        * decrement hits (2-1) for odd parity */
                       count--;
                    }
               }
          }
        cx = nx;
     }
   return (count % 2 == 1);
}

/** Polygon Drawing and Filling **/

#define STEEP_EDGE	0
#define SHALLOW_EDGE	1
#define HORZ_EDGE	2

typedef struct _PolyEdge PolyEdge;
typedef struct _IndexedValue IndexedValue;

struct _PolyEdge {
   int                 type;
   int                 xx;
   int                 dxx, dyy;
   ImlibPoint         *v0, *v1;
   int                 index;
};

struct _IndexedValue {
   int                 val;
   int                 index;
};

static int
poly_value_sorter(const void *a, const void *b)
{
   IndexedValue       *p, *q;

   p = (IndexedValue *) a;
   q = (IndexedValue *) b;
   if (p->val <= q->val)
      return -1;
   return 1;
}

static int
poly_edge_sorter(const void *a, const void *b)
{
   PolyEdge           *p, *q;

   p = (PolyEdge *) a;
   q = (PolyEdge *) b;
   if (p->xx < q->xx)
      return -1;
   if (p->xx > q->xx)
      return 1;
   if (p->dxx <= q->dxx)
      return -1;
   return 1;
}

/* general macros */

#define DEL_EDGE(j)		\
do {									\
   int m;								\
   for (m = 0; (m < nactive_edges) && (edge[m].index != (j)); m++);	\
									\
   if (m < nactive_edges)						\
     {									\
	if (edge[m].type == HORZ_EDGE) nactive_horz_edges--;		\
	nactive_edges--;						\
	memmove(edge + m, edge + m + 1,					\
              (nactive_edges - m) * sizeof(PolyEdge));			\
     }									\
} while (0)

#define DEL_HORZ_EDGES()		\
do {									\
   int m = 0;								\
									\
   while (m < nactive_edges)						\
     {									\
	if (edge[m].type == HORZ_EDGE)					\
	  {								\
	    nactive_edges--;						\
	    memmove(edge + m, edge + m + 1,				\
              (nactive_edges - m) * sizeof(PolyEdge));			\
	    m--;							\
	  }								\
	m++;								\
     }									\
   nactive_horz_edges = 0;						\
} while (0)

#define ADD_EDGE(i)		\
do {									\
    int m;								\
									\
    for (m = 0; (m < nactive_edges) && (edge[m].index != i); m++);	\
									\
    if ((m == nactive_edges) && (i < nvertices) &&			\
	(nactive_edges < (nvertices - 1)))				\
      {									\
	ImlibPoint *v0, *v1, *w;					\
	PolyEdge *ne;							\
	int  dx, dy;							\
									\
	if (i < (nvertices - 1))  m = i + 1;				\
	else  m = 0;							\
									\
	v0 = (poly->points) + i;					\
	v1 = (poly->points) + m;					\
	if ((v1->y) < (v0->y))						\
	  {								\
	    w = v0;							\
	    v0 = v1;							\
	    v1 = w;							\
	  }								\
									\
	dx = (v1->x) - (v0->x);						\
	dy = (v1->y) - (v0->y);						\
	ne = edge + nactive_edges;					\
	ne->index = i;							\
	if (dy == 0)							\
	  {								\
	    ne->type = HORZ_EDGE;					\
	    ne->dxx = 0;						\
            if ((v1->x) < (v0->x))					\
	      {								\
		w = v0;							\
		v0 = v1;						\
		v1 = w;							\
	      }								\
	    ne->xx = (v0->x) << 16;					\
	    nactive_horz_edges++;					\
	  }								\
	else								\
	  {								\
	    ne->type = STEEP_EDGE;					\
	    ne->dxx = ((dx << 16) / dy);				\
	    ne->xx = ((ne->dxx) * (y - (v0->y))) + ((v0->x) << 16);	\
	    if ((dy < dx) || (dy < (-dx)))				\
	      {								\
		ne->type = SHALLOW_EDGE;				\
		ne->dyy = ((dy << 16) / dx);				\
	      }								\
	  }								\
	ne->v0 = v0;							\
	ne->v1 = v1;							\
	nactive_edges++;						\
      }									\
} while (0)

#define GET_EDGE_RANGE(e, elx, erx)   \
do { \
   switch(e->type)							\
   {									\
    case SHALLOW_EDGE:							\
     {									\
	elx = (e->xx - (2 * e->dxx)) >> 16;				\
	erx = (e->xx + (2 * e->dxx)) >> 16;				\
	if (e->dxx < 0)							\
	  { lx = elx; elx = erx; erx = lx; }				\
	break;								\
     }									\
    case STEEP_EDGE:							\
      {									\
	lx = (e->xx >> 16);						\
	elx = erx = lx;							\
	break;								\
      }									\
    case HORZ_EDGE:							\
      {									\
	elx = e->v0->x;							\
	erx = e->v1->x;							\
	break;								\
      }									\
    default:								\
      break;								\
   }                                                                    \
} while (0)

#define CLIP_SPAN(lx, rx, clx, clrx)     \
do { \
   if (lx < (clx)) lx = (clx);           \
   if (rx > (clrx)) rx = (clrx);         \
} while (0)

#define BLEND_ALPHA(dst, a, tmp) \
do { \
   if (*dst)                                    \
     {                                          \
       tmp = ((a) * (255 - (*(dst)))) + 0x80;   \
       *(dst) += ((tmp + (tmp >> 8)) >> 8);     \
     }                                          \
   else                                         \
       *(dst) = (a);                            \
} while (0)

/* initializing macro used in drawing/filling functions */

#define INIT_POLY()		\
do { \
   sfunc = __imlib_GetShapedSpanDrawFunction(op, dst_alpha, blend);	\
   if (!sfunc)  return;							\
									\
   nvertices = poly->pointcount;					\
   if (nvertices < 1) return;						\
									\
   clrx = clx + clw - 1;						\
   clby = cly + clh - 1;						\
									\
   CLIP_SPAN(clx, clrx, poly->lx, a_a + poly->rx);			\
   if (clrx < clx) return;						\
									\
   CLIP_SPAN(cly, clby, poly->ty, poly->by);				\
   if (clby < cly) return;						\
									\
   clw = clrx - clx + 1;						\
   clh = clby - cly + 1;						\
									\
   edge = (PolyEdge *)malloc(nvertices * sizeof(PolyEdge));		\
   if (!edge) return;							\
									\
   ysort = (IndexedValue *)malloc(nvertices * sizeof(IndexedValue));	\
   if (!ysort) { free(edge); return; }					\
									\
   s0 = (DATA8 *)malloc(clw * sizeof(DATA8));				\
   if (!s0) { free(edge); free(ysort); return; }			\
									\
   s1 = (DATA8 *)malloc(clw * sizeof(DATA8));				\
   if (!s1) { free(edge); free(ysort); free(s0); return; }		\
									\
   memset(s0,0,clw);							\
									\
   k = 0;								\
   while (k < nvertices)						\
     {									\
	ysort[k].val = poly->points[k].y;				\
	ysort[k].index = k;						\
        k++;								\
     }									\
									\
   qsort(ysort, nvertices, sizeof(IndexedValue), poly_value_sorter);	\
									\
   s0 -= clx;								\
   s1 -= clx;								\
									\
   x0 = clx;								\
   x1 = clrx;								\
									\
   nx0 = clrx + 1;							\
   nx1 = clx - 1;							\
									\
   if (cly > poly->ty)							\
      ty = cly - 1;							\
   else									\
      ty = cly;								\
   by = clby;								\
									\
   p = dst + (dstw * ty);						\
   k = 0;								\
   nactive_edges = 0;							\
   nactive_horz_edges = 0;						\
   y = ty;                                                              \
} while (0)

#define DE_INIT_POLY()  \
do { \
   free(edge);        \
   free(ysort);       \
   s0 += clx;         \
   free(s0);          \
   s1 += clx;         \
   free(s1);          \
} while (0)

/** Polygon Drawing **/

/* aliased drawing */
/* draws the poly-line defined by the sequence of vertices */

static void
__imlib_Polygon_DrawToData(ImlibPoly poly, char close, DATA32 color,
                           DATA32 * dst, int dstw,
                           int clx, int cly, int clw, int clh,
                           ImlibOp op, char dst_alpha, char blend)
{
   ImlibShapedSpanDrawFunction sfunc;
   IndexedValue       *ysort;
   PolyEdge           *edge;
   int                 k, a_a = 0;
   int                 nactive_edges, nactive_horz_edges, nvertices;
   int                 clrx, clby, ty, by, y;
   int                 x0, x1, nx0, nx1;
   DATA32             *p;
   DATA8              *s0, *s1, *ps;

   INIT_POLY();

   while (y <= by)
     {
        int                 j;

        while ((k < nvertices) && (poly->points[ysort[k].index].y <= y))
          {
             int                 i = ysort[k].index;

             j = i - 1;
             if (close && (i == 0))
                j = nvertices - 1;

             if (j >= 0)
               {
                  if (poly->points[j].y < y)
                     DEL_EDGE(j);
                  else
                     ADD_EDGE(j);
               }

             j = i + 1;
             if (close && (i == (nvertices - 1)))
                j = 0;

             if (j < nvertices)
               {
                  if (poly->points[j].y < y)
                     DEL_EDGE(i);
                  else
                     ADD_EDGE(i);
               }

             k++;
          }

        /* not really needed, but... */
        qsort(edge, nactive_edges, sizeof(PolyEdge), poly_edge_sorter);

        /* clear alpha buffer */
        if (x0 <= x1)
           memset(s1 + x0, 0, x1 - x0 + 1);

        x0 = nx0;
        x1 = nx1;
        nx0 = clrx + 1;
        nx1 = clx - 1;

        /* draw to alpha buffer */
        j = 0;
        while (j < nactive_edges)
          {
             int                 lx, rx;
             int                 e_lx, e_rx;
             PolyEdge           *e;

             e = edge + j;

             GET_EDGE_RANGE(e, e_lx, e_rx);
             if ((e_lx < e->v0->x) && (e->dxx > 0))
                e_lx = e->v0->x;
             if ((e_rx > e->v0->x) && (e->dxx < 0))
                e_rx = e->v0->x;

             /* draw edge */
             switch (e->type)
               {
               case STEEP_EDGE:
                  {
                     lx = e_lx;
                     lx += (e->xx - (lx << 16)) >> 15;
                     if (IN_SEGMENT(lx, clx, clw))
                       {
                          *(s0 + lx) = 255;
                          if (lx < x0)
                             x0 = lx;
                          if (lx > x1)
                             x1 = lx;
                       }

                     if ((e->v1->y == (y + 1)) && (y < clby))
                       {
                          lx = e->v1->x;
                          if (IN_SEGMENT(lx, clx, clw))
                            {
                               *(s1 + lx) = 255;
                               if (lx < nx0)
                                  nx0 = lx;
                               if (lx > nx1)
                                  nx1 = lx;
                            }
                       }
                     break;
                  }
               case SHALLOW_EDGE:
                  {
                     int                 x, ey, eyy;

                     if (e->dyy > 0)
                        x = e_lx;
                     else
                        x = e_rx;

                     eyy = ((e->v0->y) << 16) + (x - (e->v0->x)) * (e->dyy);
                     ey = eyy >> 16;
                     ey += (eyy - (ey << 16)) >> 15;

                     if (e->dyy > 0)
                       {
                          if (x < x0)
                             x0 = x;
                          while (ey <= y)
                            {
                               if ((ey == y) && IN_SEGMENT(x, clx, clw))
                                  *(s0 + x) = 255;
                               eyy += e->dyy;
                               ey = eyy >> 16;
                               ey += (eyy - (ey << 16)) >> 15;
                               x++;
                            }
                          if (x > x1)
                             x1 = x;

                          if (((y + 1) == e->v1->y) && (y < clby))
                            {
                               if (x < nx0)
                                  nx0 = x;
                               rx = e->v1->x;
                               while ((ey == (y + 1)) && (x <= rx))
                                 {
                                    if (IN_SEGMENT(x, clx, clw))
                                       *(s1 + x) = 255;
                                    eyy += e->dyy;
                                    ey = eyy >> 16;
                                    ey += (eyy - (ey << 16)) >> 15;
                                    x++;
                                 }
                               if (x > nx1)
                                  nx1 = x;
                            }
                          break;
                       }

                     if (x > x1)
                        x1 = x;
                     while (ey <= y)
                       {
                          if ((ey == y) && IN_SEGMENT(x, clx, clw))
                             *(s0 + x) = 255;
                          eyy -= e->dyy;
                          ey = eyy >> 16;
                          ey += (eyy - (ey << 16)) >> 15;
                          x--;
                       }
                     if (x < x0)
                        x0 = x;

                     if (((y + 1) == e->v1->y) && (y < clby))
                       {
                          if (x > nx1)
                             nx1 = x;
                          lx = e->v1->x;
                          while ((ey == (y + 1)) && (x >= lx))
                            {
                               if (IN_SEGMENT(x, clx, clw))
                                  *(s1 + x) = 255;
                               eyy -= e->dyy;
                               ey = eyy >> 16;
                               ey += (eyy - (ey << 16)) >> 15;
                               x--;
                            }
                          if (x < nx0)
                             nx0 = x;
                       }
                     break;
                  }
               case HORZ_EDGE:
                  {
                     lx = e_lx;
                     rx = e_rx;
                     CLIP_SPAN(lx, rx, clx, clrx);
                     if (lx <= rx)
                       {
                          memset(s0 + lx, 255, rx - lx + 1);
                          if (lx < x0)
                             x0 = lx;
                          if (rx > x1)
                             x1 = rx;
                       }
                     break;
                  }

               default:
                  break;
               }

             e->xx += e->dxx;
             j++;
          }

        if (nactive_horz_edges > 0)
           DEL_HORZ_EDGES();

        /* draw alpha buffer to dst */
        CLIP_SPAN(x0, x1, clx, clrx);
        if ((x0 <= x1) && (y >= cly))
           sfunc(s0 + x0, color, p + x0, x1 - x0 + 1);

        /* exchange alpha buffers */
        ps = s0;
        s0 = s1;
        s1 = ps;

        y++;
        p += dstw;
     }

   DE_INIT_POLY();
}

/* anti-aliased drawing */

static void
__imlib_Polygon_DrawToData_AA(ImlibPoly poly, char close, DATA32 color,
                              DATA32 * dst, int dstw,
                              int clx, int cly, int clw, int clh,
                              ImlibOp op, char dst_alpha, char blend)
{
   ImlibShapedSpanDrawFunction sfunc;
   IndexedValue       *ysort;
   PolyEdge           *edge;
   int                 k, a_a = 1;
   int                 nactive_edges, nactive_horz_edges, nvertices;
   int                 clrx, clby, ty, by, y, yy, prev_y, prev_yy;
   int                 x0, x1, nx0, nx1;
   DATA32             *p;
   DATA8              *s0, *s1, *ps;

   INIT_POLY();

   yy = y << 16;
   prev_y = y - 1;
   prev_yy = prev_y << 16;

   while (y <= by)
     {
        int                 j;

        while ((k < nvertices) && (poly->points[ysort[k].index].y <= y))
          {
             int                 i = ysort[k].index;

             j = i - 1;
             if (close && (i == 0))
                j = nvertices - 1;

             if (j >= 0)
               {
                  if (poly->points[j].y < y)
                     DEL_EDGE(j);
                  else
                     ADD_EDGE(j);
               }

             j = i + 1;
             if (close && (i == (nvertices - 1)))
                j = 0;

             if (j < nvertices)
               {
                  if (poly->points[j].y <= y)
                     DEL_EDGE(i);
                  else
                     ADD_EDGE(i);
               }

             k++;
          }

        /* not really needed, but... */
        qsort(edge, nactive_edges, sizeof(PolyEdge), poly_edge_sorter);

        /* clear alpha buffer */
        if (x0 <= x1)
           memset(s1 + x0, 0, x1 - x0 + 1);

        x0 = nx0;
        x1 = nx1;
        nx0 = clrx + 1;
        nx1 = clx - 1;

        /* draw to alpha buffer */
        j = 0;
        while (j < nactive_edges)
          {
             int                 lx, rx;
             int                 e_lx, e_rx;
             PolyEdge           *e;

             e = edge + j;

             GET_EDGE_RANGE(e, e_lx, e_rx);
             if ((e_lx < e->v0->x) && (e->dxx > 0))
                e_lx = e->v0->x;
             if ((e_rx > e->v0->x) && (e->dxx < 0))
                e_rx = e->v0->x;

             /* draw aa edge */
             switch (e->type)
               {
               case STEEP_EDGE:
                  {
                     DATA32              tmp;
                     DATA8               aa;

                     aa = (e->xx - (e_lx << 16)) >> 8;
                     rx = e_lx + 1;
                     if (IN_SEGMENT(rx, clx, clw))
                       {
                          ps = s0 + rx;
                          BLEND_ALPHA(ps, aa, tmp);
                          if (rx > x1)
                             x1 = rx;
                       }
                     if (IN_SEGMENT(e_lx, clx, clw))
                       {
                          aa = 255 - aa;
                          ps = s0 + e_lx;
                          BLEND_ALPHA(ps, aa, tmp);
                          if (e_lx < x0)
                             x0 = e_lx;
                       }

                     if ((e->v1->y == (y + 1)) && (y < clby))
                       {
                          lx = (e->xx + e->dxx) >> 16;
                          aa = ((e->xx + e->dxx - (lx << 16)) >> 8);
                          rx = lx + 1;
                          if (IN_SEGMENT(rx, clx, clw))
                            {
                               ps = s1 + rx;
                               BLEND_ALPHA(ps, aa, tmp);
                               if (rx > nx1)
                                  nx1 = rx;
                            }
                          if (IN_SEGMENT(lx, clx, clw))
                            {
                               aa = 255 - aa;
                               ps = s1 + lx;
                               BLEND_ALPHA(ps, aa, tmp);
                               if (lx < nx0)
                                  nx0 = lx;
                            }
                       }
                     break;
                  }
               case SHALLOW_EDGE:
                  {
                     int                 x, ey, eyy;

                     if (e->dyy > 0)
                        x = e_lx;
                     else
                        x = e_rx;

                     eyy = ((e->v0->y) << 16) + (x - (e->v0->x)) * (e->dyy);
                     ey = eyy >> 16;

                     if (e->dyy > 0)
                       {
                          if (x < x0)
                             x0 = x;
                          while (ey < y)
                            {
                               DATA32              tmp;
                               DATA8               aa;

                               if ((ey == prev_y) && IN_SEGMENT(x, clx, clw))
                                 {
                                    aa = ((eyy - prev_yy) >> 8);
                                    ps = s0 + x;
                                    BLEND_ALPHA(ps, aa, tmp);
                                 }
                               eyy += e->dyy;
                               ey = eyy >> 16;
                               x++;
                            }
                          lx = x;
                          while (ey == y)
                            {
                               DATA32              tmp;
                               DATA8               aa;

                               if (IN_SEGMENT(x, clx, clw))
                                 {
                                    aa = 255 - ((eyy - yy) >> 8);
                                    ps = s0 + x;
                                    BLEND_ALPHA(ps, aa, tmp);
                                 }
                               eyy += e->dyy;
                               ey = eyy >> 16;
                               x++;
                            }
                          if (x > x1)
                             x1 = x;

                          if (((y + 1) == e->v1->y) && (y < clby))
                            {
                               x = lx;
                               if (x < nx0)
                                  nx0 = x;
                               rx = e->v1->x;
                               eyy =
                                  ((e->v0->y) << 16) + (x -
                                                        (e->v0->x)) * (e->dyy);
                               ey = eyy >> 16;
                               while ((ey == y) && (x <= rx))
                                 {
                                    DATA32              tmp;
                                    DATA8               aa;

                                    if (IN_SEGMENT(x, clx, clw))
                                      {
                                         aa = ((eyy - yy) >> 8);
                                         ps = s1 + x;
                                         BLEND_ALPHA(ps, aa, tmp);
                                      }
                                    eyy += e->dyy;
                                    ey = eyy >> 16;
                                    x++;
                                 }
                               if (x > nx1)
                                  nx1 = x;
                            }
                          break;
                       }

                     if (x > x1)
                        x1 = x;
                     while (ey < y)
                       {
                          DATA32              tmp;
                          DATA8               aa;

                          if ((ey == prev_y) && IN_SEGMENT(x, clx, clw))
                            {
                               aa = ((eyy - prev_yy) >> 8);
                               ps = s0 + x;
                               BLEND_ALPHA(ps, aa, tmp);
                            }
                          eyy -= e->dyy;
                          ey = eyy >> 16;
                          x--;
                       }
                     rx = x;
                     while (ey == y)
                       {
                          DATA32              tmp;
                          DATA8               aa;

                          if (IN_SEGMENT(x, clx, clw))
                            {
                               aa = 255 - ((eyy - yy) >> 8);
                               ps = s0 + x;
                               BLEND_ALPHA(ps, aa, tmp);
                            }
                          eyy -= e->dyy;
                          ey = eyy >> 16;
                          x--;
                       }
                     if (x < x0)
                        x0 = x;

                     if (((y + 1) == e->v1->y) && (y < clby))
                       {
                          x = rx;
                          if (x > nx1)
                             nx1 = x;
                          lx = e->v1->x;
                          eyy =
                             ((e->v0->y) << 16) + (x - (e->v0->x)) * (e->dyy);
                          ey = eyy >> 16;
                          while ((ey == y) && (x >= lx))
                            {
                               DATA32              tmp;
                               DATA8               aa;

                               if (IN_SEGMENT(x, clx, clw))
                                 {
                                    aa = ((eyy - yy) >> 8);
                                    ps = s1 + x;
                                    BLEND_ALPHA(ps, aa, tmp);
                                 }
                               eyy -= e->dyy;
                               ey = eyy >> 16;
                               x--;
                            }
                          if (x < nx0)
                             nx0 = x;
                       }
                     break;
                  }
               case HORZ_EDGE:
                  {
                     lx = e_lx;
                     rx = e_rx;
                     CLIP_SPAN(lx, rx, clx, clrx);
                     if (lx <= rx)
                       {
                          memset(s0 + lx, 255, rx - lx + 1);
                          if (lx < x0)
                             x0 = lx;
                          if (rx > x1)
                             x1 = rx;
                       }
                     break;
                  }

               default:
                  break;
               }

             e->xx += e->dxx;
             j++;
          }

        if (nactive_horz_edges > 0)
           DEL_HORZ_EDGES();

        /* draw alpha buffer to dst */
        CLIP_SPAN(x0, x1, clx, clrx);
        if ((x0 <= x1) && (y >= cly))
           sfunc(s0 + x0, color, p + x0, x1 - x0 + 1);

        /* exchange alpha buffers */
        ps = s0;
        s0 = s1;
        s1 = ps;

        prev_yy = yy;
        prev_y = y;

        y++;
        yy = y << 16;

        p += dstw;
     }

   DE_INIT_POLY();
}

void
__imlib_Polygon_DrawToImage(ImlibPoly poly, char close, DATA32 color,
                            ImlibImage * im, int clx, int cly, int clw, int clh,
                            ImlibOp op, char blend, char anti_alias)
{
   if ((!poly) || (!poly->points) || (poly->pointcount < 1) || (clw < 0))
      return;
   if (blend && (!A_VAL(&color)))
      return;
   if (poly->pointcount == 1)
     {
        (void)__imlib_Point_DrawToImage(poly->points[0].x, poly->points[0].y,
                                        color, im, clx, cly, clw, clh, op,
                                        blend, 0);
        return;
     }
   if (poly->pointcount == 2)
     {
        (void)__imlib_Line_DrawToImage(poly->points[0].x, poly->points[0].y,
                                       poly->points[1].x, poly->points[1].y,
                                       color, im, clx, cly, clw, clh, op, blend,
                                       anti_alias, 0);
        return;
     }

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

   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   if (anti_alias)
      __imlib_Polygon_DrawToData_AA(poly, close, color,
                                    im->data, im->w,
                                    clx, cly, clw, clh,
                                    op, IMAGE_HAS_ALPHA(im), blend);
   else
      __imlib_Polygon_DrawToData(poly, close, color,
                                 im->data, im->w,
                                 clx, cly, clw, clh,
                                 op, IMAGE_HAS_ALPHA(im), blend);
}

/** Polygon Filling **/

/* aliased filling */

static void
__imlib_Polygon_FillToData(ImlibPoly poly, DATA32 color,
                           DATA32 * dst, int dstw,
                           int clx, int cly, int clw, int clh,
                           ImlibOp op, char dst_alpha, char blend)
{
   ImlibShapedSpanDrawFunction sfunc;
   IndexedValue       *ysort;
   PolyEdge           *edge;
   int                 k, a_a = 0;
   int                 nactive_edges, nactive_horz_edges, nvertices;
   int                 clrx, clby, ty, by, y;
   int                 x0, x1, nx0, nx1;
   DATA32             *p;
   DATA8              *s0, *s1, *ps;

   INIT_POLY();

   while (y <= by)
     {
        int                 j;

        while ((k < nvertices) && (poly->points[ysort[k].index].y <= y))
          {
             int                 i = ysort[k].index;

             if (i > 0)
                j = i - 1;
             else
                j = nvertices - 1;

             if (poly->points[j].y < y)
                DEL_EDGE(j);
             else
                ADD_EDGE(j);

             if (i < (nvertices - 1))
                j = i + 1;
             else
                j = 0;

             if (poly->points[j].y < y)
                DEL_EDGE(i);
             else
                ADD_EDGE(i);

             k++;
          }

        qsort(edge, nactive_edges, sizeof(PolyEdge), poly_edge_sorter);

        /* clear alpha buffer */
        if (x0 <= x1)
           memset(s1 + x0, 0, x1 - x0 + 1);

        x0 = nx0;
        x1 = nx1;
        nx0 = clrx + 1;
        nx1 = clx - 1;

        /* draw to alpha buffer */
        j = 0;
        while (j < nactive_edges)
          {
             int                 lx, rx;
             int                 le_lx, le_rx;
             int                 re_lx, re_rx;
             PolyEdge           *le, *re;

             if (j < (nactive_edges - 1))
               {
                  le = edge + j;
                  re = le + 1;
               }
             else if (j > 0)
               {
                  le = edge + (j - 1);
                  le->xx -= le->dxx;
                  re = le + 1;
               }
             else
               {
                  re = le = edge;
               }

             GET_EDGE_RANGE(le, le_lx, le_rx);
             if ((le_lx < le->v0->x) && (le->dxx > 0))
                le_lx = le->v0->x;
             GET_EDGE_RANGE(re, re_lx, re_rx);
             if ((re_rx > re->v0->x) && (re->dxx < 0))
                re_rx = re->v0->x;

             /* draw left edge */
             switch (le->type)
               {
               case STEEP_EDGE:
                  {
                     le_rx += (le->xx - (le_rx << 16)) >> 15;
                     if (le_rx < x0)
                        x0 = le_rx;

                     if ((le->v1->y == (y + 1)) && (y < clby))
                       {
                          lx = le->v1->x;
                          if (IN_SEGMENT(lx, clx, clw))
                            {
                               *(s1 + lx) = 255;
                               if (lx < nx0)
                                  nx0 = lx;
                            }
                       }
                     break;
                  }
               case SHALLOW_EDGE:
                  {
                     int                 x, ey, eyy;

                     x = le_lx;
                     eyy = ((le->v0->y) << 16) + (x - (le->v0->x)) * (le->dyy);
                     ey = eyy >> 16;
                     ey += (eyy - (ey << 16)) >> 15;

                     if (le->dyy > 0)
                       {
                          while (ey < y)
                            {
                               eyy += le->dyy;
                               ey = eyy >> 16;
                               ey += (eyy - (ey << 16)) >> 15;
                               x++;
                            }
                          le_rx = x;
                          if (x < x0)
                             x0 = x;

                          if (((y + 1) == le->v1->y) && (y < clby))
                            {
                               if (x < nx0)
                                  nx0 = x;
                               rx = le->v1->x;
                               while ((ey <= (y + 1)) && (x <= rx))
                                 {
                                    if ((ey == (y + 1))
                                        && IN_SEGMENT(x, clx, clw))
                                       *(s1 + x) = 255;
                                    eyy += le->dyy;
                                    ey = eyy >> 16;
                                    ey += (eyy - (ey << 16)) >> 15;
                                    x++;
                                 }
                               if (x > nx1)
                                  nx1 = x;
                            }
                          break;
                       }

                     while (ey > y)
                       {
                          eyy += le->dyy;
                          ey = eyy >> 16;
                          ey += (eyy - (ey << 16)) >> 15;
                          x++;
                       }
                     le_rx = x;
                     if (x < x0)
                        x0 = x;
                     break;
                  }
               case HORZ_EDGE:
                  {
                     lx = le_lx;
                     rx = le_rx;
                     CLIP_SPAN(lx, rx, clx, clrx);
                     if (lx <= rx)
                       {
                          memset(s0 + lx, 255, rx - lx + 1);
                          if (lx < x0)
                             x0 = lx;
                       }
                     le_rx++;
                     break;
                  }

               default:
                  break;
               }

             /* draw right edge */
             switch (re->type)
               {
               case STEEP_EDGE:
                  {
                     re_lx += (re->xx - (re_lx << 16)) >> 15;
                     if (re_lx > x1)
                        x1 = re_lx;

                     if ((re->v1->y == (y + 1)) && (y < clby))
                       {
                          rx = re->v1->x;
                          if (IN_SEGMENT(rx, clx, clw))
                            {
                               *(s1 + rx) = 255;
                               if (rx > nx1)
                                  nx1 = rx;
                            }
                       }
                     break;
                  }
               case SHALLOW_EDGE:
                  {
                     int                 x, ey, eyy;

                     x = re_rx;
                     eyy = ((re->v0->y) << 16) + (x - (re->v0->x)) * (re->dyy);
                     ey = eyy >> 16;
                     ey += (eyy - (ey << 16)) >> 15;

                     if (re->dyy > 0)
                       {
                          while (ey > y)
                            {
                               eyy -= re->dyy;
                               ey = eyy >> 16;
                               ey += (eyy - (ey << 16)) >> 15;
                               x--;
                            }
                          re_lx = x;
                          if (x > x1)
                             x1 = x;
                          break;
                       }

                     while (ey < y)
                       {
                          eyy -= re->dyy;
                          ey = eyy >> 16;
                          ey += (eyy - (ey << 16)) >> 15;
                          x--;
                       }
                     re_lx = x;
                     if (x > x1)
                        x1 = x;

                     if (((y + 1) == re->v1->y) && (y < clby))
                       {
                          if (x > nx1)
                             nx1 = x;
                          lx = re->v1->x;
                          while ((ey <= (y + 1)) && (x >= lx))
                            {
                               if ((ey == (y + 1)) && IN_SEGMENT(x, clx, clw))
                                  *(s1 + x) = 255;
                               eyy -= re->dyy;
                               ey = eyy >> 16;
                               ey += (eyy - (ey << 16)) >> 15;
                               x--;
                            }
                          if (x < nx0)
                             nx0 = x;
                       }
                     break;
                  }
               case HORZ_EDGE:
                  {
                     lx = re_lx;
                     rx = re_rx;
                     CLIP_SPAN(lx, rx, clx, clrx);
                     if (lx <= rx)
                       {
                          memset(s0 + lx, 255, rx - lx + 1);
                          if (rx > x1)
                             x1 = rx;
                       }
                     re_lx--;
                     break;
                  }

               default:
                  break;
               }

             /* draw span between edges */
             lx = le_rx;
             rx = re_lx;
             CLIP_SPAN(lx, rx, clx, clrx);
             if ((lx <= rx) && (y >= cly))
                memset(s0 + lx, 255, rx - lx + 1);

             le->xx += le->dxx;
             if (le != re)
                re->xx += re->dxx;

             j += 2;
          }

        if (nactive_horz_edges > 0)
           DEL_HORZ_EDGES();

        /* draw alpha buffer to dst */
        CLIP_SPAN(x0, x1, clx, clrx);
        if ((x0 <= x1) && (y >= cly))
           sfunc(s0 + x0, color, p + x0, x1 - x0 + 1);

        /* exchange alpha buffers */
        ps = s0;
        s0 = s1;
        s1 = ps;

        y++;
        p += dstw;
     }

   DE_INIT_POLY();
}

/* anti-aliased filling */

static void
__imlib_Polygon_FillToData_AA(ImlibPoly poly, DATA32 color,
                              DATA32 * dst, int dstw,
                              int clx, int cly, int clw, int clh,
                              ImlibOp op, char dst_alpha, char blend)
{
   ImlibShapedSpanDrawFunction sfunc;
   IndexedValue       *ysort;
   PolyEdge           *edge;
   int                 k, a_a = 1;
   int                 nactive_edges, nactive_horz_edges, nvertices;
   int                 clrx, clby, ty, by, y, yy, prev_y, prev_yy;
   int                 x0, x1, nx0, nx1;
   DATA32             *p;
   DATA8              *s0, *s1, *ps;

   INIT_POLY();

   yy = y << 16;
   prev_y = y - 1;
   prev_yy = prev_y << 16;

   while (y <= by)
     {
        int                 j;

        while ((k < nvertices) && (poly->points[ysort[k].index].y <= y))
          {
             int                 i = ysort[k].index;

             if (i > 0)
                j = i - 1;
             else
                j = nvertices - 1;

             if (poly->points[j].y < y)
                DEL_EDGE(j);
             else
                ADD_EDGE(j);

             if (i < (nvertices - 1))
                j = i + 1;
             else
                j = 0;

             if (poly->points[j].y < y)
                DEL_EDGE(i);
             else
                ADD_EDGE(i);

             k++;
          }

        qsort(edge, nactive_edges, sizeof(PolyEdge), poly_edge_sorter);

        /* clear alpha buffer */
        if (x0 <= x1)
           memset(s1 + x0, 0, x1 - x0 + 1);

        x0 = nx0;
        x1 = nx1;
        nx0 = clrx + 1;
        nx1 = clx - 1;
        /* draw to alpha buffer */
        j = 0;
        while (j < nactive_edges)
          {
             int                 lx, rx;
             int                 le_lx, le_rx;
             int                 re_lx, re_rx;
             PolyEdge           *le, *re;

             if (j < (nactive_edges - 1))
               {
                  le = edge + j;
                  re = le + 1;
               }
             else if (j > 0)
               {
                  le = edge + (j - 1);
                  le->xx -= le->dxx;
                  re = le + 1;
               }
             else
               {
                  re = le = edge;
               }

             GET_EDGE_RANGE(le, le_lx, le_rx);
             if ((le_lx < le->v0->x) && (le->dxx > 0))
                le_lx = le->v0->x;
             GET_EDGE_RANGE(re, re_lx, re_rx);
             if ((re_rx > re->v0->x) && (re->dxx < 0))
                re_rx = re->v0->x;

             /* draw left aa edge */
             switch (le->type)
               {
               case STEEP_EDGE:
                  {
                     DATA32              tmp;
                     DATA8               aa;

                     if (le_lx < x0)
                        x0 = le_lx;
                     le_rx = le_lx + 1;

                     if (IN_SEGMENT(le_lx, clx, clw))
                       {
                          aa = 255 - ((le->xx - (le_lx << 16)) >> 8);
                          ps = s0 + le_lx;
                          BLEND_ALPHA(ps, aa, tmp);
                       }

                     if ((le->v1->y == (y + 1)) && (y < clby))
                       {
                          lx = (le->xx + le->dxx) >> 16;
                          if (IN_SEGMENT(lx, clx, clw))
                            {
                               aa =
                                  255 - ((le->xx + le->dxx - (lx << 16)) >> 8);
                               ps = s1 + lx;
                               BLEND_ALPHA(ps, aa, tmp);
                               if (lx < nx0)
                                  nx0 = lx;
                            }
                       }
                     break;
                  }
               case SHALLOW_EDGE:
                  {
                     int                 x, ey, eyy;

                     if (le_lx < x0)
                        x0 = le_lx;
                     x = le_lx;
                     eyy = ((le->v0->y) << 16) + (x - (le->v0->x)) * (le->dyy);
                     ey = eyy >> 16;

                     if (le->dyy > 0)
                       {
                          while (ey < y)
                            {
                               DATA32              tmp;
                               DATA8               aa;

                               if ((ey == prev_y) && IN_SEGMENT(x, clx, clw))
                                 {
                                    aa = ((eyy - prev_yy) >> 8);
                                    ps = s0 + x;
                                    BLEND_ALPHA(ps, aa, tmp);
                                 }
                               eyy += le->dyy;
                               ey = eyy >> 16;
                               x++;
                            }
                          le_rx = x;

                          if (((y + 1) == le->v1->y) && (y < clby))
                            {
                               if (x < nx0)
                                  nx0 = x;
                               rx = le->v1->x;
                               while ((ey == y) && (x <= rx))
                                 {
                                    DATA32              tmp;
                                    DATA8               aa;

                                    if (IN_SEGMENT(x, clx, clw))
                                      {
                                         aa = ((eyy - yy) >> 8);
                                         ps = s1 + x;
                                         BLEND_ALPHA(ps, aa, tmp);
                                      }
                                    eyy += le->dyy;
                                    ey = eyy >> 16;
                                    x++;
                                 }
                            }
                          break;
                       }

                     while (ey >= y)
                       {
                          DATA32              tmp;
                          DATA8               aa;

                          if ((ey == y) && IN_SEGMENT(x, clx, clw))
                            {
                               aa = 255 - ((eyy - yy) >> 8);
                               ps = s0 + x;
                               BLEND_ALPHA(ps, aa, tmp);
                            }
                          eyy += le->dyy;
                          ey = eyy >> 16;
                          x++;
                       }
                     le_rx = x;
                     break;
                  }
               case HORZ_EDGE:
                  {
                     lx = le_lx;
                     rx = le_rx;
                     CLIP_SPAN(lx, rx, clx, clrx);
                     if (lx <= rx)
                       {
                          memset(s0 + lx, 255, rx - lx + 1);
                          if (lx < x0)
                             x0 = lx;
                       }
                     le_rx++;
                     break;
                  }

               default:
                  break;
               }

             /* draw right aa edge */
             switch (re->type)
               {
               case STEEP_EDGE:
                  {
                     DATA32              tmp;
                     DATA8               aa;

                     rx = re_lx + 1;
                     if (rx > x1)
                        x1 = rx;

                     if (IN_SEGMENT(rx, clx, clw))
                       {
                          aa = (re->xx - (re_lx << 16)) >> 8;
                          ps = s0 + rx;
                          BLEND_ALPHA(ps, aa, tmp);
                       }

                     if ((re->v1->y == (y + 1)) && (y < clby))
                       {
                          lx = (re->xx + re->dxx) >> 16;
                          rx = lx + 1;
                          if (IN_SEGMENT(rx, clx, clw))
                            {
                               aa = ((re->xx + re->dxx - (lx << 16)) >> 8);
                               ps = s1 + rx;
                               BLEND_ALPHA(ps, aa, tmp);
                               if (rx > nx1)
                                  nx1 = rx;
                            }
                       }
                     break;
                  }
               case SHALLOW_EDGE:
                  {
                     int                 x, ey, eyy;

                     if (re_rx > x1)
                        x1 = re_rx;
                     x = re_rx;
                     eyy = ((re->v0->y) << 16) + (x - (re->v0->x)) * (re->dyy);
                     ey = eyy >> 16;

                     if (re->dyy > 0)
                       {
                          while (ey >= y)
                            {
                               DATA32              tmp;
                               DATA8               aa;

                               if ((ey == y) && IN_SEGMENT(x, clx, clw))
                                 {
                                    aa = 255 - ((eyy - yy) >> 8);
                                    ps = s0 + x;
                                    BLEND_ALPHA(ps, aa, tmp);
                                 }
                               eyy -= re->dyy;
                               ey = eyy >> 16;
                               x--;
                            }
                          re_lx = x;
                          break;
                       }

                     while (ey < y)
                       {
                          DATA32              tmp;
                          DATA8               aa;

                          if ((ey == prev_y) && IN_SEGMENT(x, clx, clw))
                            {
                               aa = ((eyy - prev_yy) >> 8);
                               ps = s0 + x;
                               BLEND_ALPHA(ps, aa, tmp);
                            }
                          eyy -= re->dyy;
                          ey = eyy >> 16;
                          x--;
                       }
                     re_lx = x;

                     if (((y + 1) == re->v1->y) && (y < clby))
                       {
                          if (x > nx1)
                             nx1 = x;
                          lx = re->v1->x;
                          while ((ey == y) && (x >= lx))
                            {
                               DATA32              tmp;
                               DATA8               aa;

                               if (IN_SEGMENT(x, clx, clw))
                                 {
                                    aa = ((eyy - yy) >> 8);
                                    ps = s1 + x;
                                    BLEND_ALPHA(ps, aa, tmp);
                                 }
                               eyy -= re->dyy;
                               ey = eyy >> 16;
                               x--;
                            }
                       }
                     break;
                  }
               case HORZ_EDGE:
                  {
                     lx = re_lx;
                     rx = re_rx;
                     CLIP_SPAN(lx, rx, clx, clrx);
                     if (lx <= rx)
                       {
                          memset(s0 + lx, 255, rx - lx + 1);
                          if (rx > x1)
                             x1 = rx;
                       }
                     re_lx--;
                     break;
                  }

               default:
                  break;
               }

             /* draw span between edges */
             lx = le_rx;
             rx = re_lx;
             CLIP_SPAN(lx, rx, clx, clrx);
             if ((lx <= rx) && (y >= cly))
                memset(s0 + lx, 255, rx - lx + 1);

             le->xx += le->dxx;
             if (le != re)
                re->xx += re->dxx;

             j += 2;
          }

        if (nactive_horz_edges > 0)
           DEL_HORZ_EDGES();

        /* draw alpha buffer to dst */
        CLIP_SPAN(x0, x1, clx, clrx);
        if ((x0 <= x1) && (y >= cly))
           sfunc(s0 + x0, color, p + x0, x1 - x0 + 1);

        /* exchange alpha buffers */
        ps = s0;
        s0 = s1;
        s1 = ps;

        prev_yy = yy;
        prev_y = y;

        y++;
        yy = y << 16;

        p += dstw;
     }

   DE_INIT_POLY();
}

void
__imlib_Polygon_FillToImage(ImlibPoly poly, DATA32 color,
                            ImlibImage * im, int clx, int cly, int clw, int clh,
                            ImlibOp op, char blend, char anti_alias)
{
   if ((!poly) || (!poly->points) || (poly->pointcount < 1) || (clw < 0))
      return;
   if (blend && (!A_VAL(&color)))
      return;
   if (poly->pointcount == 1)
     {
        (void)__imlib_Point_DrawToImage(poly->points[0].x, poly->points[0].y,
                                        color, im, clx, cly, clw, clh, op,
                                        blend, 0);
        return;
     }
   if (poly->pointcount == 2)
     {
        (void)__imlib_Line_DrawToImage(poly->points[0].x, poly->points[0].y,
                                       poly->points[1].x, poly->points[1].y,
                                       color, im, clx, cly, clw, clh, op, blend,
                                       anti_alias, 0);
        return;
     }

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

   if (blend && IMAGE_HAS_ALPHA(im))
      __imlib_build_pow_lut();

   if (anti_alias)
      __imlib_Polygon_FillToData_AA(poly, color,
                                    im->data, im->w,
                                    clx, cly, clw, clh,
                                    op, IMAGE_HAS_ALPHA(im), blend);
   else
      __imlib_Polygon_FillToData(poly, color,
                                 im->data, im->w,
                                 clx, cly, clw, clh,
                                 op, IMAGE_HAS_ALPHA(im), blend);
}
