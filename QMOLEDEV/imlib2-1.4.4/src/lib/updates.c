#include "common.h"
#include "updates.h"

enum _t_used {
   T_UNUSED = 0,
   T_USED = 1
};

struct _tile {
   enum _t_used        used;
};

#define TBITS 5
#define TB TBITS
#define TM ((1 << TBITS) - 1)
#define TS (1 << TBITS)

#define T(x, y) t[((y) * tw) + (x)]

ImlibUpdate        *
__imlib_MergeUpdate(ImlibUpdate * u, int w, int h, int hgapmax)
{
   ImlibUpdate        *nu = NULL, *uu;
   struct _tile       *t;
   int                 tw, th, x, y, i;
   int                *gaps = NULL;

   /* if theres no rects to process.. return NULL */
   if (!u)
      return NULL;
   tw = w >> TB;
   if (w & TM)
      tw++;
   th = h >> TB;
   if (h & TM)
      th++;
   t = malloc(tw * th * sizeof(struct _tile));
   /* fill in tiles to be all not used */
   for (i = 0, y = 0; y < th; y++)
     {
        for (x = 0; x < tw; x++)
           t[i++].used = T_UNUSED;
     }
   /* fill in all tiles */
   for (uu = u; uu; uu = uu->next)
     {
        CLIP(uu->x, uu->y, uu->w, uu->h, 0, 0, w, h);
        for (y = uu->y >> TB; y <= ((uu->y + uu->h - 1) >> TB); y++)
          {
             for (x = uu->x >> TB; x <= ((uu->x + uu->w - 1) >> TB); x++)
                T(x, y).used = T_USED;
          }
     }
   /* scan each line - if > hgapmax gaps between tiles, then fill smallest */
   gaps = malloc(tw * sizeof(int));
   for (y = 0; y < th; y++)
     {
        int                 hgaps = 0, start = -1, min;
        char                have = 1, gap = 0;

        for (x = 0; x < tw; x++)
           gaps[x] = 0;
        for (x = 0; x < tw; x++)
          {
             if ((have) && (T(x, y).used == T_UNUSED))
               {
                  start = x;
                  gap = 1;
                  have = 0;
               }
             else if ((!have) && (gap) && (T(x, y).used & T_USED))
               {
                  gap = 0;
                  hgaps++;
                  have = 1;
                  gaps[start] = x - start;
               }
             else if (T(x, y).used & T_USED)
                have = 1;
          }
        while (hgaps > hgapmax)
          {
             start = -1;
             min = tw;

             for (x = 0; x < tw; x++)
               {
                  if ((gaps[x] > 0) && (gaps[x] < min))
                    {
                       start = x;
                       min = gaps[x];
                    }
               }
             if (start >= 0)
               {
                  gaps[start] = 0;
                  for (x = start;
                       T(x, y).used == T_UNUSED; T(x++, y).used = T_USED);
                  hgaps--;
               }
          }
     }
   free(gaps);
   /* coalesce tiles into larger blocks and make new rect list */
   for (y = 0; y < th; y++)
     {
        for (x = 0; x < tw; x++)
          {
             if (T(x, y).used & T_USED)
               {
                  int                 xx, yy, ww, hh, ok;

                  for (xx = x + 1, ww = 1;
                       (T(xx, y).used & T_USED) && (xx < tw); xx++, ww++);
                  for (yy = y + 1, hh = 1, ok = 1;
                       (yy < th) && (ok); yy++, hh++)
                    {
                       for (xx = x; xx < (x + ww); xx++)
                         {
                            if (!(T(xx, yy).used & T_USED))
                              {
                                 ok = 0;
                                 xx = x + ww;
                                 hh--;
                              }
                         }
                    }
                  for (yy = y; yy < (y + hh); yy++)
                    {
                       for (xx = x; xx < (x + ww); xx++)
                          T(xx, yy).used = T_UNUSED;
                    }
                  nu = __imlib_AddUpdate(nu, (x << TB), (y << TB),
                                         (ww << TB), (hh << TB));
               }
          }
     }
   free(t);
   __imlib_FreeUpdates(u);
   return nu;
}

ImlibUpdate        *
__imlib_AddUpdate(ImlibUpdate * u, int x, int y, int w, int h)
{
   ImlibUpdate        *nu;

   if ((w < 1) || (h < 1) || ((x + w) < 1) || ((y + h) < 1))
      return u;
   nu = malloc(sizeof(ImlibUpdate));
   nu->x = x;
   nu->y = y;
   nu->w = w;
   nu->h = h;
   nu->next = u;
   return nu;
}

void
__imlib_FreeUpdates(ImlibUpdate * u)
{
   ImlibUpdate        *uu;

   uu = u;
   while (uu)
     {
        u = uu;
        uu = uu->next;
        free(u);
     }
}

ImlibUpdate        *
__imlib_DupUpdates(ImlibUpdate * u)
{
   ImlibUpdate        *uu, *cu, *pu, *ru;

   if (!u)
      return NULL;

   uu = malloc(sizeof(ImlibUpdate));
   memcpy(uu, u, sizeof(ImlibUpdate));
   cu = u->next;
   pu = u;
   ru = uu;
   while (cu)
     {
        uu = malloc(sizeof(ImlibUpdate));
        memcpy(uu, u, sizeof(ImlibUpdate));
        pu->next = uu;
        pu = cu;
        cu = cu->next;
     }
   return ru;
}
