#include "common.h"
#ifdef BUILD_X11
#include <X11/Xlib.h>
#include "image.h"
#include "context.h"
#include "color.h"
#include "rgba.h"

static Context     *context = NULL;
static int          max_context_count = 128;
static int          context_counter = 0;

void
__imlib_SetMaxContexts(int num)
{
   max_context_count = num;
   __imlib_FlushContexts();
}

int
__imlib_GetMaxContexts(void)
{
   return max_context_count;
}

void
__imlib_FlushContexts(void)
{
   Context            *ct, *pct, *ctt;

   ct = context;
   pct = NULL;
   while (ct)
     {
        ctt = ct;
        ct = ct->next;
        /* it hasnt been referenced in the last max_context_count refernces */
        /* thus old and getrid of it */
        if (ctt->last_use < (context_counter - max_context_count))
          {
             if (pct)
                context = ctt->next;
             else
                pct->next = ctt->next;
             if (ctt->palette)
               {
                  int                 i, num[] = { 256, 128, 64, 32, 16, 8, 1 };
                  unsigned long       pixels[256];

                  for (i = 0; i < num[ctt->palette_type]; i++)
                     pixels[i] = (unsigned long)ctt->palette[i];
                  XFreeColors(ctt->display, ctt->colormap, pixels,
                              num[ctt->palette_type], 0);

                  free(ctt->palette);
                  free(ctt->r_dither);
                  free(ctt->g_dither);
                  free(ctt->b_dither);
               }
             else if (ctt->r_dither)
               {
                  free(ctt->r_dither);
                  free(ctt->g_dither);
                  free(ctt->b_dither);
               }
             free(ctt);
          }
        else
           pct = ctt;
     }
}

void
__imlib_FreeContextForDisplay(Display * d)
{
   Context            *ct;

   ct = context;
   while (ct)
     {
        if (ct->display == d)
           ct->last_use = -(max_context_count * 2);
        ct = ct->next;
     }
   __imlib_FlushContexts();
}

void
__imlib_FreeContextForColormap(Display * d, Colormap cm)
{
   Context            *ct;

   ct = context;
   while (ct)
     {
        if ((ct->display == d) && (ct->colormap == cm))
           ct->last_use = -(max_context_count * 2);
        ct = ct->next;
     }
   __imlib_FlushContexts();
}

void
__imlib_FreeContextForVisual(Display * d, Visual * v)
{
   Context            *ct;

   ct = context;
   while (ct)
     {
        if ((ct->display == d) && (ct->visual == v))
           ct->last_use = -(max_context_count * 2);
        ct = ct->next;
     }
   __imlib_FlushContexts();
}

Context            *
__imlib_FindContext(Display * d, Visual * v, Colormap c, int depth)
{
   Context            *ct, *pct;

   pct = NULL;
   ct = context;
   while (ct)
     {
        if ((ct->display == d) && (ct->visual == v) &&
            (ct->colormap == c) && (ct->depth == depth))
          {
             if (pct)
               {
                  pct->next = ct->next;
                  ct->next = context;
                  context = ct;
               }
             return ct;
          }
        pct = ct;
        ct = ct->next;
     }
   return NULL;
}

Context            *
__imlib_NewContext(Display * d, Visual * v, Colormap c, int depth)
{
   Context            *ct;

   context_counter++;
   ct = malloc(sizeof(Context));
   ct->last_use = context_counter;
   ct->display = d;
   ct->visual = v;
   ct->colormap = c;
   ct->depth = depth;
   ct->next = NULL;

   if (depth <= 8)
     {
        ct->palette = __imlib_AllocColorTable(d, c, &(ct->palette_type), v);
        ct->r_dither = malloc(sizeof(DATA8) * DM_X * DM_Y * 256);
        ct->g_dither = malloc(sizeof(DATA8) * DM_X * DM_Y * 256);
        ct->b_dither = malloc(sizeof(DATA8) * DM_X * DM_Y * 256);
        __imlib_RGBA_init((void *)ct->r_dither, (void *)ct->g_dither,
                          (void *)ct->b_dither, depth, ct->palette_type);
     }
   else
     {
        ct->palette = NULL;
        ct->palette_type = 0;
        if ((depth > 8) && (depth <= 16))
          {
             ct->r_dither = malloc(sizeof(DATA16) * 4 * 4 * 256);
             ct->g_dither = malloc(sizeof(DATA16) * 4 * 4 * 256);
             ct->b_dither = malloc(sizeof(DATA16) * 4 * 4 * 256);
             __imlib_RGBA_init((void *)ct->r_dither, (void *)ct->g_dither,
                               (void *)ct->b_dither, depth, 0);
          }
        else
          {
             ct->r_dither = NULL;
             ct->g_dither = NULL;
             ct->b_dither = NULL;
             __imlib_RGBA_init((void *)ct->r_dither, (void *)ct->g_dither,
                               (void *)ct->b_dither, depth, 0);
          }
     }
   return ct;
}

Context            *
__imlib_GetContext(Display * d, Visual * v, Colormap c, int depth)
{
   Context            *ct;

   ct = __imlib_FindContext(d, v, c, depth);
   if (ct)
     {
        ct->last_use = context_counter;
        return ct;
     }
   ct = __imlib_NewContext(d, v, c, depth);
   ct->next = context;
   context = ct;
   __imlib_FlushContexts();
   return ct;
}

#endif /* BUILD_X11 */
