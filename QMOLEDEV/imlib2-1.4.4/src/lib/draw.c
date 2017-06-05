#include "common.h"
#ifdef BUILD_X11
#include <X11/Xlib.h>
#include <X11/extensions/XShm.h>
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include "rend.h"
#include "draw.h"

char
__imlib_CreatePixmapsForImage(Display * d, Drawable w, Visual * v, int depth,
                              Colormap cm, ImlibImage * im, Pixmap * p,
                              Mask * m, int sx, int sy, int sw, int sh, int dw,
                              int dh, char antialias, char hiq,
                              char dither_mask, int mat,
                              ImlibColorModifier * cmod)
{
   ImlibImagePixmap   *ip = NULL;
   Pixmap              pmap = 0;
   Pixmap              mask = 0;
   long long           mod_count = 0;

   if (cmod)
      mod_count = cmod->modification_count;
   ip = __imlib_FindCachedImagePixmap(im, dw, dh, d, v, depth, sx, sy,
                                      sw, sh, cm, antialias, hiq, dither_mask,
                                      mod_count);
   if (ip)
     {
        if (p)
           *p = ip->pixmap;
        if (m)
           *m = ip->mask;
        ip->references++;
#ifdef DEBUG_CACHE
        fprintf(stderr,
                "[Imlib2]  Match found in cache.  Reference count is %d, pixmap 0x%08x, mask 0x%08x\n",
                ip->references, ip->pixmap, ip->mask);
#endif
        return 2;
     }
   if (p)
     {
        pmap = XCreatePixmap(d, w, dw, dh, depth);
        *p = pmap;
     }
   if (m)
     {
        if (IMAGE_HAS_ALPHA(im))
           mask = XCreatePixmap(d, w, dw, dh, 1);
        *m = mask;
     }
   __imlib_RenderImage(d, im, pmap, mask, v, cm, depth, sx, sy, sw, sh, 0, 0,
                       dw, dh, antialias, hiq, 0, dither_mask, mat, cmod,
                       OP_COPY);
   ip = __imlib_ProduceImagePixmap();
   ip->visual = v;
   ip->depth = depth;
   ip->image = im;
   if (im->file)
      ip->file = strdup(im->file);
   ip->border.left = im->border.left;
   ip->border.right = im->border.right;
   ip->border.top = im->border.top;
   ip->border.bottom = im->border.bottom;
   ip->colormap = cm;
   ip->display = d;
   ip->w = dw;
   ip->h = dh;
   ip->source_x = sx;
   ip->source_y = sy;
   ip->source_w = sw;
   ip->source_h = sh;
   ip->antialias = antialias;
   ip->modification_count = mod_count;
   ip->dither_mask = dither_mask;
   ip->hi_quality = hiq;
   ip->references = 1;
   ip->pixmap = pmap;
   ip->mask = mask;
   __imlib_AddImagePixmapToCache(ip);
#ifdef DEBUG_CACHE
   fprintf(stderr,
           "[Imlib2]  Created pixmap.  Reference count is %d, pixmap 0x%08x, mask 0x%08x\n",
           ip->references, ip->pixmap, ip->mask);
#endif
   return 1;
}

#endif /* BUILD_X11 */
