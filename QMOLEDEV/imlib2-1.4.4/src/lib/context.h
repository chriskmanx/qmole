#ifndef __CONTEXT
#define __CONTEXT 1

typedef struct _context Context;

struct _context {
   int                 last_use;
   Display            *display;
   Visual             *visual;
   Colormap            colormap;
   int                 depth;
   Context            *next;

   DATA8              *palette;
   DATA8               palette_type;
   void               *r_dither;
   void               *g_dither;
   void               *b_dither;
};

__hidden void       __imlib_SetMaxContexts(int num);
__hidden int        __imlib_GetMaxContexts(void);
__hidden void       __imlib_FlushContexts(void);
__hidden void       __imlib_FreeContextForDisplay(Display * d);
__hidden void       __imlib_FreeContextForColormap(Display * d, Colormap cm);
__hidden void       __imlib_FreeContextForVisual(Display * d, Visual * v);
__hidden Context   *__imlib_FindContext(Display * d, Visual * v, Colormap c,
                                        int depth);
__hidden Context   *__imlib_NewContext(Display * d, Visual * v, Colormap c,
                                       int depth);
__hidden Context   *__imlib_GetContext(Display * d, Visual * v, Colormap c,
                                       int depth);

#endif
