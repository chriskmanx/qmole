#ifndef __UPDATES
#define __UPDATES 1
typedef struct _imlibupdate ImlibUpdate;

struct _imlibupdate
{
   int x, y, w, h;
   ImlibUpdate *next;
};

__hidden ImlibUpdate *__imlib_AddUpdate(ImlibUpdate *u, int x, int y, int w, int h);
__hidden ImlibUpdate *__imlib_MergeUpdate(ImlibUpdate *u, int w, int h, int hgapmax);
__hidden void __imlib_FreeUpdates(ImlibUpdate *u);
__hidden ImlibUpdate *__imlib_DupUpdates(ImlibUpdate *u);

#endif
