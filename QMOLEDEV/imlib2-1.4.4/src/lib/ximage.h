#ifndef __XIMAGE
#define __XIMAGE 1

#include <X11/Xlib.h>
#include <X11/extensions/XShm.h>

__hidden extern signed char x_does_shm;

void                __imlib_SetMaxXImageCount(Display * d, int num);
__hidden int        __imlib_GetMaxXImageCount(Display * d);
__hidden void       __imlib_SetMaxXImageTotalSize(Display * d, int num);
__hidden int        __imlib_GetMaxXImageTotalSize(Display * d);
__hidden void       __imlib_FlushXImage(Display * d);
__hidden void       __imlib_ConsumeXImage(Display * d, XImage * xim);
__hidden XImage    *__imlib_ProduceXImage(Display * d, Visual * v, int depth,
                                          int w, int h, char *shared);
__hidden void       __imlib_ShmCheck(Display * d);
__hidden XImage    *__imlib_ShmGetXImage(Display * d, Visual * v, Drawable draw,
                                         int depth, int x, int y, int w, int h,
                                         XShmSegmentInfo * si);
__hidden void       __imlib_ShmDetach(Display * d, XShmSegmentInfo * si);

#endif
