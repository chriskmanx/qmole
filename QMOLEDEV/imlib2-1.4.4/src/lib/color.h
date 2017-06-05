#ifndef __COLOR
#define __COLOR 1

#ifdef BUILD_X11

extern DATA16       _max_colors;

__hidden int        __imlib_XActualDepth(Display * d, Visual * v);
__hidden Visual    *__imlib_BestVisual(Display * d, int screen,
                                       int *depth_return);
__hidden DATA8     *__imlib_AllocColorTable(Display * d, Colormap cmap,
                                            DATA8 * type_return, Visual * v);
__hidden DATA8     *__imlib_AllocColors332(Display * d, Colormap cmap,
                                           Visual * v);
__hidden DATA8     *__imlib_AllocColors666(Display * d, Colormap cmap,
                                           Visual * v);
__hidden DATA8     *__imlib_AllocColors232(Display * d, Colormap cmap,
                                           Visual * v);
__hidden DATA8     *__imlib_AllocColors222(Display * d, Colormap cmap,
                                           Visual * v);
__hidden DATA8     *__imlib_AllocColors221(Display * d, Colormap cmap,
                                           Visual * v);
__hidden DATA8     *__imlib_AllocColors121(Display * d, Colormap cmap,
                                           Visual * v);
__hidden DATA8     *__imlib_AllocColors111(Display * d, Colormap cmap,
                                           Visual * v);
__hidden DATA8     *__imlib_AllocColors1(Display * d, Colormap cmap,
                                         Visual * v);

#endif

#endif
