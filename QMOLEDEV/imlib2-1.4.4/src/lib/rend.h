#ifndef __REND
#define __REND 1

__hidden DATA32     __imlib_RenderGetPixel(Display * d, Drawable w, Visual * v,
                                           Colormap cm, int depth, DATA8 r,
                                           DATA8 g, DATA8 b);

__hidden void       __imlib_RenderDisconnect(Display * d);

__hidden void       __imlib_RenderImage(Display * d, ImlibImage * im,
                                        Drawable w, Drawable m,
                                        Visual * v, Colormap cm, int depth,
                                        int sx, int sy, int sw, int sh,
                                        int dx, int dy, int dw, int dh,
                                        char anitalias, char hiq, char blend,
                                        char dither_mask, int mat,
                                        ImlibColorModifier * cmod, ImlibOp op);

__hidden void       __imlib_RenderImageSkewed(Display * d, ImlibImage * im,
                                              Drawable w, Drawable m,
                                              Visual * v, Colormap cm,
                                              int depth, int sx, int sy, int sw,
                                              int sh, int dx, int dy, int hsx,
                                              int hsy, int vsx, int vsy,
                                              char antialias, char hiq,
                                              char blend, char dither_mask,
                                              int mat,
                                              ImlibColorModifier * cmod,
                                              ImlibOp op);

#endif
