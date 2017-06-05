#ifndef __DRAW
#define __DRAW 1

__hidden char       __imlib_CreatePixmapsForImage(Display * d, Drawable w,
                                                  Visual * v, int depth,
                                                  Colormap cm, ImlibImage * im,
                                                  Pixmap * p, Mask * m, int sx,
                                                  int sy, int sw, int sh,
                                                  int dw, int dh,
                                                  char anitalias, char hiq,
                                                  char dither_mask, int mat,
                                                  ImlibColorModifier * cmod);

#endif
