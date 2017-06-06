/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _PIXMAP_H
# define _PIXMAP_H

#include <X11/Xatom.h>
#ifdef PIXMAP_SUPPORT
# include <Imlib2.h>
#elif !defined(DATA64)
typedef struct {
  int left, top, right, bottom;
} Imlib_Border;
typedef void *Imlib_Image;
typedef void *Imlib_Color_Modifier;
#endif

#include "misc.h"

/************ Macros and Definitions ************/
#ifdef PIXMAP_SUPPORT
# define background_is_image()    ((buffer_pixmap) || (images[image_bg].current && images[image_bg].current->iml && images[image_bg].current->iml->im))
# define background_is_trans()    (images[image_bg].mode & MODE_TRANS)
# define background_is_viewport() (images[image_bg].mode & MODE_VIEWPORT)
# define background_is_auto()     (images[image_bg].mode & MODE_AUTO)
# define background_is_pixmap()   (background_is_image() || (images[image_bg].mode & (MODE_TRANS | MODE_VIEWPORT | MODE_AUTO)))
# define delete_simage(simg)      do { \
                                    IMLIB_FREE_PIXMAP((simg)->pmap->pixmap); \
                                    imlib_context_set_image((simg)->iml->im); \
                                    imlib_free_image_and_decache(); \
                                    (simg)->pmap->pixmap = None; (simg)->iml->im = NULL; \
                                  } while (0)
# define CONVERT_SHADE(s)         (0xff - (((s) * 0xff) / 100))
# define CONVERT_TINT_RED(t)      (((t) & 0xff0000) >> 16)
# define CONVERT_TINT_GREEN(t)    (((t) & 0x00ff00) >> 8)
# define CONVERT_TINT_BLUE(t)     ((t) & 0x0000ff)
#else
# define background_is_image()    (0)
# define background_is_trans()    (0)
# define background_is_viewport() (0)
# define background_is_auto()     (0)
# define background_is_pixmap()   (0)
# define get_image_type_string(t) ((char *) "")
# define delete_simage(simg)      NOP
#endif
#define LIBAST_X_CREATE_PIXMAP(w, h)  X_CREATE_PIXMAP(Xdisplay, (TermWin.parent ? TermWin.parent : Xroot), (w), (h), Xdepth)
#define LIBAST_X_FREE_PIXMAP(p)       X_FREE_PIXMAP(Xdisplay, p)
#define LIBAST_X_CREATE_GC(f, gcv)    X_CREATE_GC(Xdisplay, (TermWin.parent ? TermWin.parent : Xroot), (f), (gcv))
#define LIBAST_X_FREE_GC(gc)          X_FREE_GC(Xdisplay, gc)

#define GEOM_LEN 19

enum {
  image_bg,
  image_up,
  image_down,
  image_left,
  image_right,
  image_sb,
  image_sa,
  image_st,
  image_menu,
  image_menuitem,
  image_submenu,
  image_button,
  image_bbar,
  image_gbar,
  image_dialog,
  image_max
};

/* Image manipulation operations */
#define OP_NONE		0x00
#define OP_TILE		0x01
#define OP_HSCALE	0x02
#define OP_VSCALE	0x04
#define OP_PROPSCALE	0x08
#define OP_SCALE	(OP_HSCALE | OP_VSCALE)

/* Image modes */
#define MODE_SOLID	0x00
#define MODE_IMAGE	0x01
#define MODE_TRANS	0x02
#define MODE_VIEWPORT	0x04
#define MODE_AUTO	0x08
#define MODE_MASK	0x0f
#define ALLOW_SOLID	0x00
#define ALLOW_IMAGE	0x10
#define ALLOW_TRANS	0x20
#define ALLOW_VIEWPORT	0x40
#define ALLOW_AUTO	0x80
#define ALLOW_MASK	0xf0

/* Image states */
#define IMAGE_STATE_CURRENT   (0)
#define IMAGE_STATE_NORMAL    (1)
#define IMAGE_STATE_SELECTED  (2)
#define IMAGE_STATE_CLICKED   (3)
#define IMAGE_STATE_DISABLED  (4)

/* Render options */
#define RENDER_NORMAL         (0)
#define RENDER_FORCE_PIXMAP   (1 << 0)

/* Helper macros */
#define FOREACH_IMAGE(x)                  do {unsigned char idx; for (idx = 0; idx < image_max; idx++) { x } } while (0)
#define image_set_mode(which, bit)        do {images[which].mode &= ~(MODE_MASK); images[which].mode |= (bit);} while (0)
#define image_allow_mode(which, bit)      (images[which].mode |= (bit))
#define image_disallow_mode(which, bit)   (images[which].mode &= ~(bit))
#define image_mode_is(which, bit)         (images[which].mode & (bit))
#define image_mode_fallback(which)        do {if (image_mode_is((which), ALLOW_IMAGE)) {image_set_mode((which), MODE_IMAGE);} else {image_set_mode((which), MODE_SOLID);}} while (0)
#define redraw_all_images()               do {render_simage(images[image_bg].current, TermWin.vt, TermWin_TotalWidth(), TermWin_TotalHeight(), image_bg, 0); \
                                              scr_touch(); scrollbar_draw(IMAGE_STATE_CURRENT, MODE_MASK); if (image_mode_any(MODE_AUTO)) enl_ipc_sync(); \
                                          } while (0)
#define reload_image(iml)                 do {Imlib_Image tmp_im; \
                                              if ((iml) && ((iml)->im)) { \
                                                imlib_context_set_image((iml)->im); \
                                                tmp_im = imlib_load_image_immediately(imlib_image_get_filename()); \
                                                imlib_free_image_and_decache(); (iml)->im = tmp_im; \
                                              } \
                                          } while (0)

/* Elements of an simage to be reset */
#define RESET_NONE		(0UL)
#define RESET_IMLIB_MOD		(1UL << 0)
#define RESET_IMLIB_RMOD	(1UL << 1)
#define RESET_IMLIB_GMOD	(1UL << 2)
#define RESET_IMLIB_BMOD	(1UL << 3)
#define RESET_ALL_TINT		(RESET_IMLIB_RMOD | RESET_IMLIB_GMOD | RESET_IMLIB_BMOD)
#define RESET_ALL_MOD		(RESET_IMLIB_MOD | RESET_IMLIB_RMOD | RESET_IMLIB_GMOD | RESET_IMLIB_BMOD)
#define RESET_IMLIB_BORDER	(1UL << 4)
#define RESET_IMLIB_BEVEL	(1UL << 5)
#define RESET_IMLIB_PAD 	(1UL << 6)
#define RESET_IMLIB_IM		(1UL << 7)
#define RESET_ALL_IMLIB		(RESET_ALL_MOD | RESET_IMLIB_BORDER | RESET_IMLIB_BEVEL | RESET_IMLIB_PAD | RESET_IMLIB_IM)
#define RESET_PMAP_GEOM		(1UL << 8)
#define RESET_PMAP_PIXMAP	(1UL << 9)
#define RESET_PMAP_MASK		(1UL << 10)
#define RESET_ALL_PMAP		(RESET_PMAP_GEOM | RESET_PMAP_PIXMAP | RESET_PMAP_MASK)
#define RESET_ALL_SIMG		(RESET_ALL_IMLIB | RESET_ALL_PMAP)
#define RESET_NORM              (1UL << 11)
#define RESET_SELECTED          (1UL << 12)
#define RESET_CLICKED           (1UL << 13)
#define RESET_DISABLED          (1UL << 14)
#define RESET_MODE              (1UL << 15)
#define RESET_ALL               (RESET_NORM | RESET_SELECTED | RESET_CLICKED | RESET_DISABLED | RESET_MODE)

/************ Structures ************/
typedef struct {
  unsigned short op;
  short w, h, x, y;
  Pixmap pixmap;
  Pixmap mask;
} pixmap_t;
typedef struct {
  Imlib_Border *edges;
  unsigned char up;
} bevel_t;
typedef struct cmod_struct {
  unsigned short gamma, brightness, contrast;
  Imlib_Color_Modifier imlib_mod;
} colormod_t;
typedef struct {
  Imlib_Image im;
  Imlib_Border *border, *pad;
  bevel_t *bevel;
  colormod_t *mod, *rmod, *gmod, *bmod;
  short last_w, last_h;
} imlib_t;
typedef struct {
  pixmap_t *pmap;
  imlib_t *iml;
  Pixel fg, bg;
} simage_t;
typedef struct {
  Window win;
  unsigned char mode, userdef;
  simage_t *norm, *selected, *clicked, *disabled, *current;
} image_t;
typedef short renderop_t;

/************ Variables ************/
extern image_t images[image_max];
extern Pixmap desktop_pixmap, viewport_pixmap, buffer_pixmap;
extern Window desktop_window;

/************ Function Prototypes ************/
#ifndef PIXMAP_SUPPORT
# define free_simage(s)                              NOP
# define create_simage()                             ((simage_t *) NULL)
# define load_image(f, s)                            ((unsigned char) 0)
# define check_image_ipc(w)                          ((unsigned char) 0)
# define redraw_image(w)                             NOP
# define redraw_images_by_mode(w)                    NOP
# define paste_simage(s, which, win, d, x, y, w, h)  NOP
# define set_icon_pixmap(f, h)                       NOP
#endif

_XFUNCPROTOBEGIN

extern const char *get_image_type(unsigned char);
extern unsigned char image_mode_any(unsigned char);
#ifdef PIXMAP_SUPPORT
extern const char *imlib_strerror(Imlib_Load_Error);
extern unsigned short parse_pixmap_ops(char *);
extern unsigned short set_pixmap_scale(const char *, pixmap_t *);
extern unsigned char check_image_ipc(unsigned char);
extern image_t *create_eterm_image(void);
extern void reset_eterm_image(image_t *, unsigned long);
extern void free_eterm_image(image_t *);
extern simage_t *create_simage(void);
extern void reset_simage(simage_t *, unsigned long);
extern void free_simage(simage_t *);
extern colormod_t *create_colormod(void);
extern void reset_colormod(colormod_t *);
extern void free_colormod(colormod_t *);
extern Pixmap create_trans_pixmap(simage_t *, unsigned char, Drawable, int, int, unsigned short, unsigned short);
extern Pixmap create_viewport_pixmap(simage_t *, Drawable, int, int, unsigned short, unsigned short);
extern void paste_simage(simage_t *, unsigned char, Window, Drawable, unsigned short, unsigned short, unsigned short, unsigned short);
extern void redraw_image(unsigned char);
extern void redraw_images_by_mode(unsigned char);
#endif
extern void render_simage(simage_t *, Window, unsigned short, unsigned short, unsigned char, renderop_t);
#ifdef PIXMAP_SUPPORT
extern const char *search_path(const char *, const char *);
extern unsigned char load_image(const char *, simage_t *);
extern void update_cmod(colormod_t *);
extern void update_cmod_tables(imlib_t *);
extern void free_desktop_pixmap(void);
# ifdef PIXMAP_OFFSET
extern unsigned char need_colormod(imlib_t *);
extern void colormod_trans(Pixmap, imlib_t *, GC, unsigned short, unsigned short);
extern unsigned char update_desktop_info(int *, int *);
extern Window get_desktop_window(void);
extern Pixmap get_desktop_pixmap(void);
# endif
extern void shaped_window_apply_mask(Drawable, Pixmap);
extern void set_icon_pixmap(char *, XWMHints *);
#endif

_XFUNCPROTOEND

#endif /* _PIXMAP_H */
