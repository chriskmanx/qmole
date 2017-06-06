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

static const char cvs_ident[] = "$Id: pixmap.c 59414 2011-05-15 21:24:19Z mej $";

#include "config.h"
#include "feature.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#ifdef HAVE_X_SHAPE_EXT
# include <X11/X.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/extensions/shape.h>
#endif

#include "buttons.h"
#include "command.h"
#include "draw.h"
#include "e.h"
#include "icon.h"
#include "menus.h"
#include "options.h"
#include "pixmap.h"
#include "screen.h"
#include "scrollbar.h"
#include "startup.h"
#include "term.h"
#include "windows.h"

/* FIXME:  Workaround for older versions of libast. */
#ifndef WORDS_BIGENDIAN
#  define WORDS_BIGENDIAN 0
#endif

/* Optimized check for rm, gm, and bm all < 256 */
#define COLORMODS_HAVE_SATURATION(rm, gm, bm)  ((rm|gm|bm) >> 8)

/* Assembler routines for 32 bit cpu with mmx */
extern void shade_ximage_15_mmx(void *data, int bpl, int w, int h, int rm, int gm, int bm);
extern void shade_ximage_16_mmx(void *data, int bpl, int w, int h, int rm, int gm, int bm);
extern void shade_ximage_32_mmx(void *data, int bpl, int w, int h, int rm, int gm, int bm);

/* Assembler routines for 64 bit cpu with sse2 */
extern void shade_ximage_15_sse2(void *data, int bpl, int w, int h, int rm, int gm, int bm);
extern void shade_ximage_16_sse2(void *data, int bpl, int w, int h, int rm, int gm, int bm);
extern void shade_ximage_32_sse2(void *data, int bpl, int w, int h, int rm, int gm, int bm);

#ifdef PIXMAP_SUPPORT
static Imlib_Border bord_none = { 0, 0, 0, 0 };
#endif

Pixmap buffer_pixmap = None;

#ifdef PIXMAP_OFFSET
Pixmap desktop_pixmap = None, viewport_pixmap = None;
Window desktop_window = None;
unsigned char desktop_pixmap_is_mine = 0;
#endif

image_t images[image_max] = {
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL},
    {None, 0, 0, NULL, NULL, NULL, NULL, NULL}
};

#ifdef PIXMAP_SUPPORT
static const char *get_iclass_name(unsigned char);
#endif
static void copy_buffer_pixmap(unsigned char mode, unsigned long fill, unsigned short width, unsigned short height);

const char *
get_image_type(unsigned char type)
{
    switch (type) {
        case image_bg:
            return "image_bg";
            break;
        case image_up:
            return "image_up";
            break;
        case image_down:
            return "image_down";
            break;
        case image_left:
            return "image_left";
            break;
        case image_right:
            return "image_right";
            break;
        case image_sb:
            return "image_sb";
            break;
        case image_sa:
            return "image_sa";
            break;
        case image_st:
            return "image_st";
            break;
        case image_menu:
            return "image_menu";
            break;
        case image_menuitem:
            return "image_menuitem";
            break;
        case image_submenu:
            return "image_submenu";
            break;
        case image_button:
            return "image_button";
            break;
        case image_bbar:
            return "image_bbar";
            break;
        case image_gbar:
            return "image_gbar";
            break;
        case image_dialog:
            return "image_dialog";
            break;
        case image_max:
        default:
            return "image_max";
            break;
    }
    ASSERT_NOTREACHED_RVAL("");
}

unsigned char
image_mode_any(unsigned char mode)
{
    unsigned char ismode = 0;

    FOREACH_IMAGE(if (image_mode_is(idx, mode)) ismode = 1;);
    return ismode;
}

#ifdef PIXMAP_SUPPORT
const char *
imlib_strerror(Imlib_Load_Error err)
{
    switch (err) {
        case IMLIB_LOAD_ERROR_NONE:
            return "Success";
            break;
        case IMLIB_LOAD_ERROR_FILE_DOES_NOT_EXIST:
            return "No such file or directory";
            break;
        case IMLIB_LOAD_ERROR_FILE_IS_DIRECTORY:
            return "Is a directory";
            break;
        case IMLIB_LOAD_ERROR_PERMISSION_DENIED_TO_READ:
            return "Permission denied";
            break;
        case IMLIB_LOAD_ERROR_NO_LOADER_FOR_FILE_FORMAT:
            return "No loader available for that file format";
            break;
        case IMLIB_LOAD_ERROR_PATH_TOO_LONG:
            return "Path too long";
            break;
        case IMLIB_LOAD_ERROR_PATH_COMPONENT_NON_EXISTANT:
            return "Path component does not exist";
            break;
        case IMLIB_LOAD_ERROR_PATH_COMPONENT_NOT_DIRECTORY:
            return "Path component is not a directory";
            break;
        case IMLIB_LOAD_ERROR_PATH_POINTS_OUTSIDE_ADDRESS_SPACE:
            return "Path points outside address space";
            break;
        case IMLIB_LOAD_ERROR_TOO_MANY_SYMBOLIC_LINKS:
            return "Too many symbolic links in path";
            break;
        case IMLIB_LOAD_ERROR_OUT_OF_MEMORY:
            return "Out of memory";
            break;
        case IMLIB_LOAD_ERROR_OUT_OF_FILE_DESCRIPTORS:
            return "No more file descriptors";
            break;
        case IMLIB_LOAD_ERROR_PERMISSION_DENIED_TO_WRITE:
            return "Permission denied";
            break;
        case IMLIB_LOAD_ERROR_OUT_OF_DISK_SPACE:
            return "Disk full";
            break;
        case IMLIB_LOAD_ERROR_UNKNOWN:
        default:
            return "Unknown error";
            break;
    }
    ASSERT_NOTREACHED_RVAL("");
}

unsigned short
parse_pixmap_ops(char *str)
{
    unsigned short op = OP_NONE;
    char *token;

    REQUIRE_RVAL(str && *str, OP_NONE);
    D_PIXMAP(("parse_pixmap_ops(str [%s]) called.\n", str));

    for (; (token = (char *) strsep(&str, ":"));) {
        if (!BEG_STRCASECMP(token, "tile")) {
            op |= OP_TILE;
        } else if (!BEG_STRCASECMP(token, "hscale")) {
            op |= OP_HSCALE;
        } else if (!BEG_STRCASECMP(token, "vscale")) {
            op |= OP_VSCALE;
        } else if (!BEG_STRCASECMP(token, "scale")) {
            op |= OP_SCALE;
        } else if (!BEG_STRCASECMP(token, "propscale")) {
            op |= OP_PROPSCALE;
        }
    }
    return (op);
}

unsigned short
set_pixmap_scale(const char *geom, pixmap_t *pmap)
{

    static char str[GEOM_LEN + 1] = { '\0' };
    unsigned int w = 0, h = 0;
    int x = 0, y = 0;
    unsigned short op;
    int flags;
    unsigned short changed = 0;
    char *p, *opstr;
    int n;

    if (!geom)
        return 0;

    D_PIXMAP(("scale_pixmap(\"%s\")\n", geom));
    if (!strcmp(geom, "?")) {
        sprintf(str, "[%dx%d+%d+%d]", pmap->w, pmap->h, pmap->x, pmap->y);
        xterm_seq(ESCSEQ_XTERM_TITLE, str);
        return 0;
    }
    if ((opstr = strchr(geom, ':'))) {
        *opstr++ = '\0';
        op = parse_pixmap_ops(opstr);
    } else {
        op = pmap->op;
    }
    if (!(p = strchr(geom, ';')))
        p = strchr(geom, '\0');
    n = (p - geom);
    if (n > GEOM_LEN - 1)
        return 0;

    strncpy(str, geom, n);
    str[n] = '\0';

    flags = XParseGeometry(str, &x, &y, &w, &h);

    if (!flags) {
        flags |= WidthValue;    /* default is tile */
        w = 0;
    }
    if (!(flags & XValue)) {
        x = 50;
    }
    if (!(flags & HeightValue)) {
        h = w;
    }
    if (!(flags & YValue)) {
        if (flags & XNegative)
            flags |= YNegative;
        y = x;
    }

    if (flags & (WidthValue | HeightValue)) {
        if (w && !h) {
            w = pmap->w * ((float) w / 100);
            h = pmap->h;
        } else if (h && !w) {
            w = pmap->w;
            h = pmap->h * ((float) h / 100);
        }
    }
    if (pmap->w != (int) w) {
        pmap->w = (int) w;
        changed++;
    }
    if (pmap->h != (int) h) {
        pmap->h = (int) h;
        changed++;
    }

    if (!(flags & WidthValue) && geom[0] != '=') {
        x += pmap->x;
        y += pmap->y;
    } else {
        if (flags & XNegative)
            x += 100;
        if (flags & YNegative)
            y += 100;
    }
    BOUND(x, 0, 100);
    BOUND(y, 0, 100);

    if (pmap->x != x) {
        pmap->x = x;
        changed++;
    }
    if (pmap->y != y) {
        pmap->y = y;
        changed++;
    }
    if (pmap->op != op) {
        pmap->op = op;
        changed++;
    }
    D_PIXMAP(("Returning %hu, *pmap == { op [%hu], w [%hd], h [%hd], x [%hd], y [%hd] }\n", changed, pmap->op, pmap->w, pmap->h,
              pmap->x, pmap->y));
    return changed;
}

image_t *create_eterm_image(void)
{
    image_t *i;

    i = (image_t *) MALLOC(sizeof(image_t));
    MEMSET(i, 0, sizeof(image_t));
    return (i);
}

void
reset_eterm_image(image_t *img, unsigned long mask)
{
    ASSERT(img != NULL);

    D_PIXMAP(("reset_image(%8p, 0x%08x)\n", img, mask));

    if ((mask & RESET_NORM) && img->norm) {
        reset_simage(img->norm, mask);
    }
    if ((mask & RESET_SELECTED) && img->selected) {
        reset_simage(img->selected, mask);
    }
    if ((mask & RESET_CLICKED) && img->clicked) {
        reset_simage(img->clicked, mask);
    }
    if ((mask & RESET_DISABLED) && img->disabled) {
        reset_simage(img->disabled, mask);
    }
    if (mask & RESET_MODE) {
        img->mode = 0;
    }
    if (mask & RESET_ALL) {
        img->userdef = 0;
        img->win = None;
        img->current = img->norm;
    }
}

void
free_eterm_image(image_t *img)
{
    if (img->selected == img->norm) {
        img->selected = NULL;
    }
    if (img->clicked == img->norm) {
        img->clicked = NULL;
    }
    if (img->disabled == img->norm) {
        img->disabled = NULL;
    }
    free_simage(img->norm);
    if (img->clicked == img->selected) {
        img->clicked = NULL;
    }
    if (img->disabled == img->selected || img->disabled == img->clicked) {
        img->disabled = NULL;
    }
    if (img->selected) {
        free_simage(img->selected);
    }
    if (img->clicked) {
        free_simage(img->clicked);
    }
    if (img->disabled) {
        free_simage(img->disabled);
    }
#if 0
    /* FIXME:  There should really be an option to specify whether or not
       to free the image itself, but since we never MALLOC() an entire
       image_t, it's not really needed at this point.  And I'm lazy. :-) */
    FREE(img);
#endif
}

simage_t *create_simage(void)
{
    simage_t *s;

    s = (simage_t *) MALLOC(sizeof(simage_t));
    MEMSET(s, 0, sizeof(simage_t));
    s->pmap = (pixmap_t *) MALLOC(sizeof(pixmap_t));
    s->iml = (imlib_t *) MALLOC(sizeof(imlib_t));
    MEMSET(s->pmap, 0, sizeof(pixmap_t));
    MEMSET(s->iml, 0, sizeof(imlib_t));
    return s;
}

void
reset_simage(simage_t *simg, unsigned long mask)
{

    ASSERT(simg != NULL);

    D_PIXMAP(("reset_simage(%8p, 0x%08x)\n", simg, mask));

    if ((mask & RESET_PMAP_PIXMAP) && simg->pmap->pixmap != None) {
        IMLIB_FREE_PIXMAP(simg->pmap->pixmap);
        simg->pmap->pixmap = None;
        simg->pmap->mask = None;
    }
    if ((mask & RESET_IMLIB_IM) && simg->iml->im) {
        imlib_context_set_image(simg->iml->im);
        imlib_free_image_and_decache();
        simg->iml->im = NULL;
    }
    if ((mask & RESET_IMLIB_BORDER) && simg->iml->border) {
        FREE(simg->iml->border);
        simg->iml->border = NULL;
    }
    if ((mask & RESET_IMLIB_BEVEL) && simg->iml->bevel) {
        FREE(simg->iml->bevel->edges);
        FREE(simg->iml->bevel);
        simg->iml->bevel = NULL;
    }
    if ((mask & RESET_IMLIB_PAD) && simg->iml->pad) {
        FREE(simg->iml->pad);
        simg->iml->pad = NULL;
    }
    if ((mask & RESET_IMLIB_MOD) && simg->iml->mod) {
        FREE(simg->iml->mod);
        simg->iml->mod = NULL;
    }
    if ((mask & RESET_IMLIB_RMOD) && simg->iml->rmod) {
        if (simg->iml->rmod->imlib_mod) {
            imlib_context_set_color_modifier(simg->iml->rmod->imlib_mod);
            imlib_free_color_modifier();
        }
        FREE(simg->iml->rmod);
        simg->iml->rmod = NULL;
    }
    if ((mask & RESET_IMLIB_GMOD) && simg->iml->gmod) {
        if (simg->iml->gmod->imlib_mod) {
            imlib_context_set_color_modifier(simg->iml->gmod->imlib_mod);
            imlib_free_color_modifier();
        }
        FREE(simg->iml->gmod);
        simg->iml->gmod = NULL;
    }
    if ((mask & RESET_IMLIB_BMOD) && simg->iml->bmod) {
        if (simg->iml->bmod->imlib_mod) {
            imlib_context_set_color_modifier(simg->iml->bmod->imlib_mod);
            imlib_free_color_modifier();
        }
        FREE(simg->iml->bmod);
        simg->iml->bmod = NULL;
    }
    if (mask & RESET_PMAP_GEOM) {
        simg->pmap->w = 0;
        simg->pmap->h = 0;
        simg->pmap->x = 50;
        simg->pmap->y = 50;
        simg->pmap->op = OP_NONE;
    }
}

void
free_simage(simage_t *s)
{
    reset_simage(s, RESET_ALL_SIMG);
    FREE(s->iml);
    FREE(s->pmap);
    FREE(s);
}

colormod_t *create_colormod(void)
{
    colormod_t *cmod;

    cmod = (colormod_t *) MALLOC(sizeof(colormod_t));
    cmod->brightness = cmod->contrast = cmod->gamma = 0x100;
    cmod->imlib_mod = NULL;
    return cmod;
}

void
reset_colormod(colormod_t *cmod)
{
    ASSERT(cmod != NULL);
    cmod->brightness = cmod->contrast = cmod->gamma = 0x100;
    if (cmod->imlib_mod) {
        imlib_context_set_color_modifier(cmod->imlib_mod);
        imlib_reset_color_modifier();
    }
}

void
free_colormod(colormod_t *cmod)
{
    ASSERT(cmod != NULL);
    if (cmod->imlib_mod) {
        imlib_context_set_color_modifier(cmod->imlib_mod);
        imlib_free_color_modifier();
    }
    FREE(cmod);
}

static const char *
get_iclass_name(unsigned char which)
{
    switch (which) {
        case image_bg:
            return "ETERM_BG";
            break;
        case image_up:
            return "ETERM_ARROW_UP";
            break;
        case image_down:
            return "ETERM_ARROW_DOWN";
            break;
        case image_left:
            return "ETERM_ARROW_LEFT";
            break;
        case image_right:
            return "ETERM_ARROW_RIGHT";
            break;
        case image_sb:
            return "ETERM_TROUGH";
            break;
        case image_sa:
            return "ETERM_ANCHOR";
            break;
        case image_st:
            return "ETERM_THUMB";
            break;
        case image_menu:
            return "ETERM_MENU_ITEM";
            break;              /* FIXME:  This should be ETERM_MENU_BOX */
        case image_menuitem:
            return "ETERM_MENU_ITEM";
            break;
        case image_submenu:
            return "ETERM_MENU_SUBMENU";
            break;
        case image_button:
            return "ETERM_MENU_ITEM";
            break;              /* FIXME:  These four should    */
        case image_bbar:
            return "ETERM_MENU_BOX";
            break;              /* have their own image classes */
        case image_gbar:
            return "ETERM_ANCHOR";
            break;
        case image_dialog:
            return "ETERM_MENU_BOX";
            break;
        default:
            ASSERT_NOTREACHED_RVAL(NULL);
            break;
    }
}

unsigned char
check_image_ipc(unsigned char reset)
{
    static unsigned char checked = 0;
    register unsigned short i;
    char buff[255], *reply;
    const char *iclass;

    if (reset) {
        checked = 0;
    }
    if (checked) {
        return ((checked == 1) ? 1 : 0);
    }
    for (i = 0; i < image_max; i++) {
        if (!image_mode_is(i, MODE_AUTO)) {
            continue;
        }
        iclass = get_iclass_name(i);
        snprintf(buff, sizeof(buff), "imageclass %s query", iclass);
        reply = enl_send_and_wait(buff);
        if (strstr(reply, "not")) {
            libast_print_error("ImageClass \"%s\" is not defined in Enlightenment.  Disallowing \"auto\" mode for this image.\n", iclass);
            image_mode_fallback(i);
        } else if (strstr(reply, "Error")) {
            /* *INDENT-OFF* */
            FOREACH_IMAGE(
                          if (image_mode_is(idx, MODE_AUTO)) {
                              if (image_mode_is(idx, ALLOW_IMAGE)) {
                                  image_set_mode(idx, MODE_IMAGE);
                              } else {
                                  image_set_mode(idx, MODE_SOLID);
                              }
                          }
                          if (image_mode_is(idx, ALLOW_AUTO)) {
                              image_disallow_mode(idx, ALLOW_AUTO);
                          }
                          );
            /* *INDENT-ON* */
            libast_print_error("Looks like this version of Enlightenment doesn't support the IPC "
                        "commands I need.  Disallowing \"auto\" mode for all images.\n");
            FREE(reply);
            checked = 2;
            return 0;
        }
        FREE(reply);
    }
    checked = 1;
    return 1;
}

Pixmap
create_trans_pixmap(simage_t *simg, unsigned char which, Drawable d, int x, int y, unsigned short width, unsigned short height)
{
    int pw, ph;
    Window dummy;
    Screen *scr;
    Pixmap p = None;
    GC gc;

    D_PIXMAP(("create_trans_pixmap(%8p, 0x%08x, %u, %d, %d, %hu, %hu) called.\n", simg, d, which, x, y, width, height));
    scr = ScreenOfDisplay(Xdisplay, Xscreen);
    if (!scr)
        return None;

    if (!update_desktop_info(&pw, &ph)) {
        D_PIXMAP(("update_desktop_info() failed.\n"));
        return None;
    }
    if (refresh_type == NO_REFRESH) {
        /* If we're hidden/shaded, translated coordinates to be parent relative, then use the
           internal cache data to tell where we are on the desktop.  Otherwise, just ask X. */
        XTranslateCoordinates(Xdisplay, d, TermWin.parent, x, y, &x, &y, &dummy);
        x += TermWin.x;
        y += TermWin.y;
    } else {
        XTranslateCoordinates(Xdisplay, d, desktop_window, x, y, &x, &y, &dummy);
    }
    p = LIBAST_X_CREATE_PIXMAP(width, height);
    gc = LIBAST_X_CREATE_GC(0, NULL);
    D_PIXMAP(("Created p [0x%08x] as a %hux%hu pixmap at %d, %d relative to window 0x%08x\n", p, width, height, x, y,
              desktop_window));
    if (p != None) {
        D_PIXMAP(("Tiling %ux%u desktop pixmap 0x%08x onto p.\n", pw, ph, desktop_pixmap));
        XSetTile(Xdisplay, gc, desktop_pixmap);
        XSetTSOrigin(Xdisplay, gc, pw - (x % pw), ph - (y % ph));
        XSetFillStyle(Xdisplay, gc, FillTiled);
        XFillRectangle(Xdisplay, p, gc, 0, 0, width, height);
        if ((which != image_bg || (BITFIELD_IS_SET(image_options, IMAGE_OPTIONS_ITRANS))
             || images[image_bg].current != images[image_bg].norm)
            && need_colormod(simg->iml)) {
            colormod_trans(p, simg->iml, gc, width, height);
        }
        if (simg->iml->bevel) {
            D_PIXMAP(("Beveling pixmap 0x%08x with edges %d, %d, %d, %d\n", p, simg->iml->bevel->edges->left,
                      simg->iml->bevel->edges->top, simg->iml->bevel->edges->right, simg->iml->bevel->edges->bottom));
            bevel_pixmap(p, width, height, simg->iml->bevel->edges, simg->iml->bevel->up);
        }
    }
    LIBAST_X_FREE_GC(gc);
    return p;
}

Pixmap
create_viewport_pixmap(simage_t *simg, Drawable d, int x, int y, unsigned short width, unsigned short height)
{
    short xsize, ysize;
    Window dummy;
    unsigned int pw, ph, pb, pd;
    int px, py;
    Pixmap p = None, mask = None;
    GC gc;
    Screen *scr;

    D_PIXMAP(("create_viewport_pixmap(%8p, 0x%08x, %d, %d, %hu, %hu) called.\n", simg, d, x, y, width, height));
    scr = ScreenOfDisplay(Xdisplay, Xscreen);
    if (!scr)
        return None;
    if (desktop_window == None) {
        get_desktop_window();
        if (desktop_window == None) {
            D_PIXMAP(("No desktop window found.\n"));
            return None;
        }
    }
    if (viewport_pixmap == None) {
        imlib_t *tmp_iml = images[image_bg].current->iml;

        imlib_context_set_image(tmp_iml->im);
        imlib_context_set_drawable(d);
        imlib_image_set_has_alpha(0);
        imlib_context_set_anti_alias(1);
        imlib_context_set_dither(1);
        imlib_context_set_blend(0);
        xsize = imlib_image_get_width();
        ysize = imlib_image_get_height();
        if (tmp_iml->border) {
            imlib_image_set_border(tmp_iml->border);
        } else {
            imlib_image_set_border(&bord_none);
        }
        imlib_context_set_color_modifier((tmp_iml->mod && tmp_iml->mod->imlib_mod) ? tmp_iml->mod->imlib_mod : NULL);
        if ((images[image_bg].current->pmap->w > 0) || (images[image_bg].current->pmap->op & OP_SCALE)) {
            D_PIXMAP(("Scaling image to %dx%d\n", scr->width, scr->height));
            imlib_render_pixmaps_for_whole_image_at_size(&viewport_pixmap, &mask, scr->width, scr->height);
        } else {
            D_PIXMAP(("Tiling image at %dx%d\n", xsize, ysize));
            imlib_render_pixmaps_for_whole_image(&viewport_pixmap, &mask);
        }
        if (viewport_pixmap == None) {
            libast_print_error("Delayed image load failure for \"%s\".  Using solid color mode.\n", imlib_image_get_filename());
            image_set_mode(image_bg, MODE_SOLID);
            reset_simage(simg, RESET_ALL_SIMG);
            return None;
        }
        IMLIB_REGISTER_PIXMAP(viewport_pixmap);
        D_PIXMAP(("Created viewport_pixmap == 0x%08x\n", viewport_pixmap));
    } else {
        XGetGeometry(Xdisplay, viewport_pixmap, &dummy, &px, &py, &pw, &ph, &pb, &pd);
        xsize = (short) pw;
        ysize = (short) ph;
    }
    if (simg->pmap->pixmap != None) {
        XGetGeometry(Xdisplay, simg->pmap->pixmap, &dummy, &px, &py, &pw, &ph, &pb, &pd);
        if (pw != width || ph != height) {
            IMLIB_FREE_PIXMAP(simg->pmap->pixmap);
            simg->pmap->pixmap = None;
        } else {
            p = simg->pmap->pixmap;
        }
    }
    if (p == None) {
        p = LIBAST_X_CREATE_PIXMAP(width, height);
        D_PIXMAP(("Created p == 0x%08x\n", p));
    }
    gc = LIBAST_X_CREATE_GC(0, NULL);
    XTranslateCoordinates(Xdisplay, d, desktop_window, x, y, &x, &y, &dummy);
    D_PIXMAP(("Translated coords are %d, %d\n", x, y));
    if ((images[image_bg].current->pmap->w > 0) || (images[image_bg].current->pmap->op & OP_SCALE)) {
        XCopyArea(Xdisplay, viewport_pixmap, p, gc, x, y, width, height, 0, 0);
    } else {
        XSetTile(Xdisplay, gc, viewport_pixmap);
        XSetTSOrigin(Xdisplay, gc, xsize - (x % xsize), ysize - (y % ysize));
        XSetFillStyle(Xdisplay, gc, FillTiled);
        XFillRectangle(Xdisplay, p, gc, 0, 0, width, height);
    }
    LIBAST_X_FREE_GC(gc);
    return p;
}

void
paste_simage(simage_t *simg, unsigned char which, Window win, Drawable d, unsigned short x, unsigned short y, unsigned short w,
             unsigned short h)
{
    Pixmap pmap = None, mask = None;
    GC gc;

    ASSERT(simg != NULL);
    D_PIXMAP(("paste_simage(%8p, %s, 0x%08x, 0x%08x, %hd, %hd, %hd, %hd) called.\n", simg, get_image_type(which), (int) win,
              (int) d, x, y, w, h));

    REQUIRE(d != None);
    REQUIRE(w > 0);
    REQUIRE(h > 0);

    if (which != image_max) {
        if (image_mode_is(which, MODE_AUTO) && image_mode_is(which, ALLOW_AUTO)) {
            char buff[255], *reply;
            const char *iclass, *state;

            check_image_ipc(0);
            if (image_mode_is(which, MODE_AUTO)) {
                iclass = get_iclass_name(which);
                if (simg == images[which].selected) {
                    state = "hilited";
                } else if (simg == images[which].clicked) {
                    state = "clicked";
                } else {
                    state = "normal";
                }
                D_PIXMAP((" -> iclass == \"%s\", state == \"%s\"\n", NONULL(iclass), NONULL(state)));
                if (iclass) {
                    snprintf(buff, sizeof(buff), "imageclass %s apply_copy 0x%x %s %hd %hd", iclass, (int) d, state, w, h);
                    reply = enl_send_and_wait(buff);
                    if (strstr(reply, "Error")) {
                        libast_print_error
                            ("Enlightenment didn't seem to like something about my syntax.  Disallowing \"auto\" mode for this image.\n");
                        image_mode_fallback(which);
                        FREE(reply);
                    } else {
                        pmap = (Pixmap) strtoul(reply, (char **) NULL, 0);
                        mask = (Pixmap) strtoul(spiftool_get_pword(2, reply), (char **) NULL, 0);
                        FREE(reply);
                        enl_ipc_sync();
                        if (pmap) {
                            gc = LIBAST_X_CREATE_GC(0, NULL);
                            XSetClipMask(Xdisplay, gc, mask);
                            XSetClipOrigin(Xdisplay, gc, x, y);
                            XCopyArea(Xdisplay, pmap, d, gc, 0, 0, w, h, x, y);
                            snprintf(buff, sizeof(buff), "imageclass %s free_pixmap 0x%08x", iclass, (int) pmap);
                            enl_ipc_send(buff);
                            LIBAST_X_FREE_GC(gc);
                            return;
                        } else {
                            libast_print_error
                                ("Enlightenment returned a null pixmap, which I can't use.  Disallowing \"auto\" mode for this image.\n");
                            FREE(reply);
                            image_mode_fallback(which);
                        }
                    }
                }
            }
        } else if (image_mode_is(which, MODE_TRANS) && image_mode_is(which, ALLOW_TRANS)) {
            Pixmap p;

            gc = LIBAST_X_CREATE_GC(0, NULL);
            p = create_trans_pixmap(simg, which, win, x, y, w, h);
            if (p != None) {
                XCopyArea(Xdisplay, p, d, gc, 0, 0, w, h, x, y);
                if (p != desktop_pixmap) {
                    LIBAST_X_FREE_PIXMAP(p);
                }
            }
            LIBAST_X_FREE_GC(gc);
        } else if (image_mode_is(which, MODE_VIEWPORT) && image_mode_is(which, ALLOW_VIEWPORT)) {
            Pixmap p;

            gc = LIBAST_X_CREATE_GC(0, NULL);
            p = create_viewport_pixmap(simg, win, x, y, w, h);
            if (simg->iml->bevel) {
                bevel_pixmap(p, w, h, simg->iml->bevel->edges, simg->iml->bevel->up);
            }
            XCopyArea(Xdisplay, p, d, gc, 0, 0, w, h, x, y);
            LIBAST_X_FREE_PIXMAP(p);
            LIBAST_X_FREE_GC(gc);
        }
    }

    if (((which == image_max) || (image_mode_is(which, MODE_IMAGE) && image_mode_is(which, ALLOW_IMAGE)))
        && (simg->iml)) {
        imlib_context_set_image(simg->iml->im);
        imlib_context_set_drawable(d);
        imlib_context_set_anti_alias(1);
        imlib_context_set_dither(1);
        imlib_context_set_blend(0);
        if (simg->iml->border) {
            imlib_image_set_border(simg->iml->border);
        } else {
            imlib_image_set_border(&bord_none);
        }
        imlib_context_set_color_modifier((simg->iml->mod && simg->iml->mod->imlib_mod) ? simg->iml->mod->imlib_mod : NULL);
        if (w == imlib_image_get_width() && h == imlib_image_get_height()) {
            imlib_render_pixmaps_for_whole_image(&pmap, &mask);
        } else {
            imlib_render_pixmaps_for_whole_image_at_size(&pmap, &mask, w, h);
        }
        if (pmap == None) {
            libast_print_error("Delayed image load failure for \"%s\".\n", NONULL(imlib_image_get_filename()));
            reset_simage(simg, RESET_ALL_SIMG);
            return;
        }
        IMLIB_REGISTER_PIXMAP(pmap);
        gc = LIBAST_X_CREATE_GC(0, NULL);
        if (mask) {
            XSetClipMask(Xdisplay, gc, mask);
            XSetClipOrigin(Xdisplay, gc, x, y);
        }
        XCopyArea(Xdisplay, pmap, d, gc, 0, 0, w, h, x, y);
        IMLIB_FREE_PIXMAP(pmap);
        LIBAST_X_FREE_GC(gc);
    }
}

void
redraw_image(unsigned char which)
{

    switch (which) {
        case image_bg:
            render_simage(images[image_bg].current, TermWin.vt, TermWin_TotalWidth(), TermWin_TotalHeight(), image_bg, 0);
            scr_touch();
            break;
        case image_up:
            scrollbar_draw_uparrow(IMAGE_STATE_CURRENT, MODE_MASK);
            break;
        case image_down:
            scrollbar_draw_downarrow(IMAGE_STATE_CURRENT, MODE_MASK);
            break;
        case image_sb:
            scrollbar_draw_trough(IMAGE_STATE_CURRENT, MODE_MASK);
            break;
        case image_sa:
        case image_st:
            scrollbar_draw_anchor(IMAGE_STATE_CURRENT, MODE_MASK);
            break;
        case image_button:
        case image_bbar:
        case image_gbar:
            bbar_draw_all(IMAGE_STATE_CURRENT, MODE_MASK);
            break;
        default:
            D_PIXMAP(("Bad value %u\n", which));
            break;
    }
}

void
redraw_images_by_mode(unsigned char mode)
{

    if (mode == MODE_SOLID) {
        redraw_all_images();
    } else {
        if (image_mode_is(image_bg, mode)) {
            render_simage(images[image_bg].current, TermWin.vt, TermWin_TotalWidth(), TermWin_TotalHeight(), image_bg, 0);
            scr_touch();
        }
        scrollbar_draw(IMAGE_STATE_CURRENT, mode);
        bbar_draw_all(IMAGE_STATE_CURRENT, mode);
    }
}
#endif /* PIXMAP_SUPPORT */

static void
copy_buffer_pixmap(unsigned char mode, unsigned long fill, unsigned short width, unsigned short height)
{
    GC gc;
    XGCValues gcvalue;

    ASSERT(buffer_pixmap == None);
    buffer_pixmap = LIBAST_X_CREATE_PIXMAP(width, height);
    gcvalue.foreground = (Pixel) fill;
    gc = LIBAST_X_CREATE_GC(GCForeground, &gcvalue);
    XSetGraphicsExposures(Xdisplay, gc, False);
    if (mode == MODE_SOLID) {
        simage_t *simg;

        simg = images[image_bg].current;
        if (simg->pmap->pixmap) {
            LIBAST_X_FREE_PIXMAP(simg->pmap->pixmap);
        }
        simg->pmap->pixmap = LIBAST_X_CREATE_PIXMAP(width, height);
        XFillRectangle(Xdisplay, simg->pmap->pixmap, gc, 0, 0, width, height);
        XCopyArea(Xdisplay, simg->pmap->pixmap, buffer_pixmap, gc, 0, 0, width, height, 0, 0);
    } else {
        XCopyArea(Xdisplay, (Pixmap) fill, buffer_pixmap, gc, 0, 0, width, height, 0, 0);
    }
    LIBAST_X_FREE_GC(gc);
}

void
render_simage(simage_t *simg, Window win, unsigned short width, unsigned short height, unsigned char which, renderop_t renderop)
{
    XGCValues gcvalue;
    GC gc;
    Pixmap pixmap = None;
    Screen *scr;

#ifdef PIXMAP_SUPPORT
    short xsize, ysize;
    short xpos = 0, ypos = 0;
    unsigned short rendered = 0;
    unsigned short xscaled = 0, yscaled = 0;
#endif
    scr = ScreenOfDisplay(Xdisplay, Xscreen);
    if (!scr)
        return;
    ASSERT(simg != NULL);
    ASSERT(simg->iml != NULL);
    ASSERT(simg->pmap != NULL);
    REQUIRE(win != None);
    D_PIXMAP(("Rendering simg->iml->im %8p (%s) at %hux%hu onto window 0x%08x\n", simg->iml->im, get_image_type(which), width,
              height, win));
    D_PIXMAP(("Image mode is 0x%02x\n", images[which].mode));
#ifdef PIXMAP_SUPPORT
    if ((which == image_bg) && image_mode_is(image_bg, MODE_VIEWPORT)) {
        width = scr->width;
        height = scr->height;
    }
#endif
    if (!(width) || !(height))
        return;
    gcvalue.foreground = gcvalue.background = PixColors[bgColor];
    gc = LIBAST_X_CREATE_GC(GCForeground | GCBackground, &gcvalue);
    pixmap = simg->pmap->pixmap;        /* Save this for later */
    if ((which == image_bg) && (buffer_pixmap != None)) {
        LIBAST_X_FREE_PIXMAP(buffer_pixmap);
        buffer_pixmap = None;
    }
#ifdef PIXMAP_SUPPORT
    /* Reset window shape mask. */
    shaped_window_apply_mask(win, None);

    if ((images[which].mode & MODE_AUTO) && (images[which].mode & ALLOW_AUTO)) {
        char buff[255];
        const char *iclass, *state;

        check_image_ipc(0);
        if (image_mode_is(which, MODE_AUTO)) {
            iclass = get_iclass_name(which);
            if (simg == images[which].selected) {
                state = "hilited";
            } else if (simg == images[which].clicked) {
                state = "clicked";
            } else {
                state = "normal";
            }
            if (iclass) {
                if (renderop & RENDER_FORCE_PIXMAP) {
                    char *reply;

                    snprintf(buff, sizeof(buff), "imageclass %s apply_copy 0x%x %s %hd %hd", iclass, (int) win, state, width,
                             height);
                    reply = enl_send_and_wait(buff);
                    if (strstr(reply, "Error")) {
                        libast_print_error
                            ("Enlightenment didn't seem to like something about my syntax.  Disallowing \"auto\" mode for this image.\n");
                        image_mode_fallback(which);
                        FREE(reply);
                    } else {
                        Pixmap pmap, mask;

                        pmap = (Pixmap) strtoul(reply, (char **) NULL, 0);
                        mask = (Pixmap) strtoul(spiftool_get_pword(2, reply), (char **) NULL, 0);
                        FREE(reply);
                        enl_ipc_sync();
                        if (pmap) {
                            if (mask) {
                                XSetClipMask(Xdisplay, gc, mask);
                                XSetClipOrigin(Xdisplay, gc, 0, 0);
                            }
                            if (simg->pmap->pixmap != None) {
                                LIBAST_X_FREE_PIXMAP(simg->pmap->pixmap);
                                simg->pmap->pixmap = None;
                            }
                            if (renderop & RENDER_FORCE_PIXMAP) {
                                simg->pmap->pixmap = LIBAST_X_CREATE_PIXMAP(width, height);
                                XCopyArea(Xdisplay, pmap, simg->pmap->pixmap, gc, 0, 0, width, height, 0, 0);
                                XSetWindowBackgroundPixmap(Xdisplay, win, simg->pmap->pixmap);
                            } else {
                                XSetWindowBackgroundPixmap(Xdisplay, win, pmap);
                            }
                            snprintf(buff, sizeof(buff), "imageclass %s free_pixmap 0x%08x", iclass, (int) pmap);
                            enl_ipc_send(buff);
                        } else {
                            libast_print_error
                                ("Enlightenment returned a null pixmap, which I can't use.  Disallowing \"auto\" mode for this image.\n");
                            FREE(reply);
                            image_mode_fallback(which);
                        }
                    }
                } else {
                    snprintf(buff, sizeof(buff), "imageclass %s apply 0x%x %s", iclass, (int) win, state);
                    enl_ipc_send(buff);
                    LIBAST_X_FREE_GC(gc);
                    return;
                }
            }
        }
    }
# ifdef PIXMAP_OFFSET
    if (image_mode_is(which, MODE_TRANS) && image_mode_is(which, ALLOW_TRANS)) {
        if (simg->pmap->pixmap != None) {
            LIBAST_X_FREE_PIXMAP(simg->pmap->pixmap);
        }
        simg->pmap->pixmap = create_trans_pixmap(simg, which, win, 0, 0, width, height);
        if (simg->pmap->pixmap != None) {
            if ((which == image_bg) && (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_DOUBLE_BUFFER))) {
                copy_buffer_pixmap(MODE_TRANS, (unsigned long) simg->pmap->pixmap, width, height);
                XSetWindowBackgroundPixmap(Xdisplay, win, buffer_pixmap);
            } else {
                XSetWindowBackgroundPixmap(Xdisplay, win, simg->pmap->pixmap);
                if (!(renderop & RENDER_FORCE_PIXMAP)) {
                    IMLIB_FREE_PIXMAP(simg->pmap->pixmap);
                    simg->pmap->pixmap = None;
                }
            }
        } else {
            image_mode_fallback(which);
        }
    } else if (image_mode_is(which, MODE_VIEWPORT) && image_mode_is(which, ALLOW_VIEWPORT)) {
        Pixmap p;

        D_PIXMAP(("Viewport mode enabled.  viewport_pixmap == 0x%08x and simg->pmap->pixmap == 0x%08x\n", viewport_pixmap,
                  simg->pmap->pixmap));
        p = create_viewport_pixmap(simg, win, 0, 0, width, height);
        if (p && (p != simg->pmap->pixmap)) {
            if (simg->pmap->pixmap != None) {
                LIBAST_X_FREE_PIXMAP(simg->pmap->pixmap);
            }
            simg->pmap->pixmap = p;
        }
        if (simg->pmap->pixmap != None) {
            D_PIXMAP(("Setting background of window 0x%08x to 0x%08x\n", win, simg->pmap->pixmap));
            if ((which == image_bg) && (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_DOUBLE_BUFFER))) {
                copy_buffer_pixmap(MODE_VIEWPORT, (unsigned long) simg->pmap->pixmap, width, height);
                XSetWindowBackgroundPixmap(Xdisplay, win, buffer_pixmap);
            } else {
                XSetWindowBackgroundPixmap(Xdisplay, win, simg->pmap->pixmap);
                if (!(renderop & RENDER_FORCE_PIXMAP)) {
                    IMLIB_FREE_PIXMAP(simg->pmap->pixmap);
                    simg->pmap->pixmap = None;
                }
            }
        } else {
            image_mode_fallback(which);
        }
    }
# endif
    if (image_mode_is(which, MODE_IMAGE) && image_mode_is(which, ALLOW_IMAGE)) {
        if (simg->iml->im) {
            int w = simg->pmap->w;
            int h = simg->pmap->h;
            int x = simg->pmap->x;
            int y = simg->pmap->y;

            imlib_context_set_image(simg->iml->im);
            imlib_context_set_drawable(win);
            imlib_context_set_anti_alias(1);
            imlib_context_set_dither(1);
            imlib_context_set_blend(0);
            xsize = imlib_image_get_width();
            ysize = imlib_image_get_height();
            D_PIXMAP(("w == %d, h == %d, x == %d, y == %d, xsize == %d, ysize == %d\n", w, h, x, y, xsize, ysize));
            if ((simg->pmap->op & OP_PROPSCALE) && w && h) {
                double x_ratio, y_ratio;

                x_ratio = ((double) width) / ((double) xsize);
                y_ratio = ((double) height) / ((double) ysize);
                if (x_ratio > 1) {
                    /* Window is larger than image.  Smaller ratio determines whether which image dimension is closer in value
                       to the corresponding window dimension, which is the scale factor we want to use */
                    if (x_ratio > y_ratio) {
                        x_ratio = y_ratio;
                    }
                } else {
                    if (x_ratio > y_ratio) {
                        x_ratio = y_ratio;
                    }
                }
                xscaled = (unsigned short) ((xsize * x_ratio) * ((float) w / 100.0));
                yscaled = (unsigned short) ((ysize * x_ratio) * ((float) h / 100.0));
            } else {
                if (!w && BITFIELD_IS_SET(simg->pmap->op, OP_HSCALE)) {
                    w = 100;
                }
                if (!h && BITFIELD_IS_SET(simg->pmap->op, OP_VSCALE)) {
                    h = 100;
                }
                if (w > 0) {
                    xscaled = width * ((float) w / 100.0);
                } else {
                    xscaled = xsize;
                }
                if (h > 0) {
                    yscaled = height * ((float) h / 100.0);
                } else {
                    yscaled = ysize;
                }
            }

            xpos = (short) ((width - xscaled) * ((float) x / 100.0));
            ypos = (short) ((height - yscaled) * ((float) y / 100.0));
            D_PIXMAP(("Calculated scaled size as %hux%hu with origin at (%hd, %hd)\n", xscaled, yscaled, xpos, ypos));
            if (simg->iml->border) {
                D_PIXMAP(("Setting image border:  { left [%d], right [%d], top [%d], bottom [%d] }\n",
                          simg->iml->border->left, simg->iml->border->right, simg->iml->border->top, simg->iml->border->bottom));
                imlib_image_set_border(simg->iml->border);
            } else {
                imlib_image_set_border(&bord_none);
            }
            imlib_context_set_color_modifier((simg->iml->mod && simg->iml->mod->imlib_mod) ? simg->iml->mod->imlib_mod : NULL);
            D_PIXMAP(("Rendering image simg->iml->im [%8p] to %hdx%hd pixmap\n", simg->iml->im, xscaled, yscaled));
            imlib_render_pixmaps_for_whole_image_at_size(&simg->pmap->pixmap, &simg->pmap->mask, xscaled, yscaled);
            rendered = 1;
            if (simg->pmap->pixmap != None) {
                IMLIB_REGISTER_PIXMAP(simg->pmap->pixmap);
                if (pixmap != None && pixmap != simg->pmap->pixmap) {
                    IMLIB_FREE_PIXMAP(pixmap);
                }
                if (xscaled != width || yscaled != height || xpos != 0 || ypos != 0) {
                    unsigned char single;

                    /* This tells us if we have a single, non-tiled image which does not entirely fill the window */
                    single = ((xscaled < width || yscaled < height) && !(simg->pmap->op & OP_TILE)) ? 1 : 0;
                    pixmap = simg->pmap->pixmap;
                    simg->pmap->pixmap = LIBAST_X_CREATE_PIXMAP(width, height);
                    if (single) {
                        XFillRectangle(Xdisplay, simg->pmap->pixmap, gc, 0, 0, width, height);
                    }
                    XSetTile(Xdisplay, gc, pixmap);
                    XSetTSOrigin(Xdisplay, gc, xpos, ypos);
                    XSetFillStyle(Xdisplay, gc, FillTiled);
                    if (single) {
                        XCopyArea(Xdisplay, pixmap, simg->pmap->pixmap, gc, 0, 0, xscaled, yscaled, xpos, ypos);
                    } else {
                        XFillRectangle(Xdisplay, simg->pmap->pixmap, gc, 0, 0, width, height);
                    }
                    IMLIB_FREE_PIXMAP(pixmap);
                } else if (renderop & RENDER_FORCE_PIXMAP) {
                    pixmap = simg->pmap->pixmap;
                    simg->pmap->pixmap = LIBAST_X_CREATE_PIXMAP(width, height);
                    D_PIXMAP(("Replacing pixmap 0x%08x with new pixmap 0x%08x.\n", pixmap, simg->pmap->pixmap));
                    XCopyArea(Xdisplay, pixmap, simg->pmap->pixmap, gc, 0, 0, width, height, 0, 0);
                    IMLIB_FREE_PIXMAP(pixmap);
                } else {
                    shaped_window_apply_mask(win, simg->pmap->mask);
                }
                if (simg->iml->bevel) {
                    bevel_pixmap(simg->pmap->pixmap, width, height, simg->iml->bevel->edges, simg->iml->bevel->up);
                }
                D_PIXMAP(("Setting background of window 0x%08x to 0x%08x\n", win, simg->pmap->pixmap));
                if ((which == image_bg) && (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_DOUBLE_BUFFER))) {
                    copy_buffer_pixmap(MODE_VIEWPORT, (unsigned long) simg->pmap->pixmap, width, height);
                    XSetWindowBackgroundPixmap(Xdisplay, win, buffer_pixmap);
                } else {
                    /* FIXME:  For efficiency, just fill the window with the pixmap
                       and handle exposes by copying from simg->pmap->pixmap. */
                    XSetWindowBackgroundPixmap(Xdisplay, win, simg->pmap->pixmap);
                    if (!(renderop & RENDER_FORCE_PIXMAP)) {
                        IMLIB_FREE_PIXMAP(simg->pmap->pixmap);
                        simg->pmap->pixmap = None;
                    }
                }
            } else {
                libast_print_error("Delayed image load failure for \"%s\".  Using solid color mode.\n", imlib_image_get_filename());
                image_set_mode(which, MODE_SOLID);
                reset_simage(simg, RESET_ALL_SIMG);
            }
        } else {
            image_set_mode(which, MODE_SOLID);
            reset_simage(simg, RESET_ALL_SIMG);
        }
    }
#endif

    /* Fall back to solid mode if all else fails. */
    if (!image_mode_is(which, MODE_MASK)) {
        if ((which == image_bg) && (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_DOUBLE_BUFFER))) {
            copy_buffer_pixmap(MODE_SOLID, (unsigned long) PixColors[bgColor], width, height);
            XSetWindowBackgroundPixmap(Xdisplay, win, buffer_pixmap);
        } else {
            if ((renderop & RENDER_FORCE_PIXMAP) || (simg->iml->bevel)) {
                if (simg->pmap->pixmap != None) {
                    LIBAST_X_FREE_PIXMAP(simg->pmap->pixmap);
                }
                simg->pmap->pixmap = LIBAST_X_CREATE_PIXMAP(width, height);
                XSetForeground(Xdisplay, gc, ((which == image_bg) ? (PixColors[bgColor]) : (simg->bg)));
                XFillRectangle(Xdisplay, simg->pmap->pixmap, gc, 0, 0, width, height);
                if (simg->iml->bevel && simg->iml->bevel->edges) {
                    DRAW_SOLID_BEVEL(simg->pmap->pixmap, width, height, simg->bg, simg->iml->bevel->up,
                                     simg->iml->bevel->edges->left);
                }
                /* FIXME:  For efficiency, just fill the window with the pixmap
                   and handle exposes by copying from simg->pmap->pixmap. */
                XSetWindowBackgroundPixmap(Xdisplay, win, simg->pmap->pixmap);
            } else {
                XSetWindowBackground(Xdisplay, win, ((which == image_bg) ? (PixColors[bgColor]) : (simg->bg)));
            }
        }
    }
    XClearWindow(Xdisplay, win);
    LIBAST_X_FREE_GC(gc);
    return;
}

#ifdef PIXMAP_SUPPORT
const char *
search_path(const char *pathlist, const char *file)
{
    static char name[PATH_MAX];
    char *p;
    const char *path;
    int maxpath, len;
    struct stat fst;

    if (!pathlist || !file) {   /* If either one is NULL, there really isn't much point in going on.... */
        return ((const char *) NULL);
    }
    getcwd(name, PATH_MAX);
    len = strlen(name);
    D_OPTIONS(("search_path(\"%s\", \"%s\") called from \"%s\".\n", pathlist, file, name));
    if (len < PATH_MAX - 1) {
        strcat(name, "/");
        strncat(name, file, PATH_MAX - len - 2);
    }
    D_OPTIONS(("Checking for file \"%s\"\n", name));
    if (!access(name, R_OK)) {
        if (stat(name, &fst)) {
            D_OPTIONS(("Unable to stat %s -- %s\n", name, strerror(errno)));
        } else {
            D_OPTIONS(("Stat returned mode 0x%08o, S_ISDIR() == %d\n", fst.st_mode, S_ISDIR(fst.st_mode)));
        }
        if (!S_ISDIR(fst.st_mode)) {
            return name;
        } else {
            D_OPTIONS(("%s is a directory.\n", name));
        }
    } else {
        D_OPTIONS(("Unable to access %s -- %s\n", name, strerror(errno)));
    }

    if (!(p = strchr(file, '@')))
        p = strchr(file, '\0');
    len = (p - file);
    /* leave room for an extra '/' and trailing '\0' */
    maxpath = sizeof(name) - (len + 2);
    if (maxpath <= 0)
        return NULL;
    /* check if we can find it now */
    strncpy(name, file, len);
    name[len] = '\0';
    D_OPTIONS(("Checking for file \"%s\"\n", name));
    if (!access(name, R_OK)) {
        if (stat(name, &fst)) {
            D_OPTIONS(("Unable to stat %s -- %s\n", name, strerror(errno)));
        } else {
            D_OPTIONS(("Stat returned mode 0x%08o, S_ISDIR() == %d\n", fst.st_mode, S_ISDIR(fst.st_mode)));
        }
        if (!S_ISDIR(fst.st_mode)) {
            return name;
        } else {
            D_OPTIONS(("%s is a directory.\n", name));
        }
    } else {
        D_OPTIONS(("Unable to access %s -- %s\n", name, strerror(errno)));
    }
    for (path = pathlist; path && *path != '\0'; path = p) {
        int n;

        /* colon delimited */
        if (!(p = strchr(path, ':')))
            p = strchr(path, '\0');
        n = (p - path);
        if (*p != '\0')
            p++;
        if (n > 0 && n <= maxpath) {

            if (*path == '~') {
                unsigned int l;
                char *home_dir = getenv("HOME");

                if (home_dir && *home_dir) {
                    l = strlen(home_dir);
                    if (l + n < (unsigned) maxpath) {
                        strcpy(name, home_dir);
                        strncat(name, path + 1, n - 1);
                        n += l - 1;
                    }
                }
            } else {
                strncpy(name, path, n);
            }
            if (name[n - 1] != '/')
                name[n++] = '/';
            name[n] = '\0';
            strncat(name, file, len);
            D_OPTIONS(("Checking for file \"%s\"\n", name));
            if (!access(name, R_OK)) {
                if (stat(name, &fst)) {
                    D_OPTIONS(("Unable to stat %s -- %s\n", name, strerror(errno)));
                } else {
                    D_OPTIONS(("Stat returned mode 0x%08o, S_ISDIR() == %d\n", fst.st_mode, S_ISDIR(fst.st_mode)));
                }
                if (!S_ISDIR(fst.st_mode)) {
                    return name;
                } else {
                    D_OPTIONS(("%s is a directory.\n", name));
                }
            } else {
                D_OPTIONS(("Unable to access %s -- %s\n", name, strerror(errno)));
            }
        }
    }
    D_OPTIONS(("File \"%s\" not found in path.\n", file));
    return ((const char *) NULL);
}

unsigned char
load_image(const char *file, simage_t *simg)
{
    const char *f;
    Imlib_Image *im;
    Imlib_Load_Error im_err;
    char *geom;

    ASSERT_RVAL(file != NULL, 0);
    ASSERT_RVAL(simg != NULL, 0);
    D_PIXMAP(("load_image(%s, %8p)\n", file, simg));
    if (*file != '\0') {
        if ((geom = strchr(file, '@'))) {
            *geom++ = 0;
        } else if ((geom = strchr(file, ';'))) {
            *geom++ = 0;
        }
        if (geom) {
            set_pixmap_scale(geom, simg->pmap);
        }
        if (!(f = search_path(rs_path, file))) {
            f = search_path(getenv(PATH_ENV), file);
        }
        if (f) {
            im = imlib_load_image_with_error_return(f, &im_err);
            if (!im) {
                libast_print_error("Unable to load image file \"%s\" -- %s\n", file, imlib_strerror(im_err));
                return 0;
            } else {
                reset_simage(simg, (RESET_IMLIB_IM | RESET_PMAP_PIXMAP | RESET_PMAP_MASK));
                simg->iml->im = im;
            }
            D_PIXMAP(("Found image %8p.\n", im));
            return 1;
        } else {
            libast_print_error("Unable to locate file \"%s\" in image path.\n");
        }
    }
    reset_simage(simg, RESET_ALL_SIMG);
    return 0;
}

void
update_cmod(colormod_t *cmod)
{
    ASSERT(cmod != NULL);
    /* When a particular R/G/B color modifier is changed, this function must be called
       to resync the Imlib2 color modifier (imlib_mod) with our new brightness,
       contrast, and gamma values. */
    if (cmod->imlib_mod) {
        imlib_context_set_color_modifier(cmod->imlib_mod);
        imlib_reset_color_modifier();
    } else {
        cmod->imlib_mod = imlib_create_color_modifier();
        imlib_context_set_color_modifier(cmod->imlib_mod);
    }
    if (cmod->brightness != 0x100) {
        imlib_modify_color_modifier_brightness((double) (cmod->brightness - 255.0) / 255.0);
    }
    if (cmod->contrast != 0x100) {
        imlib_modify_color_modifier_contrast((double) (cmod->contrast - 255.0) / 255.0);
    }
    if (cmod->gamma != 0x100) {
        imlib_modify_color_modifier_gamma((double) (cmod->gamma - 255.0) / 255.0);
    }
    imlib_context_set_color_modifier(NULL);
}

void
update_cmod_tables(imlib_t *iml)
{
    colormod_t *mod = iml->mod, *rmod = iml->rmod, *gmod = iml->gmod, *bmod = iml->bmod;
    DATA8 rt[256], gt[256], bt[256];

    REQUIRE(mod || rmod || gmod || bmod);
    /* When any changes is made to any individual color modifier for an image,
       this function must be called to update the overall Imlib2 color modifier. */
    D_PIXMAP(("Updating color modifier tables for %8p\n", iml));
    if (!mod) {
        mod = iml->mod = create_colormod();
        iml->mod->imlib_mod = imlib_create_color_modifier();
        imlib_context_set_color_modifier(mod->imlib_mod);
    } else if (!mod->imlib_mod) {
        mod->imlib_mod = imlib_create_color_modifier();
        imlib_context_set_color_modifier(mod->imlib_mod);
    } else {
        imlib_context_set_color_modifier(mod->imlib_mod);
        imlib_reset_color_modifier();
    }
    imlib_get_color_modifier_tables(rt, gt, bt, NULL);
    if (rmod && rmod->imlib_mod) {
        imlib_context_set_color_modifier(rmod->imlib_mod);
        imlib_get_color_modifier_tables(rt, NULL, NULL, NULL);
    }
    if (gmod && gmod->imlib_mod) {
        imlib_context_set_color_modifier(gmod->imlib_mod);
        imlib_get_color_modifier_tables(NULL, gt, NULL, NULL);
    }
    if (bmod && bmod->imlib_mod) {
        imlib_context_set_color_modifier(bmod->imlib_mod);
        imlib_get_color_modifier_tables(NULL, NULL, bt, NULL);
    }
    imlib_context_set_color_modifier(mod->imlib_mod);
    imlib_set_color_modifier_tables(rt, gt, bt, NULL);
    if (mod->brightness != 0x100) {
        imlib_modify_color_modifier_brightness((double) (mod->brightness - 255.0) / 255.0);
    }
    if (mod->contrast != 0x100) {
        imlib_modify_color_modifier_contrast((double) (mod->contrast - 255.0) / 255.0);
    }
    if (mod->gamma != 0x100) {
        imlib_modify_color_modifier_gamma((double) (mod->gamma - 255.0) / 255.0);
    }
}

# define MOD_IS_SET(mod) ((mod) && ((mod)->brightness != 0x100 || (mod)->contrast != 0x100 || (mod)->gamma != 0x100))
unsigned char
need_colormod(register imlib_t *iml)
{
    if (MOD_IS_SET(iml->mod) || MOD_IS_SET(iml->rmod) || MOD_IS_SET(iml->gmod) || MOD_IS_SET(iml->bmod)) {
        D_PIXMAP(("Color modifier active.\n"));
        return 1;
    } else {
        D_PIXMAP(("No color modifier active.\n"));
        return 0;
    }
}

/* New optimized routines for tinting XImages written by Willem Monsuwe <willem@stack.nl> */

#if !defined HAVE_MMX && !defined HAVE_SSE2
/* RGB 15 */
static void
shade_ximage_15(void *data, int bpl, int w, int h, int rm, int gm, int bm)
{
    unsigned char *ptr;
    int x, y;

    ptr = (unsigned char *) data + (w * sizeof(DATA16));
    if (!COLORMODS_HAVE_SATURATION(rm, gm, bm)) {
        /* No saturation */
        for (y = h; --y >= 0;) {
            for (x = -w; x < 0; x++) {
                int r, g, b;

                b = ((DATA16 *) ptr)[x];
                r = (b & 0x7c00) * rm;
                g = (b & 0x3e0) * gm;
                b = (b & 0x1f) * bm;
                ((DATA16 *) ptr)[x] = ((r >> 8) & 0x7c00)
                    | ((g >> 8) & 0x3e0)
                    | ((b >> 8) & 0x1f);
            }
            ptr += bpl;
        }
    } else {
        for (y = h; --y >= 0;) {
            for (x = -w; x < 0; x++) {
                int r, g, b;

                b = ((DATA16 *) ptr)[x];
                r = (((b >> 10) & 0x001f ) * rm) >> 8;
                r = (r > 0x001f) ? 0x7c00 : (r << 10);
                g = (((b >>  5) & 0x001f ) * gm) >> 8;
                g = (g > 0x001f) ? 0x03e0 : (g << 5);
                b = (((b >>  0) & 0x001f ) * bm) >> 8;
                b = (b > 0x001f) ? 0x001f : (b << 0);
                ((DATA16 *) ptr)[x] = (r|g|b);
            }
            ptr += bpl;
        }
    }
}

/* RGB 16 */
static void
shade_ximage_16(void *data, int bpl, int w, int h, int rm, int gm, int bm)
{
    unsigned char *ptr;
    int x, y;

    ptr = (unsigned char *) data + (w * sizeof(DATA16));
    if (!COLORMODS_HAVE_SATURATION(rm, gm, bm)) {
        /* No saturation */
        for (y = h; --y >= 0;) {
            for (x = -w; x < 0; x++) {
                int r, g, b;

                b = ((DATA16 *) ptr)[x];
                r = (b & 0xf800) * rm;
                g = (b & 0x7e0) * gm;
                b = (b & 0x1f) * bm;
                ((DATA16 *) ptr)[x] = ((r >> 8) & 0xf800)
                    | ((g >> 8) & 0x7e0)
                    | ((b >> 8) & 0x1f);
            }
            ptr += bpl;
        }
    } else {
        for (y = h; --y >= 0;) {
            for (x = -w; x < 0; x++) {
                int r, g, b;

                b = ((DATA16 *) ptr)[x];
                r = (((b >> 11) & 0x001f) * rm) >> 8;
		r = (r > 0x001f) ? 0xf800 : (r << 11);
                g = (((b >>  5) & 0x003f) * gm) >> 8;
		g = (g > 0x003f) ? 0x07e0 : (g << 5);
                b = (((b >>  0) & 0x001f) * bm) >> 8;
		b = (b > 0x001f) ? 0x001f : (b << 0);
                ((DATA16 *) ptr)[x] = (r|g|b);
            }
            ptr += bpl;
        }
    }
}

/* RGB 32 */
static void
shade_ximage_32(void *data, int bpl, int w, int h, int rm, int gm, int bm)
{
    unsigned char *ptr;
    int x, y;

    ptr = (unsigned char *) data + (w * 4);
    if (!COLORMODS_HAVE_SATURATION(rm, gm, bm)) {
        /* No saturation */
        for (y = h; --y >= 0;) {
            for (x = -(w * 4); x < 0; x += 4) {
# if WORDS_BIGENDIAN
                ptr[x + 1] = ((ptr[x + 1] * rm) >> 8);
                ptr[x + 2] = ((ptr[x + 2] * gm) >> 8);
                ptr[x + 3] = ((ptr[x + 3] * bm) >> 8);
# else
                ptr[x + 2] = ((ptr[x + 2] * rm) >> 8);
                ptr[x + 1] = ((ptr[x + 1] * gm) >> 8);
                ptr[x + 0] = ((ptr[x + 0] * bm) >> 8);
# endif
            }
            ptr += bpl;
        }
    } else {
        for (y = h; --y >= 0;) {
            for (x = -(w * 4); x < 0; x += 4) {
                int r, g, b;
# if WORDS_BIGENDIAN
                r = (ptr[x + 1] * rm) >> 8;
                ptr[x + 1] = r|(!(r >> 8) - 1);
                g = (ptr[x + 2] * gm) >> 8;
                ptr[x + 2] = g|(!(g >> 8) - 1);
                b = (ptr[x + 3] * bm) >> 8;
                ptr[x + 3] = b|(!(b >> 8) - 1);
# else
                r = (ptr[x + 2] * rm) >> 8;
                ptr[x + 2] = r|(!(r >> 8) - 1);
                g = (ptr[x + 1] * gm) >> 8;
                ptr[x + 1] = g|(!(g >> 8) - 1);
                b = (ptr[x + 0] * bm) >> 8;
                ptr[x + 0] = b|(!(b >> 8) - 1);
# endif
            }
            ptr += bpl;
        }
    }
}
#endif

/* RGB 24 */
static void
shade_ximage_24(void *data, int bpl, int w, int h, int rm, int gm, int bm)
{
    unsigned char *ptr;
    int x, y;

    ptr = (unsigned char *) data + (w * 3);
    if (!COLORMODS_HAVE_SATURATION(rm, gm, bm)) {
        /* No saturation */
        for (y = h; --y >= 0;) {
            for (x = -(w * 3); x < 0; x += 3) {
# if WORDS_BIGENDIAN
                ptr[x + 0] = (ptr[x + 0] * rm) >> 8;
                ptr[x + 1] = (ptr[x + 1] * gm) >> 8;
                ptr[x + 2] = (ptr[x + 2] * bm) >> 8;
# else
                ptr[x + 2] = (ptr[x + 2] * rm) >> 8;
                ptr[x + 1] = (ptr[x + 1] * gm) >> 8;
                ptr[x + 0] = (ptr[x + 0] * bm) >> 8;
# endif
            }
            ptr += bpl;
        }
    } else {
        for (y = h; --y >= 0;) {
            for (x = -(w * 3); x < 0; x += 3) {
                int r, g, b;

# if WORDS_BIGENDIAN
                r = (ptr[x + 0] * rm) >> 8;
                ptr[x + 0] = r|(!(r >> 8) - 1);
                g = (ptr[x + 1] * gm) >> 8;
                ptr[x + 1] = g|(!(g >> 8) - 1);
                b = (ptr[x + 2] * bm) >> 8;
                ptr[x + 2] = b|(!(b >> 8) - 1);
# else
                r = (ptr[x + 2] * rm) >> 8;
                ptr[x + 2] = r|(!(r >> 8) - 1);
                g = (ptr[x + 1] * gm) >> 8;
                ptr[x + 1] = g|(!(g >> 8) - 1);
                b = (ptr[x + 0] * bm) >> 8;
                ptr[x + 0] = b|(!(b >> 8) - 1);
# endif
            }
            ptr += bpl;
        }
    }
}

#  ifdef PIXMAP_OFFSET
void
colormod_trans(Pixmap p, imlib_t *iml, GC gc, unsigned short w, unsigned short h)
{
    XImage *ximg;
    register unsigned long i;

#if 0
    register unsigned long v;
    unsigned long x, y;
    int r, g, b;
    register int br, bg, bb;
    register unsigned int mr, mg, mb;
#endif
    unsigned short rm, gm, bm, shade;
    Imlib_Color ctab[256];
    int real_depth = 0;

    D_PIXMAP(("colormod_trans(p == 0x%08x, gc, w == %hu, h == %hu) called.\n", p, w, h));
    REQUIRE(p != None);
    if (iml->mod) {
        shade = iml->mod->brightness;
    } else {
        shade = 256;
    }
    if (iml->rmod) {
        rm = (iml->rmod->brightness * shade) >> 8;
    } else {
        rm = shade;
    }
    if (iml->gmod) {
        gm = (iml->gmod->brightness * shade) >> 8;
    } else {
        gm = shade;
    }
    if (iml->bmod) {
        bm = (iml->bmod->brightness * shade) >> 8;
    } else {
        bm = shade;
    }

    if (rm == 256 && gm == 256 && bm == 256) {
        return;                 /* Nothing to do */
    }
    D_PIXMAP((" -> rm == %hu, gm == %hu, bm == %hu, shade == %hu\n", rm, gm, bm, shade));
    if (Xdepth <= 8) {

        XColor cols[256];

        for (i = 0; i < (unsigned long) (1 << Xdepth); i++) {
            cols[i].pixel = i;
            cols[i].flags = DoRed | DoGreen | DoBlue;
        }
        XQueryColors(Xdisplay, cmap, cols, 1 << Xdepth);
        for (i = 0; i < (unsigned long) (1 << Xdepth); i++) {
            ctab[i].red = cols[i].red >> 8;
            ctab[i].green = cols[i].green >> 8;
            ctab[i].blue = cols[i].blue >> 8;
        }
    } else if (Xdepth == 16) {

        XWindowAttributes xattr;

        XGetWindowAttributes(Xdisplay, desktop_window, &xattr);
        if ((xattr.visual->green_mask == 0x3e0)) {
            real_depth = 15;
        }
    }
    if (!real_depth) {
        real_depth = Xdepth;
    }
    ximg = XGetImage(Xdisplay, p, 0, 0, w, h, -1, ZPixmap);
    if (!ximg) {
        libast_print_warning("XGetImage(Xdisplay, 0x%08x, 0, 0, %d, %d, -1, ZPixmap) returned NULL.\n", p, w, h);
        return;
    }
    D_PIXMAP(("XGetImage(Xdisplay, 0x%08x, 0, 0, %d, %d, -1, ZPixmap) returned %8p.\n", p, w, h, ximg));
    if (Xdepth <= 8) {
#if FIXME_BLOCK
        D_PIXMAP(("Rendering low-depth image, depth == %d\n", (int) Xdepth));
        for (y = 0; y < h; y++) {
            for (x = 0; x < w; x++) {
                v = XGetPixel(ximg, x, y);
                r = (ctab[v & 0xff].r * rm) >> 8;
                g = (ctab[v & 0xff].g * gm) >> 8;
                b = (ctab[v & 0xff].b * bm) >> 8;
                v = Imlib_best_color_match(imlib_id, &r, &g, &b);
                XPutPixel(ximg, x, y, v);
            }
        }
#endif
    } else {
        D_PIXMAP(("Rendering high-depth image, depth == %d\n", real_depth));
        /* Swap rm and bm for bgr */
        {
            XWindowAttributes xattr;

            XGetWindowAttributes(Xdisplay, desktop_window, &xattr);
            if (xattr.visual->blue_mask > xattr.visual->red_mask) {
                unsigned short tmp;

                tmp = rm;
                rm = bm;
                bm = tmp;
            }
        }
        /* Determine bitshift and bitmask values */
        switch (real_depth) {
            case 15:
#ifdef HAVE_SSE2
                D_PIXMAP(("Using SSE2 - 15 bit\n"));
                shade_ximage_15_sse2(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#elif defined HAVE_MMX
                D_PIXMAP(("Using MMX - 15 bit\n"));
                shade_ximage_15_mmx(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#else
                D_PIXMAP(("No MMX Found - 15 bit\n"));
                shade_ximage_15(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#endif
                break;
            case 16:
#ifdef HAVE_SSE2
                D_PIXMAP(("Using SSE2 - 16 bit\n"));
                shade_ximage_16_sse2(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#elif defined HAVE_MMX
                D_PIXMAP(("Using MMX - 16 bit\n"));
                shade_ximage_16_mmx(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#else
                D_PIXMAP(("No MMX Found - 16 bit\n"));
                shade_ximage_16(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#endif
                break;
            case 24:
                if (ximg->bits_per_pixel != 32) {
                    D_PIXMAP(("Rendering 24 bit\n"));
                    shade_ximage_24(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
                    break;
                }
                /* drop */
            case 32:
#ifdef HAVE_SSE2
                D_PIXMAP(("Using SSE2 - 32 bit\n"));
                shade_ximage_32_sse2(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#elif defined HAVE_MMX
                D_PIXMAP(("Using MMX - 32 bit\n"));
                shade_ximage_32_mmx(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#else
                D_PIXMAP(("No MMX Found - 32 bit\n"));
                shade_ximage_32(ximg->data, ximg->bytes_per_line, w, h, rm, gm, bm);
#endif
                break;
            default:
                libast_print_warning("Bit depth of %d is unsupported for tinting/shading.\n", real_depth);
                return;
        }
    }
    XPutImage(Xdisplay, p, gc, ximg, 0, 0, 0, 0, w, h);
    XDestroyImage(ximg);
}

unsigned char
update_desktop_info(int *w, int *h)
{
    unsigned int pw, ph, pb, pd;
    int px, py;
    Window dummy;

    if (w) {
        *w = 0;
    }
    if (h) {
        *h = 0;
    }
    if (desktop_window == None) {
        get_desktop_window();
    }
    if (desktop_window == None) {
        libast_print_error("Unable to locate desktop window.  If you are running Enlightenment, please\n"
                    "restart.  If not, please set your background image with Esetroot, then try again.");
        return 0;
    }
    if (desktop_pixmap == None) {
        get_desktop_pixmap();
    }
    if (desktop_pixmap == None) {
        return 0;
    }
    XGetGeometry(Xdisplay, desktop_pixmap, &dummy, &px, &py, &pw, &ph, &pb, &pd);
    if ((pw <= 0) || (ph <= 0)) {
        /* Reset and try again. */
        get_desktop_window();
        get_desktop_pixmap();
        XGetGeometry(Xdisplay, desktop_pixmap, &dummy, &px, &py, &pw, &ph, &pb, &pd);
    }
    if ((pw <= 0) || (ph <= 0)) {
        libast_print_error("Value of desktop pixmap property is invalid.  Please restart your \n"
                    "window manager or use Esetroot to set a new one.");
        desktop_pixmap = None;
        return 0;
    }
    if (w) {
        *w = pw;
    }
    if (h) {
        *h = ph;
    }
    return 1;
}

Window
get_desktop_window(void)
{

    Atom type;
    int format;
    unsigned long length, after;
    unsigned char *data;
    unsigned int nchildren;
    Window w, root, *children, parent;

    D_PIXMAP(("Current desktop window is 0x%08x\n", (unsigned int) desktop_window));
    if ((desktop_window != None) && (desktop_window != Xroot)) {
        XSelectInput(Xdisplay, desktop_window, None);
    }

    for (w = TermWin.parent; w; w = parent) {

        D_PIXMAP(("  Current window ID is:  0x%08x\n", w));
        if ((XQueryTree(Xdisplay, w, &root, &parent, &children, &nchildren)) == False) {
            D_PIXMAP(("    Egad!  XQueryTree() returned false!\n"));
            return None;
        }
        D_PIXMAP(("    Window is 0x%08x with %d children, root is 0x%08x, parent is 0x%08x\n", w, nchildren, root, parent));
        if (nchildren) {
            XFree(children);
        }


        if ((XGetWindowProperty
             (Xdisplay, w, props[PROP_TRANS_PIXMAP], 0L, 1L, False, AnyPropertyType, &type, &format, &length, &after,
              &data)) != Success) {
            if ((XGetWindowProperty
                 (Xdisplay, w, props[PROP_TRANS_COLOR], 0L, 1L, False, AnyPropertyType, &type, &format, &length, &after,
                  &data)) != Success) {
                continue;
            }
        }
        XFree(data);
        if (type != None) {
            D_PIXMAP(("Found desktop as window 0x%08x\n", w));
            if (w != Xroot) {
                XSelectInput(Xdisplay, w, PropertyChangeMask);
            }
            if (desktop_window == w) {
                D_PIXMAP(("  Desktop window has not changed.\n"));
                return ((Window) 1);
            } else {
                D_PIXMAP(("  Desktop window has changed  Updating desktop_window.\n"));
                return (desktop_window = w);
            }
        }
    }

    D_PIXMAP(("No suitable parent found.\n"));
    return (desktop_window = None);
}

Pixmap
get_desktop_pixmap(void)
{
    Pixmap p;
    Atom type;
    int format;
    static Pixmap color_pixmap = None, orig_desktop_pixmap;
    unsigned long length, after;
    unsigned char *data;

    D_PIXMAP(("Current desktop pixmap is 0x%08x\n", (unsigned int) desktop_pixmap));
    if (desktop_pixmap == None) {
        orig_desktop_pixmap = None;     /* Forced re-read. */
    }
    if (desktop_window == None) {
        D_PIXMAP(("No desktop window.  Aborting.\n"));
        free_desktop_pixmap();
        return (None);
    }

    if (color_pixmap != None) {
        D_PIXMAP(("Removing old solid color pixmap 0x%08x.\n", color_pixmap));
        LIBAST_X_FREE_PIXMAP(color_pixmap);
        color_pixmap = None;
    }
    XGetWindowProperty(Xdisplay, desktop_window, props[PROP_TRANS_PIXMAP], 0L, 1L, False, AnyPropertyType, &type, &format, &length,
                       &after, &data);
    if (type == XA_PIXMAP) {
        p = *((Pixmap *) data);
        XFree(data);
        if (p != None) {
            D_PIXMAP(("  Found pixmap 0x%08x\n", p));
            if (orig_desktop_pixmap == p) {
                D_PIXMAP(("Desktop pixmap is unchanged.\n"));
                return ((Pixmap) 1);
            } else {
                D_PIXMAP(("Desktop pixmap has changed.  Updating desktop_pixmap\n"));
                free_desktop_pixmap();
                orig_desktop_pixmap = p;
                if (!(BITFIELD_IS_SET(image_options, IMAGE_OPTIONS_ITRANS)) && need_colormod(images[image_bg].current->iml)) {
                    int px, py;
                    unsigned int pw, ph, pb, pd;
                    Window w;
                    GC gc;
                    XGCValues gcvalue;
                    Screen *scr = ScreenOfDisplay(Xdisplay, Xscreen);

                    gcvalue.foreground = gcvalue.background = PixColors[bgColor];
                    gc = LIBAST_X_CREATE_GC(GCForeground | GCBackground, &gcvalue);
                    XGetGeometry(Xdisplay, p, &w, &px, &py, &pw, &ph, &pb, &pd);
                    D_PIXMAP(("XGetGeometry() returned w = 0x%08x, pw == %u, ph == %u\n", w, pw, ph));
                    if (pw < (unsigned int) scr->width || ph < (unsigned int) scr->height) {
                        desktop_pixmap = LIBAST_X_CREATE_PIXMAP(pw, ph);
                        XCopyArea(Xdisplay, p, desktop_pixmap, gc, 0, 0, pw, ph, 0, 0);
                        colormod_trans(desktop_pixmap, images[image_bg].current->iml, gc, pw, ph);
                    } else {
                        desktop_pixmap = LIBAST_X_CREATE_PIXMAP(scr->width, scr->height);
                        XCopyArea(Xdisplay, p, desktop_pixmap, gc, 0, 0, scr->width, scr->height, 0, 0);
                        colormod_trans(desktop_pixmap, images[image_bg].current->iml, gc, scr->width, scr->height);
                    }
                    LIBAST_X_FREE_GC(gc);
                    desktop_pixmap_is_mine = 1;
                    D_PIXMAP(("Returning 0x%08x\n", (unsigned int) desktop_pixmap));
                    return (desktop_pixmap);
                } else {
                    desktop_pixmap_is_mine = 0;
                    D_PIXMAP(("Returning 0x%08x\n", (unsigned int) p));
                    return (desktop_pixmap = p);
                }
            }
        }
    } else {
        XFree(data);
    }
    XGetWindowProperty(Xdisplay, desktop_window, props[PROP_TRANS_COLOR], 0L, 1L, False, AnyPropertyType, &type, &format, &length,
                       &after, &data);
    if (type == XA_CARDINAL) {
        XGCValues gcvalue;
        GC gc;
        Pixel pix;

        free_desktop_pixmap();
        pix = *((Pixel *) data);
        XFree(data);
        D_PIXMAP(("  Found solid color 0x%08x\n", pix));
        gcvalue.foreground = pix;
        gcvalue.background = pix;
        gc = LIBAST_X_CREATE_GC(GCForeground | GCBackground, &gcvalue);
        color_pixmap = LIBAST_X_CREATE_PIXMAP(16, 16);
        XFillRectangle(Xdisplay, color_pixmap, gc, 0, 0, 16, 16);
        D_PIXMAP(("Created solid color pixmap 0x%08x for desktop_pixmap.\n", color_pixmap));
        LIBAST_X_FREE_GC(gc);
        return (desktop_pixmap = color_pixmap);
    } else {
        XFree(data);
    }
    D_PIXMAP(("No suitable attribute found.\n"));
    free_desktop_pixmap();
    return (desktop_pixmap = None);
}

void
free_desktop_pixmap(void)
{
    if (desktop_pixmap_is_mine && desktop_pixmap != None) {
        LIBAST_X_FREE_PIXMAP(desktop_pixmap);
        desktop_pixmap_is_mine = 0;
    }
    desktop_pixmap = None;
}

# endif /* PIXMAP_OFFSET */

void
shaped_window_apply_mask(Drawable d, Pixmap mask)
{

    static signed char have_shape = -1;

    REQUIRE(d != None);
    D_PIXMAP(("Applying mask 0x%08x to drawable 0x%08x\n", mask, d));
# ifdef HAVE_X_SHAPE_EXT
    if (have_shape == -1) {     /* Don't know yet. */
        int unused;

        D_PIXMAP(("Looking for shape extension.\n"));
        if (XQueryExtension(Xdisplay, "SHAPE", &unused, &unused, &unused)) {
            have_shape = 1;
        } else {
            have_shape = 0;
        }
    }
    if (have_shape == 1) {
        D_PIXMAP(("Shape extension available, applying mask.\n"));
        XShapeCombineMask(Xdisplay, d, ShapeBounding, 0, 0, mask, ShapeSet);
    } else if (have_shape == 0) {
        D_PIXMAP(("Shape extension not available.\n"));
        return;
    }
# else
    D_PIXMAP(("Shape support disabled.\n"));
# endif
}

void
set_icon_pixmap(char *filename, XWMHints * pwm_hints)
{
    const char *icon_path;
    Imlib_Image temp_im = (Imlib_Image) NULL;
    Imlib_Load_Error im_err;
    Imlib_Color_Modifier tmp_cmod;
    XWMHints *wm_hints;
    int w = 8, h = 8;

    if (pwm_hints) {
        wm_hints = pwm_hints;
    } else {
        wm_hints = XGetWMHints(Xdisplay, TermWin.parent);
    }

    /* Make sure no shading/tinting is applied to the icon. */
    tmp_cmod = imlib_create_color_modifier();
    imlib_context_set_color_modifier(tmp_cmod);
    imlib_reset_color_modifier();
    if (filename && *filename) {
        if (!(icon_path = search_path(rs_path, filename)))
            icon_path = search_path(getenv(PATH_ENV), filename);
        if (icon_path) {
            XIconSize *icon_sizes;
            int count, i;

            temp_im = imlib_load_image_with_error_return(icon_path, &im_err);
            if (!temp_im) {
                libast_print_error("Unable to load icon file \"%s\" -- %s\n", icon_path, imlib_strerror(im_err));
            } else {
                /* If we're going to render the image anyway, might as well be nice and give it to the WM in a size it likes. */
                if (XGetIconSizes(Xdisplay, Xroot, &icon_sizes, &count)) {
                    for (i = 0; i < count; i++) {
                        D_PIXMAP(("Got icon sizes:  Width %d to %d +/- %d, Height %d to %d +/- %d\n", icon_sizes[i].min_width,
                                  icon_sizes[i].max_width, icon_sizes[i].width_inc, icon_sizes[i].min_height,
                                  icon_sizes[i].max_height, icon_sizes[i].height_inc));
                        if (icon_sizes[i].max_width > 64 || icon_sizes[i].max_height > 64) {
                            continue;
                        }
                        /* Find the largest supported size <= 64 */
                        w = MAX(icon_sizes[i].max_width, w);
                        h = MAX(icon_sizes[i].max_height, h);
                    }
                    fflush(stdout);
                    XFree(icon_sizes);
                } else {
                    w = h = 48;
                }
                BOUND(w, 8, 64);
                BOUND(h, 8, 64);
                imlib_context_set_image(temp_im);
            }
        }
    }

    if (!temp_im) {
        w = h = 48;
        temp_im = imlib_create_image_using_data(48, 48, (DATA32 *) (icon_data + 2));
        imlib_context_set_image(temp_im);
        imlib_image_set_has_alpha(1);
    }
    imlib_context_set_drawable(TermWin.parent);
    imlib_context_set_anti_alias(1);
    imlib_context_set_dither(1);
    imlib_context_set_blend(0);
    imlib_render_pixmaps_for_whole_image_at_size(&wm_hints->icon_pixmap, &wm_hints->icon_mask, w, h);
#ifdef STRICT_ICCCM
    wm_hints->icon_window = XCreateSimpleWindow(Xdisplay, TermWin.parent, 0, 0, w, h, 0, 0L, 0L);
    shaped_window_apply_mask(wm_hints->icon_window, wm_hints->icon_mask);
    XSetWindowBackgroundPixmap(Xdisplay, wm_hints->icon_window, wm_hints->icon_pixmap);
    wm_hints->flags |= IconWindowHint;
#else
    wm_hints->flags |= IconPixmapHint | IconMaskHint;
#endif

    /* Set the EWMH icon hint for _NET_WM-compliant WM's. */
    XChangeProperty(Xdisplay, TermWin.parent,
                    props[PROP_EWMH_ICON], XA_CARDINAL, 32, PropModeReplace,
                    (unsigned char *) icon_data, sizeof(icon_data) / sizeof(CARD32));
    imlib_free_image_and_decache();

    /* Only set the hints ourselves if we were passed a NULL pointer for pwm_hints */
    if (!pwm_hints) {
        XSetWMHints(Xdisplay, TermWin.parent, wm_hints);
        XFree(wm_hints);
    }
}
#endif /* PIXMAP_SUPPORT */
