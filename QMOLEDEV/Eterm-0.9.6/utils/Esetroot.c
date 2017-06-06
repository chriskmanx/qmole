/*

 * Esetroot -- Set the root pixmap.  This program enables non-Enlightenment
 *             users to use Eterm's support for pseudotransparency.
 *
 * Written by Nat Friedman <ndf@mit.edu> with modifications by Gerald Britton
 * <gbritton@mit.edu> and Michael Jennings <mej@eterm.org>
 *
 */

static const char cvs_ident[] = "$Id: Esetroot.c 51650 2010-08-26 01:34:13Z lucas $";

#include "../config.h"

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <errno.h>
#include <libast.h>

#ifdef PIXMAP_SUPPORT
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#include <Imlib2.h>

void set_pixmap_property(Pixmap p);

Display *Xdisplay;
Screen *scr;
Window Xroot;
int screen;
unsigned char debug = 0;

#define Xdepth	 (DefaultDepth(Xdisplay, screen))

void
set_pixmap_property(Pixmap p)
{

    Atom prop_root, prop_esetroot, type;
    int format;
    unsigned long length, after;
    unsigned char *data_root, *data_esetroot;

    prop_root = XInternAtom(Xdisplay, "_XROOTPMAP_ID", True);
    prop_esetroot = XInternAtom(Xdisplay, "ESETROOT_PMAP_ID", True);

    if (debug) {
        fprintf(stderr, "%s:%d:  set_pixmap_property(0x%08x):  prop_root == 0x%08x, prop_esetroot == 0x%08x\n", __FILE__, __LINE__,
                (unsigned int) p, (unsigned int) prop_root, (unsigned int) prop_esetroot);
    }
    if (prop_root != None && prop_esetroot != None) {
        XGetWindowProperty(Xdisplay, Xroot, prop_root, 0L, 1L, False, AnyPropertyType, &type, &format, &length, &after, &data_root);
        if (type == XA_PIXMAP) {
            XGetWindowProperty(Xdisplay, Xroot, prop_esetroot, 0L, 1L, False, AnyPropertyType, &type, &format, &length, &after,
                               &data_esetroot);
            if (data_root && data_esetroot) {
                if (debug) {
                    fprintf(stderr, "%s:%d:  set_pixmap_property(0x%08x):  data_root == 0x%08x, data_esetroot == 0x%08x\n",
                            __FILE__, __LINE__, (unsigned int) p, (unsigned int) *((Pixmap *) data_root),
                            (unsigned int) *((Pixmap *) data_esetroot));
                }
                if (type == XA_PIXMAP && *((Pixmap *) data_root) == *((Pixmap *) data_esetroot)) {
                    if (debug) {
                        fprintf(stderr, "%s:%d:  set_pixmap_property(0x%08x):  XKillClient() is being called.\n", __FILE__,
                                __LINE__, (unsigned int) p);
                    }
                    XKillClient(Xdisplay, *((Pixmap *) data_root));
                }
            }
        }
    }
    /* This will locate the property, creating it if it doesn't exist */
    prop_root = XInternAtom(Xdisplay, "_XROOTPMAP_ID", False);
    prop_esetroot = XInternAtom(Xdisplay, "ESETROOT_PMAP_ID", False);

    /* The call above should have created it.  If that failed, we can't continue. */
    if (prop_root == None || prop_esetroot == None) {
        fprintf(stderr, "Esetroot:  creation of pixmap property failed.\n");
        exit(1);
    }
    XChangeProperty(Xdisplay, Xroot, prop_root, XA_PIXMAP, 32, PropModeReplace, (unsigned char *) &p, 1);
    XChangeProperty(Xdisplay, Xroot, prop_esetroot, XA_PIXMAP, 32, PropModeReplace, (unsigned char *) &p, 1);
    if (debug) {
        fprintf(stderr, "%s:%d:  set_pixmap_property(0x%08x):  _XROOTPMAP_ID and ESETROOT_PMAP_ID set to 0x%08x.\n", __FILE__,
                __LINE__, (unsigned int) p, (unsigned int) p);
    }
    XSetCloseDownMode(Xdisplay, RetainPermanent);
    XFlush(Xdisplay);
}
#endif

int
main(int argc, char *argv[])
{
#ifdef PIXMAP_SUPPORT
    unsigned char scale = 0, center = 0, fit = 0, mirror = 0;
    char *displayname = NULL, *fname = NULL, *bgcolor = NULL;
    Imlib_Image im;
    Pixmap p = None, temp_pmap = None, m = None;
    register unsigned char i;
    GC gc;
    XGCValues gcv;
    XColor xcolor;
    int w, h, x, y;

    if (argc < 2) {
        fprintf(stderr, "%s [-display <display_name>] [-bgcolor <color>] [-scale] [-center] [-fit] [-mirror] pixmap\n", *argv);
        fprintf(stderr, "\t Short options are also recognized (-d, -b, -s, -c, -f, and -m)\n");
        exit(0);
    }
    for (i = 1; i < argc; i++) {
        if (*argv[i] != '-') {
            break;
        }
        if (argv[i][1] == 'd') {
            displayname = argv[++i];
        } else if (argv[i][1] == 'b') {
            bgcolor = argv[++i];
        } else if (argv[i][1] == 's') {
            scale = 1;
        } else if (argv[i][1] == 'c') {
            center = 1;
        } else if (argv[i][1] == 'f') {
            fit = 1;
        } else if (argv[i][1] == 'm') {
            mirror = 1;
        } else if (argv[i][1] == 'x') {
            fprintf(stderr, "Debugging activated.\n");
            debug = 1;
        } else {
            fprintf(stderr, "%s:  Unrecognized option \'%c\'\n\n", *argv, argv[i][1]);
            fprintf(stderr, "%s [-display <display_name>] [-bgcolor <color>] [-scale] [-center] [-fit] [-mirror] pixmap\n", *argv);
            fprintf(stderr, "\t Short options are also recognized (-d, -b, -s, -c, -f, and -m)\n");
            exit(2);
        }
    }

    fname = argv[i];
    if (scale) {
        center = 0;
        mirror = 0;
    }

    if (debug) {
        fprintf(stderr, "%s:%d:  Display name is \"%s\"\n", __FILE__, __LINE__, displayname ? displayname : "(nil)");
        fprintf(stderr, "%s:%d:  Background color name is \"%s\"\n", __FILE__, __LINE__, bgcolor ? bgcolor : "(nil)");
        fprintf(stderr, "%s:%d:  Image will be %s\n", __FILE__, __LINE__,
                scale ? "scaled" : (center ? "centered" : (fit ? "fit" : "tiled")));
        fprintf(stderr, "%s:%d:  Image file is %s\n", __FILE__, __LINE__, fname ? fname : "(nil)");
    }
    if (!displayname) {
        displayname = getenv("DISPLAY");
        if (debug) {
            fprintf(stderr, "%s:%d:  Display name set to %s via getenv(\"DISPLAY\")\n", __FILE__, __LINE__,
                    displayname ? displayname : "(nil)");
        }
    }
    if (!displayname) {
        displayname = ":0.0";
        if (debug) {
            fprintf(stderr, "%s:%d:  Display name defaulted to %s\n", __FILE__, __LINE__, displayname ? displayname : "(nil)");
        }
    }
    if ((Xdisplay = XOpenDisplay(displayname)) == 0) {
        fprintf(stderr, "%s:  Unable to open display %s\n", *argv, displayname);
        exit(1);
    }
    screen = DefaultScreen(Xdisplay);
    Xroot = RootWindow(Xdisplay, screen);
    scr = ScreenOfDisplay(Xdisplay, screen);
    if (debug) {
        fprintf(stderr, "%s:%d:  Chose screen %d\n", __FILE__, __LINE__, screen);
        fprintf(stderr, "%s:%d:  Root window is 0x%08x\n", __FILE__, __LINE__, (unsigned int) Xroot);
        fprintf(stderr, "%s:%d:  Found screen information at %8p\n", __FILE__, __LINE__, scr);
    }
    imlib_context_set_display(Xdisplay);
    imlib_context_set_visual(DefaultVisual(Xdisplay, DefaultScreen(Xdisplay)));
    im = imlib_load_image_immediately(fname);
    if (!im) {
        fprintf(stderr, "%s:  Unable to load image file \"%s\".\n", *argv, fname);
        exit(1);
    } else if (debug) {
        fprintf(stderr, "%s:%d:  The Imlib Image is at %8p\n", __FILE__, __LINE__, im);
    }
    imlib_context_set_image(im);
    if (scale) {
        w = scr->width;
        h = scr->height;
    } else if (mirror) {
        w = imlib_image_get_width() * 2;
        h = imlib_image_get_height() * 2;
    } else {
        w = imlib_image_get_width();
        h = imlib_image_get_height();
    }
    if (fit) {
        double x_ratio, y_ratio;

        x_ratio = ((double) scr->width) / ((double) w);
        y_ratio = ((double) scr->height) / ((double) h);
        if (x_ratio > y_ratio) {
            x_ratio = y_ratio;
        }
        w = (int) (w * x_ratio);
        h = (int) (h * x_ratio);
    }

    p = XCreatePixmap(Xdisplay, Xroot, scr->width, scr->height, Xdepth);
    gcv.foreground = gcv.background = BlackPixel(Xdisplay, screen);
    if (bgcolor && XParseColor(Xdisplay, DefaultColormap(Xdisplay, screen), bgcolor, &xcolor)
        && XAllocColor(Xdisplay, DefaultColormap(Xdisplay, screen), &xcolor)) {
        gcv.foreground = gcv.background = xcolor.pixel;
    }
    gc = XCreateGC(Xdisplay, p, (GCForeground | GCBackground), &gcv);
    if (scale) {
        XFillRectangle(Xdisplay, p, gc, 0, 0, w, h);
    }
    if (center || fit) {
        XFillRectangle(Xdisplay, p, gc, 0, 0, scr->width, scr->height);
        x = (scr->width - w) >> 1;
        y = (scr->height - h) >> 1;
    } else {
        x = 0;
        y = 0;
    }
    if (debug) {
        fprintf(stderr, "%s:%d:  Assigned width and height for rendering as %dx%d\n", __FILE__, __LINE__, w, h);
        fprintf(stderr, "%s:%d:  Created %dx%d+%d+%d pixmap 0x%08x\n", __FILE__, __LINE__, scr->width, scr->height, x, y,
                (unsigned int) p);
        fprintf(stderr, "%s:%d:  Applied Graphics Context %8p to pixmap.\n", __FILE__, __LINE__, gc);
    }
    imlib_context_set_anti_alias(1);
    imlib_context_set_dither(1);
    imlib_context_set_blend(0);
    if (mirror) {
        temp_pmap = XCreatePixmap(Xdisplay, Xroot, w, h, Xdepth);
        imlib_context_set_drawable(temp_pmap);
        imlib_render_image_on_drawable(0, 0);
        imlib_image_flip_horizontal();
        imlib_render_image_on_drawable(imlib_image_get_width(), 0);
        imlib_image_flip_vertical();
        imlib_render_image_on_drawable(imlib_image_get_width(), imlib_image_get_height());
        imlib_image_flip_horizontal();
        imlib_render_image_on_drawable(0, imlib_image_get_height());
    } else {
        imlib_context_set_drawable(Xroot);
        imlib_render_pixmaps_for_whole_image_at_size(&temp_pmap, &m, w, h);
    }
    if (debug) {
        fprintf(stderr, "%s:%d:  Rendered at %dx%d onto pixmap 0x%08x\n", __FILE__, __LINE__, w, h, (unsigned int) temp_pmap);
    }
    if (temp_pmap != None) {
        if (m) {
            XFreePixmap(Xdisplay, m);
            m = None;
        }
        XSetTile(Xdisplay, gc, temp_pmap);
        XSetTSOrigin(Xdisplay, gc, x, y);
        XSetFillStyle(Xdisplay, gc, FillTiled);
        if (center || fit) {
            XFillRectangle(Xdisplay, p, gc, x, y, w, h);
        } else {
            XFillRectangle(Xdisplay, p, gc, x, y, scr->width, scr->height);
        }
        XGrabServer(Xdisplay);
        set_pixmap_property(p);
        XSetWindowBackgroundPixmap(Xdisplay, Xroot, p);
        XClearWindow(Xdisplay, Xroot);
        XUngrabServer(Xdisplay);
        XFlush(Xdisplay);
    }
#else
    USE_VAR(argc);
    USE_VAR(argv);
    fprintf(stderr, "Eterm was built without pixmap support, so Esetroot is fairly useless.  Sorry.\n");
#endif
    return 0;
}
