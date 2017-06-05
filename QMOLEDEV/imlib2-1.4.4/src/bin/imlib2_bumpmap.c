#include "config.h"
#include <X11/Xlib.h>
#include <X11/extensions/XShm.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

/*
#include <sys/time.h>
#include "common.h"
#include "image.h"
#include "rend.h"
#include "rgba.h"
#include "ximage.h"
#include "color.h"
 */
#include "Imlib2.h"

Display            *disp;
Window              win;
Visual             *vis;
Colormap            cm;
int                 depth;

int
main(int argc, char **argv)
{
   int                 w, h, x, y;
   Imlib_Image         im = NULL, im_bg = NULL;
   XEvent              ev;
   const char         *display_name = getenv("DISPLAY");

   /**
    * Initialization according to options
    */
   printf("Initialising\n");

   /**
    * First tests to determine which rendering task to perform
    */
   if (display_name == NULL)
       display_name = ":0";
   disp = XOpenDisplay(display_name);
   if (disp == NULL)
     {
       fprintf(stderr, "Can't open display %s\n", display_name);
       return 1;
     }
   vis = DefaultVisual(disp, DefaultScreen(disp));
   depth = DefaultDepth(disp, DefaultScreen(disp));
   cm = DefaultColormap(disp, DefaultScreen(disp));
   win =
       XCreateSimpleWindow(disp, DefaultRootWindow(disp), 0, 0, 100, 100, 0, 0,
                           0);
   XSelectInput(disp, win,
                ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
                PointerMotionMask | ExposureMask);

   /**
    * Start rendering
    */
   printf("Rendering\n");
   imlib_context_set_display(disp);
   imlib_context_set_visual(vis);
   imlib_context_set_colormap(cm);
   imlib_context_set_drawable(win);
   imlib_context_set_dither(1);
   imlib_context_set_blend(0);
   imlib_context_set_color_modifier(NULL);

   im_bg = imlib_load_image(PACKAGE_DATA_DIR"/data/images/imlib2.png");
   im = imlib_load_image(PACKAGE_DATA_DIR"/data/images/imlib2.png");

   imlib_context_set_image(im_bg);
   w = imlib_image_get_width();
   h = imlib_image_get_height();

   XResizeWindow(disp, win, w, h);
   XMapWindow(disp, win);
   XSync(disp, False);

   x = -9999;
   y = -9999;
   while (1)
     {
        Imlib_Image        *temp, *temp2;

        do
          {
             XNextEvent(disp, &ev);
             switch (ev.type)
               {
                 case Expose:
                    break;
                 case ButtonRelease:
                    exit(0);
                    break;
                 case MotionNotify:
                    x = ev.xmotion.x;
                    y = ev.xmotion.y;
                 default:
                    break;

               }
          }
        while (XPending(disp));

        imlib_context_set_blend(0);
        imlib_context_set_image(im_bg);
        temp = imlib_clone_image();
        imlib_context_set_image(temp);

        /*    imlib_blend_image_onto_image(im_bg, 0,
         * 0, 0, w, h,
         * 0, 0, w, h);
         * first = 0; */

        imlib_apply_filter
            ("bump_map_point(x=[],y=[],map="PACKAGE_DATA_DIR"/data/images/imlib2.png);", &x, &y);

        temp2 = im_bg;
        im_bg = temp;
        imlib_context_set_image(im_bg);
        imlib_render_image_on_drawable(0, 0);
        im_bg = temp2;
        imlib_context_set_image(temp);
        imlib_free_image();
     }

   return 0;
}
