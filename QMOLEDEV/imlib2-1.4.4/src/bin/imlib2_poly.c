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

#include "Imlib2.h"

Display            *disp;
Window              win;
Visual             *vis;
Colormap            cm;
int                 depth;

int
main(int argc, char **argv)
{
   int                 w, h;
   Imlib_Image         im_bg = NULL;
   XEvent              ev;
   KeySym              keysym;
   static char         kbuf[20];
   ImlibPolygon        poly, poly1, poly2;
   const char         *display_name = getenv("DISPLAY");

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
                PointerMotionMask | ExposureMask | KeyPressMask);

   /**
    * Start rendering
    */
   imlib_context_set_display(disp);
   imlib_context_set_visual(vis);
   imlib_context_set_colormap(cm);
   imlib_context_set_drawable(win);
   imlib_context_set_blend(0);
   imlib_context_set_color_modifier(NULL);
   imlib_context_set_blend(0);

   im_bg = imlib_create_image(400, 400);
   imlib_context_set_image(im_bg);
   w = imlib_image_get_width();
   h = imlib_image_get_height();
   imlib_context_set_color(0, 0, 0, 255);
   imlib_image_fill_rectangle(0, 0, w, h);

   XResizeWindow(disp, win, w, h);
   XMapWindow(disp, win);
   XSync(disp, False);

   poly = imlib_polygon_new();
   imlib_polygon_add_point(poly, 20, 20);
   imlib_polygon_add_point(poly, 70, 20);
   imlib_polygon_add_point(poly, 70, 70);
   imlib_polygon_add_point(poly, 20, 70);

   poly1 = imlib_polygon_new();
   imlib_polygon_add_point(poly1, 100, 20);
   imlib_polygon_add_point(poly1, 190, 100);
   imlib_polygon_add_point(poly1, 120, 70);

   poly2 = imlib_polygon_new();
   imlib_polygon_add_point(poly2, 290, 20);
   imlib_polygon_add_point(poly2, 200, 100);
   imlib_polygon_add_point(poly2, 270, 70);

   while (1)
     {
        do
          {
             XNextEvent(disp, &ev);
             switch (ev.type)
               {
                 case ButtonRelease:
                    exit(0);
                    break;
                 case KeyPress:
                    XLookupString(&ev.xkey, (char *)kbuf, sizeof(kbuf), &keysym,
                                  NULL);
                    switch (*kbuf)
                      {
                        case ' ':
                           imlib_context_set_anti_alias
                               (!imlib_context_get_anti_alias());
                           printf("AA is %s\n",
                                  imlib_context_get_anti_alias()? "on" : "off");
                           break;
                        case 'q':
                           exit(0);
                        default:
                           break;
                      }
                    break;
                 default:
                    break;

               }
          }
        while (XPending(disp));

        imlib_context_set_image(im_bg);
        imlib_context_set_color(0, 0, 0, 255);
        imlib_image_fill_rectangle(0, 0, w, h);
        imlib_context_set_color(255, 255, 255, 255);
        imlib_image_fill_polygon(poly);
        imlib_image_fill_polygon(poly1);
        imlib_image_fill_polygon(poly2);
        imlib_render_image_on_drawable(0, 0);
     }
   return 0;
}
