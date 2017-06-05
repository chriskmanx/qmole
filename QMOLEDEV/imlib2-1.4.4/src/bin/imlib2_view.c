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
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include "Imlib2.h"

Display            *disp;
Window              win;
Pixmap              pm = 0;
Visual             *vis;
Colormap            cm;
int                 depth;
int                 image_width = 0, image_height = 0;
Imlib_Image         bg_im = NULL;

static int
                    progress(Imlib_Image im, char percent, int update_x, int update_y,
                             int update_w, int update_h);

static int
progress(Imlib_Image im, char percent, int update_x, int update_y,
         int update_w, int update_h)
{
   /* first time it's called */
   imlib_context_set_drawable(pm);
   imlib_context_set_anti_alias(0);
   imlib_context_set_dither(0);
   imlib_context_set_blend(0);
   if (image_width == 0)
     {
        int                 x, y, onoff;

        imlib_context_set_image(im);
        image_width = imlib_image_get_width();
        image_height = imlib_image_get_height();
        if (pm)
           XFreePixmap(disp, pm);
        pm = XCreatePixmap(disp, win, image_width, image_height, depth);
        imlib_context_set_drawable(pm);
        if (bg_im)
          {
             imlib_context_set_image(bg_im);
             imlib_free_image_and_decache();
          }
        bg_im = imlib_create_image(image_width, image_height);
        imlib_context_set_image(bg_im);
        for (y = 0; y < image_height; y += 8)
          {
             onoff = (y / 8) & 0x1;
             for (x = 0; x < image_width; x += 8)
               {
                  if (onoff)
                     imlib_context_set_color(144, 144, 144, 255);
                  else
                     imlib_context_set_color(100, 100, 100, 255);
                  imlib_image_fill_rectangle(x, y, 8, 8);
                  onoff++;
                  if (onoff == 2)
                     onoff = 0;
               }
          }
        imlib_render_image_part_on_drawable_at_size(0, 0, image_width,
                                                    image_height, 0, 0,
                                                    image_width, image_height);
        XSetWindowBackgroundPixmap(disp, win, pm);
        XResizeWindow(disp, win, image_width, image_height);
        XMapWindow(disp, win);
        XSync(disp, False);
     }
   imlib_context_set_anti_alias(0);
   imlib_context_set_dither(0);
   imlib_context_set_blend(1);
   imlib_blend_image_onto_image(im, 0,
                                update_x, update_y,
                                update_w, update_h,
                                update_x, update_y, update_w, update_h);
   imlib_context_set_blend(0);
   imlib_render_image_part_on_drawable_at_size(update_x, update_y,
                                               update_w, update_h,
                                               update_x, update_y,
                                               update_w, update_h);
   XSetWindowBackgroundPixmap(disp, win, pm);
   XClearArea(disp, win, update_x, update_y, update_w, update_h, False);
   XFlush(disp);
   return 1;
}

int
main(int argc, char **argv)
{
   Imlib_Image        *im = NULL;
   char               *file = NULL;
   int                 no = 1;
   const char         *display_name = getenv("DISPLAY");

   if (argc < 2)
      return 1;

   file = argv[no];
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
   win = XCreateSimpleWindow(disp, DefaultRootWindow(disp), 0, 0, 10, 10,
                             0, 0, 0);
   XSelectInput(disp, win, ButtonPressMask | ButtonReleaseMask |
                ButtonMotionMask | PointerMotionMask);
   imlib_context_set_display(disp);
   imlib_context_set_visual(vis);
   imlib_context_set_colormap(cm);
   imlib_context_set_progress_function(progress);
   imlib_context_set_progress_granularity(10);
   imlib_context_set_drawable(win);
   im = imlib_load_image(file);
   while (!im)
     {
        no++;
        if (no == argc)
          {
             fprintf(stderr, "Image format not available\n");
             exit(0);
          }
        file = argv[no];
        image_width = 0;
        im = imlib_load_image(file);
        imlib_context_set_image(im);
     }
   if (!im)
     {
        fprintf(stderr, "Image format not available\n");
        exit(0);
     }
   for (;;)
     {
        int                 x, y, b, count, fdsize, xfd, timeout = 0;
        XEvent              ev;
        static int          zoom_mode = 0, zx, zy;
        static double       zoom = 1.0;
        struct timeval      tval;
        fd_set              fdset;
        double              t1;

        XFlush(disp);
        XNextEvent(disp, &ev);
        switch (ev.type)
          {
          case ButtonPress:
             b = ev.xbutton.button;
             x = ev.xbutton.x;
             y = ev.xbutton.y;
             if (b == 3)
               {
                  zoom_mode = 1;
                  zx = x;
                  zy = y;
                  imlib_context_set_drawable(pm);
                  imlib_context_set_image(bg_im);
                  imlib_context_set_anti_alias(0);
                  imlib_context_set_dither(0);
                  imlib_context_set_blend(0);
                  imlib_render_image_part_on_drawable_at_size
                     (0, 0, image_width, image_height,
                      0, 0, image_width, image_height);
                  XSetWindowBackgroundPixmap(disp, win, pm);
                  XClearWindow(disp, win);
               }
             break;
          case ButtonRelease:
             b = ev.xbutton.button;
             x = ev.xbutton.x;
             y = ev.xbutton.y;
             if (b == 3)
                zoom_mode = 0;
             if (b == 1)
               {
                  no++;
                  if (no == argc)
                     no = argc - 1;
                  file = argv[no];
                  image_width = 0;
                  zoom = 1.0;
                  zoom_mode = 0;
                  imlib_context_set_image(im);
                  imlib_free_image_and_decache();
                  im = imlib_load_image(file);
                  while (!im)
                    {
                       no++;
                       if (no == argc)
                          exit(0);
                       file = argv[no];
                       image_width = 0;
                       im = imlib_load_image(file);
                    }
                  imlib_context_set_image(im);
               }
             if (b == 2)
               {
                  no--;
                  if (no == 0)
                     no = 1;
                  file = argv[no];
                  image_width = 0;
                  zoom = 1.0;
                  zoom_mode = 0;
                  imlib_context_set_image(im);
                  imlib_free_image_and_decache();
                  im = imlib_load_image(file);
                  while (!im)
                    {
                       no--;
                       if (no == 0)
                          no = 1;
                       file = argv[no];
                       image_width = 0;
                       im = imlib_load_image(file);
                    }
                  imlib_context_set_image(im);
               }
             break;
          case MotionNotify:
             while (XCheckTypedWindowEvent(disp, win, MotionNotify, &ev));
             x = ev.xmotion.x;
             y = ev.xmotion.y;
             if (zoom_mode)
               {
                  int                 sx, sy, sw, sh, dx, dy, dw, dh;

                  zoom = ((double)x - (double)zx) / 32.0;
                  if (zoom < 0)
                     zoom = 1.0 + ((zoom * 32.0) / ((double)(zx + 1)));
                  else
                     zoom += 1.0;
                  if (zoom <= 0.0001)
                     zoom = 0.0001;
                  if (zoom > 1.0)
                    {
                       dx = 0;
                       dy = 0;
                       dw = image_width;
                       dh = image_height;

                       sx = zx - (zx / zoom);
                       sy = zy - (zy / zoom);
                       sw = image_width / zoom;
                       sh = image_height / zoom;
                    }
                  else
                    {
                       dx = zx - (zx * zoom);
                       dy = zy - (zy * zoom);
                       dw = image_width * zoom;
                       dh = image_height * zoom;

                       sx = 0;
                       sy = 0;
                       sw = image_width;
                       sh = image_height;
                    }
                  imlib_context_set_anti_alias(0);
                  imlib_context_set_dither(0);
                  imlib_context_set_blend(0);
                  imlib_context_set_image(bg_im);
                  imlib_render_image_part_on_drawable_at_size
                     (sx, sy, sw, sh, dx, dy, dw, dh);
                  XSetWindowBackgroundPixmap(disp, win, pm);
                  XClearWindow(disp, win);
                  XFlush(disp);
                  timeout = 1;
               }
          default:
             break;
          }
        t1 = 0.2;
        tval.tv_sec = (long)t1;
        tval.tv_usec = (long)((t1 - ((double)tval.tv_sec)) * 1000000);
        xfd = ConnectionNumber(disp);
        fdsize = xfd + 1;
        FD_ZERO(&fdset);
        FD_SET(xfd, &fdset);
        if (timeout)
           count = select(fdsize, &fdset, NULL, NULL, &tval);
        else
           count = select(fdsize, &fdset, NULL, NULL, NULL);
        if (count < 0)
          {
             if ((errno == ENOMEM) || (errno == EINVAL) || (errno == EBADF))
                exit(1);
          }
        else
          {
             if ((count == 0) && (timeout))
               {
                  int                 sx, sy, sw, sh, dx, dy, dw, dh;

                  if (zoom > 1.0)
                    {
                       dx = 0;
                       dy = 0;
                       dw = image_width;
                       dh = image_height;

                       sx = zx - (zx / zoom);
                       sy = zy - (zy / zoom);
                       sw = image_width / zoom;
                       sh = image_height / zoom;
                    }
                  else
                    {
                       dx = zx - (zx * zoom);
                       dy = zy - (zy * zoom);
                       dw = image_width * zoom;
                       dh = image_height * zoom;

                       sx = 0;
                       sy = 0;
                       sw = image_width;
                       sh = image_height;
                    }
                  imlib_context_set_anti_alias(1);
                  imlib_context_set_dither(1);
                  imlib_context_set_blend(0);
                  imlib_context_set_image(bg_im);
                  imlib_render_image_part_on_drawable_at_size
                     (sx, sy, sw, sh, dx, dy, dw, dh);
                  XSetWindowBackgroundPixmap(disp, win, pm);
                  XClearWindow(disp, win);
                  XFlush(disp);
                  timeout = 0;
               }
          }
     }
   return 0;
}
