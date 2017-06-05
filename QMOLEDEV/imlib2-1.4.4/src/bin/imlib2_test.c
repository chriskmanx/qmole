#include "config.h"
/* include X11 stuff */
#include <X11/Xlib.h>
/* include Imlib2 stuff */
#include <Imlib2.h>
/* needed for sprintf and fprintf */
#include <stdio.h>
/* needed for getenv */
#include <stdlib.h>

/* some globals for our window & X display */
Display            *disp;
Window              win;
Visual             *vis;
Colormap            cm;
int                 depth;

/* the program... */
int
main(int argc, char **argv)
{
   /* events we get from X */
   XEvent              ev;

   /* areas to update */
   Imlib_Updates       updates, current_update;

   /* our virtual framebuffer image we draw into */
   Imlib_Image         buffer;

   /* a font */
   Imlib_Font          font;

   /* our color range */
   Imlib_Color_Range   range;

   /* our mouse x, y coordinates */
   int                 mouse_x = 0, mouse_y = 0;

   const char         *display_name = getenv("DISPLAY");

   /* connect to X */
   if (display_name == NULL)
       display_name = ":0";
   disp = XOpenDisplay(display_name);
   if (disp == NULL)
     {
       fprintf(stderr, "Can't open display %s\n", display_name);
       return 1;
     }
   /* get default visual , colormap etc. you could ask imlib2 for what it */
   /* thinks is the best, but this example is intended to be simple */
   vis = DefaultVisual(disp, DefaultScreen(disp));
   depth = DefaultDepth(disp, DefaultScreen(disp));
   cm = DefaultColormap(disp, DefaultScreen(disp));
   /* create a window 640x480 */
   win = XCreateSimpleWindow(disp, DefaultRootWindow(disp),
                             0, 0, 640, 480, 0, 0, 0);
   /* tell X what events we are interested in */
   XSelectInput(disp, win, ButtonPressMask | ButtonReleaseMask |
                PointerMotionMask | ExposureMask);
   /* show the window */
   XMapWindow(disp, win);
   /* set our cache to 2 Mb so it doesnt have to go hit the disk as long as */
   /* the images we use use less than 2Mb of RAM (that is uncompressed) */
   imlib_set_cache_size(2048 * 1024);
   /* set the font cache to 512Kb - again to avoid re-loading */
   imlib_set_font_cache_size(512 * 1024);
   /* add the ./ttfonts dir to our font path - you'll want a notepad.ttf */
   /* in that dir for the text to display */
   imlib_add_path_to_font_path(PACKAGE_DATA_DIR"/data/fonts");
   /* set the maximum number of colors to allocate for 8bpp and less to 128 */
   imlib_set_color_usage(128);
   /* dither for depths < 24bpp */
   imlib_context_set_dither(1);
   /* set the display , visual, colormap and drawable we are using */
   imlib_context_set_display(disp);
   imlib_context_set_visual(vis);
   imlib_context_set_colormap(cm);
   imlib_context_set_drawable(win);
   /* infinite event loop */
   for (;;)
     {
        /* image variable */
        Imlib_Image         image;

        /* width and height values */
        int                 w, h, text_w, text_h;

        /* init our updates to empty */
        updates = imlib_updates_init();
        /* while there are events form X - handle them */
        do
          {
             XNextEvent(disp, &ev);
             switch (ev.type)
               {
                 case Expose:
                    /* window rectangle was exposed - add it to the list of */
                    /* rectangles we need to re-render */
                    updates = imlib_update_append_rect(updates,
                                                       ev.xexpose.x,
                                                       ev.xexpose.y,
                                                       ev.xexpose.width,
                                                       ev.xexpose.height);
                    break;
                 case ButtonPress:
                    /* if we click anywhere in the window, exit */
                    exit(0);
                    break;
                 case MotionNotify:
                    /* if the mouse moves - note it */
                    /* add a rectangle update for the new mouse position */
                    image = imlib_load_image(PACKAGE_DATA_DIR"/data/images/mush.png");
                    imlib_context_set_image(image);
                    w = imlib_image_get_width();
                    h = imlib_image_get_height();
                    imlib_context_set_image(image);
                    imlib_free_image();
                    /* the old position - so we wipe over where it used to be */
                    updates = imlib_update_append_rect(updates,
                                                       mouse_x - (w / 2),
                                                       mouse_y - (h / 2), w, h);
                    font = imlib_load_font("notepad/30");
                    if (font)
                      {
                         char                text[4096];

                         imlib_context_set_font(font);
                         sprintf(text, "Mouse is at %i, %i", mouse_x, mouse_y);
                         imlib_get_text_size(text, &text_w, &text_h);
                         imlib_free_font();
                         updates = imlib_update_append_rect(updates,
                                                            320 - (text_w / 2),
                                                            240 - (text_h / 2),
                                                            text_w, text_h);
                      }

                    mouse_x = ev.xmotion.x;
                    mouse_y = ev.xmotion.y;
                    /* the new one */
                    updates = imlib_update_append_rect(updates,
                                                       mouse_x - (w / 2),
                                                       mouse_y - (h / 2), w, h);
                    font = imlib_load_font("notepad/30");
                    if (font)
                      {
                         char                text[4096];

                         imlib_context_set_font(font);
                         sprintf(text, "Mouse is at %i, %i", mouse_x, mouse_y);
                         imlib_get_text_size(text, &text_w, &text_h);
                         imlib_free_font();
                         updates = imlib_update_append_rect(updates,
                                                            320 - (text_w / 2),
                                                            240 - (text_h / 2),
                                                            text_w, text_h);
                      }
                 default:
                    /* any other events - do nothing */
                    break;
               }
          }
        while (XPending(disp));

        /* no more events for now ? ok - idle time so lets draw stuff */

        /* take all the little rectangles to redraw and merge them into */
        /* something sane for rendering */
        updates = imlib_updates_merge_for_rendering(updates, 640, 480);
        for (current_update = updates;
             current_update;
             current_update = imlib_updates_get_next(current_update))
          {
             int                 up_x, up_y, up_w, up_h;

             /* find out where the first update is */
             imlib_updates_get_coordinates(current_update,
                                           &up_x, &up_y, &up_w, &up_h);

             /* create our buffer image for renderign this update */
             buffer = imlib_create_image(up_w, up_h);

             /* we can blend stuff now */
             imlib_context_set_blend(1);

             /* fill the window background */
             /* load the background image - you'll need to have some images */
             /* in ./test_images lying around for this to actually work */
             image = imlib_load_image(PACKAGE_DATA_DIR"/data/images/bg.png");
             /* we're working with this image now */
             imlib_context_set_image(image);
             /* get its size */
             w = imlib_image_get_width();
             h = imlib_image_get_height();
             /* now we want to work with the buffer */
             imlib_context_set_image(buffer);
             /* if the iimage loaded */
             if (image)
               {
                  /* blend image onto the buffer and scale it to 640x480 */
                  imlib_blend_image_onto_image(image, 0,
                                               0, 0, w, h,
                                               -up_x, -up_y, 640, 480);
                  /* working with the loaded image */
                  imlib_context_set_image(image);
                  /* free it */
                  imlib_free_image();
               }

             /* draw an icon centered around the mouse position */
             image = imlib_load_image(PACKAGE_DATA_DIR"/data/images/mush.png");
             imlib_context_set_image(image);
             w = imlib_image_get_width();
             h = imlib_image_get_height();
             imlib_context_set_image(buffer);
             if (image)
               {
                  imlib_blend_image_onto_image(image, 0,
                                               0, 0, w, h,
                                               mouse_x - (w / 2) - up_x,
                                               mouse_y - (h / 2) - up_y, w, h);
                  imlib_context_set_image(image);
                  imlib_free_image();
               }

             /* draw a gradient on top of things at the top left of the window */
             /* create a range */
             range = imlib_create_color_range();
             imlib_context_set_color_range(range);
             /* add white opaque as the first color */
             imlib_context_set_color(255, 255, 255, 255);
             imlib_add_color_to_color_range(0);
             /* add an orange color, semi-transparent 10 units from the first */
             imlib_context_set_color(255, 200, 10, 100);
             imlib_add_color_to_color_range(10);
             /* add black, fully transparent at the end 20 units away */
             imlib_context_set_color(0, 0, 0, 0);
             imlib_add_color_to_color_range(20);
             /* draw the range */
             imlib_context_set_image(buffer);
             imlib_image_fill_color_range_rectangle(-up_x, -up_y, 128, 128,
                                                    -45.0);
             /* free it */
             imlib_free_color_range();

             /* draw text - centered with the current mouse x, y */
             font = imlib_load_font("notepad/30");
             if (font)
               {
                  char                text[4096];

                  /* set the current font */
                  imlib_context_set_font(font);
                  /* set the image */
                  imlib_context_set_image(buffer);
                  /* set the color (black) */
                  imlib_context_set_color(0, 0, 0, 255);
                  /* print text to display in the buffer */
                  sprintf(text, "Mouse is at %i, %i", mouse_x, mouse_y);
                  /* query the size it will be */
                  imlib_get_text_size(text, &text_w, &text_h);
                  /* draw it */
                  imlib_text_draw(320 - (text_w / 2) - up_x,
                                  240 - (text_h / 2) - up_y, text);
                  /* free the font */
                  imlib_free_font();
               }

             /* dont blend the image onto the drawable - slower */
             imlib_context_set_blend(0);
             /* set the buffer image as our current image */
             imlib_context_set_image(buffer);
             /* render the image at 0, 0 */
             imlib_render_image_on_drawable(up_x, up_y);
             /* don't need that temproary buffer image anymore */
             imlib_free_image();
          }
        /* if we had updates - free them */
        if (updates)
           imlib_updates_free(updates);
        /* loop again waiting for events */
     }
   return 0;
}
