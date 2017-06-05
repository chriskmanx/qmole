/*
  Module       : main.c
  Purpose      : GDK/Imlib Quick Image Viewer (qiv)
  More         : see qiv README
  Homepage     : http://qiv.spiegl.de/
  Original     : http://www.klografx.net/qiv/
  Policy       : GNU GPL
*/

#include <gdk/gdkx.h>
#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <ctype.h>
#include <string.h>

#ifdef HAVE_MAGIC
#include <magic.h>
#endif

#include "qiv.h"
#include "main.h"

qiv_image main_img;
qiv_mgl   magnify_img; /* [lc] */

static void filter_images(int *, char **);
static int check_extension(const char *);
static void qiv_signal_usr1();
static void qiv_signal_usr2();
static gboolean qiv_handle_timer(gpointer);
static void qiv_timer_restart(gpointer);

#ifdef HAVE_MAGIC
static int check_magic(const char *name);
#endif

int main(int argc, char **argv)
{
  struct timeval tv;

  // [as] workaround for problem with X composite extension
  // is this still needed with imlib2 ??
  putenv("XLIB_SKIP_ARGB_VISUALS=1");

/*
  // [as] thinks that this is not portable enough
  // [lc]
  // I use a virtual screen of 1600x1200, and the resolution is 1024x768,
  // so I changed how screen_[x,y] is obtained; it seems that gtk 1.2
  // cannot give the geometry of viewport, so I borrowed from the source of
  // xvidtune the code for calling XF86VidModeGetModeLine, this requires
  // the linking option -lXxf86vm.
  XF86VidModeModeLine modeline;
  int dot_clock;
*/

  /* Randomize seed for 'true' random */
  gettimeofday(&tv,NULL);
  srand(tv.tv_usec*1000000+tv.tv_sec);

  /* Initialize GDK */

  gdk_init(&argc,&argv);

  /* Set up our options, image list, etc */
  strncpy(select_dir, SELECT_DIR, sizeof select_dir);
  reset_mod(&main_img);
  options_read(argc, argv, &main_img);

  /* Load things from GDK/Imlib */

  qiv_main_loop = g_main_new(TRUE);
  cmap = gdk_colormap_get_system();
  screen_x = gdk_screen_width();
  screen_y = gdk_screen_height();

 /* statusbar with pango */
  layout = pango_layout_new(gdk_pango_context_get()); 
  fontdesc = pango_font_description_from_string (STATUSBAR_FONT);

  /* set fontsize to 8 if no fontsize is given */
  if(!pango_font_description_get_size(fontdesc))
  {
    pango_font_description_set_size(fontdesc,  PANGO_SCALE * STATUSBAR_FS);
  }
  metrics = pango_context_get_metrics (gdk_pango_context_get(), fontdesc, NULL);
  pango_layout_set_font_description (layout, fontdesc); 

/*
  // [as] thinks that this is not portable enough
  // [lc]
  // I use a virtual screen of 1600x1200, and the resolution is 1024x768,
  // so I changed how screen_[x,y] is obtained; it seems that gtk 1.2
  // cannot give the geometry of viewport, so I borrowed from the source of
  // xvidtune the code for calling XF86VidModeGetModeLine, this requires
  // the linking option -lXxf86vm.
  XF86VidModeGetModeLine(GDK_DISPLAY(), DefaultScreen(GDK_DISPLAY()),
                         &dot_clock, &modeline);
  //printf("> hdisplay %d vdisplay %d\n", modeline.hdisplay, modeline.vdisplay);
  screen_x=MIN(screen_x, modeline.hdisplay);
  screen_y=MIN(screen_y, modeline.vdisplay);
*/

#ifdef GTD_XINERAMA
  get_preferred_xinerama_screens();
//  screen_x=MIN(screen_x, preferred_screen->width);
//  screen_y=MIN(screen_y, preferred_screen->height);
#endif


  max_rand_num = images;

  if (filter) /* Filter graphic images */
    filter_images(&images,image_names);

  if (!images) { /* No images to display */
    g_print("qiv: cannot load any images.\n");
    usage(argv[0],1);
  }

  /* get colors */

  color_alloc(STATUSBAR_BG, &text_bg);
  color_alloc(ERROR_BG, &error_bg);
  color_alloc(image_bg_spec, &image_bg);

  /* Display first image first, except in random mode */

  if (random_order)
    next_image(0);

  //disabled because 'params' is never used, see above
  //if (to_root || to_root_t || to_root_s) {
  //  params.flags |= PARAMS_VISUALID;
  //  (GdkVisual*)params.visualid = gdk_window_get_visual(GDK_ROOT_PARENT());
  //}

  /* Setup callbacks */

  gdk_event_handler_set(qiv_handle_event, &main_img, NULL);
  qiv_timer_restart(NULL);

  /* And signal catchers */

  signal(SIGTERM, finish);
  signal(SIGINT, finish);
  signal(SIGUSR1, qiv_signal_usr1);
  signal(SIGUSR2, qiv_signal_usr2);

  /* Load & display the first image */

  qiv_load_image(&main_img);

  if(watch_file){
    g_idle_add (qiv_watch_file, &main_img);
  }

  g_main_run(qiv_main_loop); /* will never return */
  return 0;
}


void qiv_exit(int code)
{
  if (cmap) gdk_colormap_unref(cmap);
  destroy_image(&main_img);

  pango_font_description_free (fontdesc);
  g_object_unref (layout);
  pango_font_metrics_unref(metrics);

  g_main_destroy(qiv_main_loop);
  finish(SIGTERM);        /* deprecated, subject to change */
}


/*
 * functions for handling signals
 */

static void qiv_signal_usr1()
{
  next_image(1);
  qiv_load_image(&main_img);
}

static void qiv_signal_usr2()
{
  next_image(-1);
  qiv_load_image(&main_img);
}


/*
 * Slideshow timer function
 *
 * If this function returns false, the timer is destroyed
 * and qiv_timer_restart() is automatically called, which
 * then starts the timer again. Thus images which takes some
 * time to load will still be displayed for "delay" seconds.
 */

static gboolean qiv_handle_timer(gpointer data)
{
  if (*(int *)data || slide) {
    next_image(0);
    qiv_load_image(&main_img);
  }
  return FALSE;
}


/*
 *    Slideshow timer (re)start function
 */

static void qiv_timer_restart(gpointer dummy)
{
  g_timeout_add_full(G_PRIORITY_DEFAULT_IDLE, delay,
                     qiv_handle_timer, &slide,
                     qiv_timer_restart);
}

/* Filter images by extension */

static void filter_images(int *images, char **image_names)
{
  int i = 0;

  while(i < *images) {
    if (check_extension(image_names[i])
#ifdef HAVE_MAGIC
            || check_magic(image_names[i])
#endif
       ) {
      i++;
    } else {
      int j = i;
      if (j < *images-1)
          image_idx--;
      while(j < *images-1) {
        image_names[j] = image_names[j+1];
        ++j;
      }
      --(*images);
    }
  }
  if (image_idx < 0)
    image_idx = 0;
}

static int check_extension(const char *name)
{
  char *extn = strrchr(name, '.');
  int i;

  if (extn)
    for (i=0; image_extensions[i]; i++)
      if (strcasecmp(extn, image_extensions[i]) == 0)
        return 1;

  return 0;
}

#ifdef HAVE_MAGIC
static int check_magic(const char *name)
{
  magic_t cookie;
  const char *description=NULL;
  int i;
  int ret=0;

  cookie = magic_open(MAGIC_NONE);
  magic_load(cookie,NULL);
  description = magic_file(cookie, name);
  if(description)
  {
    for(i=0; image_magic[i]; i++ )
      if (strncasecmp(description, image_magic[i], strlen(image_magic[i])) == 0)
      {
        ret = 1;
        break;
      }
  }
  magic_close(cookie);
  return ret;
}
#endif
