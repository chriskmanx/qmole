/*
  Up to release 1.3 of imlib2 there is a problem with the includes.
  Imlib2.h does not load Xlib.h.
  Workaround: add the following line:
#include <X11/Xlib.h>
*/
#include <gdk/gdk.h>
#include <Imlib2.h>
#include <unistd.h>
#include <stdlib.h>
#ifdef GTD_XINERAMA
# include <X11/Xlib.h>
# include <X11/extensions/Xinerama.h>
#endif

/*
  // [as] thinks that this is not portable enough
  // [lc]
  // I use a virtual screen of 1600x1200, and the resolution is 1024x768,
  // so I changed how screen_[x,y] is obtained; it seems that gtk 1.2
  // cannot give the geometry of viewport, so I borrowed from the source of
  // xvidtune the code for calling XF86VidModeGetModeLine, this requires
  // the linking option -lXxf86vm.
#include <X11/Xlib.h>
#include <X11/extensions/xf86vmode.h> // for XF86VidModeGetModeLine
*/

#define VERSION "2.2.4"
#define VERSION_FULL "QIV - Quick Image Viewer v2.2.4 - http://qiv.spiegl.de/"
#define TRASH_DIR ".qiv-trash"
#define SELECT_DIR ".qiv-select"
#define SLIDE_DELAY 3000 /* milliseconds */
#define IMAGE_BG "black"
#define STATUSBAR_BG "#FFB900"
#define STATUSBAR_FS 8  /* default fontsize if none is given */
#define ERROR_BG "#0000FF"
#define DEFAULT_BRIGHTNESS 256
#define DEFAULT_CONTRAST 256
#define DEFAULT_GAMMA 256
#define BUF_LEN 1024

/* FILENAME_LEN is the maximum length of any path/filename that can be
 * handled.  MAX_DELETE determines how many items can be placed into
 * the kill-ring for undelete handling. */

#define FILENAME_LEN 1024
#define MAX_DELETE 1024

/* [pw] Warning: qiv uses the imlib 1.x units. Not what imlib 2 wants. */
typedef struct _qiv_color_modifier {
  int gamma, brightness, contrast;
} qiv_color_modifier;

typedef struct _qiv_image {
  qiv_color_modifier mod; /* Image modifier (for brightness..) */
  GdkPixmap *p; /* Pixmap of the image to display */
  GdkWindow *win; /* Main window for windowed and fullscreen mode */
  int error; /* 1 if Imlib couldn't load image */
  gint win_x, win_y, win_w, win_h; /* window co-ordinates */
  gint orig_w, orig_h; /* Size of original image in pixels */
  GdkGC *bg_gc;     /* image window background */
  GdkGC *text_gc;   /* statusbar text color */
  GdkGC *status_gc; /* statusbar background */

  /* These are used to work out how to redraw in fullscreen mode */
  gint win_ox, win_oy, win_ow, win_oh; /* coordinates currently drawn at */
  gint text_ow, text_oh; /* old size of the statusbar */
  int statusbar_was_on;  /* true if statusbar was visible last frame */
  int exposed;           /* window became visible */
  int drag;              /* user is currently dragging the image */
  double drag_start_x, drag_start_y; /* position of cursor at drag start */
  int drag_win_x, drag_win_y; /* position of win at drag start */
//  char        infotext[BUF_LEN];
  gchar win_title[BUF_LEN];
  gint text_len, text_w, text_h;
} qiv_image;

typedef struct _qiv_mgl {
  /* [pw] needs a seperate context? */
  qiv_color_modifier mod; /* Image modifier (for brightness..) */
  GdkPixmap *p; /* Pixmap of the image to display */
  GdkWindow *win; /* window for magnify */
  gint win_x, win_y, win_w, win_h; /* window coordinates */
  gint frame_x, frame_y; /* UpLeft Corner of frame (titlebar included) of */
                         /* main qiv_image */
//  GdkGC *bg_gc;     /* image window background */
//  int exposed;        /* window became visible */
} qiv_mgl;  /* the magnifying glass [lc] */

typedef struct _qiv_deletedfile {
  char *filename, *trashfile;
  int pos;
} qiv_deletedfile;

extern int              first;
extern char             infotext[BUF_LEN];
extern GMainLoop        *qiv_main_loop;
extern gint             screen_x, screen_y;
extern GdkFont          *text_font;
extern GdkColormap      *cmap;
extern char             *image_bg_spec;
extern GdkColor         image_bg;
extern GdkColor         text_bg;
extern GdkColor         error_bg;
extern int              images;
extern char             **image_names;
extern int              image_idx;
extern int              max_image_cnt;
extern time_t           current_mtime;
extern qiv_deletedfile  *deleted_files;
extern int              delete_idx;
extern char             select_dir[FILENAME_LEN];
extern PangoLayout     *layout;
extern PangoFontDescription *fontdesc;
extern PangoFontMetrics *metrics;

extern int     filter;
extern gint    center;
extern gint    cycle;
extern gint    default_brightness;
extern gint    default_contrast;
extern gint    default_gamma;
extern gint    delay;
extern int     readonly;
extern int     random_order;
extern int     random_replace;
extern int     fullscreen;
extern int     maxpect;
extern int     statusbar_fullscreen;
extern int     statusbar_window;
extern int     slide;
extern int     scale_down;
extern int     recursive;
extern int     to_root;
extern int     to_root_t;
extern int     to_root_s;
extern int     transparency;
extern int     do_grab;
extern int     disable_grab;
extern int     max_rand_num;
extern int     fixed_window_size;
extern int     fixed_zoom_factor;
extern int     zoom_factor;
extern int     watch_file;
extern int     browse;
extern int     magnify;     // [lc]
extern qiv_mgl magnify_img; // [lc]
extern int     autorotate;
extern int     rotation;

extern const char   *helpstrs[], **helpkeys, *image_extensions[];

#ifdef GTD_XINERAMA
extern XineramaScreenInfo preferred_screen[1];
extern XineramaScreenInfo statusbar_screen[1];
extern int user_screen;
extern int number_xinerama_screens;
extern void get_preferred_xinerama_screens(void);
#endif

/* main.c */

extern void qiv_exit(int);
extern void qiv_load_image();

/* image.c */

/* Modes for update_image */
#define REDRAW 0
#define MOVED  1
#define ZOOMED 2
#define FULL_REDRAW 3

extern void qiv_load_image(qiv_image *);
extern void set_desktop_image(qiv_image *);
extern void zoom_in(qiv_image *);
extern void zoom_out(qiv_image *);
extern void zoom_maxpect(qiv_image *);
extern void reload_image(qiv_image *q);
extern void reset_coords(qiv_image *);
extern void check_size(qiv_image *, gint);
extern void render_to_pixmap(qiv_image *, double *);
extern void update_image(qiv_image *, int);
extern void reset_mod(qiv_image *);
extern void destroy_image(qiv_image *q);
extern void center_image(qiv_image *q);
extern void correct_image_position(qiv_image *q);
extern void hide_cursor(qiv_image *q);
extern void show_cursor(qiv_image *q);
extern void setup_magnify(qiv_image *, qiv_mgl *); // [lc]
extern void update_magnify(qiv_image *, qiv_mgl *,int, gint, gint); // [lc]

/* event.c */

extern void qiv_handle_event(GdkEvent *, gpointer);

/* options.c */

extern void options_read(int, char **, qiv_image *);

/* utils.c */

extern int  move2trash(void);
extern int  copy2select(void);
extern int  undelete_image(void);
extern void jump2image(char *);
extern void run_command(qiv_image *, char *, char *, int *, const char ***);
extern void finish(int);
extern void next_image(int);
extern int checked_atoi(const char *);
extern void usage(char *, int);
extern void show_help(char *, int);
extern int get_random(int, int, int);
extern gboolean color_alloc(const char *, GdkColor *);
extern void swap(int *, int *);
#define myround qiv_round
extern int myround(double);
extern gboolean qiv_watch_file (gpointer);
extern int rreaddir(const char *, int);
extern int rreadfile(const char *);
extern int find_image(int images, char **image_names, char *name);
