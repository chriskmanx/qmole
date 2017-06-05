/*
  Module       : utils.c
  Purpose      : Various utilities for qiv
  More         : see qiv README
  Policy       : GNU GPL
  Homepage     : http://qiv.spiegl.de/
  Original     : http://www.klografx.net/qiv/
*/

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <gdk/gdkx.h>
#include <dirent.h>
#include <fcntl.h>
#include <errno.h>
#include "qiv.h"
#include "xmalloc.h"

#ifdef STAT_MACROS_BROKEN
#undef S_ISDIR
#endif

#if !defined(S_ISDIR) && defined(S_IFDIR)
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

/* move current image to .qiv-trash */
int move2trash()
{
  char *ptr, *ptr2, *filename = image_names[image_idx];
  char trashfile[FILENAME_LEN], path_result[PATH_MAX];
  int i;

  if (readonly)
    return 0;

  if(!(ptr = strrchr(filename, '/'))) {   /* search rightmost slash */
    /* no slash in filename */
    strncpy(path_result, filename, PATH_MAX);
    /* move file to TRASH_DIR/filename */
    snprintf(trashfile, sizeof trashfile, "%s/%s", TRASH_DIR, path_result);
  } else {
    /* find full path to file */
    *ptr = 0;
    if(!realpath(filename,path_result)) {
      g_print("Error: realpath failure while moving file to trash\a\n");
      *ptr = '/';
      return 1;
    }
    /* move file to fullpath/TRASH_DIR/filename */
    snprintf(trashfile, sizeof trashfile, "%s/%s/%s", path_result, TRASH_DIR, ptr+1);

    strncat(path_result, "/", PATH_MAX - strlen(path_result) );
    strncat(path_result, ptr + 1, PATH_MAX - strlen(path_result) );
    *ptr = '/';
  }

#ifdef DEBUG
  g_print("*** trashfile: '%s'\n",trashfile);
#endif
  ptr = ptr2 = trashfile;
  if(trashfile[0] == '/') {  /* filename starts with a slash? */
    ptr += 1;
  }
  while((ptr = strchr(ptr,'/'))) {
    *ptr = '\0';
    if(access(ptr2,F_OK)) {
      if(mkdir(ptr2,0700)) {
        g_print("Error: Could not make directory '%s'\a\n",ptr2);
        return 1;
      }
    }
    *ptr = '/';
    ptr += 1;
  }

  unlink(trashfile); /* Just in case it already exists... */
  if(rename(filename,trashfile)) {
    g_print("Error: Could not rename '%s' to '%s'\a\n",filename,trashfile);
    return 1;
  } else {
    qiv_deletedfile *del;

    if (!deleted_files)
      deleted_files = (qiv_deletedfile*)xcalloc(MAX_DELETE,sizeof *deleted_files);

    del = &deleted_files[delete_idx++];
    if (delete_idx == MAX_DELETE)
      delete_idx = 0;
    if (del->filename)
	free(del->trashfile);
    del->filename = filename;
    del->trashfile = strdup(trashfile);
    del->pos = image_idx;

    --images;
    for(i=image_idx;i<images;++i) {
      image_names[i] = image_names[i+1];
    }

    /* If deleting the last file out of x */
    if(images == image_idx)
      image_idx = 0;

    /* If deleting the only file left */
    if(!images)
      exit(0);
  }
  return 0;
}

/* copy current image to SELECTDIR */
int copy2select()
{
  char *ptr, *filename = image_names[image_idx];
  char dstfile[FILENAME_LEN], dstfilebak[FILENAME_LEN], tmp[FILENAME_LEN], buf[BUFSIZ];
  int fdi, fdo, n, n2;

  /* try to create something; if select_dir doesn't exist, create one */
  snprintf(dstfile, sizeof dstfile, "%s/.qiv-select", select_dir);
  if((n = open(dstfile, O_CREAT, 0666)) == -1) {
    switch(errno) {
      case EEXIST:
        unlink(dstfile);
        break;
      case ENOTDIR:
      case ENOENT:
        if(mkdir(select_dir, 0777) == -1) {
          g_print("*** Error: Cannot create select_dir '%s': %s\a\n", select_dir, strerror(errno));
          return -1;
        }
        break;
      default:
        g_print("*** Error: Cannot open select_dir '%s': %s\a\n", select_dir, strerror(errno));
        return -1;
    }
  } else {
    close(n);
    unlink(dstfile);
  }

  if((ptr = strrchr(filename, '/')) != NULL) {   /* search rightmost slash */
    ptr++;
  } else {
    ptr = filename;
  }
  snprintf(dstfile, sizeof dstfile, "%s/%s", select_dir, ptr);

#ifdef DEBUG
  g_print("*** selectfile: '%s'\n",dstfile);
#endif
  ptr = dstfile;

  /* Just in case the file already exists... */
  /* unlink(dstfile); */
  snprintf(dstfilebak, sizeof dstfilebak, "%s", dstfile);
  while ((n2 = open(dstfilebak, O_RDONLY)) != -1) {
    close(n2);
    snprintf(tmp, sizeof dstfilebak, "%s.bak", dstfilebak);
    strncpy(dstfilebak, tmp, sizeof dstfilebak);
  }
  if ( strncmp(dstfile, dstfilebak, sizeof dstfile) != 0 ) {
#ifdef DEBUG
    g_print("*** renaming: '%s' to '%s'\n",dstfile,dstfilebak);
#endif
    rename(dstfile, dstfilebak);
  }

  fdi = open(filename, O_RDONLY);
  fdo = open(dstfile, O_CREAT | O_WRONLY, 0666);
  if(fdi == -1 || fdo == -1) {
    g_print("*** Error: Could not copy file: '%s'\a\n", strerror(errno));
  }
  while((n = read(fdi, buf, BUFSIZ)) > 0) write(fdo, buf, n);
  close(fdi);
  close(fdo);

  return 0;
}


/* move the last deleted image out of the delete list */
int undelete_image()
{
  int i;
  qiv_deletedfile *del;
  char *ptr;

  if (readonly)
    return 0;

  if (!deleted_files) {
    g_print("Error: nothing to undelete\a\n");
    return 1;
  }
  if (--delete_idx < 0)
    delete_idx = MAX_DELETE - 1;
  del = &deleted_files[delete_idx];
  if (!del->filename) {
    g_print("Error: nothing to undelete\a\n");
    return 1;
  }

  if (rename(del->trashfile,del->filename) < 0) {
    g_print("Error: undelete_image '%s' failed\a\n", del->filename);
    del->filename = NULL;
    free(del->trashfile);
    return 1;
  }

  /* unlink TRASH_DIR if empty */
  ptr = del->trashfile;
  /* the path without the filename is the TRASH_DIR */
  ptr = strrchr(ptr,'/');
  *ptr = '\0';
  if(rmdir(del->trashfile)) {
    /* we can't delete the TRASH_DIR because there are still files */
  }
  *ptr = '/';

  image_idx = del->pos;
  for(i=images;--i>=image_idx;) {
    image_names[i+1] = image_names[i];
  }
  images++;
  image_names[image_idx] = del->filename;
  del->filename = NULL;
  free(del->trashfile);

  return 0;
}

#define MAXOUTPUTBUFFER 16384
#define MAXLINES 100

/* run a command ... */
void run_command(qiv_image *q, char *n, char *filename, int *numlines, const char ***output)
{
  static char nr[100];
  static char *buffer = 0;
  static const char *lines[MAXLINES + 1];
  int pipe_stdout[2];
  int pid;
  char *newfilename;
  int i;
  struct stat before, after;

  stat(filename, &before);

  if (!buffer)
    buffer = xmalloc(MAXOUTPUTBUFFER + 1);

  *numlines = 0;
  *output = lines;

  snprintf(infotext, sizeof infotext, "Running: 'qiv-command %s %s'", n, filename);

  snprintf(nr, sizeof nr, "%s", n);

  /* Use some pipes for stdout and stderr */

  if (pipe(pipe_stdout) < 0) {
    perror("pipe");
    return;
  }

  pid = fork();

  if (pid == 0) {
    /* Child */
    dup2(pipe_stdout[1], 1);
    dup2(pipe_stdout[1], 2);
    close(pipe_stdout[1]);

    execlp("qiv-command", "qiv-command", nr, filename, NULL);
    perror("Error calling qiv-command");
    abort();
  }
  else if (pid > 0) {
    /* parent */
    int len = 0;
    char *p = buffer;

    gboolean finished = FALSE;
    close(pipe_stdout[1]);

    do {
      char *s = p, *q;
      finished = waitpid(pid, 0, WNOHANG) > 0;
      len = read(pipe_stdout[0], s, MAXOUTPUTBUFFER - (s - buffer));

      if (len < 0 || (finished && len == 0))
	break;

      s[len] = '\0';
      /* Process the buffer into lines */
      for (; *numlines < MAXLINES && p < s + len; ) {
	lines[(*numlines)++] = p;
	/* Find the end of the line */
	q = strchr(p, '\n');
	if (!q) break;
	*q = '\0';
	p = q + 1;
      }
    } while (len > 0);
    lines[(*numlines)] = 0;
    if (!finished)
      waitpid(pid, 0, 0);

    close(pipe_stdout[0]);
  }
  else {
    perror("fork");
    return;
  }

  /* Check for special keyword "NEWNAME=" in first line
   * indicating that the filename has changed */
  if ( lines[0] && strncmp(lines[0], "NEWNAME=", 8) == 0 ) {
    newfilename = strdup(lines[0]);
    newfilename += 8;
#ifdef DEBUG
    g_print("*** filename has changed from: '%s' to '%s'\n", image_names[image_idx], newfilename);
#endif

    image_names[image_idx] = strdup(newfilename);
    filename = strdup(newfilename);

    /* delete this line from the output */
    (*numlines)--;
    lines[0] = 0;
    for (i = 0; i < *numlines; i++) {
      lines[i] = lines[i+1];
    }
    update_image(q, FULL_REDRAW);
    return;
  }

  stat(filename, &after);

  /* If image modified reload, otherwise redraw */
  if (before.st_size == after.st_size &&
      before.st_ctime == after.st_ctime &&
      before.st_mtime == after.st_mtime)
    update_image(q, FULL_REDRAW);
  else
    qiv_load_image(q);
}


/*
   This routine jumps x images forward or backward or
   directly to image x
   Enter jf10\n ... jumps 10 images forward
   Enter jb5\n  ... jumps 5 images backward
   Enter jt15\n ... jumps to image 15
*/
void jump2image(char *cmd)
{
  int direction = 0;
  int x;

#ifdef DEBUG
    g_print("*** starting jump2image function: '%s'\n", cmd);
#endif

  if(cmd[0] == 'f' || cmd[0] == 'F')
    direction = 1;
  else if(cmd[0] == 'b' || cmd[0] == 'B')
    direction = -1;
  else if(!(cmd[0] == 't' || cmd[0] == 'T'))
    return;

  /* get number of images to jump or image to jump to */
  x = atoi(cmd+1);

  if (direction == 1) {
    if ((image_idx + x) > (images-1))
      image_idx = images-1;
    else
      image_idx += x;
  }
  else if (direction == -1) {
    if ((image_idx - x) < 0)
      image_idx = 0;
    else
      image_idx -= x;
  }
  else {
    if (x > images || x < 1)
      return;
    else
      image_idx = x-1;
  }

#ifdef DEBUG
    g_print("*** end of jump2image function\n");
#endif

}

void finish(int sig)
{
  gdk_pointer_ungrab(CurrentTime);
  gdk_keyboard_ungrab(CurrentTime);
  exit(0);
}

/*
  Update selected image index image_idx
  Direction determines if the next or the previous
  image is selected.
*/
void next_image(int direction)
{
  static int last_modif = 1;	/* Delta of last change of index of image */

  if (!direction)
    direction = last_modif;
  else
    last_modif = direction;
  if (random_order)
    image_idx = get_random(random_replace, images, direction);
  else {
    image_idx = (image_idx + direction) % images;
    if (image_idx < 0)
      image_idx += images;
    else if (cycle && image_idx == 0)
      qiv_exit(0);
  }
}

int checked_atoi (const char *s)
{
    char *endptr;
    int num = strtol(s, &endptr, 0);

    if (endptr == s || *endptr != '\0') {
	g_print("Error: %s is not a valid number.\n", s);
	gdk_exit(1);
    }

    return num;
}

void usage(char *name, int exit_status)
{
    g_print("qiv (Quick Image Viewer) v%s\n"
	"Usage: qiv [options] files ...\n"
	"See 'man qiv' or type 'qiv --help' for options.\n",
        VERSION);

    gdk_exit(exit_status);
}

void show_help(char *name, int exit_status)
{
    int i;

    g_print("qiv (Quick Image Viewer) v%s\n"
	"Usage: qiv [options] files ...\n\n",
        VERSION);

    g_print(
          "General options:\n"
          "    --file, -F x           Read file names from text file x or stdin\n"
          "    --bg_color, -o x       Set root background color to x\n"
          "    --brightness, -b x     Set brightness to x (-32..32)\n"
          "    --browse, -B           Scan directory of file for browsing\n"
          "    --center, -e           Disable window centering\n"
          "    --contrast, -c x       Set contrast to x (-32..32)\n"
          "    --cycle, -C            do not cycle after last image\n"
          "    --display x            Open qiv window on display x\n"
          "    --do_grab, -a          Grab the pointer in windowed mode\n"
          "    --disable_grab, -G     Disable pointer/kbd grab in fullscreen mode\n"
          "    --fixed_width, -w x    Window with fixed width x\n"
          "    --fixed_zoom, -W x     Window with fixed zoom factor (percentage x)\n"
          "    --fullscreen, -f       Use fullscreen window on start-up\n"
          "    --gamma, -g x          Set gamma to x (-32..32)\n"
          "    --help, -h             This help screen\n"
          "    --ignore_path_sort, -P Sort filenames by the name only\n"
          "    --readonly, -R         Disable the deletion feature\n"
          "    --maxpect, -m          Zoom to screen size and preserve aspect ratio\n"
          "    --merged_case_sort, -M Sort filenames with AaBbCc... alpha order\n"
          "    --no_filter, -n        Do not filter images by extension\n"
          "    --no_statusbar, -i     Disable statusbar\n"
          "    --statusbar, -I        Enable statusbar\n"
          "    --no_sort, -D          Do not apply any sorting to the list of files\n"
          "    --numeric_sort, -N     Sort filenames with numbers intuitively\n"
          "    --root, -x             Set centered desktop background and exit\n"
          "    --root_t, -y           Set tiled desktop background and exit\n"
          "    --root_s, -z           Set stretched desktop background and exit\n"
          "    --scale_down, -t       Shrink image(s) larger than the screen to fit\n"
          "    --transparency, -p     Enable transparency for transparent images\n"
          "    --watch, -T            Reload the image if it has changed on disk\n"
          "    --recursivedir, -u     Recursively include all files\n"
          "    --select_dir, -A x     Store the selected files in dir x (default is .qiv-select)\n"
#if GDK_PIXBUF_MINOR >= 12
          "    --autorotate, -l       Autorotate JPEGs according to EXIF rotation tag\n"
#endif
          "    --rotate, -q x         Rotate 90(x=1),180(x=2),270(x=3) degrees clockwise\n"
#ifdef GTD_XINERAMA
          "    --xineramascreen, -X x Use screen x as preferred Xinerama screen\n"
#endif
          "    --version, -v          Print version information and exit\n"
          "\n"
          "Slideshow options:\n"
	  "This can also be used for the desktop background (x/y/z)\n"
          "    --slide, -s            Start slideshow immediately\n"
          "    --random, -r           Random order\n"
          "    --shuffle, -S          Shuffled order\n"
          "    --delay, -d x          Wait x seconds between images [default=%d]\n"
          "\n"
          "Keys:\n", SLIDE_DELAY/1000);

    /* skip header and blank line */
    for (i=0; helpkeys[i]; i++)
        g_print("    %s\n", helpkeys[i]);

    g_print("\nValid image extensions:\nUse --no_filter/-n to disable");

    for (i=0; image_extensions[i]; i++)
	g_print("%s%s", (i%8) ? " " : "\n    ", image_extensions[i]);
    g_print("\n\n");

    g_print("Homepage: http://qiv.spiegl.de/\n"
	    "Please mail bug reports and comments to Andy Spiegl <qiv.andy@spiegl.de>\n");

    gdk_exit(exit_status);
}

/* returns a random number from the integers 0..num-1, either with
   replacement (replace=1) or without replacement (replace=0) */
int get_random(int replace, int num, int direction)
{
  static int index = -1;
  static int *rindices = NULL;  /* the array of random intgers */
  static int rsize;

  int n,m,p,q;

  if (!rindices)
    rindices = (int *) xmalloc((unsigned) max_rand_num*sizeof(int));
  if (rsize != num) {
    rsize = num;
    index = -1;
  }

  if (index < 0)         /* no more indices left in this cycle. Build a new */
    {		         /* array of random numbers, by not sorting on random keys */
      index = num-1;

      for (m=0;m<num;m++)
	{
	  rindices[m] = m; /* Make an array of growing numbers */
	}

      for (n=0;n<num;n++)   /* simple insertion sort, fine for num small */
	{
	  p=(int)(((float)rand()/RAND_MAX) * (num-n)) + n ; /* n <= p < num */
	  q=rindices[n];
	  rindices[n]=rindices[p]; /* Switch the two numbers to make random order */
	  rindices[p]=q;
	}
    }

  return rindices[index--];
}

/* Recursively gets all files from a directory if <recursive> is true,
 * else just reads directory */
int rreaddir(const char *dirname, int recursive)
{
  DIR *d;
  struct dirent *entry;
  char cdirname[FILENAME_LEN], name[FILENAME_LEN];
  struct stat sb;
  int before_count = images;

  strncpy(cdirname, dirname, sizeof cdirname);
  cdirname[FILENAME_LEN-1] = '\0';

  if (!(d = opendir(cdirname)))
    return -1;
  while ((entry = readdir(d)) != NULL) {
    if (strcmp(entry->d_name,".") == 0 ||
        strcmp(entry->d_name,"..") == 0 ||
        strcmp(entry->d_name,TRASH_DIR) == 0)
      continue;
    snprintf(name, sizeof name, "%s/%s", cdirname, entry->d_name);
    if (lstat(name, &sb) >= 0) {
      if (S_ISDIR(sb.st_mode)) {
        if (!recursive)
          continue;
        rreaddir(name,1);
      }
      else {
        if (images >= max_image_cnt) {
          max_image_cnt += 8192;
          if (!image_names)
            image_names = (char**)xmalloc(max_image_cnt * sizeof(char*));
          else
            image_names = (char**)xrealloc(image_names,max_image_cnt*sizeof(char*));
        }
        image_names[images++] = strdup(name);
      }
    }
  }
  closedir(d);
  return images - before_count;
}

/* Read image filenames from a file */
int rreadfile(const char *filename)
{
	FILE *fp;
	struct stat sb;
	int before_count = images;

         if (strcmp(filename,"-")) {
                 fp = fopen(filename, "r");
                 if(!fp) return -1;
         } else
                 fp = stdin;

	if (!images) {
		max_image_cnt = 8192;
		image_names = (char**)xmalloc(max_image_cnt * sizeof(char*));
	}

	while (1) {
		char line[ BUFSIZ ];
		size_t linelen;

		if (fgets(line, sizeof(line), fp) == NULL ) {
			if (ferror(fp))
				g_print("Error while reading %s: %s\n", filename, strerror(errno));
			fclose(fp);
			break;
		}
		linelen = strlen(line) -1;
		if (line[linelen] == '\n') line[linelen--]  = '\0';
		if (line[linelen] == '\r') line[linelen--]  = '\0';

		if (stat(line, &sb) >= 0 && S_ISDIR(sb.st_mode))
			rreaddir(line,1);
		else {
			if (images >= max_image_cnt) {
				max_image_cnt += 8192;
				image_names = (char**)xrealloc(image_names,max_image_cnt*sizeof(char*));
			}
			image_names[images++] = strdup(line);
		}
	}
    return images - before_count;
}

gboolean color_alloc(const char *name, GdkColor *color)
{
    gboolean result;

    result = gdk_color_parse(name, color);

    if (!result) {
        fprintf(stderr, "qiv: can't parse color '%s'\n", name);
        name = "black";
    }

    result = gdk_colormap_alloc_color(cmap, color, FALSE, TRUE);

    if (!result) {
        fprintf(stderr, "qiv: can't alloc color '%s'\n", name);
        color->pixel = 0;
    }

    return result;
}

void swap(int *a, int *b)
{
    int temp;
    temp = *a;
    *a = *b;
    *b = temp;
}

/* rounding a float to an int */
int myround( double a )
{
  return( (a-(int)a > 0.5) ? (int)a+1 : (int)a);
}

/* File watcher, an idle thread checking whether the loaded file has changed */

gboolean qiv_watch_file (gpointer data)
{
  struct stat statbuf;
  qiv_image *q=data;
  if(!watch_file)
  	return FALSE;

  stat(image_names[image_idx], &statbuf);

  if(current_mtime!=statbuf.st_mtime && statbuf.st_size){
	  reload_image(q);
          update_image(q, REDRAW);
  }
  usleep(200);  /* avoid eating 100% cpu */

  return TRUE;
}

int find_image(int images, char **image_names, char *name)
{
  int i;
  for (i=0; i<images; i++) {
    if (strcmp(name,image_names[i]) == 0)
      return i;
  }
  return 0;
}

#ifdef GTD_XINERAMA
/**
 * Find screen which maximizes f(screen)
 */
static XineramaScreenInfo *
xinerama_maximize_screen_function (XineramaScreenInfo * screens, int nscreens,
                         long (*f)(XineramaScreenInfo *))
{
  XineramaScreenInfo * screen;
  long value;
  long maxvalue = 0;
  XineramaScreenInfo * maximal_screen = screens;

  for (screen = screens; nscreens--; screen++) {
    //g_print("screen: %i\n", screen->screen_number);
    value = f(screen);
    if (value > maxvalue) {
      maxvalue = value;
      maximal_screen = screen;
    }
  }
  return maximal_screen;
}

static long
xinerama_screen_number_pixels (XineramaScreenInfo * screen)
{
//  g_print("npixels: screen->width = %d, screen->height = %d\n", screen->width, screen->height);
  return screen->width * screen->height;
}

/**
 * We will want to find the lower-rightmost screen (on which we
 * would like to display the statusbar in full-screen mode).
 *
 * In the general case (screens of differing sizes and arbitrarily placed)
 * the "lower-rightmost" screen is not particularly well defined.  We
 * take the definition as follows:
 *
 * Let (l_i, r_i) be the absolute pixel coordinates of the lower right corner
 * corner of screen i.  The lower-rightmost screen is the one for which
 * l_i + r_i is a maximum.
 */
static long
xinerama_screen_lower_rightness (XineramaScreenInfo * screen)
{
  return screen->x_org + screen->y_org + screen->width + screen->height;
}

void
get_preferred_xinerama_screens(void)
{
  Display * dpy;
  XineramaScreenInfo *screens = 0;

  dpy = XOpenDisplay(gdk_get_display());
  if (dpy && XineramaIsActive(dpy))
    screens = XineramaQueryScreens(dpy, &number_xinerama_screens);

//  g_print("number_xinerama_screens: %i\n", number_xinerama_screens);
  if (screens) {
//    g_print("xinerama screen: %i\n", user_screen);
    if (user_screen > -1 && user_screen < number_xinerama_screens) {
      // set user selected screen
      *preferred_screen = screens[user_screen];
//      g_print("preferred screen (user): %i\n", preferred_screen->screen_number);
    } else {
      // auto select largest screen
      *preferred_screen
        = *xinerama_maximize_screen_function(screens, number_xinerama_screens,
                                             xinerama_screen_number_pixels);
//      g_print("preferred screen (auto): %i\n", preferred_screen->screen_number);
    }

    // find lower rightmost screens for the statusbar
    *statusbar_screen
      = *xinerama_maximize_screen_function(screens, number_xinerama_screens,
                                 xinerama_screen_lower_rightness);
    XFree(screens);
  }
  else {
    /* If we don't have Xinerama, fake it: */
    preferred_screen->screen_number = 0;
    preferred_screen->x_org = 0;
    preferred_screen->y_org = 0;
    preferred_screen->width = gdk_screen_width();
    preferred_screen->height = gdk_screen_height();

    *statusbar_screen = *preferred_screen;
  }
  if (dpy)
    XCloseDisplay(dpy);
}
#endif
