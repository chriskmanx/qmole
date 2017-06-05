#include "common.h"
#include <time.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#ifdef BUILD_X11
# include <X11/Xlib.h>
#endif
#include "image.h"
#include "file.h"
#include "loaderpath.h"
#include "Imlib2.h"

static ImlibImage  *images = NULL;

#ifdef BUILD_X11
static ImlibImagePixmap *pixmaps = NULL;
#endif
static ImlibLoader *loaders = NULL;
static int          cache_size = 4096 * 1024;

/* attach a string key'd data and/or int value to an image that cna be */
/* looked up later by its string key */
void
__imlib_AttachTag(ImlibImage * im, const char *key, int val, void *data,
                  ImlibDataDestructorFunction destructor)
{
   ImlibImageTag      *t;

   /* no string key? abort */
   if (!key)
      return;

   /* if a tag of that name already exists - remove it and free it */
   if ((t = __imlib_RemoveTag(im, key)))
      __imlib_FreeTag(im, t);
   /* allocate the struct */
   t = malloc(sizeof(ImlibImageTag));
   /* fill it int */
   t->key = strdup(key);
   t->val = val;
   t->data = data;
   t->destructor = destructor;
   t->next = im->tags;
   /* prepend it to the list of tags */
   im->tags = t;
}

/* look up a tage by its key on the image it was attached to */
ImlibImageTag      *
__imlib_GetTag(ImlibImage * im, const char *key)
{
   ImlibImageTag      *t;

   t = im->tags;
   while (t)
     {
        if (!strcmp(t->key, key))
           return t;
        t = t->next;
     }
   /* no tag found - return NULL */
   return NULL;
}

/* remove a tag by looking it up by its key and removing it from */
/* the list of keys */
ImlibImageTag      *
__imlib_RemoveTag(ImlibImage * im, const char *key)
{
   ImlibImageTag      *t, *tt;

   tt = NULL;
   t = im->tags;
   while (t)
     {
        if (!strcmp(t->key, key))
          {
             if (tt)
                tt->next = t->next;
             else
                im->tags = t->next;
             return t;
          }
        tt = t;
        t = t->next;
     }
   /* no tag found - NULL */
   return NULL;
}

/* free the data struct for the tag and if a destructor function was */
/* provided call it on the data member */
void
__imlib_FreeTag(ImlibImage * im, ImlibImageTag * t)
{
   free(t->key);
   if (t->destructor)
      t->destructor(im, t->data);
   free(t);
}

/* free all the tags attached to an image */
void
__imlib_FreeAllTags(ImlibImage * im)
{
   ImlibImageTag      *t, *tt;

   t = im->tags;
   while (t)
     {
        tt = t;
        t = t->next;
        __imlib_FreeTag(im, tt);
     }
}

/* set the cache size */
void
__imlib_SetCacheSize(int size)
{
   cache_size = size;
   __imlib_CleanupImageCache();
#ifdef BUILD_X11
   __imlib_CleanupImagePixmapCache();
#endif
}

/* return the cache size */
int
__imlib_GetCacheSize(void)
{
   return cache_size;
}

/* create an image data struct and fill it in */
ImlibImage         *
__imlib_ProduceImage(void)
{
   ImlibImage         *im;

   im = malloc(sizeof(ImlibImage));
   memset(im, 0, sizeof(ImlibImage));
   im->data = NULL;
   im->file = NULL;
   im->real_file = NULL;
   im->key = NULL;
   im->flags = F_FORMAT_IRRELEVANT | F_BORDER_IRRELEVANT | F_ALPHA_IRRELEVANT;
   im->loader = NULL;
   im->next = NULL;
   im->tags = NULL;
   return im;
}

/* free an image struct */
void
__imlib_ConsumeImage(ImlibImage * im)
{
#ifdef BUILD_X11
   ImlibImagePixmap   *ip;
#endif

   __imlib_FreeAllTags(im);
   if (im->file)
      free(im->file);
   if (im->real_file)
      free(im->real_file);
   if (im->key)
      free(im->key);
   if ((IMAGE_FREE_DATA(im)) && (im->data))
      free(im->data);
   if (im->format)
      free(im->format);
   free(im);
#ifdef BUILD_X11
   ip = pixmaps;
   while (ip)
     {
        if (ip->image == im)
          {
             ip->image = NULL;
             ip->dirty = 1;
          }
        ip = ip->next;
     }
#endif
}

ImlibImage         *
__imlib_FindCachedImage(const char *file)
{
   ImlibImage         *im, *previous_im;

   im = images;
   previous_im = NULL;
   /* go through the images list */
   while (im)
     {
        /* if the filenames match and it's valid */
        if ((!strcmp(file, im->file)) && (IMAGE_IS_VALID(im)))
          {
             /* move the image to the head of the pixmap list */
             if (previous_im)
               {
                  previous_im->next = im->next;
                  im->next = images;
                  images = im;
               }
             /* return it */
             return im;
          }
        previous_im = im;
        im = im->next;
     }
   return NULL;
}

/* add an image to the cache of images (at the start) */
void
__imlib_AddImageToCache(ImlibImage * im)
{
   im->next = images;
   images = im;
}

/* remove (unlink) an image from the cache of images */
void
__imlib_RemoveImageFromCache(ImlibImage * im)
{
   ImlibImage         *current_im, *previous_im;

   current_im = images;
   previous_im = NULL;
   while (current_im)
     {
        if (im == current_im)
          {
             if (previous_im)
                previous_im->next = im->next;
             else
                images = im->next;
             return;
          }
        previous_im = current_im;
        current_im = current_im->next;
     }
}

/* work out how much we have floaitng aroudn in our speculative cache */
/* (images and pixmaps that have 0 reference counts) */
int
__imlib_CurrentCacheSize(void)
{
   ImlibImage         *im;

#ifdef BUILD_X11
   ImlibImagePixmap   *ip;
#endif
   int                 current_cache = 0;

   /* go through the image cache */
   im = images;
   while (im)
     {
        /* mayaswell clean out stuff thats invalid that we dont need anymore */
        if (im->references == 0)
          {
             if (!(IMAGE_IS_VALID(im)))
               {
                  ImlibImage         *tmp_im;

                  tmp_im = im;
                  im = im->next;
                  __imlib_RemoveImageFromCache(tmp_im);
                  __imlib_ConsumeImage(tmp_im);
#ifdef BUILD_X11
                  ip = pixmaps;
                  while (ip)
                    {
                       if (ip->image == tmp_im)
                         {
                            ip->image = NULL;
                            ip->dirty = 1;
                         }
                       ip = ip->next;
                    }
#endif
                  continue;
               }
             /* it's valid but has 0 ref's - append to cache size count */
             else
                current_cache += im->w * im->h * sizeof(DATA32);
          }
        im = im->next;
     }
#ifdef BUILD_X11
   /* go through the pixmaps */
   ip = pixmaps;
   while (ip)
     {
        /* if the pixmap has 0 references */
        if (ip->references == 0)
          {
             /* if the image is invalid */
             if ((ip->dirty) || ((ip->image) && (!(IMAGE_IS_VALID(ip->image)))))
               {
                  ImlibImagePixmap   *tmp_ip;

                  tmp_ip = ip;
                  ip = ip->next;
                  __imlib_RemoveImagePixmapFromCache(tmp_ip);
                  __imlib_ConsumeImagePixmap(tmp_ip);
                  continue;
               }
             else
               {
                  /* add the pixmap data size to the cache size */
                  if (ip->pixmap)
                    {
                       if (ip->depth < 8)
                          current_cache += ip->w * ip->h * (ip->depth / 8);
                       else if (ip->depth == 8)
                          current_cache += ip->w * ip->h;
                       else if (ip->depth <= 16)
                          current_cache += ip->w * ip->h * 2;
                       else if (ip->depth <= 32)
                          current_cache += ip->w * ip->h * 4;
                    }
                  /* if theres a mask add it too */
                  if (ip->mask)
                     current_cache += ip->w * ip->h / 8;
               }
          }
        ip = ip->next;
     }
#endif
   return current_cache;
}

/* clean out images fromthe cache if the cache is overgrown */
void
__imlib_CleanupImageCache(void)
{
   ImlibImage         *im, *im_last;
   int                 current_cache;
   char                operation = 1;

   current_cache = __imlib_CurrentCacheSize();
   im_last = NULL;
   im = images;
   /* remove 0 ref count invalid (dirty) images */
   while (im)
     {
        im_last = im;
        im = im->next;
        if ((im_last->references <= 0) && (!(IMAGE_IS_VALID(im_last))))
          {
             __imlib_RemoveImageFromCache(im_last);
             __imlib_ConsumeImage(im_last);
          }
     }
   /* while the cache size of 0 ref coutn data is bigger than the set value */
   /* clean out the oldest members of the imaeg cache */
   while ((current_cache > cache_size) && (operation))
     {
        im_last = NULL;
        operation = 0;
        im = images;
        while (im)
          {
             if (im->references <= 0)
                im_last = im;
             im = im->next;
          }
        if (im_last)
          {
             __imlib_RemoveImageFromCache(im_last);
             __imlib_ConsumeImage(im_last);
             operation = 1;
          }
        if (operation)
           current_cache = __imlib_CurrentCacheSize();
     }
}

#ifdef BUILD_X11
/* create a pixmap cache data struct */
ImlibImagePixmap   *
__imlib_ProduceImagePixmap(void)
{
   ImlibImagePixmap   *ip;

   ip = malloc(sizeof(ImlibImagePixmap));
   memset(ip, 0, sizeof(ImlibImagePixmap));
   ip->display = NULL;
   ip->visual = NULL;
   ip->image = NULL;
   ip->next = NULL;
   ip->file = NULL;
   return ip;
}

/* free a pixmap cache data struct and the pixmaps in it */
void
__imlib_ConsumeImagePixmap(ImlibImagePixmap * ip)
{
#ifdef DEBUG_CACHE
   fprintf(stderr,
           "[Imlib2]  Deleting pixmap.  Reference count is %d, pixmap 0x%08x, mask 0x%08x\n",
           ip->references, ip->pixmap, ip->mask);
#endif
   if (ip->pixmap)
      XFreePixmap(ip->display, ip->pixmap);
   if (ip->mask)
      XFreePixmap(ip->display, ip->mask);
   if (ip->file)
      free(ip->file);
   free(ip);
}

ImlibImagePixmap   *
__imlib_FindCachedImagePixmap(ImlibImage * im, int w, int h, Display * d,
                              Visual * v, int depth, int sx, int sy, int sw,
                              int sh, Colormap cm, char aa, char hiq,
                              char dmask, DATABIG modification_count)
{
   ImlibImagePixmap   *ip, *previous_ip;

   ip = pixmaps;
   previous_ip = NULL;
   /* go through the pixmap list */
   while (ip)
     {
        /* if all the pixmap attributes match */
        if ((ip->w == w) && (ip->h == h) && (ip->depth == depth) && (!ip->dirty)
            && (ip->visual == v) && (ip->display == d)
            && (ip->source_x == sx) && (ip->source_x == sy)
            && (ip->source_w == sw) && (ip->source_h == sh)
            && (ip->colormap == cm) && (ip->antialias == aa)
            && (ip->modification_count == modification_count)
            && (ip->dither_mask == dmask)
            && (ip->border.left == im->border.left)
            && (ip->border.right == im->border.right)
            && (ip->border.top == im->border.top)
            && (ip->border.bottom == im->border.bottom) &&
            (((im->file) && (ip->file) && !strcmp(im->file, ip->file)) ||
             ((!im->file) && (!ip->file) && (im == ip->image))))
          {
             /* move the pixmap to the head of the pixmap list */
             if (previous_ip)
               {
                  previous_ip->next = ip->next;
                  ip->next = pixmaps;
                  pixmaps = ip;
               }
             /* return it */
             return ip;
          }
        previous_ip = ip;
        ip = ip->next;
     }
   return NULL;
}

ImlibImagePixmap   *
__imlib_FindCachedImagePixmapByID(Display * d, Pixmap p)
{
   ImlibImagePixmap   *ip;

   ip = pixmaps;
   /* go through the pixmap list */
   while (ip)
     {
        /* if all the pixmap attributes match */
        if ((ip->pixmap == p) && (ip->display == d))
           return ip;
        ip = ip->next;
     }
   return NULL;
}

/* add a pixmap cahce struct to the pixmap cache (at the start of course */
void
__imlib_AddImagePixmapToCache(ImlibImagePixmap * ip)
{
   ip->next = pixmaps;
   pixmaps = ip;
}

/* remove a pixmap cache struct fromt he pixmap cache */
void
__imlib_RemoveImagePixmapFromCache(ImlibImagePixmap * ip)
{
   ImlibImagePixmap   *current_ip, *previous_ip;

   current_ip = pixmaps;
   previous_ip = NULL;
   while (current_ip)
     {
        if (ip == current_ip)
          {
             if (previous_ip)
                previous_ip->next = ip->next;
             else
                pixmaps = ip->next;
             return;
          }
        previous_ip = current_ip;
        current_ip = current_ip->next;
     }
}

/* clean out 0 reference count & dirty pixmaps from the cache */
void
__imlib_CleanupImagePixmapCache(void)
{
   ImlibImagePixmap   *ip, *ip_last;
   int                 current_cache;
   char                operation = 0;

   current_cache = __imlib_CurrentCacheSize();
   ip_last = NULL;
   ip = pixmaps;
   while (ip)
     {
        ip_last = ip;
        ip = ip->next;
        if ((ip_last->references <= 0) && (ip_last->dirty))
          {
             __imlib_RemoveImagePixmapFromCache(ip_last);
             __imlib_ConsumeImagePixmap(ip_last);
          }
     }
   while ((current_cache > cache_size) && (operation))
     {
        ip_last = NULL;
        operation = 0;
        ip = pixmaps;
        while (ip)
          {
             if (ip->references <= 0)
                ip_last = ip;
             ip = ip->next;
          }
        if (ip_last)
          {
             __imlib_RemoveImagePixmapFromCache(ip_last);
             __imlib_ConsumeImagePixmap(ip_last);
             operation = 1;
          }
        if (operation)
           current_cache = __imlib_CurrentCacheSize();
     }
}
#endif

/* try dlopen()ing the file if we succeed finish filling out the malloced */
/* loader struct and return it */
ImlibLoader        *
__imlib_ProduceLoader(char *file)
{
   ImlibLoader        *l;
   void                (*l_formats) (ImlibLoader * l);

   l = malloc(sizeof(ImlibLoader));
   l->num_formats = 0;
   l->formats = NULL;
   l->handle = dlopen(file, RTLD_NOW | RTLD_LOCAL);
   if (!l->handle)
     {
        free(l);
        return NULL;
     }
   l->load = dlsym(l->handle, "load");
   l->save = dlsym(l->handle, "save");
   l_formats = dlsym(l->handle, "formats");

   /* each loader must provide formats() and at least load() or save() */
   if (!l_formats || (!l->load && !l->save))
     {
        dlclose(l->handle);
        free(l);
        return NULL;
     }
   l_formats(l);
   l->file = strdup(file);
   l->next = NULL;
   return l;
}

/* list all the filenames of loaders  int he system loaders dir and the user */
/* loader dir */
char              **
__imlib_ListLoaders(int *num_ret)
{
   char              **list = NULL, **l, *s;
   int                 num, i, pi = 0;

   *num_ret = 0;
   /* same for system loader path */
   s = (char *)malloc(sizeof(SYS_LOADERS_PATH) + 8 + 1);
   sprintf(s, SYS_LOADERS_PATH "/loaders");
#ifndef __EMX__
   l = __imlib_FileDir(s, &num);
#else
   l = __imlib_FileDir(__XOS2RedirRoot(s), &num);
#endif
   if (num > 0)
     {
        *num_ret += num;
        list = realloc(list, sizeof(char *) * *num_ret);

        for (i = 0; i < num; i++)
          {
             s = (char *)realloc(s,
                                 sizeof(SYS_LOADERS_PATH) + 9 + strlen(l[i]) +
                                 1);
             sprintf(s, SYS_LOADERS_PATH "/loaders/%s", l[i]);
#ifndef __EMX__
             list[pi + i] = strdup(s);
#else
             list[pi + i] = strdup(__XOS2RedirRoot(s));
#endif
          }
        __imlib_FileFreeDirList(l, num);
     }
   free(s);

   /* List currently contains *everything in there* we need to weed out
    * the .so, .la, .a versions of the same loader or whatever else.
    * dlopen can take an extension-less name and do the Right Thing
    * with it, so that's what we'll give it. */
   list = __imlib_TrimLoaderList(list, num_ret);

   return list;
}

char              **
__imlib_TrimLoaderList(char **list, int *num)
{
   int                 i, n, size = 0;

   char              **ret = NULL;

   if (!list)
      return NULL;
   if (*num == 0)
      return list;

   n = *num;

   for (i = 0; i < n; i++)
     {
        char               *ext;

        if (!list[i])
           continue;
        ext = strrchr(list[i], '.');
        if ((ext) && (
#ifdef __CYGWIN__
                        (!strcasecmp(ext, ".dll")) ||
#endif
                        (!strcasecmp(ext, ".so"))))
          {
             /* Don't add the same loader multiple times... */
             if (!__imlib_ItemInList(ret, size, list[i]))
               {
                  ret = realloc(ret, sizeof(char *) * (size + 1));

                  ret[size++] = strdup(list[i]);
               }
          }
        if (list[i])
           free(list[i]);
     }
   if (list)
      free(list);
   *num = size;
   return ret;
}

int
__imlib_ItemInList(char **list, int size, char *item)
{
   int                 i;

   if (!size)
      return 0;
   if (!list)
      return 0;
   if (!item)
      return 0;

   for (i = 0; i < size; i++)
     {
        if (!strcmp(list[i], item))
           return 1;
     }
   return 0;
}

/* fre the struct for a loader and close its dlopen'd handle */
void
__imlib_ConsumeLoader(ImlibLoader * l)
{
   if (l->file)
      free(l->file);
   if (l->handle)
      dlclose(l->handle);
   if (l->formats)
     {
        int                 i;

        for (i = 0; i < l->num_formats; i++)
           free(l->formats[i]);
        free(l->formats);
     }
   free(l);
}

void
__imlib_RescanLoaders(void)
{
   static time_t       last_scan_time = 0;
   static time_t       last_modified_system_time = 0;
   static int          scanned = 0;
   time_t              current_time;
   char                do_reload = 0;

   /* dont stat the dir and rescan if we checked in the last 5 seconds */
   current_time = time(NULL);
   if ((current_time - last_scan_time) < 5)
      return;
   /* ok - was the system loaders dir contents modified ? */
   last_scan_time = current_time;
#ifndef __EMX__
   if (__imlib_FileIsDir(SYS_LOADERS_PATH "/loaders/"))
#else
   if (__imlib_FileIsDir(__XOS2RedirRoot(SYS_LOADERS_PATH "/loaders/")))
#endif
     {
#ifndef __EMX__
        current_time = __imlib_FileModDate(SYS_LOADERS_PATH "/loaders/");
#else
        current_time =
           __imlib_FileModDate(__XOS2RedirRoot(SYS_LOADERS_PATH "/loaders/"));
#endif
        if ((current_time > last_modified_system_time) || (!scanned))
          {
             /* yup - set the "do_reload" flag */
             do_reload = 1;
             last_modified_system_time = current_time;
          }
     }
   /* if we dont ned to reload the loaders - get out now */
   if (!do_reload)
      return;
   __imlib_RemoveAllLoaders();
   __imlib_LoadAllLoaders();
   scanned = 1;
}

/* remove all loaders int eh list we have cached so we can re-load them */
void
__imlib_RemoveAllLoaders(void)
{
   ImlibLoader        *l, *il;

   l = loaders;
   while (l)
     {
        il = l;
        l = l->next;
        __imlib_ConsumeLoader(il);
     }
   loaders = NULL;
}

/* find all the loaders we can find and load them up to see what they can */
/* load / save */
void
__imlib_LoadAllLoaders(void)
{
   int                 i, num;
   char              **list;

   /* list all the loaders imlib can find */
   list = __imlib_ListLoaders(&num);
   /* no loaders? well don't load anything */
   if (!list)
      return;

   /* go through the list of filenames for loader .so's and load them */
   /* (or try) and if it succeeds, append to our loader list */
   for (i = num - 1; i >= 0; i--)
     {
        ImlibLoader        *l;

        l = __imlib_ProduceLoader(list[i]);
        if (l)
          {
             l->next = loaders;
             loaders = l;
          }
        if (list[i])
           free(list[i]);
     }
   free(list);
}

ImlibLoader        *
__imlib_FindBestLoaderForFile(const char *file, int for_save)
{
   char               *extension, *lower, *rfile;
   ImlibLoader        *l = NULL;

   /* use the file extension for a "best guess" as to what loader to try */
   /* first at any rate */

   rfile = __imlib_FileRealFile(file);
   extension = __imlib_FileExtension(rfile);
   free(rfile);
   /* change the extensiont o all lower case as all "types" are listed as */
   /* lower case strings fromt he loader that represent all the possible */
   /* extensions that file format could have */
   lower = extension;
   while (*lower)
     {
        *lower = tolower(*lower);
        lower++;
     }
   if (!extension)
      return NULL;
   /* go through the loaders - first loader that claims to handle that */
   /* image type (extension wise) wins as a first guess to use - NOTE */
   /* this is an OPTIMISATION - it is possible the file has no extension */
   /* or has an unrecognised one but still is loadable by a loader. */
   /* if thkis initial loader failes to load the load mechanism will */
   /* systematically go from most recently used to least recently used */
   /* loader until one succeeds - or none are left and all have failed */
   /* and only if all fail does the laod fail. the lao9der that does */
   /* succeed gets it way tot he head of the list so it's going */
   /* to be used first next time in this search mechanims - this */
   /* assumes you tend to laod a few image types and ones generally */
   /* of the same format */
   l = loaders;
   while (l)
     {
        int                 i;

        /* go through all the formats that loader supports */
        for (i = 0; i < l->num_formats; i++)
          {
             /* does it match ? */
             if (!strcmp(l->formats[i], extension))
               {
                  /* does it provide the function we need? */
                  if ((for_save && l->save) || (!for_save && l->load))
                    {
                       /* free the memory allocated for the extension */
                       free(extension);
                       /* return the loader */
                       return l;
                    }
               }
          }
        l = l->next;
     }
   /* free the memory allocated for the extension */
   free(extension);
   /* return the loader */
   return l;
}

ImlibLoader        *
__imlib_FindBestLoaderForFileFormat(const char *file, char *format,
                                    int for_save)
{
   char               *extension, *lower;
   ImlibLoader        *l = NULL;

   /* if the format is provided ("png" "jpg" etc.) use that */
   if (format)
      extension = strdup(format);
   /* otherwise us the extension */
   else
     {
        extension = __imlib_FileExtension(file);
        /* change the extension to all lower case as all "types" are listed as */
        /* lower case strings fromt he loader that represent all the possible */
        /* extensions that file format could have */
        if (extension)
          {
             lower = extension;
             while (*lower)
               {
                  *lower = tolower(*lower);
                  lower++;
               }
          }
     }
   if (!extension)
      return NULL;
   /* look through the loaders one by one to see if one matches that format */
   l = loaders;
   while (l)
     {
        int                 i;

        /* go through all the formats that loader supports */
        for (i = 0; i < l->num_formats; i++)
          {
             /* does it match ? */
             if (!strcmp(l->formats[i], extension))
               {
                  /* does it provide the function we need? */
                  if ((for_save && l->save) || (!for_save && l->load))
                    {
                       /* free the memory allocated for the extension */
                       free(extension);
                       /* return the loader */
                       return l;
                    }
               }
          }
        l = l->next;
     }
   /* free the memory allocated for the extension */
   free(extension);
   /* return the loader */
   return l;
}

/* set or unset the alpha flag on the umage (alpha = 1 / 0 ) */
void
__imlib_SetImageAlphaFlag(ImlibImage * im, char alpha)
{
   if (alpha)
      SET_FLAG(im->flags, F_HAS_ALPHA);
   else
      UNSET_FLAG(im->flags, F_HAS_ALPHA);
}

/* create a new image struct from data passed that is wize w x h then return */
/* a pointer to that image sturct */
ImlibImage         *
__imlib_CreateImage(int w, int h, DATA32 * data)
{
   ImlibImage         *im;

   im = __imlib_ProduceImage();
   im->w = w;
   im->h = h;
   im->data = data;
   im->references = 1;
   SET_FLAG(im->flags, F_UNCACHEABLE);
   return im;
}

ImlibImage         *
__imlib_LoadImage(const char *file, ImlibProgressFunction progress,
                  char progress_granularity, char immediate_load,
                  char dont_cache, ImlibLoadError * er)
{
   ImlibImage         *im;
   ImlibLoader        *best_loader;
   char                loader_ret = 0;

   if (!file)
      return NULL;
   if (file[0] == 0)
      return NULL;
   /* see if we already have the image cached */
   im = __imlib_FindCachedImage(file);
   /* if we found a cached image and we shoudl always check that it is */
   /* accurate to the disk conents if they changed since we last loaded */
   /* and that it is still a valid image */
   if ((im) && (IMAGE_IS_VALID(im)))
     {
        if (IMAGE_ALWAYS_CHECK_DISK(im))
          {
             time_t              current_modified_time;

             current_modified_time = __imlib_FileModDate(file);
             /* if the file on disk is newer than the cached one */
             if (current_modified_time > im->moddate)
               {
                  /* invalidate image */
                  SET_FLAG(im->flags, F_INVALID);
               }
             else
               {
                  /* image is ok to re-use - program is just being stupid loading */
                  /* the same data twice */
                  im->references++;
                  return im;
               }
          }
        else
          {
             im->references++;
             return im;
          }
     }
   /* either image in cache is invalid or we dont even have it in cache */
   /* so produce a new one and load an image into that */
   im = __imlib_ProduceImage();
   im->file = strdup(file);
   if (__imlib_IsRealFile(file))
     {
        im->real_file = strdup(im->file);
        im->key = NULL;
     }
   else
     {
        im->real_file = __imlib_FileRealFile(file);
        im->key = __imlib_FileKey(file);
     }
   im->moddate = __imlib_FileModDate(file);
   /* ok - just check all our loaders are up to date */
   __imlib_RescanLoaders();
   /* take a guess by extension on the best loader to use */
   best_loader = __imlib_FindBestLoaderForFile(im->real_file, 0);
   errno = 0;
   if (best_loader)
      loader_ret =
         best_loader->load(im, progress, progress_granularity, immediate_load);
   /* width is still 0 - the loader didnt manage to do anything */
   if (im->w == 0)
     {
        ImlibLoader        *l, *previous_l = NULL;

        errno = 0;
        l = loaders;
        /* run through all loaders and try load until one succeeds */
        while ((l) && (im->w == 0))
          {
             /* if its not the best loader that already failed - try load */
             if (l != best_loader)
                loader_ret =
                   l->load(im, progress, progress_granularity, immediate_load);
             /* if it failed - advance */
             if (im->w == 0)
               {
                  previous_l = l;
                  l = l->next;
               }
          }
        /* if we have a loader then its the loader that succeeded */
        /* move the successful loader to the head of the list */
        /* as long as it's not already at the head of the list */
        if ((l) && (previous_l))
          {
             im->loader = l;
             previous_l->next = l->next;
             l->next = loaders;
             loaders = l;
          }
        if (im->w > 0)
           im->loader = l;
     }
   else
      im->loader = best_loader;
   /* all loaders have been tried and they all failed. free the skeleton */
   /* image struct we had and return NULL */
   if (im->w == 0)
     {
        /* if the caller wants an error return */
        if (er)
          {
             /* set to a default fo no error */
             *er = IMLIB_LOAD_ERROR_NONE;
             /* if the errno is set */
             if (errno != 0)
               {
                  /* default to unknown error */
                  *er = IMLIB_LOAD_ERROR_UNKNOWN;
                  /* standrad fopen() type errors translated */
                  if (errno == EEXIST)
                     *er = IMLIB_LOAD_ERROR_FILE_DOES_NOT_EXIST;
                  else if (errno == EISDIR)
                     *er = IMLIB_LOAD_ERROR_FILE_IS_DIRECTORY;
                  else if (errno == EISDIR)
                     *er = IMLIB_LOAD_ERROR_FILE_IS_DIRECTORY;
                  else if (errno == EACCES)
                     *er = IMLIB_LOAD_ERROR_PERMISSION_DENIED_TO_READ;
                  else if (errno == ENAMETOOLONG)
                     *er = IMLIB_LOAD_ERROR_PATH_TOO_LONG;
                  else if (errno == ENOENT)
                     *er = IMLIB_LOAD_ERROR_PATH_COMPONENT_NON_EXISTANT;
                  else if (errno == ENOTDIR)
                     *er = IMLIB_LOAD_ERROR_PATH_COMPONENT_NOT_DIRECTORY;
                  else if (errno == EFAULT)
                     *er = IMLIB_LOAD_ERROR_PATH_POINTS_OUTSIDE_ADDRESS_SPACE;
                  else if (errno == ELOOP)
                     *er = IMLIB_LOAD_ERROR_TOO_MANY_SYMBOLIC_LINKS;
                  else if (errno == ENOMEM)
                     *er = IMLIB_LOAD_ERROR_OUT_OF_MEMORY;
                  else if (errno == EMFILE)
                     *er = IMLIB_LOAD_ERROR_OUT_OF_FILE_DESCRIPTORS;
                  if (*er != IMLIB_LOAD_ERROR_UNKNOWN)
                    {
                       /* free the stuct we created */
                       __imlib_ConsumeImage(im);
                       return NULL;
                    }
               }
             errno = 0;
          }
        __imlib_ConsumeImage(im);
        return NULL;
     }

   /* the load succeeded - make sure the image is referenced then add */
   /* it to our cache if dont_cache isn't set */
   im->references = 1;
   if (loader_ret == 2)
      dont_cache = 1;
   if (!dont_cache)
      __imlib_AddImageToCache(im);
   else
      SET_FLAG(im->flags, F_UNCACHEABLE);
   return im;
}

#ifdef BUILD_X11
/* find an imagepixmap cache entry by the display and pixmap id */
ImlibImagePixmap   *
__imlib_FindImlibImagePixmapByID(Display * d, Pixmap p)
{
   ImlibImagePixmap   *ip;

   ip = pixmaps;
   /* go through the pixmap list */
   while (ip)
     {
        /* if all the pixmap ID & Display match */
        if ((ip->pixmap == p) && (ip->display == d))
          {
#ifdef DEBUG_CACHE
             fprintf(stderr,
                     "[Imlib2]  Match found.  Reference count is %d, pixmap 0x%08x, mask 0x%08x\n",
                     ip->references, ip->pixmap, ip->mask);
#endif
             return ip;
          }
        ip = ip->next;
     }
   return NULL;
}
#endif

/* free and image - if its uncachable and refcoutn is 0 - free it in reality */
void
__imlib_FreeImage(ImlibImage * im)
{
   /* if the refcount is positive */
   if (im->references >= 0)
     {
        /* reduce a reference from the count */
        im->references--;
        /* if its uncachchable ... */
        if (IMAGE_IS_UNCACHEABLE(im))
          {
             /* and we're down to no references for the image then free it */
             if (im->references == 0)
                __imlib_ConsumeImage(im);
          }
        /* otherwise clean up our cache if the image becoem 0 ref count */
        else if (im->references == 0)
           __imlib_CleanupImageCache();
     }
}

#ifdef BUILD_X11
/* free a cached pixmap */
void
__imlib_FreePixmap(Display * d, Pixmap p)
{
   ImlibImagePixmap   *ip;

   /* find the pixmap in the cache by display and id */
   ip = __imlib_FindImlibImagePixmapByID(d, p);
   if (ip)
     {
        /* if tis positive reference count */
        if (ip->references > 0)
          {
             /* dereference it by one */
             ip->references--;
#ifdef DEBUG_CACHE
             fprintf(stderr,
                     "[Imlib2]  Reference count is now %d for pixmap 0x%08x\n",
                     ip->references, ip->pixmap);
#endif
             /* if it becaume 0 reference count - clean the cache up */
             if (ip->references == 0)
                __imlib_CleanupImagePixmapCache();
          }
     }
   else
     {
#ifdef DEBUG_CACHE
        fprintf(stderr, "[Imlib2]  Pixmap 0x%08x not found.  Freeing.\n", p);
#endif
        XFreePixmap(d, p);
     }
}

/* mark all pixmaps generated from this image as dirty so the cache code */
/* wont pick up on them again since they are now invalid since the original */
/* data they were generated from has changed */
void
__imlib_DirtyPixmapsForImage(ImlibImage * im)
{
   ImlibImagePixmap   *ip;

   ip = pixmaps;
   /* go through the pixmap list */
   while (ip)
     {
        /* if image matches */
        if (ip->image == im)
           ip->dirty = 1;
        ip = ip->next;
     }
   __imlib_CleanupImagePixmapCache();
}
#endif

/* dirty and image by settings its invalid flag */
void
__imlib_DirtyImage(ImlibImage * im)
{
   SET_FLAG(im->flags, F_INVALID);
   /* and dirty all pixmaps generated from it */
   __imlib_DirtyPixmapsForImage(im);
}

void
__imlib_SaveImage(ImlibImage * im, const char *file,
                  ImlibProgressFunction progress, char progress_granularity,
                  ImlibLoadError * er)
{
   ImlibLoader        *l;
   char                e, *pfile;

   if (!file)
     {
        if (er)
           *er = IMLIB_LOAD_ERROR_FILE_DOES_NOT_EXIST;
        return;
     }
   /* ok - just check all our loaders are up to date */
   __imlib_RescanLoaders();
   /* set the filename to the saved one */
   pfile = im->file;
   im->file = strdup(file);

   if (im->real_file)
      free(im->real_file);

   im->real_file = strdup(im->file);

   /* find the laoder for the format - if its null use the extension */
   l = __imlib_FindBestLoaderForFileFormat(im->real_file, im->format, 1);
   /* no loader - abort */
   if (!l)
     {
        if (er)
           *er = IMLIB_LOAD_ERROR_NO_LOADER_FOR_FILE_FORMAT;
        /* set the filename back to the laoder image filename */
        free(im->file);
        im->file = pfile;
        return;
     }
   /* if they want an error returned - assume none by default */
   if (er)
      *er = IMLIB_LOAD_ERROR_NONE;

   /* call the saver */
   e = l->save(im, progress, progress_granularity);
   /* set the filename back to the laoder image filename */
   free(im->file);
   im->file = pfile;

   /* if there's an error return and the save faile (e = 0) figure it out */
   if ((er) && (e == 0))
     {
        *er = IMLIB_LOAD_ERROR_UNKNOWN;
        if (errno == EEXIST)
           *er = IMLIB_LOAD_ERROR_FILE_DOES_NOT_EXIST;
        else if (errno == EISDIR)
           *er = IMLIB_LOAD_ERROR_FILE_IS_DIRECTORY;
        else if (errno == EISDIR)
           *er = IMLIB_LOAD_ERROR_FILE_IS_DIRECTORY;
        else if (errno == EACCES)
           *er = IMLIB_LOAD_ERROR_PERMISSION_DENIED_TO_WRITE;
        else if (errno == ENAMETOOLONG)
           *er = IMLIB_LOAD_ERROR_PATH_TOO_LONG;
        else if (errno == ENOENT)
           *er = IMLIB_LOAD_ERROR_PATH_COMPONENT_NON_EXISTANT;
        else if (errno == ENOTDIR)
           *er = IMLIB_LOAD_ERROR_PATH_COMPONENT_NOT_DIRECTORY;
        else if (errno == EFAULT)
           *er = IMLIB_LOAD_ERROR_PATH_POINTS_OUTSIDE_ADDRESS_SPACE;
        else if (errno == ELOOP)
           *er = IMLIB_LOAD_ERROR_TOO_MANY_SYMBOLIC_LINKS;
        else if (errno == ENOMEM)
           *er = IMLIB_LOAD_ERROR_OUT_OF_MEMORY;
        else if (errno == EMFILE)
           *er = IMLIB_LOAD_ERROR_OUT_OF_FILE_DESCRIPTORS;
        else if (errno == ENOSPC)
           *er = IMLIB_LOAD_ERROR_OUT_OF_DISK_SPACE;
        else if (errno == EROFS)
           *er = IMLIB_LOAD_ERROR_PERMISSION_DENIED_TO_WRITE;
     }
}
