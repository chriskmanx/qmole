#ifndef __IMAGE
# define __IMAGE 1

# include "common.h"
# ifdef BUILD_X11
#  include <X11/Xlib.h>
# else
#  define X_DISPLAY_MISSING
# endif

# include <dlfcn.h>
# include <Imlib2.h>

# ifndef RTLD_LOCAL
#  define RTLD_LOCAL 0
#  warning "your crap box doesnt define RTLD_LOCAL !?"
# endif

typedef struct _imlibimage              ImlibImage;
# ifdef BUILD_X11
typedef struct _imlibimagepixmap        ImlibImagePixmap;
# endif
typedef struct _imlibborder             ImlibBorder;
typedef struct _imlibloader             ImlibLoader;
typedef struct _imlibimagetag           ImlibImageTag;

typedef int (*ImlibProgressFunction)(ImlibImage *im, char percent,
				      int update_x, int update_y,
				      int update_w, int update_h);
typedef void (*ImlibDataDestructorFunction)(ImlibImage *im, void *data);

enum _iflags
{
   F_NONE              = 0,
   F_HAS_ALPHA         = (1 << 0),
   F_UNLOADED          = (1 << 1),
   F_UNCACHEABLE       = (1 << 2),
   F_ALWAYS_CHECK_DISK = (1 << 3),
   F_INVALID           = (1 << 4),
   F_DONT_FREE_DATA    = (1 << 5),
   F_FORMAT_IRRELEVANT = (1 << 6),
   F_BORDER_IRRELEVANT = (1 << 7),
   F_ALPHA_IRRELEVANT  = (1 << 8)
};

typedef enum   _iflags                  ImlibImageFlags;

struct _imlibborder
{
   int left, right, top, bottom;
};

struct _imlibimagetag
{
   char           *key;
   int             val;
   void           *data;
   void          (*destructor)(ImlibImage *im, void *data);
   ImlibImageTag  *next;
};

struct _imlibimage
{
   char             *file;
   int               w, h;
   DATA32           *data;
   ImlibImageFlags   flags;
   time_t            moddate;
   ImlibBorder       border;
   int               references;
   ImlibLoader      *loader;
   char             *format;
   ImlibImage       *next;
   ImlibImageTag    *tags;
   char             *real_file;
   char             *key;
};

# ifdef BUILD_X11
struct _imlibimagepixmap
{
   int               w, h;
   Pixmap            pixmap, mask;
   Display          *display;
   Visual           *visual;
   int               depth;
   int               source_x, source_y, source_w, source_h;
   Colormap          colormap;
   char              antialias, hi_quality, dither_mask;
   ImlibBorder       border;
   ImlibImage       *image;
   char             *file;
   char              dirty;
   int               references;
   DATABIG           modification_count;
   ImlibImagePixmap *next;
};
# endif

struct _imlibloader
{
   char         *file;
   int           num_formats;
   char        **formats;
   void         *handle;
   char        (*load)(ImlibImage *im,
		       ImlibProgressFunction progress,
		       char progress_granularity, char immediate_load);
   char        (*save)(ImlibImage *im,
		       ImlibProgressFunction progress,
		       char progress_granularity);
   ImlibLoader  *next;
};

EAPI     void              __imlib_AttachTag(ImlibImage *im, const char *key,
					     int val, void *data,
					     ImlibDataDestructorFunction destructor);
EAPI     ImlibImageTag    *__imlib_GetTag(ImlibImage *im, const char *key);
__hidden ImlibImageTag    *__imlib_RemoveTag(ImlibImage *im, const char *key);
__hidden void              __imlib_FreeTag(ImlibImage *im, ImlibImageTag *t);
__hidden void              __imlib_FreeAllTags(ImlibImage *im);

__hidden void              __imlib_SetCacheSize(int size);
__hidden int               __imlib_GetCacheSize(void);
__hidden ImlibImage       *__imlib_ProduceImage(void);
__hidden void              __imlib_ConsumeImage(ImlibImage *im);
__hidden ImlibImage       *__imlib_FindCachedImage(const char *file);
__hidden void              __imlib_AddImageToCache(ImlibImage *im);
__hidden void              __imlib_RemoveImageFromCache(ImlibImage *im);
__hidden int               __imlib_CurrentCacheSize(void);
__hidden void              __imlib_CleanupImageCache(void);
# ifdef BUILD_X11
__hidden ImlibImagePixmap *__imlib_ProduceImagePixmap(void);
__hidden void              __imlib_ConsumeImagePixmap(ImlibImagePixmap *ip);
__hidden ImlibImagePixmap *__imlib_FindCachedImagePixmap(ImlibImage *im, int w, int h, 
						Display *d, Visual *v,
						int depth, int sx, int sy, 
						int sw, int sh, Colormap cm,
						char aa, char hiq, char dmask,
						DATABIG modification_count);
__hidden ImlibImagePixmap *__imlib_FindCachedImagePixmapByID(Display *d, Pixmap p);
__hidden void              __imlib_AddImagePixmapToCache(ImlibImagePixmap *ip);
__hidden void              __imlib_RemoveImagePixmapFromCache(ImlibImagePixmap *ip);
__hidden void              __imlib_CleanupImagePixmapCache(void);
# endif
__hidden ImlibLoader      *__imlib_ProduceLoader(char *file);
__hidden char            **__imlib_ListLoaders(int *num_ret);
__hidden char            **__imlib_TrimLoaderList(char **list, int *num);
__hidden int               __imlib_ItemInList(char **list, int size, char *item);
__hidden void              __imlib_ConsumeLoader(ImlibLoader *l);
__hidden void              __imlib_RescanLoaders(void);
__hidden void              __imlib_RemoveAllLoaders(void);
__hidden void              __imlib_LoadAllLoaders(void);
EAPI     ImlibLoader      *__imlib_FindBestLoaderForFile(const char *file, int for_save);
__hidden ImlibLoader      *__imlib_FindBestLoaderForFileFormat(const char *file, char *format, int for_save);
__hidden void              __imlib_SetImageAlphaFlag(ImlibImage *im, char alpha);
__hidden ImlibImage       *__imlib_CreateImage(int w, int h, DATA32 *data);
__hidden ImlibImage       *__imlib_LoadImage(const char *file,
				    ImlibProgressFunction progress,
				    char progress_granularity, char immediate_load,
				    char dont_cache, ImlibLoadError *er);
# ifdef BUILD_X11
__hidden ImlibImagePixmap *__imlib_FindImlibImagePixmapByID(Display *d, Pixmap p);
# endif
__hidden void              __imlib_FreeImage(ImlibImage *im);
# ifdef BUILD_X11
__hidden void              __imlib_FreePixmap(Display *d, Pixmap p);
# endif
__hidden void              __imlib_FlushCache(void);
# ifdef BUILD_X11
__hidden void              __imlib_DirtyPixmapsForImage(ImlibImage *im);
# else
#  define	__imlib_DirtyPixmapsForImage(x)	/* x */
# endif
__hidden void              __imlib_DirtyImage(ImlibImage *im);
__hidden void              __imlib_SaveImage(ImlibImage *im, const char *file,
				    ImlibProgressFunction progress,
		                    char progress_granularity,
		                    ImlibLoadError *er);

# define IMAGE_HAS_ALPHA(im) ((im)->flags & F_HAS_ALPHA)
# define IMAGE_IS_UNLOADED(im) ((im)->flags & F_UNLOADED)
# define IMAGE_IS_UNCACHEABLE(im) ((im)->flags & F_UNCACHEABLE)
# define IMAGE_ALWAYS_CHECK_DISK(im) ((im)->flags & F_ALWAYS_CHECK_DISK)
# define IMAGE_IS_VALID(im) (!((im)->flags & F_INVALID))
# define IMAGE_FREE_DATA(im) (!((im)->flags & F_DONT_FREE_DATA))

# define SET_FLAG(flags, f) ((flags) |= (f))
# define UNSET_FLAG(flags, f) ((flags) &= (~f))

# define IMAGE_DIMENSIONS_OK(w, h) \
   ( ((w) > 0) && ((h) > 0) && \
     ((unsigned long long)(w) * (unsigned long long)(h) <= (1ULL << 29) - 1) )

#endif
