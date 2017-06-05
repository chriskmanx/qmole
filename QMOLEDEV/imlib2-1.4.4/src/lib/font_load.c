#include "config.h"
#include "common.h"
#include "colormod.h"
#include "image.h"
#include "blend.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include "font.h"
#include <sys/types.h>
#include <string.h>
#include <math.h>
#include "file.h"
#include "updates.h"
#include "rgbadraw.h"
#include "rotate.h"

extern FT_Library   ft_lib;

static int          font_cache_usage = 0;
static int          font_cache = 0;
static char       **fpath = NULL;
static int          fpath_num = 0;
static Imlib_Object_List *fonts = NULL;

static ImlibFont   *imlib_font_load(const char *name, int faceidx, int size);
static int          font_modify_cache_cb(Imlib_Hash * hash, const char *key,
                                         void *data, void *fdata);
static int          font_flush_free_glyph_cb(Imlib_Hash * hash, const char *key,
                                             void *data, void *fdata);

/* FIXME now! listdir() from evas_object_text.c */

/* separate fontname and size, find font file, start imlib_font_load() then */
ImlibFont          *
imlib_font_load_joined(const char *fontname)
{
   int                 j, k, size, faceidx;
   char               *name = NULL, *file = NULL, *tmp = NULL;
   ImlibFont          *fn;

   /* split font name (in format name[:faceidx]/size) */
   for (j = strlen(fontname) - 1; (j >= 0) && (fontname[j] != '/'); j--);
   /* no "/" in font after the first char */
   if (j <= 0)
      return NULL;
   /* get size */
   size = atoi(&(fontname[j + 1]));
   /* split font faceidx index (in format name[:faceidx]/size) */
   faceidx = 0;
   for (k = j - 1; k > 0; k--)
     {
        if (fontname[k] >= '0' && fontname[k] <= '9')
           continue;
        if (fontname[k] != ':')
           break;
        faceidx = atoi(&(fontname[k + 1]));
        if (faceidx < 0)
           faceidx = 0;
        j = k;
        break;
     }
   /* split name in front off */
   name = malloc((j + 1) * sizeof(char));
   memcpy(name, fontname, j);
   name[j] = 0;
   /* find file if it exists */
   tmp = malloc(strlen(name) + 4 + 1);
   if (!tmp)
     {
        free(name);
        return NULL;
     }
   sprintf(tmp, "%s.ttf", name);
   if (__imlib_FileIsFile(tmp))
      file = strdup(tmp);
   else
     {
        sprintf(tmp, "%s.TTF", name);
        if (__imlib_FileIsFile(tmp))
           file = strdup(tmp);
        else
          {
             sprintf(tmp, "%s", name);
             if (__imlib_FileIsFile(tmp))
                file = strdup(tmp);
          }
     }
   free(tmp);
   if (!file)
     {
        for (j = 0; (j < fpath_num) && (!file); j++)
          {
             tmp = malloc(strlen(fpath[j]) + 1 + strlen(name) + 4 + 1);
             if (!tmp)
               {
                  free(name);
                  return NULL;
               }
             else
               {
                  sprintf(tmp, "%s/%s.ttf", fpath[j], name);
                  if (__imlib_FileIsFile(tmp))
                     file = strdup(tmp);
                  else
                    {
                       sprintf(tmp, "%s/%s.TTF", fpath[j], name);
                       if (__imlib_FileIsFile(tmp))
                          file = strdup(tmp);
                       else
                         {
                            sprintf(tmp, "%s/%s", fpath[j], name);
                            if (__imlib_FileIsFile(tmp))
                               file = strdup(tmp);
                         }
                    }
               }
             free(tmp);
          }
     }
   free(name);
   /* didnt find a file? abort */
   if (!file)
      return NULL;
   fn = imlib_font_load(file, faceidx, size);
   free(file);
   return fn;
}

static ImlibFont   *
imlib_font_load(const char *name, int faceidx, int size)
{
   int                 error;
   ImlibFont          *fn;
   char               *file;

   fn = imlib_font_find(name, size);
   if (fn)
      return fn;

   imlib_font_init();

   fn = malloc(sizeof(ImlibFont));
   file = (char *)name;

   error = FT_New_Face(ft_lib, file, faceidx, &(fn->ft.face));
   if (error)
     {
        free(fn);
        return NULL;
     }
   error = FT_Set_Char_Size(fn->ft.face, 0, (size * 64), 96, 96);
   if (error)
      error = FT_Set_Pixel_Sizes(fn->ft.face, 0, size);
   if (error)
     {
        int                 i;
        int                 chosen_size = 0;
        int                 chosen_width = 0;

        for (i = 0; i < fn->ft.face->num_fixed_sizes; i++)
          {
             int                 s;
             int                 d, cd;

             s = fn->ft.face->available_sizes[i].height;
             cd = chosen_size - size;
             if (cd < 0)
                cd = -cd;
             d = s - size;
             if (d < 0)
                d = -d;
             if (d < cd)
               {
                  chosen_width = fn->ft.face->available_sizes[i].width;
                  chosen_size = s;
               }
             if (d == 0)
                break;
          }
        error = FT_Set_Pixel_Sizes(fn->ft.face, chosen_width, chosen_size);
        if (error)
          {
             /* couldn't choose the size anyway... what now? */
          }
     }

   error = FT_Select_Charmap(fn->ft.face, ft_encoding_unicode);
   if (error)
     {
     }

   fn->file = strdup(file);
   fn->name = strdup(file);
   fn->size = size;

   fn->glyphs = NULL;

   fn->usage = 0;

   fn->references = 1;

   fn->fallback_prev = NULL;
   fn->fallback_next = NULL;

   fonts = imlib_object_list_prepend(fonts, fn);
   return fn;
}

void
imlib_font_free(ImlibFont * fn)
{
   fn->references--;
   if (fn->references == 0)
     {
        imlib_font_modify_cache_by(fn, 1);
        imlib_font_flush();
     }
}

int
imlib_font_insert_into_fallback_chain_imp(ImlibFont * fn, ImlibFont * fallback)
{
   /* avoid infinite recursion */
   if (fn == fallback)
      return 1;

   /* now remove the given fallback font from any chain it's already in */
   imlib_font_remove_from_fallback_chain_imp(fallback);

   /* insert fallback into fn's font chain */
   ImlibFont          *tmp = fn->fallback_next;

   fn->fallback_next = fallback;
   fallback->fallback_prev = fn;
   fallback->fallback_next = tmp;
   if (tmp)
      tmp->fallback_prev = fallback;
   return 0;
}

void
imlib_font_remove_from_fallback_chain_imp(ImlibFont * fn)
{
   /* if fn has a previous font in its font chain, then make its fallback_next fn's fallback_next since fn is going away */
   if (fn->fallback_prev)
      fn->fallback_prev->fallback_next = fn->fallback_next;
   fn->fallback_prev = NULL;
   fn->fallback_next = NULL;
}

static int
font_modify_cache_cb(Imlib_Hash * hash, const char *key, void *data,
                     void *fdata)
{
   int                *dir;
   Imlib_Font_Glyph   *fg;

   fg = data;
   dir = fdata;
   font_cache_usage += (*dir) * ((fg->glyph_out->bitmap.width * fg->glyph_out->bitmap.rows) + sizeof(Imlib_Font_Glyph) + sizeof(Imlib_Object_List) + 400);      /* fudge values */
   return 1;
   hash = 0;
   key = 0;
}

void
imlib_font_modify_cache_by(ImlibFont * fn, int dir)
{
   int                 sz_name = 0, sz_file = 0, sz_hash = 0;

   if (fn->name)
      sz_name = strlen(fn->name);
   if (fn->file)
      sz_file = strlen(fn->file);
   if (fn->glyphs)
      sz_hash = sizeof(Imlib_Hash);
   imlib_hash_foreach(fn->glyphs, font_modify_cache_cb, &dir);
   font_cache_usage += dir * (sizeof(ImlibFont) + sz_name + sz_file + sz_hash + sizeof(FT_FaceRec) + 16384);    /* fudge values */
}

int
imlib_font_cache_get(void)
{
   return font_cache;
}

void
imlib_font_cache_set(int size)
{
   font_cache = size;
   imlib_font_flush();
}

void
imlib_font_flush(void)
{
   if (font_cache_usage < font_cache)
      return;
   while (font_cache_usage > font_cache)
      imlib_font_flush_last();
}

static int
font_flush_free_glyph_cb(Imlib_Hash * hash, const char *key, void *data,
                         void *fdata)
{
   Imlib_Font_Glyph   *fg;

   fg = data;
   FT_Done_Glyph(fg->glyph);
   free(fg);
   return 1;
   hash = 0;
   key = 0;
   fdata = 0;
}

void
imlib_font_flush_last(void)
{
   Imlib_Object_List  *l;
   ImlibFont          *fn = NULL;

   for (l = fonts; l; l = l->next)
     {
        ImlibFont          *fn_tmp;

        fn_tmp = (ImlibFont *) l;
        if (fn_tmp->references == 0)
           fn = fn_tmp;
     }
   if (!fn)
      return;

   fonts = imlib_object_list_remove(fonts, fn);
   imlib_font_modify_cache_by(fn, -1);

   imlib_hash_foreach(fn->glyphs, font_flush_free_glyph_cb, NULL);
   imlib_hash_free(fn->glyphs);

   if (fn->file)
      free(fn->file);
   if (fn->name)
      free(fn->name);
   FT_Done_Face(fn->ft.face);
   free(fn);
}

ImlibFont          *
imlib_font_find(const char *name, int size)
{
   Imlib_Object_List  *l;

   for (l = fonts; l; l = l->next)
     {
        ImlibFont          *fn;

        fn = (ImlibFont *) l;
        if ((fn->size == size) && (!strcmp(name, fn->name)))
          {
             if (fn->references == 0)
                imlib_font_modify_cache_by(fn, -1);
             fn->references++;
             fonts = imlib_object_list_remove(fonts, fn);
             fonts = imlib_object_list_prepend(fonts, fn);
             return fn;
          }
     }
   return NULL;
}

/* font pathes */
void
imlib_font_add_font_path(const char *path)
{
   fpath_num++;
   if (!fpath)
      fpath = malloc(sizeof(char *));
   else
      fpath = realloc(fpath, (fpath_num * sizeof(char *)));
   fpath[fpath_num - 1] = strdup(path);
}

void
imlib_font_del_font_path(const char *path)
{
   int                 i, j;

   for (i = 0; i < fpath_num; i++)
     {
        if (!strcmp(path, fpath[i]))
          {
             if (fpath[i])
                free(fpath[i]);
             fpath_num--;
             for (j = i; j < fpath_num; j++)
                fpath[j] = fpath[j + 1];
             if (fpath_num > 0)
                fpath = realloc(fpath, fpath_num * sizeof(char *));
             else
               {
                  free(fpath);
                  fpath = NULL;
               }
          }
     }
}

int
imlib_font_path_exists(const char *path)
{
   int                 i;

   for (i = 0; i < fpath_num; i++)
     {
        if (!strcmp(path, fpath[i]))
           return 1;
     }
   return 0;
}

char              **
imlib_font_list_font_path(int *num_ret)
{
   *num_ret = fpath_num;
   return fpath;
}

/* fonts list */
char              **
imlib_font_list_fonts(int *num_ret)
{
   int                 i, j, d, l = 0;
   char              **list = NULL, **dir, *path;
   FT_Error            error;
   char               *p;

   imlib_font_init();

   for (i = 0; i < fpath_num; i++)
     {
        dir = __imlib_FileDir(fpath[i], &d);
        if (dir)
          {
             for (j = 0; j < d; j++)
               {
                  path = malloc(strlen(fpath[i]) + strlen(dir[j]) + 2);
                  sprintf(path, "%s/%s", fpath[i], dir[j]);
                  /* trim .ttf if it is there */
                  if ((p = strrchr(dir[j], '.')))
                     *p = '\0';
                  if (!__imlib_ItemInList(list, l, dir[j]))
                    {
                       if (__imlib_FileIsFile(path))
                         {
                            FT_Face             f;

                            error = FT_New_Face(ft_lib, path, 0, &f);
                            if (!error)
                              {
                                 FT_Done_Face(f);
                                 l++;
                                 if (list)
                                    list = realloc(list, sizeof(char *) * l);
                                 else
                                    list = malloc(sizeof(char *));
                                 list[l - 1] = strdup(dir[j]);
                              }
                            free(dir[j]);
                         }
                    }
                  free(path);
               }
             free(dir);
          }
     }
   *num_ret = l;
   return list;
}
