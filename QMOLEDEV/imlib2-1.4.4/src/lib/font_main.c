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

FT_Library          ft_lib;

static int          imlib_hash_gen(const char *key);
static int          imlib_list_alloc_error(void);

static int          _imlib_hash_alloc_error = 0;
static int          _imlib_list_alloc_error = 0;

void
imlib_font_init(void)
{
   static int          initialised = 0;
   int                 error;

   if (initialised)
      return;
   error = FT_Init_FreeType(&ft_lib);
   if (error)
      return;
   initialised = 1;
}

int
imlib_font_ascent_get(ImlibFont * fn)
{
   int                 val;
   int                 ret;

   val = (int)fn->ft.face->ascender;
   fn->ft.face->units_per_EM = 2048;    /* nasy hack - need to have correct 
                                         * val */
   ret =
      (val * fn->ft.face->size->metrics.y_scale) /
      (fn->ft.face->units_per_EM * fn->ft.face->units_per_EM);
   return ret;
}

int
imlib_font_descent_get(ImlibFont * fn)
{
   int                 val;
   int                 ret;

   val = -(int)fn->ft.face->descender;
   fn->ft.face->units_per_EM = 2048;    /* nasy hack - need to have correct 
                                         * val */
   ret =
      (val * fn->ft.face->size->metrics.y_scale) /
      (fn->ft.face->units_per_EM * fn->ft.face->units_per_EM);
   return ret;
}

int
imlib_font_max_ascent_get(ImlibFont * fn)
{
   int                 val;
   int                 ret;

   val = (int)fn->ft.face->bbox.yMax;
   fn->ft.face->units_per_EM = 2048;    /* nasy hack - need to have correct 
                                         * val */
   ret =
      (val * fn->ft.face->size->metrics.y_scale) /
      (fn->ft.face->units_per_EM * fn->ft.face->units_per_EM);
   return ret;
}

int
imlib_font_max_descent_get(ImlibFont * fn)
{
   int                 val;
   int                 ret;

   val = (int)fn->ft.face->bbox.yMin;
   fn->ft.face->units_per_EM = 2048;    /* nasy hack - need to have correct 
                                         * val */
   ret =
      (val * fn->ft.face->size->metrics.y_scale) /
      (fn->ft.face->units_per_EM * fn->ft.face->units_per_EM);
   return ret;
}

int
imlib_font_get_line_advance(ImlibFont * fn)
{
   int                 val;
   int                 ret;

   val = (int)fn->ft.face->height;
   fn->ft.face->units_per_EM = 2048;    /* nasy hack - need to have correct 
                                         * val */
   ret =
      (val * fn->ft.face->size->metrics.y_scale) /
      (fn->ft.face->units_per_EM * fn->ft.face->units_per_EM);
   return ret;
}

int
imlib_font_utf8_get_next(unsigned char *buf, int *iindex)
{
   /* Reads UTF8 bytes from @buf, starting at *@index and returns the code
    * point of the next valid code point. @index is updated ready for the
    * next call.
    * 
    * * Returns 0 to indicate an error (e.g. invalid UTF8) */

   int                 index = *iindex, r;
   unsigned char       d = buf[index++], d2, d3, d4;

   if (!d)
      return 0;
   if (d < 0x80)
     {
        *iindex = index;
        return d;
     }
   if ((d & 0xe0) == 0xc0)
     {
        /* 2 byte */
        d2 = buf[index++];
        if ((d2 & 0xc0) != 0x80)
           return 0;
        r = d & 0x1f;           /* copy lower 5 */
        r <<= 6;
        r |= (d2 & 0x3f);       /* copy lower 6 */
     }
   else if ((d & 0xf0) == 0xe0)
     {
        /* 3 byte */
        d2 = buf[index++];
        d3 = buf[index++];
        if ((d2 & 0xc0) != 0x80 || (d3 & 0xc0) != 0x80)
           return 0;
        r = d & 0x0f;           /* copy lower 4 */
        r <<= 6;
        r |= (d2 & 0x3f);
        r <<= 6;
        r |= (d3 & 0x3f);
     }
   else
     {
        /* 4 byte */
        d2 = buf[index++];
        d3 = buf[index++];
        d4 = buf[index++];
        if ((d2 & 0xc0) != 0x80 || (d3 & 0xc0) != 0x80 || (d4 & 0xc0) != 0x80)
           return 0;
        r = d & 0x0f;           /* copy lower 4 */
        r <<= 6;
        r |= (d2 & 0x3f);
        r <<= 6;
        r |= (d3 & 0x3f);
        r <<= 6;
        r |= (d4 & 0x3f);

     }
   *iindex = index;
   return r;
}

/* TODO put this somewhere else */

void               *
imlib_object_list_prepend(void *in_list, void *in_item)
{
   Imlib_Object_List  *new_l;
   Imlib_Object_List  *list, *item;

   list = in_list;
   item = in_item;
   new_l = item;
   new_l->prev = NULL;
   if (!list)
     {
        new_l->next = NULL;
        new_l->last = new_l;
        return new_l;
     }
   new_l->next = list;
   list->prev = new_l;
   new_l->last = list->last;
   list->last = NULL;
   return new_l;
}

void               *
imlib_object_list_remove(void *in_list, void *in_item)
{
   Imlib_Object_List  *return_l;
   Imlib_Object_List  *list, *item;

   /* checkme */
   if (!in_list)
      return in_list;

   list = in_list;
   item = in_item;
   if (!item)
      return list;
   if (item->next)
      item->next->prev = item->prev;
   if (item->prev)
     {
        item->prev->next = item->next;
        return_l = list;
     }
   else
     {
        return_l = item->next;
        if (return_l)
           return_l->last = list->last;
     }
   if (item == list->last)
      list->last = item->prev;
   item->next = NULL;
   item->prev = NULL;
   return return_l;
}

static int
imlib_hash_gen(const char *key)
{
   unsigned int        hash_num = 0;
   const unsigned char *ptr;

   if (!key)
      return 0;

   for (ptr = (unsigned char *)key; *ptr; ptr++)
      hash_num ^= (int)(*ptr);

   hash_num &= 0xff;
   return (int)hash_num;
}

Imlib_Hash         *
imlib_hash_add(Imlib_Hash * hash, const char *key, const void *data)
{
   int                 hash_num;
   Imlib_Hash_El      *el;

   _imlib_hash_alloc_error = 0;
   if (!hash)
     {
        hash = calloc(1, sizeof(struct _Imlib_Hash));
        if (!hash)
          {
             _imlib_hash_alloc_error = 1;
             return NULL;
          }
     }
   if (!(el = malloc(sizeof(struct _Imlib_Hash_El))))
     {
        if (hash->population <= 0)
          {
             free(hash);
             hash = NULL;
          }
        _imlib_hash_alloc_error = 1;
        return hash;
     };
   if (key)
     {
        el->key = strdup(key);
        if (!el->key)
          {
             free(el);
             _imlib_hash_alloc_error = 1;
             return hash;
          }
        hash_num = imlib_hash_gen(key);
     }
   else
     {
        el->key = NULL;
        hash_num = 0;
     }
   el->data = (void *)data;

   hash->buckets[hash_num] =
      imlib_object_list_prepend(hash->buckets[hash_num], el);

   if (imlib_list_alloc_error())
     {
        _imlib_hash_alloc_error = 1;
        if (el->key)
           free(el->key);
        free(el);
        return hash;
     }
   hash->population++;
   return hash;
}

void               *
imlib_hash_find(Imlib_Hash * hash, const char *key)
{
   int                 hash_num;
   Imlib_Hash_El      *el;
   Imlib_Object_List  *l;

   _imlib_hash_alloc_error = 0;
   if (!hash)
      return NULL;
   hash_num = imlib_hash_gen(key);
   for (l = hash->buckets[hash_num]; l; l = l->next)
     {
        el = (Imlib_Hash_El *) l;
        if (((el->key) && (key) && (!strcmp(el->key, key)))
            || ((!el->key) && (!key)))
          {
             if (l != hash->buckets[hash_num])
               {
                  /* FIXME: move to front of list without alloc */
                  hash->buckets[hash_num] =
                     imlib_object_list_remove(hash->buckets[hash_num], el);
                  hash->buckets[hash_num] =
                     imlib_object_list_prepend(hash->buckets[hash_num], el);
                  if (imlib_list_alloc_error())
                    {
                       _imlib_hash_alloc_error = 1;
                       return el->data;
                    }
               }
             return el->data;
          }
     }
   return NULL;
}

static int
imlib_hash_size(Imlib_Hash * hash)
{
   if (!hash)
      return 0;
   return 256;
}

void
imlib_hash_free(Imlib_Hash * hash)
{
   int                 i, size;

   if (!hash)
      return;
   size = imlib_hash_size(hash);
   for (i = 0; i < size; i++)
     {
        while (hash->buckets[i])
          {
             Imlib_Hash_El      *el;

             el = (Imlib_Hash_El *) hash->buckets[i];
             if (el->key)
                free(el->key);
             hash->buckets[i] = imlib_object_list_remove(hash->buckets[i], el);
             free(el);
          }
     }
   free(hash);
}

void
imlib_hash_foreach(Imlib_Hash * hash, int (*func) (Imlib_Hash * hash,
                                                   const char *key, void *data,
                                                   void *fdata),
                   const void *fdata)
{
   int                 i, size;

   if (!hash)
      return;
   size = imlib_hash_size(hash);
   for (i = 0; i < size; i++)
     {
        Imlib_Object_List  *l, *next_l;

        for (l = hash->buckets[i]; l;)
          {
             Imlib_Hash_El      *el;

             next_l = l->next;
             el = (Imlib_Hash_El *) l;
             if (!func(hash, el->key, el->data, (void *)fdata))
                return;
             l = next_l;
          }
     }
}

int
imlib_list_alloc_error(void)
{
   return _imlib_list_alloc_error;
}
