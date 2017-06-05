
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

/* TODO separate fonts and data stuff */

typedef struct _Imlib_Font 		ImlibFont;
typedef struct _Imlib_Font_Glyph 	Imlib_Font_Glyph;

typedef struct _Imlib_Object_List 	Imlib_Object_List;
typedef struct _Imlib_Hash 		Imlib_Hash;
typedef struct _Imlib_Hash_El 		Imlib_Hash_El;

struct _Imlib_Object_List
{
   Imlib_Object_List  *next, *prev;
   Imlib_Object_List  *last;
};

struct _Imlib_Hash
{
   int                 population;
   Imlib_Object_List  *buckets[256];
};

struct _Imlib_Hash_El
{
   Imlib_Object_List   _list_data;
   char               *key;
   void               *data;
};

struct _Imlib_Font
{
   Imlib_Object_List   _list_data;
   char               *name;
   char               *file;
   int                 size;

   struct
   {
      FT_Face             face;
   }
   ft;

   Imlib_Hash         *glyphs;

   int                 usage;

   int                 references;

   /* using a double-linked list for the fallback chain */
   struct _Imlib_Font	*fallback_prev;
   struct _Imlib_Font	*fallback_next;
};

struct _Imlib_Font_Glyph
{
   FT_Glyph            glyph;
   FT_BitmapGlyph      glyph_out;
};

/* functions */

void                imlib_font_init(void);
int                 imlib_font_ascent_get(ImlibFont * fn);
int                 imlib_font_descent_get(ImlibFont * fn);
int                 imlib_font_max_ascent_get(ImlibFont * fn);
int                 imlib_font_max_descent_get(ImlibFont * fn);
int                 imlib_font_get_line_advance(ImlibFont * fn);
int                 imlib_font_utf8_get_next(unsigned char *buf, int *iindex);
void                imlib_font_add_font_path(const char *path);
void                imlib_font_del_font_path(const char *path);
int                 imlib_font_path_exists(const char *path);
char              **imlib_font_list_font_path(int *num_ret);
char              **imlib_font_list_fonts(int *num_ret);

ImlibFont          *imlib_font_load_joined(const char *name);
void                imlib_font_free(ImlibFont * fn);
int                 imlib_font_insert_into_fallback_chain_imp(ImlibFont * fn,
                                                              ImlibFont * fallback);
void                imlib_font_remove_from_fallback_chain_imp(ImlibFont * fn);
int                 imlib_font_cache_get(void);
void                imlib_font_cache_set(int size);
void                imlib_font_flush(void);
void                imlib_font_modify_cache_by(ImlibFont * fn, int dir);
void                imlib_font_modify_cache_by(ImlibFont * fn, int dir);
void                imlib_font_flush_last(void);
ImlibFont          *imlib_font_find(const char *name, int size);
ImlibFont          *imlib_font_find_glyph(ImlibFont * fn, int gl, unsigned int *ret_index);

void                imlib_font_query_size(ImlibFont * fn, const char *text,
					  int *w, int *h);
int                 imlib_font_query_inset(ImlibFont * fn, const char *text);
void                imlib_font_query_advance(ImlibFont * fn, const char *text,
					     int *h_adv, int *v_adv);
int                 imlib_font_query_char_coords(ImlibFont * fn,
						 const char *text, int pos,
						 int *cx, int *cy, int *cw,
						 int *ch);
int                 imlib_font_query_text_at_pos(ImlibFont * fn,
						 const char *text, int x, int y,
						 int *cx, int *cy, int *cw,
						 int *ch);

Imlib_Font_Glyph   *imlib_font_cache_glyph_get(ImlibFont * fn, FT_UInt index);
void                imlib_render_str(ImlibImage * im, ImlibFont * f, int drx,
				     int dry, const char *text, DATA8 r,
				     DATA8 g, DATA8 b, DATA8 a, char dir,
				     double angle, int *retw, int *reth,
				     int blur, int *nextx, int *nexty,
				     ImlibOp op, int clx, int cly, int clw,
				     int clh);
void                imlib_font_draw(ImlibImage * dst, DATA32 col,
				    ImlibFont * fn, int x, int y,
				    const char *text, int *nextx, int *nexty,
				    int clx, int cly, int clw, int clh);

/* data manipulation */

void               *imlib_object_list_prepend(void *in_list, void *in_item);
void               *imlib_object_list_remove(void *in_list, void *in_item);
Imlib_Hash         *imlib_hash_add(Imlib_Hash * hash, const char *key,
				   const void *data);
void               *imlib_hash_find(Imlib_Hash * hash, const char *key);
void                imlib_hash_free(Imlib_Hash * hash);
void                imlib_hash_foreach(Imlib_Hash * hash,
				       int (*func) (Imlib_Hash * hash,
						    const char *key, void *data,
						    void *fdata),
				       const void *fdata);
