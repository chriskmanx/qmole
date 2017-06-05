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

/* string extents */
void
imlib_font_query_size(ImlibFont * fn, const char *text, int *w, int *h)
{
   int                 use_kerning;
   int                 pen_x, pen_y;
   int                 start_x, end_x;
   int                 chr;
   FT_UInt             prev_index;

   start_x = 0;
   end_x = 0;
   pen_x = 0;
   pen_y = 0;
   use_kerning = FT_HAS_KERNING(fn->ft.face);
   prev_index = 0;
   for (chr = 0; text[chr];)
     {
        FT_UInt             index;
        Imlib_Font_Glyph   *fg;
        ImlibFont          *fn_in_chain;
        int                 chr_x, chr_y, chr_w;
        int                 gl;

        gl = imlib_font_utf8_get_next((unsigned char *)text, &chr);
        if (gl == 0)
           break;
        fn_in_chain = imlib_font_find_glyph(fn, gl, &index);
        if ((use_kerning) && (prev_index) && (index))
          {
             FT_Vector           delta;

             FT_Get_Kerning(fn_in_chain->ft.face, prev_index, index,
                            ft_kerning_default, &delta);
             pen_x += delta.x << 2;
          }
        fg = imlib_font_cache_glyph_get(fn_in_chain, index);
        if (!fg)
           continue;

        chr_x = (pen_x >> 8) + fg->glyph_out->left;
        chr_y = (pen_y >> 8) + fg->glyph_out->top;
        chr_w = fg->glyph_out->bitmap.width;

        if (pen_x == 0)
           start_x = chr_x;
        if ((chr_x + chr_w) > end_x)
           end_x = chr_x + chr_w;

        pen_x += fg->glyph->advance.x >> 8;
        prev_index = index;
     }
   if (w)
      *w = (pen_x >> 8) - start_x;
   if (h)
      *h = imlib_font_max_ascent_get(fn) - imlib_font_max_descent_get(fn);      /* TODO: compute this inside the loop since we now may be dealing with multiple fonts */
}

/* text x inset */
int
imlib_font_query_inset(ImlibFont * fn, const char *text)
{
   FT_UInt             index;
   Imlib_Font_Glyph   *fg;
   ImlibFont          *fn_in_chain;
   int                 chr;
   int                 gl;

   chr = 0;
   if (!text[0])
      return 0;
   gl = imlib_font_utf8_get_next((unsigned char *)text, &chr);
   if (gl == 0)
      return 0;
   fn_in_chain = imlib_font_find_glyph(fn, gl, &index);
   fg = imlib_font_cache_glyph_get(fn_in_chain, index);
   if (!fg)
      return 0;
   return -fg->glyph_out->left;
}

/* h & v advance */
void
imlib_font_query_advance(ImlibFont * fn, const char *text, int *h_adv,
                         int *v_adv)
{
   int                 use_kerning;
   int                 pen_x, pen_y;
   int                 start_x;
   int                 chr;
   FT_UInt             prev_index;

   start_x = 0;
   pen_x = 0;
   pen_y = 0;
   use_kerning = FT_HAS_KERNING(fn->ft.face);
   prev_index = 0;
   for (chr = 0; text[chr];)
     {
        FT_UInt             index;
        Imlib_Font_Glyph   *fg;
        ImlibFont          *fn_in_chain;
        int                 chr_x, chr_y, chr_w;
        int                 gl;

        gl = imlib_font_utf8_get_next((unsigned char *)text, &chr);
        if (gl == 0)
           break;
        fn_in_chain = imlib_font_find_glyph(fn, gl, &index);
        if ((use_kerning) && (prev_index) && (index))
          {
             FT_Vector           delta;

             FT_Get_Kerning(fn_in_chain->ft.face, prev_index, index,
                            ft_kerning_default, &delta);
             pen_x += delta.x << 2;
          }
        fg = imlib_font_cache_glyph_get(fn_in_chain, index);
        if (!fg)
           continue;

        chr_x = (pen_x >> 8) + fg->glyph_out->left;
        chr_y = (pen_y >> 8) + fg->glyph_out->top;
        chr_w = fg->glyph_out->bitmap.width;

        pen_x += fg->glyph->advance.x >> 8;
        prev_index = index;
     }
   if (v_adv)
      *v_adv = imlib_font_get_line_advance(fn); /* TODO: compute this in the loop since we may be dealing with multiple fonts */
   if (h_adv)
      *h_adv = (pen_x >> 8) - start_x;
}

/* x y w h for char at char pos */
int
imlib_font_query_char_coords(ImlibFont * fn, const char *text, int pos,
                             int *cx, int *cy, int *cw, int *ch)
{
   int                 use_kerning;
   int                 pen_x, pen_y;
   int                 prev_chr_end;
   int                 chr;
   int                 asc, desc;
   FT_UInt             prev_index;

   pen_x = 0;
   pen_y = 0;
   use_kerning = FT_HAS_KERNING(fn->ft.face);
   prev_index = 0;
   prev_chr_end = 0;
   asc = imlib_font_max_ascent_get(fn);
   desc = imlib_font_max_descent_get(fn);
   for (chr = 0; text[chr];)
     {
        int                 pchr;
        FT_UInt             index;
        Imlib_Font_Glyph   *fg;
        ImlibFont          *fn_in_chain;
        int                 chr_x, chr_y, chr_w;
        int                 gl, kern;
        FT_Vector           delta;

        pchr = chr;
        gl = imlib_font_utf8_get_next((unsigned char *)text, &chr);
        if (gl == 0)
           break;
        fn_in_chain = imlib_font_find_glyph(fn, gl, &index);
        kern = 0;
        if ((use_kerning) && (prev_index) && (index))
          {
             FT_Get_Kerning(fn_in_chain->ft.face, prev_index, index,
                            ft_kerning_default, &delta);
             kern = delta.x << 2;
             pen_x += kern;
          }
        fg = imlib_font_cache_glyph_get(fn_in_chain, index);
        if (!fg)
           continue;

        if (kern < 0)
           kern = 0;
        chr_x = ((pen_x - kern) >> 8) + fg->glyph_out->left;
        chr_y = (pen_y >> 8) + fg->glyph_out->top;
        chr_w = fg->glyph_out->bitmap.width + (kern >> 8);
        if (text[chr])
          {
             int                 advw;

             advw = ((fg->glyph->advance.x + (kern << 8)) >> 16);
             if (chr_w < advw)
                chr_w = advw;
          }
        if (chr_x > prev_chr_end)
          {
             chr_w += (chr_x - prev_chr_end);
             chr_x = prev_chr_end;
          }
        if (pchr == pos)
          {
             if (cx)
                *cx = chr_x;
             if (cy)
                *cy = -asc;
             if (cw)
                *cw = chr_w;
             if (ch)
                *ch = asc + desc;
             return 1;
          }
        prev_chr_end = chr_x + chr_w;
        pen_x += fg->glyph->advance.x >> 8;
        prev_index = index;
     }
   return 0;
}

/* char pos of text at xy pos */
int
imlib_font_query_text_at_pos(ImlibFont * fn, const char *text, int x, int y,
                             int *cx, int *cy, int *cw, int *ch)
{
   int                 use_kerning;
   int                 pen_x, pen_y;
   int                 prev_chr_end;
   int                 chr;
   int                 asc, desc;
   FT_UInt             prev_index;

   pen_x = 0;
   pen_y = 0;
   use_kerning = FT_HAS_KERNING(fn->ft.face);
   prev_index = 0;
   prev_chr_end = 0;
   asc = imlib_font_max_ascent_get(fn);
   desc = imlib_font_max_descent_get(fn);
   for (chr = 0; text[chr];)
     {
        int                 pchr;
        FT_UInt             index;
        Imlib_Font_Glyph   *fg;
        ImlibFont          *fn_in_chain;
        int                 chr_x, chr_y, chr_w;
        int                 gl, kern;
        FT_Vector           delta;

        pchr = chr;
        gl = imlib_font_utf8_get_next((unsigned char *)text, &chr);
        if (gl == 0)
           break;
        fn_in_chain = imlib_font_find_glyph(fn, gl, &index);
        kern = 0;
        if ((use_kerning) && (prev_index) && (index))
          {
             FT_Get_Kerning(fn_in_chain->ft.face, prev_index, index,
                            ft_kerning_default, &delta);
             kern = delta.x << 2;
             pen_x += kern;
          }
        fg = imlib_font_cache_glyph_get(fn_in_chain, index);
        if (!fg)
           continue;

        if (kern < 0)
           kern = 0;
        chr_x = ((pen_x - kern) >> 8) + fg->glyph_out->left;
        chr_y = (pen_y >> 8) + fg->glyph_out->top;
        chr_w = fg->glyph_out->bitmap.width + (kern >> 8);
        if (text[chr])
          {
             int                 advw;

             advw = ((fg->glyph->advance.x + (kern << 8)) >> 16);
             if (chr_w < advw)
                chr_w = advw;
          }
        if (chr_x > prev_chr_end)
          {
             chr_w += (chr_x - prev_chr_end);
             chr_x = prev_chr_end;
          }
        if ((x >= chr_x) && (x <= (chr_x + chr_w)) && (y > -asc) && (y < desc))
          {
             if (cx)
                *cx = chr_x;
             if (cy)
                *cy = -asc;
             if (cw)
                *cw = chr_w;
             if (ch)
                *ch = asc + desc;
             return pchr;
          }
        prev_chr_end = chr_x + chr_w;
        pen_x += fg->glyph->advance.x >> 8;
        prev_index = index;
     }
   return -1;
}
