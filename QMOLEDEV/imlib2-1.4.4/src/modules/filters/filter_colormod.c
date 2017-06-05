#include "filter_common.h"
#include <string.h>
#include <math.h>

#define GET_INT(x, ptr) (((ptr)->type == VAR_PTR) ?	\
			(x) = (*(int *)(ptr)->data) :	\
			((ptr)->type == VAR_CHAR) ?	\
			(x) = strtol((ptr)->data, 0, 0)	\
			: 0)

#define GET_DOUBLE(x, ptr) (((ptr)->type == VAR_PTR) ?	\
			(x) = (*(double *)(ptr)->data) :	\
			((ptr)->type == VAR_CHAR) ?	\
			(x) = strtod((ptr)->data, 0)	\
			: 0)

static void
mod_brightness(double t[256], double v)
{
   int                 i;

   for (i = 256; --i >= 0;)
      t[i] += v;
}

static void
mod_contrast(double t[256], double v)
{
   int                 i;

   for (i = 256; --i >= 0;)
      t[i] = ((t[i] - 0.5) * v) + 0.5;
}

static void
mod_gamma(double t[256], double v)
{
   int                 i;

   for (i = 256; --i >= 0;)
      t[i] = pow(t[i], 1 / v);
}

static void
mod_tint(double t[256], double v)
{
   int                 i;

   for (i = 256; --i >= 0;)
      t[i] *= v;
}

static              Imlib_Image
colormod(Imlib_Image im, pIFunctionParam par)
{
   double              a_d[256], r_d[256], g_d[256], b_d[256];
   DATA8               a_b[256], r_b[256], g_b[256], b_b[256];
   pIFunctionParam     ptr;
   int                 x = 0, y = 0, h, w, i;
   double              v = 0.0;

   imlib_context_set_image(im);
   w = imlib_image_get_width();
   h = imlib_image_get_height();

   for (i = 256; --i >= 0;)
      a_d[i] = r_d[i] = g_d[i] = b_d[i] = (double)i / 255;

   for (ptr = par; ptr; ptr = ptr->next)
     {
        if (!strcmp("x", ptr->key))
          {
             GET_INT(x, ptr);
          }
        else if (!strcmp("y", ptr->key))
          {
             GET_INT(y, ptr);
          }
        else if (!strcmp("w", ptr->key))
          {
             GET_INT(w, ptr);
          }
        else if (!strcmp("h", ptr->key))
          {
             GET_INT(h, ptr);
          }
        else if (!memcmp("brightness", ptr->key, 10))
          {
             GET_DOUBLE(v, ptr);
             if (!ptr->key[10])
               {
                  mod_brightness(r_d, v);
                  mod_brightness(g_d, v);
                  mod_brightness(b_d, v);
                  mod_brightness(a_d, v);
               }
             else if (!strcmp("_r", ptr->key + 10))
               {
                  mod_brightness(r_d, v);
               }
             else if (!strcmp("_g", ptr->key + 10))
               {
                  mod_brightness(g_d, v);
               }
             else if (!strcmp("_b", ptr->key + 10))
               {
                  mod_brightness(b_d, v);
               }
             else if (!strcmp("_a", ptr->key + 10))
               {
                  mod_brightness(a_d, v);
               }
          }
        else if (!memcmp("contrast", ptr->key, 8))
          {
             GET_DOUBLE(v, ptr);
             if (!ptr->key[8])
               {
                  mod_contrast(r_d, v);
                  mod_contrast(g_d, v);
                  mod_contrast(b_d, v);
                  mod_contrast(a_d, v);
               }
             else if (!strcmp("_r", ptr->key + 8))
               {
                  mod_contrast(r_d, v);
               }
             else if (!strcmp("_g", ptr->key + 8))
               {
                  mod_contrast(g_d, v);
               }
             else if (!strcmp("_b", ptr->key + 8))
               {
                  mod_contrast(b_d, v);
               }
             else if (!strcmp("_a", ptr->key + 8))
               {
                  mod_contrast(a_d, v);
               }
          }
        else if (!memcmp("gamma", ptr->key, 5))
          {
             GET_DOUBLE(v, ptr);
             if (!ptr->key[5])
               {
                  mod_gamma(r_d, v);
                  mod_gamma(g_d, v);
                  mod_gamma(b_d, v);
                  mod_gamma(a_d, v);
               }
             else if (!strcmp("_r", ptr->key + 5))
               {
                  mod_gamma(r_d, v);
               }
             else if (!strcmp("_g", ptr->key + 5))
               {
                  mod_gamma(g_d, v);
               }
             else if (!strcmp("_b", ptr->key + 5))
               {
                  mod_gamma(b_d, v);
               }
             else if (!strcmp("_a", ptr->key + 5))
               {
                  mod_gamma(a_d, v);
               }
          }
        else if (!memcmp("tint", ptr->key, 4))
          {
             GET_DOUBLE(v, ptr);
             if (!ptr->key[4])
               {
                  mod_tint(r_d, v);
                  mod_tint(g_d, v);
                  mod_tint(b_d, v);
                  mod_tint(a_d, v);
               }
             else if (!strcmp("_r", ptr->key + 4))
               {
                  mod_tint(r_d, v);
               }
             else if (!strcmp("_g", ptr->key + 4))
               {
                  mod_tint(g_d, v);
               }
             else if (!strcmp("_b", ptr->key + 4))
               {
                  mod_tint(b_d, v);
               }
             else if (!strcmp("_a", ptr->key + 4))
               {
                  mod_tint(a_d, v);
               }
          }
     }
   for (i = 256; --i >= 0;)
     {
        if (a_d[i] < 0)
           a_d[i] = 0;
        if (a_d[i] > 1)
           a_d[i] = 1;
        a_b[i] = a_d[i] * 255;
        if (r_d[i] < 0)
           r_d[i] = 0;
        if (r_d[i] > 1)
           r_d[i] = 1;
        r_b[i] = r_d[i] * 255;
        if (g_d[i] < 0)
           g_d[i] = 0;
        if (g_d[i] > 1)
           g_d[i] = 1;
        g_b[i] = g_d[i] * 255;
        if (b_d[i] < 0)
           b_d[i] = 0;
        if (b_d[i] > 1)
           b_d[i] = 1;
        b_b[i] = b_d[i] * 255;
     }
   imlib_context_set_color_modifier(imlib_create_color_modifier());
   imlib_set_color_modifier_tables(r_b, g_b, b_b, a_b);
   imlib_apply_color_modifier_to_rectangle(x, y, w, h);
   imlib_free_color_modifier();
   return im;
}

void
init(struct imlib_filter_info *info)
{
   char               *filters[] = { "colormod" };
   int                 i = (sizeof(filters) / sizeof(*filters));

   info->name = strdup("Tinting");
   info->author = strdup("Willem Monsuwe (willem@stack.nl)");
   info->description =
       strdup("Provides most common color modification filters.");
   info->num_filters = i;
   info->filters = malloc(sizeof(char *) * i);
   while (--i >= 0)
      info->filters[i] = strdup(filters[i]);

}

void
deinit()
{
   return;
}

void               *
exec(char *filter, void *im, pIFunctionParam par)
{
   if (!strcmp(filter, "colormod"))
      return colormod((Imlib_Image) im, par);
   return im;
}
