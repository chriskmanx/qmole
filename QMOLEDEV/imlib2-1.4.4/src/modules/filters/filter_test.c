#include "filter_common.h"
#include <string.h>

void
init(struct imlib_filter_info *info)
{
   char               *filters[] = { "tint", "cool_text", "gradient" };
   int                 i = 0;

   info->name = strdup("Test Filter");
   info->author = strdup("Chris Ross - Boris - chris@darkrock.co.uk");
   info->description =
       strdup
       ("This filter is used to show that the imlib2 filter system works!");
   info->num_filters = 3;
   info->filters = malloc(sizeof(char *) * 3);
   for (i = 0; i < info->num_filters; i++)
      info->filters[i] = strdup(filters[i]);

}

void
deinit(void)
{
   return;
}

void               *
exec(char *filter, void *im, pIFunctionParam params)
{
   Imlib_Image         imge = im;
   Imlib_Image         anoim;
   IFunctionParam     *ptr;

   if (strcmp(filter, "tint") == 0)
     {
        Imlib_Color_Modifier cm;
        DATA8               atab[256];
        int                 x = 0, y = 0, w = 0, h = 0;
        DATA8               r = 255, b = 255, g = 255, a = 255;

/*     
      printf( "filter_test.c: tint called\n" );
 */
        /* Set friendly defaults */
        imlib_context_set_image(imge);
        w = imlib_image_get_width();
        h = imlib_image_get_height();

        for (ptr = params; ptr != NULL; ptr = ptr->next)
          {
             ASSIGN_DATA8("red", r);
             ASSIGN_DATA8("blue", b);
             ASSIGN_DATA8("green", g);
             ASSIGN_INT("x", x);
             ASSIGN_INT("y", y);
             ASSIGN_INT("w", w);
             ASSIGN_INT("h", h);
             ASSIGN_DATA8("alpha", a);
          }
/*
      printf( "Using values red=%d,blue=%d,green=%d,x=%d,y=%d,height=%d,width=%d,alpha=%d\n", r,b,g,x,y,w,h,a );
 */
        anoim = imlib_create_image(w, h);
        cm = imlib_create_color_modifier();
        imlib_context_set_color_modifier(cm);
        imlib_context_set_image(anoim);

        imlib_context_set_color(r, g, b, 255);
        imlib_image_fill_rectangle(0, 0, w, h);
        imlib_context_set_blend(1);
        imlib_image_set_has_alpha(1);

        memset(atab, a, sizeof(atab));
        imlib_set_color_modifier_tables(NULL, NULL, NULL, atab);
        imlib_apply_color_modifier_to_rectangle(0, 0, w, h);

        imlib_context_set_image(imge);
        imlib_blend_image_onto_image(anoim, 0, 0, 0, w, h, x, y, w, h);

        imlib_free_color_modifier();
        imlib_context_set_image(anoim);
        imlib_free_image_and_decache();
        imlib_context_set_image(imge);

        return imge;
     }

   if (strcmp(filter, "cool_text") == 0)
     {
        return imge;
     }
   if (strcmp(filter, "gradient") == 0)
     {
        int                 angle = 0;

        for (ptr = params; ptr != NULL; ptr = ptr->next)
          {
             ASSIGN_INT("angle", angle);
          }
        return imge;
     }
   return imge;
}
