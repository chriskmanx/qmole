#include "filter_common.h"
#include <string.h>
#include <math.h>
#include "colormod.h"
#include "blend.h"

#define PI (4 * atan(1))

static              Imlib_Image
bump_map(Imlib_Image im, pIFunctionParam par)
{
   Imlib_Image         map = im;
   pIFunctionParam     ptr;
   double              an = 0, el = 30, d = 0x200;
   double              red = 0x200, green = 0x200, blue = 0x200;
   double              ambient = 0;

   int                 free_map = 0;
   DATA32             *src;
   DATA32             *mp, *mpy, *mpp;
   double              z, z_2, x2, y2;
   int                 w, h, i, j, w2, h2, wh2, mx, my;

   for (ptr = par; ptr; ptr = ptr->next)
     {
        ASSIGN_IMAGE("map", map);
        ASSIGN_INT("angle", an);
        ASSIGN_INT("elevation", el);
        ASSIGN_INT("depth", d);
        ASSIGN_INT("red", red);
        ASSIGN_INT("green", green);
        ASSIGN_INT("blue", blue);
        ASSIGN_INT("ambient", ambient);
     }
   if (!map)
      return im;

   red /= 0x100;
   green /= 0x100;
   blue /= 0x100;
   ambient /= 0x100;
   d /= 0x100;

   imlib_context_set_image(im);
   src = imlib_image_get_data();
   w = imlib_image_get_width();
   h = imlib_image_get_height();

   imlib_context_set_image(map);
   mpp = imlib_image_get_data_for_reading_only();
   w2 = imlib_image_get_width();
   h2 = imlib_image_get_height();
   wh2 = w2 * h2;

   an *= (PI / 180);
   el *= (PI / 180);

   x2 = sin(an) * cos(el);
   y2 = cos(an) * cos(el);
   z = sin(el);

   d /= (255 * (255 + 255 + 255));
   z_2 = z * z;

   my = h2;
   for (j = h; --j >= 0;)
     {
        mp = mpp;
        mpp += w2;
        if (--my <= 0)
          {
             mpp -= wh2;
             my = h2;
          }
        mpy = mpp;
        mx = w2;
        for (i = w; --i >= 0;)
          {
             double              x1, y1, v;
             int                 r, g, b, gr;

             gr = A_VAL(mp) * (R_VAL(mp) + G_VAL(mp) + B_VAL(mp));
             y1 = d * (double)(A_VAL(mpy) * (R_VAL(mpy) +
                                             G_VAL(mpy) + B_VAL(mpy)) - gr);
             mp++;
             mpy++;
             if (--mx <= 0)
               {
                  mp -= w2;
                  mpy -= w2;
                  mx = w2;
               }
             x1 = d * (double)(A_VAL(mp) * (R_VAL(mp) +
                                            G_VAL(mp) + B_VAL(mp)) - gr);
             v = x1 * x2 + y1 * y2 + z;
             v /= sqrt((x1 * x1) + (y1 * y1) + 1.0);
             v += ambient;
             r = v * R_VAL(src) * red;
             g = v * G_VAL(src) * green;
             b = v * B_VAL(src) * blue;
             if (r < 0)
                r = 0;
             if (r > 255)
                r = 255;
             if (g < 0)
                g = 0;
             if (g > 255)
                g = 255;
             if (b < 0)
                b = 0;
             if (b > 255)
                b = 255;
             R_VAL(src) = r;
             G_VAL(src) = g;
             B_VAL(src) = b;

             src++;
          }
     }
   if (free_map)
     {
        imlib_context_set_image(map);
        imlib_free_image();
     }
   return im;
}

static              Imlib_Image
bump_map_point(Imlib_Image im, pIFunctionParam par)
{
   Imlib_Image         map = im;
   pIFunctionParam     ptr;
   double              x = 0, y = 0, z = 30, d = 0x200;
   double              red = 0x200, green = 0x200, blue = 0x200;
   double              ambient = 0;

   int                 free_map = 0;
   DATA32             *src;
   DATA32             *mp, *mpy, *mpp;
   double              z_2, x2, y2;
   int                 w, h, i, j, w2, h2, wh2, mx, my;

   for (ptr = par; ptr; ptr = ptr->next)
     {
        ASSIGN_IMAGE("map", map);
        ASSIGN_INT("x", x);
        ASSIGN_INT("y", y);
        ASSIGN_INT("z", z);
        ASSIGN_INT("depth", d);
        ASSIGN_INT("red", red);
        ASSIGN_INT("green", green);
        ASSIGN_INT("blue", blue);
        ASSIGN_INT("ambient", ambient);
     }
   if (!map)
      return im;

   red /= 0x100;
   green /= 0x100;
   blue /= 0x100;
   ambient /= 0x100;
   d /= 0x100;

   imlib_context_set_image(im);
   src = imlib_image_get_data();
   w = imlib_image_get_width();
   h = imlib_image_get_height();

   imlib_context_set_image(map);
   mpp = imlib_image_get_data_for_reading_only();
   w2 = imlib_image_get_width();
   h2 = imlib_image_get_height();
   wh2 = w2 * h2;

   d /= (255 * (255 + 255 + 255));
   z_2 = z * z;

   my = h2;
   y2 = -y;
   for (j = h; --j >= 0;)
     {
        mp = mpp;
        mpp += w2;
        if (--my <= 0)
          {
             mpp -= wh2;
             my = h2;
          }
        mpy = mpp;
        mx = w2;
        x2 = -x;
        for (i = w; --i >= 0;)
          {
             double              x1, y1, v;
             int                 r, g, b, gr;

             gr = A_VAL(mp) * (R_VAL(mp) + G_VAL(mp) + B_VAL(mp));
             y1 = d * (double)(A_VAL(mpy) * (R_VAL(mpy) +
                                             G_VAL(mpy) + B_VAL(mpy)) - gr);
             mp++;
             mpy++;
             if (--mx <= 0)
               {
                  mp -= w2;
                  mpy -= w2;
                  mx = w2;
               }
             x1 = d * (double)(A_VAL(mp) * (R_VAL(mp) +
                                            G_VAL(mp) + B_VAL(mp)) - gr);
             v = x1 * x2 + y1 * y2 + z;
             v /= sqrt((x1 * x1) + (y1 * y1) + 1.0);
             v /= sqrt((x2 * x2) + (y2 * y2) + z_2);
             v += ambient;
             r = v * R_VAL(src) * red;
             g = v * G_VAL(src) * green;
             b = v * B_VAL(src) * blue;
             if (r < 0)
                r = 0;
             if (r > 255)
                r = 255;
             if (g < 0)
                g = 0;
             if (g > 255)
                g = 255;
             if (b < 0)
                b = 0;
             if (b > 255)
                b = 255;
             R_VAL(src) = r;
             G_VAL(src) = g;
             B_VAL(src) = b;

             x2++;
             src++;
          }
        y2++;
     }
   if (free_map)
     {
        imlib_context_set_image(map);
        imlib_free_image();
     }
   return im;
}

void
init(struct imlib_filter_info *info)
{
   char               *filters[] = { "bump_map_point", "bump_map" };
   int                 i = (sizeof(filters) / sizeof(*filters));

   info->name = strdup("Bump Mapping");
   info->author = strdup("Willem Monsuwe (willem@stack.nl)");
   info->description =
       strdup
       ("Provides bumpmapping to a point and bumpmapping from an infinite light source. *very* cool.");
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
   if (!strcmp(filter, "bump_map"))
      return bump_map((Imlib_Image) im, par);
   if (!strcmp(filter, "bump_map_point"))
      return bump_map_point((Imlib_Image) im, par);
   return im;
}
