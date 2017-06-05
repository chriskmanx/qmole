/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005, Øyvind Kolås.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 *
 */

#include "config.h"
#include "babl-internal.h"    /* needed for babl_log */

static void model_html (Babl *babl);
static void type_html (Babl *babl);
static void format_html (Babl *babl);
static void conversion_html (Babl *babl);

static int  each_item (Babl *babl,
                       void *user_data);
static int  show_item (Babl *babl,
                       void *user_data);
static int  hide_item (Babl *babl,
                       void *user_data);


int
main (void)
{
  babl_init ();

  printf ("<br/><a href='javascript:");
  printf ("show(\"x_types\");show(\"x_models\");show(\"x_formats\");show(\"x_conversions\");");
  babl_type_class_for_each (show_item, NULL);
  babl_model_class_for_each (show_item, NULL);
  babl_format_class_for_each (show_item, NULL);
/*  babl_conversion_each (show_item, NULL);*/
  printf ("'>+</a>");

  printf ("<a href='javascript:");
  printf ("hide(\"x_types\");hide(\"x_models\");hide(\"x_formats\");hide(\"x_conversions\");");
  babl_type_class_for_each (hide_item, NULL);
  babl_model_class_for_each (hide_item, NULL);
  babl_format_class_for_each (hide_item, NULL);
  /*babl_conversion_each (hide_item, NULL);*/
  printf ("'>-</a>");

  printf ("<div class='expander'>");
  printf ("<div class='expander_title'><a style='font-size:110%%' name='Data-types' href='javascript:toggle_visible(\"x_types\")'>Data types</a></div><div class='expander_content' id='x_types'>\n");
  babl_type_class_for_each (each_item, NULL);
  printf ("</div>\n");
  printf ("</div>\n");

  printf ("<div class='expander'>");
  printf ("<div class='expander_title'><a style='font-size:110%%' name='Color-models' href='javascript:toggle_visible(\"x_models\")'>Color models</a></div><div class='expander_content' id='x_models'>\n");
  babl_model_class_for_each (each_item, NULL);
  printf ("</div>\n");
  printf ("</div>\n");


  printf ("<div class='expander'>");
  printf ("<div class='expander_title'><a style='font-size:110%%' name='Pixel-formats' href='javascript:toggle_visible(\"x_formats\")'>Pixel formats</a></div><div class='expander_content' id='x_formats'>\n");
  babl_format_class_for_each (each_item, NULL);
  printf ("</div>\n");
  printf ("</div>\n");

/*
   printf ("<div class='expander'>");
   printf ("<div class='expander_title'><a style='font-size:110%%' name='Conversions' href='javascript:toggle_visible(\"x_conversions\")'>Conversions</a></div><div class='expander_content' id='x_conversions'>\n");
   babl_conversion_each (each_item, NULL);
   printf ("</div>\n");
   printf ("</div>\n");
 */
  babl_exit ();

  return 0;
}


static char normalized_buf[512];

static const char *normalize (const char *str)
{
  char *s = normalized_buf;

  strcpy (normalized_buf, str);

  while (*s)
    {
      if ((*s >= 'a' && *s <= 'z') ||
          (*s >= 'A' && *s <= 'Z') ||
          (*s >= '0' && *s <= '9'))
        {
        }
      else
        {
          *s = '_';
        }
      s++;
    }
  return normalized_buf;
}


static int
show_item (Babl *babl,
           void *user_data)
{
  printf ("show(\"x_%s\");", normalize (babl->instance.name));
  return 0;
}


static int
hide_item (Babl *babl,
           void *user_data)
{
  printf ("hide(\"x_%s\");", normalize (babl->instance.name));
  return 0;
}

static int
each_item (Babl *babl,
           void *user_data)
{
  printf ("<div class='expander'>");
  printf ("<div class='expander_title'><a href='javascript:toggle_visible(\"x_%s\")'>%s</a></div>\n",
          normalize (babl->instance.name), babl->instance.name);
  printf ("<div class='expander_content' id='x_%s'>\n",
          normalize (babl->instance.name));


  switch (babl->class_type)
    {
      case BABL_TYPE:
        type_html (babl);
        break;

      case BABL_MODEL:
        model_html (babl);
        break;

      case BABL_FORMAT:
        format_html (babl);
        break;

      case BABL_CONVERSION:
      case BABL_CONVERSION_LINEAR:
      case BABL_CONVERSION_PLANE:
      case BABL_CONVERSION_PLANAR:
        conversion_html (babl);
        break;

      default:
        break;
    }

  printf ("</div>\n");
  printf ("</div>\n");
  return 0;
}

static void
model_html (Babl *babl)
{
  int i;

  printf ("<dl>");
  printf ("<dt>components</dt><dd><table class='nopad'>");

  for (i = 0; i < babl->model.components; i++)
    {
      printf ("<tr><td class='type'>%s</td></tr>",
              BABL (babl->model.component[i])->instance.name);
    }
  printf ("</table></dd></dl>");
}

static void
type_html (Babl *babl)
{
  printf ("<dl><dt>bits</dt><dd>%i</dd>", babl->type.bits);
  printf ("<dt>bytes</dt><dd>%i</dd></dl>", babl->type.bits / 8);
}


static void
conversion_html (Babl *babl)
{
  printf ("\n");
}

static void
format_html (Babl *babl)
{
  int i;

  printf ("<dl>");
  printf ("<dt>bytes/pixel</dt><dd>%i</dd>", babl->format.bytes_per_pixel);
  printf ("<dt>model</dt><dd>%s</dd>", BABL (babl->format.model)->instance.name);
  printf ("<dt>components</dt><dd><table class='nopad'>");

  for (i = 0; i < babl->format.components; i++)
    {
      printf ("<tr><td class='type'>%s</td><td class='component'>%s</td></tr>",
              BABL (babl->format.type[i])->instance.name,
              BABL (babl->format.component[i])->instance.name);
    }
  printf ("</table></dd></dl>");
}

