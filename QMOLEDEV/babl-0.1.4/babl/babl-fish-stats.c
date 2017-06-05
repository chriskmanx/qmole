/* babl - dynamically extendable universal pixel fish library.
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
 */

#include "config.h"
#include "babl-internal.h"

static FILE *output_file = NULL;
static int   qux         = 0;

static char *utf8_bar[] = { " ", "·", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█" };
/*
static char *utf8_bar[]=  {"!","▁","▃","▅","▇","█","!","!","!"};
static char *utf8_bar[]={"·", "█", "▇", "▆", "▅", "▄", "▃", "▂", "▁", };
static char *utf8_bar[]={" ","1","2","3","4","5","6","7","8"};
*/

static int
table_destination_each (Babl *babl,
                        void *userdata)
{
  Babl *source      = userdata;
  Babl *destination = babl;

  qux++;
  if (qux % babl_formats_count () == qux / babl_formats_count ())
    fprintf (output_file, "<td class='cell'>&nbsp;</td>");
  else
    {
      Babl *fish = babl_fish (source, destination);

      babl_assert (fish);


      switch (fish->class_type)
        {
          case BABL_FISH_PATH:

            fprintf (output_file, "<td class='cell'%s><a href='javascript:o()'>%s",
                     fish->fish.processings > 0 ? " style='background-color: #69f'" : "",
                     utf8_bar[babl_list_size (fish->fish_path.conversion_list)]);

            {
              int i;
              fprintf (output_file, "<div class='tooltip'>");
              fprintf (output_file, "<h3><span class='g'>path</span> %s <span class='g'>to</span> %s</h3>", source->instance.name, destination->instance.name);
              if (fish->fish.processings > 0)
                {
                  fprintf (output_file, "<span class='g'>Processings:</span>%i<br/>", fish->fish.processings);
                  fprintf (output_file, "<span class='g'>Pixels:</span>%li<br/>", fish->fish.pixels);
                }
              fprintf (output_file, "<table>\n");

              fprintf (output_file, "<tr>");
              fprintf (output_file, "<td><em>conversion</em></td>");
              fprintf (output_file, "<td style='text-align:right'><em>cost</em></td>");
              fprintf (output_file, "<td style='text-align:right'><em>error</em></td>");
              fprintf (output_file, "</tr>");

              for (i = 0; i < babl_list_size (fish->fish_path.conversion_list); i++)
                {
                  fprintf (output_file, "<tr>");
                  fprintf (output_file, "<td>%s</td>", BABL (fish->fish_path.conversion_list->items[i])->instance.name);
                  fprintf (output_file, "<td class='r'>%li</td>",
                    babl_conversion_cost (&BABL (fish->fish_path.conversion_list->items[i])->conversion));
                  fprintf (output_file, "<td class='r'>%e</td>",
                    babl_conversion_error (&BABL (fish->fish_path.conversion_list->items[i])->conversion));
                  fprintf (output_file, "</tr>");
                }

              fprintf (output_file, "<tr>");
              fprintf (output_file, "<td><em>total</em></td>");
              fprintf (output_file, "<td class='r'><em>%3.0f</em></td>", fish->fish_path.cost);
              fprintf (output_file, "<td class='r'><em>%e</em></td>", fish->fish.error);
              fprintf (output_file, "</tr>");
              fprintf (output_file, "</table>");
              fprintf (output_file, "</div>");
            }
            fprintf (output_file, "</a></td>\n");
            break;

          case BABL_FISH_REFERENCE:
            fprintf (output_file, "<td class='cell'%s><a href='javascript:o()'>&nbsp",
                     fish->fish.processings > 0 ? " style='background-color: #f99'" : "");
            fprintf (output_file, "<div class='tooltip'>");
            fprintf (output_file, "<h3><span class='g'>Reference</span> %s <span class='g'>to</span> %s</h3>", source->instance.name, destination->instance.name);

            if (fish->fish.processings > 0)
              {
                fprintf (output_file, "<span class='g'>Processings:</span>%i<br/>", fish->fish.processings);
                fprintf (output_file, "<span class='g'>Pixels:</span>%li<br/>", fish->fish.pixels);
              }
            fprintf (output_file, "</div>");
            fprintf (output_file, "</a></td>\n");
            break;

          case BABL_FISH_SIMPLE:
            fprintf (output_file, "<td class='cell'%s><a href='javascript:o()'>&middot;",
                     fish->fish.processings > 1 ? " style='background-color: #69f'" : "");
            fprintf (output_file, "<div class='tooltip'>");
            fprintf (output_file, "<h3><span class='g'>Simple</span> %s <span class='g'>to</span> %s</h3>", source->instance.name, destination->instance.name);


            fprintf (output_file, "%s<br/>", BABL (fish->fish_simple.conversion)->instance.name);
            fprintf (output_file, "<span class='g'>cost:</span> %li<br/>", babl_conversion_cost ((fish->fish_simple.conversion)));
            fprintf (output_file, "<span class='g'>error:</span> %e<br/>", babl_conversion_error ((fish->fish_simple.conversion)));

            if (fish->fish.processings > 0)
              {
                fprintf (output_file, "<span class='g'>Processings:</span>%i<br/>", fish->fish.processings);
                fprintf (output_file, "<span class='g'>Pixels:</span>%li<br/>", fish->fish.pixels);
              }
            fprintf (output_file, "</div>");
            fprintf (output_file, "</a></td>\n");
            break;

          default:
            babl_fatal ("Unknown fish type");
            break;
        }
    }
  return 0;
}

static int source_no = 0;

static int
table_source_each (Babl *babl,
                   void *userdata)
{
  char        expanded_name[512];
  const char *s;
  char       *d;

  s = babl->instance.name;
  d = &expanded_name[0];

  while (*s)
    {
      switch (*s)
        {
          case ' ':
            *(d++) = '&';
            *(d++) = 'n';
            *(d++) = 'b';
            *(d++) = 's';
            *(d++) = 'p';
            *(d++) = ';';
            *(d)   = '\0';
            s++;
            break;

          default:
            *(d++) = *(s++);
            *(d)   = '\0';
            break;
        }
    }

  fprintf (output_file, "<tr>");
  fprintf (output_file, "<td class='format_name'><a href='javascript:o();'>%s", expanded_name);
  {
    int i;

    fprintf (output_file, "<div class='tooltip' id='format_%p'>", babl);
    fprintf (output_file, "<h3>%s</h3>", babl->instance.name);

    fprintf (output_file, "<dl>");
    fprintf (output_file, "<dt>bytes/pixel</dt><dd>%i</dd>", babl->format.bytes_per_pixel);
    fprintf (output_file, "<dt>model</dt><dd>%s</dd>", BABL (babl->format.model)->instance.name);
    fprintf (output_file, "<dt>loss</dt><dd>%f</dd>", babl_format_loss (babl));
    fprintf (output_file, "<dt>components</dt><dd><table class='nopad'>");

    for (i = 0; i < babl->format.components; i++)
      {
        fprintf (output_file, "<tr><td class='type'>%s</td><td class='component'>%s</td></tr>",
                 BABL (babl->format.type[i])->instance.name,
                 BABL (babl->format.component[i])->instance.name);
      }
    fprintf (output_file, "</table></dd></dl>");

    fprintf (output_file, "</div>\n");
  }

  fprintf (output_file, "</a></td>");
  babl_format_class_for_each (table_destination_each, babl);
  fprintf (output_file, "</tr>\n");
  source_no++;
  return 0;
}

/* copied from babl-fish-path.c */
#define BABL_LEGAL_ERROR    0.000001
static double legal_error (void)
{
  static double error = 0.0;
  const char   *env;

  if (error != 0.0)
    return error;

  env = getenv ("BABL_TOLERANCE");
  if (env && env[0] != '\0')
    error = atof (env);
  else
    error = BABL_LEGAL_ERROR;
  return error;
}

static int
each_conv (Babl *babl,
           void *data)
{
  double error, cost;

  if (BABL (babl->conversion.source)->class_type != BABL_FORMAT)
    return 0;

  error = babl_conversion_error (&babl->conversion);
  cost  = babl_conversion_cost (&babl->conversion);

  if (error > legal_error ())
    {
      fprintf (output_file, "<dt style='background-color: #fcc;'>%s</dt>", babl->instance.name);
      fprintf (output_file, "<dd style='background-color: #fcc;'>");
    }
  else
    {
      fprintf (output_file, "<dt>%s</dt><dd>", babl->instance.name);
    }
  fprintf (output_file, "<em>error:</em> %f <em>cost:</em> %4.0f <em>processings:</em> %i <em>pixels:</em> %li", error, cost,
           babl->conversion.processings, babl->conversion.pixels);
  fprintf (output_file, "</dd>");

  return 0;
}

static void
conversions (void)
{
  fprintf (output_file, "<h2>Conversions</h2><dl>\n");
  babl_conversion_class_for_each (each_conv, NULL);
  fprintf (output_file, "</dl>\n");
}



void
babl_fish_stats (FILE *file)
{
  output_file = file;

  fprintf (output_file,
           "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
           "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
           "<html>\n"
           "<head>\n"
           "<title>BablFishPath introspection</title>\n"

           "<style type='text/css'>"
           " body {"
           "   font-family: sans;"
           "   margin-left: 1em;"
           " }"
           " .cell {"
           "  overflow : none;"
           "  height: 1em;"
           "  font-family: monospace;"
           "  border: 1px solid #eee;"
           "  padding: 0;"
           "  margin : 0;"
           "}"
           ".cell>a {"
           "    text-decoration: none;"
           "    color: black;"
           "    cursor: help;"
           "}"
           "div.tooltip {"
           "   border: 0.2em solid black;"
           "   padding-top: 1em;"
           "   padding-right: 2em;"
           "   display: none;"
           "   padding-left: 2em;"
           "   padding-bottom: 3em;"
           "   background-color: white;"
           "   background-repeat: no-repeat;"
           "   background-image: url(graphics/babl-48x48.png);"
           "   background-position: bottom right;"
           "   color: black;"
           "}"
           " .cell>a:hover {"
           "  background-color: black;"
           "  color: white;"
           "}"
           " .format_name {"
           "  height: 1em;"
           "  background-color: #eee;"
           "  padding-right: 0.5em;"
           "  padding-left:  0.5em;"
           "  border-bottom: 1px solid #fff;"
           "}"
           " .format_name>a {"
           "  text-decoration: none;"
           "  color: blue;"
           "    cursor: help;"
           " }"
           " .format_name>a:hover {"
           "  background-color: blue;"
           "  color: white;"
           " }"

           "a:hover>div.tooltip {"
           "   display: block;"
           "   position: fixed;"
           "   bottom: 0;"
           "   right: 0;"
           "}"

           "td.component {"
           "  background-color: #060;"
           "  padding-left: 0.5em;"
           "  padding-top: 0.1em;"
           "  padding-bottom: 0.1em;"
           "  overflow: hidden;"
           "  width: 4em;"
           "  color: white;"
           "  border: 1px solid white;"
           "}"
           "td.type {"
           "  background-color: #006;"
           "  padding-left: 0.5em;"
           "  padding-top: 0.1em;"
           "  padding-bottom: 0.1em;"
           "  overflow: hidden;"
           "  width: 4em;"
           "  color: white;"
           "  border: 1px solid white;"
           "}"
           ".g {"
           "  color: gray;"
           "}"
           ".r {"
           "  text-align: right;"
           "}"

           "</style>"

           "<script type='text/javascript'>"
           "var tick_count=0;"
           "function o ()"
           "{"
           "   tick_count++;"
           "   if (tick_count == 11)"
           "        alert(\"«The mind is it's own place,\\nand in itself can make a heaven of hell;\\na hell of heaven.»\\n--Milton\");"
           "   else if (tick_count == 42)"
           "        alert(\"«So long and thanks for all the fish.»\\n--Adams\");"
           "}"
           "</script>"


           "</head>\n");

  fprintf (output_file, "<body>\n");

  fprintf (output_file, "<h1>BablFishPath introspection</h1>");
  fprintf (output_file, "<p>Instrumentation and pathlengths.</p>");

  fprintf (output_file, "<table cellspacing='0'><tr><td>Source format</td><td colspan='32'>Destination formats</td></tr>\n");

  babl_format_class_for_each (table_source_each, NULL);

  fprintf (output_file, "</table>");

  fprintf (output_file, "<div style='height:20em'></div>\n");

  conversions ();

  fprintf (output_file, "</body></html>\n");
}

