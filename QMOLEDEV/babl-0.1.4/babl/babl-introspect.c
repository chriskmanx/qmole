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
 */

#include "config.h"
#include "babl-internal.h"    /* for babl_log */

#define BABL_LOG

#ifdef BABL_LOG

static void sampling_introspect (Babl *babl);
static void model_introspect (Babl *babl);
static void type_introspect (Babl *babl);
static void format_introspect (Babl *babl);

static int  each_introspect (Babl *babl,
                             void *user_data);
#endif

void
babl_introspect (Babl *babl)
{
#ifdef BABL_LOG
  Babl *extender_backup = babl_extender ();

  babl_set_extender (babl_extension_quiet_log ());

  if (babl)
    {
      each_introspect (babl, NULL);
      return;
    }
  babl_log ("Introspection report");
  babl_log ("====================================================");

  babl_log ("");
  babl_log ("Data Types:");
  babl_type_class_for_each (each_introspect, NULL);
  babl_log ("");
  babl_log ("Sampling (chroma subsampling) factors:");
  babl_sampling_class_for_each (each_introspect, NULL);
  babl_log ("");
  babl_log ("Components:");
  babl_component_class_for_each (each_introspect, NULL);
  babl_log ("");
  babl_log ("Models (of components):");
  babl_model_class_for_each (each_introspect, NULL);
  babl_log ("");
  babl_log ("Pixel formats:");
  babl_format_class_for_each (each_introspect, NULL);
  babl_log ("");
  babl_log ("conversions:");
  babl_conversion_class_for_each (each_introspect, NULL);
  babl_log ("");
  babl_log ("extensions:");
  babl_extension_class_for_each (each_introspect, NULL);
  babl_log ("");
  babl_log ("fishes");
  babl_fish_class_for_each (each_introspect, NULL);
  babl_log ("");

  babl_set_extender (extender_backup);
#endif
}

#ifdef BABL_LOG

static void
item_conversions_introspect (Babl *babl)
{
  int i;
  BablList *list;

  list = babl->type.from_list;
  if (list)
    {
      babl_log ("\t\tconversions from %s: %i",
                babl->instance.name, babl_list_size (list));

      for (i = 0; i < babl_list_size (list); i++)
        babl_log ("\t\t\t'%s'", BABL (list->items[i])->instance.name);
   }
}

static void
model_introspect (Babl *babl)
{
  int i;

  babl_log ("\t\tcomponents=%i", babl->model.components);

  for (i = 0; i < babl->model.components; i++)
    {
      babl_log ("\t\tindex[%i] = \"%s\"", i,
                BABL (babl->model.component[i])->instance.name);
    }
}

static void
type_introspect (Babl *babl)
{
  babl_log ("\t\tbits=%i", babl->type.bits);
}


static void
sampling_introspect (Babl *babl)
{
  babl_log ("\t\thorizontal = %i",
            babl->sampling.horizontal);
  babl_log ("\t\tvertical   = %i",
            babl->sampling.vertical);
}


static void
format_introspect (Babl *babl)
{
  int i;

  babl_log ("\t\tmodel=\"%s\"", babl->format.model->instance.name);
  babl_log ("\t\tplanar=%i", babl->format.planar);
  babl_log ("\t\tcomponents=%i", babl->format.components);

  for (i = 0; i < babl->format.components; i++)
    {
      babl_log ("\t\tband[%i] type=\"%s\" sampling=\"%s\" component=\"%s\"",
                i, babl->format.type[i]->instance.name,
                babl->format.sampling[i]->instance.name,
                babl->format.component[i]->instance.name);
    }
}

static void
conversion_introspect (Babl *babl)
{
  babl_log ("\t\tprocessings:%i pixels:%li",
            babl->conversion.processings, babl->conversion.pixels);
  if (BABL (babl->conversion.source)->class_type == BABL_FORMAT)
    {
      babl_log ("\t\terror: %f", babl_conversion_error (&babl->conversion));
    }
}

static void
fish_introspect (Babl *babl)
{
  babl_log ("\t\tprocessings:%i pixels:%li",
            babl->fish.processings, babl->fish.pixels);
}

static int
each_introspect (Babl *babl,
                 void *user_data)
{
  babl_log ("\t\"%s\"\t%i\t%s",
            babl->instance.name,
            babl->instance.id,
            babl_class_name (babl->class_type));
  switch (babl->class_type)
    {
      case BABL_TYPE:
        type_introspect (babl);
        item_conversions_introspect (babl);
        break;

      case BABL_COMPONENT:
        break;

      case BABL_MODEL:
        model_introspect (babl);
        item_conversions_introspect (babl);
        break;

      case BABL_FORMAT:
        format_introspect (babl);
        item_conversions_introspect (babl);
        break;

      case BABL_SAMPLING:
        sampling_introspect (babl);
        item_conversions_introspect (babl);
        break;

      case BABL_CONVERSION:
      case BABL_CONVERSION_PLANE:
      case BABL_CONVERSION_PLANAR:
      case BABL_CONVERSION_LINEAR:
        conversion_introspect (babl);
        break;

      case BABL_FISH:
      case BABL_FISH_REFERENCE:
      case BABL_FISH_SIMPLE:
        fish_introspect (babl);
        break;

      default:
        break;
    }
  return 0;
}
#endif
