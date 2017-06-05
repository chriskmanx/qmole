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
#include "babl-internal.h"

static int OK;

static int
foo (Babl *babl,
     void *user_data)
{
/*  babl_log ("%s", babl->instance.name);*/
  return 0;
}

static Babl *babl_conversion_destination (Babl *babl);

static int
type_sanity (Babl *babl,
             void *user_data)
{
  /* ensure that every type has reference conversions to
   * and from double */
  int      ok, i;
  BablList *list;

  ok = 0;
  list = babl->type.from_list;
  if (list)
    {
      for (i = 0; i < babl_list_size (list); i++)
        {
          if (babl_conversion_destination ((Babl *) list->items[i]) == babl_type_from_id (BABL_DOUBLE))
            {
              ok = 1;
              break;
            }
        }
    }
  if (!ok)
    {
      OK = 0;
      babl_log ("lack of sanity! type '%s' has no conversion to double",
                babl->instance.name);
    }

  return 0;
}


static int
model_sanity (Babl *babl,
              void *user_data)
{
  /* ensure that every type has reference conversions to
   * and from rgba */
  int      ok, i;
  BablList *list;

  ok = 0;
  list = babl->model.from_list;
  if (list)
    {
      for (i = 0; i < babl_list_size (list); i++)
        {
          if (babl_conversion_destination ((Babl *) list->items[i]) == babl_model_from_id (BABL_RGBA))
            {
              ok = 1;
              break;
            }
        }
    }
  if (!ok)
    {
      OK = 0;
      babl_log ("lack of sanity! model '%s' has no conversion to 'rgba'",
                babl->instance.name);
    }

  return 0;
}

static int
id_sanity (Babl *babl,
           void *user_data)
{
  if (0 && 0 == babl->instance.id &&
      babl->instance.creator &&
      !strcmp (BABL (babl->instance.creator)->instance.name, "BablBase"))
    {
      OK = 0;
      babl_log ("%s\t'%s' has id==0",
                babl_class_name (babl->class_type), babl->instance.name);
    }
  return 0;
}

int
babl_sanity (void)
{
  OK=1;

  babl_type_class_for_each         (id_sanity, NULL);
  babl_component_class_for_each    (id_sanity, NULL);
  babl_model_class_for_each        (id_sanity, NULL);
  babl_format_class_for_each       (id_sanity, NULL);

  babl_type_class_for_each         (type_sanity, NULL);
  babl_sampling_class_for_each     (foo, NULL);
  babl_component_class_for_each    (foo, NULL);
  babl_model_class_for_each        (model_sanity, NULL);
  babl_format_class_for_each       (foo, NULL);
  babl_conversion_class_for_each   (foo, NULL);

  return OK;
}

static Babl *babl_conversion_destination (Babl *babl)
{
  return (Babl *)babl->conversion.destination;
}
