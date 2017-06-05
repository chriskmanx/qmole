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

static char *
create_name (BablConversion *conversion)
{
  return conversion->instance.name;
}

Babl *
babl_fish_simple (BablConversion *conversion)
{
  Babl *babl = NULL;
  char *name;

  babl_assert (BABL_IS_BABL (conversion));

  name = create_name (conversion);
  babl = babl_db_exist_by_name (babl_fish_db (), name);
  if (babl)
    {
      /* There is an instance already registered by the required name,
       * returning the preexistent one instead.
       */
      return babl;
    }

  babl = babl_malloc (sizeof (BablFishSimple) +
                      strlen (name) + 1);
  babl->class_type    = BABL_FISH_SIMPLE;
  babl->instance.id   = babl_fish_get_id (conversion->source, conversion->destination);
  babl->instance.name = ((char *) babl) + sizeof (BablFishSimple);
  strcpy (babl->instance.name, name);
  babl->fish.source      = conversion->source;
  babl->fish.destination = conversion->destination;

  babl->fish.processings       = 0;
  babl->fish.pixels            = 0;
  babl->fish_simple.conversion = conversion;
  babl->fish.error             = 0.0;/* babl fish simple should only be used by bablfish
                                   reference, and babl fish reference only requests clean
                                   conversions */

  /* Since there is not an already registered instance by the required
   * name, inserting newly created class into database.
   */
  babl_db_insert (babl_fish_db (), babl);
  return babl;
}
