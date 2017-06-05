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

#define HORIZONTAL_MIN    1
#define HORIZONTAL_MAX    4
#define VERTICAL_MIN      1
#define VERTICAL_MAX      4

#include "config.h"
#include "babl-internal.h"

static BablSampling sampling_db[(HORIZONTAL_MAX - HORIZONTAL_MIN + 1) *
                                (VERTICAL_MAX - VERTICAL_MIN + 1)];

Babl *
babl_sampling (int horizontal,
               int vertical)
{
  if (vertical >= 1 &&
      vertical <= 4 &&
      horizontal >= 1 &&
      horizontal <= 4)
    return (Babl *) &sampling_db [ (vertical - 1) * 4 + (horizontal - 1)];
  else
    babl_log ("babl_samping(%i,%i): arguments out of bounds",
              horizontal, vertical);
  return NULL;
}

void
babl_sampling_class_for_each (BablEachFunction each_fun,
                              void            *user_data)
{
  int horizontal;
  int vertical;

  for (horizontal = HORIZONTAL_MIN; horizontal <= HORIZONTAL_MAX; horizontal++)
    for (vertical = VERTICAL_MIN; vertical <= VERTICAL_MAX; vertical++)
      {
        int index = (vertical - VERTICAL_MIN) * VERTICAL_MAX + (horizontal - HORIZONTAL_MIN);
        if (each_fun (BABL (&sampling_db[index]), user_data))
          return;
      }
}

void
babl_sampling_class_init (void)
{
  int horizontal;
  int vertical;

  for (horizontal = HORIZONTAL_MIN; horizontal <= HORIZONTAL_MAX; horizontal++)
    for (vertical = VERTICAL_MIN; vertical <= VERTICAL_MAX; vertical++)
      {
        int index = (vertical - VERTICAL_MIN) * VERTICAL_MAX + (horizontal - HORIZONTAL_MIN);
        sampling_db[index].instance.class_type = BABL_SAMPLING;
        sampling_db[index].instance.id         = 0;
        sampling_db[index].horizontal          = horizontal;
        sampling_db[index].vertical            = vertical;
        sampling_db[index].instance.name       = sampling_db[index].name;
        sampling_db[index].name[0]             = '0' + horizontal;
        sampling_db[index].name[1]             = ':';
        sampling_db[index].name[2]             = '0' + vertical;
        sampling_db[index].name[3]             = '\0';
      }
}
