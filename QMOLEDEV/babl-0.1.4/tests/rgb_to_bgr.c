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
#include <math.h>
#include "babl-internal.h"

#define PIXELS       3
#define TOLERANCE    0

unsigned char source_buf [PIXELS * 3] =
{ 10,   20, 30,
  30, 30, 30,
  40, 50, 60 };

unsigned char reference_buf [PIXELS * 3] =
{ 30,   20, 10,
  30, 30, 30,
  60, 50, 40 };

unsigned char destination_buf [PIXELS * 3];

static int
test (void)
{
  Babl *fish;
  int   i;
  int   OK = 1;

  fish = babl_fish (
    babl_format_new (
      babl_model ("RGB"),
      babl_type ("u8"),
      babl_component ("R"),
      babl_component ("G"),
      babl_component ("B"),
      NULL
    ),
    babl_format_new (
      babl_model ("RGB"),
      babl_type ("u8"),
      babl_component ("B"),
      babl_component ("G"),
      babl_component ("R"),
      NULL
    )
         );

  babl_process (fish, source_buf, destination_buf, PIXELS);

  for (i = 0; i < PIXELS * 3; i++)
    {
      if (abs (destination_buf[i] - reference_buf[i]) > TOLERANCE)
        {
          babl_log ("%2i (%2i%%3=%i, %2i/3=%i) is %i should be %i",
                    i, i, i % 3, i, i / 3, destination_buf[i], reference_buf[i]);
          OK = 0;
        }
    }
  if (!OK)
    return -1;
  return 0;
}

int
main (int    argc,
      char **argv)
{
  babl_init ();
  if (test ())
    return -1;
  babl_exit ();
  return 0;
}
