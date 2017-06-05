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

#define PIXELS    5

float grayscale_buf [PIXELS] = { -0.1, 0.0, 0.4, 1.0, 2.0 };

float rgb_buf_ref [PIXELS * 3] =
{ -0.1, -0.1, -0.1, 0.0, 0.0, 0.0, 0.4, 0.4, 0.4, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0 };

float rgb_buf     [PIXELS * 3];

static int
test (void)
{
  Babl *fish;
  int   i;
  int   OK = 1;


  fish = babl_fish (
    babl_format_new (
      babl_model ("Y"),
      babl_type ("float"),
      babl_component ("Y"),
      NULL
    ),
    babl_format_new (
      babl_model ("RGB"),
      babl_type ("float"),
      babl_component ("R"),
      babl_component ("G"),
      babl_component ("B"),
      NULL
    )
         );

  babl_process (fish,
                grayscale_buf, rgb_buf,
                PIXELS);

  for (i = 0; i < PIXELS * 3; i++)
    {
      if (rgb_buf[i] != rgb_buf_ref[i])
        {
          babl_log ("index %i is problematic", i);
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



