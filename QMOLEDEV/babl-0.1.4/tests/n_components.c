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

#define PIXELS       7
#define COMPONENTS   2048
#define TOLERANCE    0

float source_buf [PIXELS * COMPONENTS] =
{
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
   0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 
  /* the rest of the input buffer is nulls */
}; 

unsigned char reference_buf [PIXELS * COMPONENTS] =
{ 
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  26, 51, 77, 102, 128, 153, 26, 51, 77, 102, 128, 153,
  /* the rest of the reference buffer is nulls */
};

unsigned char destination_buf [PIXELS * COMPONENTS];

static int
test (void)
{
  int   components;
  int   OK = 1;

  for (components = 1; components < 2048; components ++)
  {
    Babl *fish;
    Babl *src_fmt;
    Babl *dst_fmt;
    int   i;

    src_fmt = babl_format_n (babl_type ("float"), components);
    dst_fmt = babl_format_n (babl_type ("u8"), components);

    fish = babl_fish (src_fmt, dst_fmt);

    babl_process (fish, source_buf, destination_buf, PIXELS);

    for (i = 0; i < PIXELS * components; i++)
      {
        if (abs (destination_buf[i] - reference_buf[i]) > TOLERANCE)
          {
            babl_log ("%i-components, pixel %i component %i is %i should be %i",
                      components, i / components, i % components, destination_buf[i], reference_buf[i]);
            OK = 0;
          }
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
