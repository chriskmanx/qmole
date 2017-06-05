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
#include <string.h>
#include <assert.h>

#include "babl.h"
#include "babl-classes.h"
#include "babl-ids.h"
#include "babl-base.h"

static long
convert_double_float (char *src,
                      char *dst,
                      int   src_pitch,
                      int   dst_pitch,
                      long  n)
{
  while (n--)
    {
      (*(float *) dst) = (*(double *) src);
      dst             += dst_pitch;
      src             += src_pitch;
    }
  return n;
}

static long
convert_float_double (char *src,
                      char *dst,
                      int   src_pitch,
                      int   dst_pitch,
                      long  n)
{
  while (n--)
    {
      (*(double *) dst) = (*(float *) src);
      dst              += dst_pitch;
      src              += src_pitch;
    }
  return n;
}

void
babl_base_type_float (void)
{
  babl_type_new (
    "float",
    "id", BABL_FLOAT,
    "bits", 32,
    NULL);

  babl_conversion_new (
    babl_type_from_id (BABL_FLOAT),
    babl_type_from_id (BABL_DOUBLE),
    "plane", convert_float_double,
    NULL
  );

  babl_conversion_new (
    babl_type_from_id (BABL_DOUBLE),
    babl_type_from_id (BABL_FLOAT),
    "plane", convert_double_float,
    NULL
  );
}
