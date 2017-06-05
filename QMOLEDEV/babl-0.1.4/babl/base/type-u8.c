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
#include <stdint.h>

#include "babl-internal.h"
#include "babl-base.h"

#include <math.h>
static inline long
convert_double_u8_scaled (double        min_val,
                          double        max_val,
                          unsigned char min,
                          unsigned char max,
                          char         *src,
                          char         *dst,
                          int           src_pitch,
                          int           dst_pitch,
                          long          n)
{
  while (n--)
    {
      double        dval = *(double *) src;
      unsigned char u8val;

      if (dval < min_val)
        u8val = min;
      else if (dval > max_val)
        u8val = max;
      else
        u8val = rint ((dval - min_val) / (max_val - min_val) * (max - min) + min);

      *(unsigned char *) dst = u8val;
      src                   += src_pitch;
      dst                   += dst_pitch;
    }
  return n;
}

static inline long
convert_u8_double_scaled (double        min_val,
                          double        max_val,
                          unsigned char min,
                          unsigned char max,
                          char         *src,
                          char         *dst,
                          int           src_pitch,
                          int           dst_pitch,
                          long          n)
{
  while (n--)
    {
      int    u8val = *(unsigned char *) src;
      double dval;

      if (u8val < min)
        dval = min_val;
      else if (u8val > max)
        dval = max_val;
      else
        dval = (u8val - min) / (double) (max - min) * (max_val - min_val) + min_val;

      (*(double *) dst) = dval;

      dst += dst_pitch;
      src += src_pitch;
    }
  return n;
}

#define MAKE_CONVERSIONS(name, min_val, max_val, min, max)      \
  static long \
  convert_ ## name ## _double (void *src, \
                               void *dst, \
                               int src_pitch, \
                               int dst_pitch, \
                               long n)                               \
  { \
    return convert_u8_double_scaled (min_val, max_val, min, max, \
                                     src, dst, src_pitch, dst_pitch, n); \
  }                                                               \
  static long \
  convert_double_ ## name (void *src, \
                           void *dst, \
                           int src_pitch, \
                           int dst_pitch, \
                           long n)                                 \
  { \
    return convert_double_u8_scaled (min_val, max_val, min, max, \
                                     src, dst, src_pitch, dst_pitch, n); \
  }

MAKE_CONVERSIONS (u8, 0.0, 1.0, 0x00, UINT8_MAX)
MAKE_CONVERSIONS (u8_luma, 0.0, 1.0, 16, 235)
MAKE_CONVERSIONS (u8_chroma, 0.0, 1.0, 16, 240)

void
babl_base_type_u8 (void)
{
  babl_type_new (
    "u8",
    "id", BABL_U8,
    "bits", 8,
    NULL);

  babl_type_new (
    "u8-luma",
    "id", BABL_U8_LUMA,
    "bits", 8,
    NULL
  );

  babl_type_new (
    "u8-chroma",
    "id", BABL_U8_CHROMA,
    "integer",
    "unsigned",
    "bits", 8,
    "min", (long) 16,
    "max", (long) 240,
    "min_val", -0.5,
    "max_val", 0.5,
    NULL
  );
  babl_conversion_new (
    babl_type_from_id (BABL_U8),
    babl_type_from_id (BABL_DOUBLE),
    "plane", convert_u8_double,
    NULL
  );
  babl_conversion_new (
    babl_type_from_id (BABL_DOUBLE),
    babl_type_from_id (BABL_U8),
    "plane", convert_double_u8,
    NULL
  );
  babl_conversion_new (
    babl_type_from_id (BABL_U8_LUMA),
    babl_type_from_id (BABL_DOUBLE),
    "plane", convert_u8_luma_double,
    NULL
  );
  babl_conversion_new (
    babl_type_from_id (BABL_DOUBLE),
    babl_type_from_id (BABL_U8_LUMA),
    "plane", convert_double_u8_luma,
    NULL
  );
  babl_conversion_new (
    babl_type_from_id (BABL_U8_CHROMA),
    babl_type_from_id (BABL_DOUBLE),
    "plane", convert_u8_chroma_double,
    NULL
  );
  babl_conversion_new (
    babl_type_from_id (BABL_DOUBLE),
    babl_type_from_id (BABL_U8_CHROMA),
    "plane", convert_double_u8_chroma,
    NULL
  );
}
