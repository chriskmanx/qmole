/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005, Øyvind Kolås.
 *
 * Optimized 8bit conversion routines as used by legacy GIMP code.
 * Copyright (C) 2008  Sven Neumann
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
#include <stdio.h>

#include "babl.h"

#include "base/util.h"
#include "extensions/util.h"

#define INLINE    inline


/* lookup tables used in conversion */

static float lut_linear[1 << 8];
static float lut_gamma_2_2[1 << 8];


static void
tables_init (void)
{
  int i;

  /* fill tables for conversion from 8 bit integer to float */
  for (i = 0; i < 1 << 8; i++)
    {
      double value = i / 255.0;

      lut_linear[i]    = value;
      lut_gamma_2_2[i] = gamma_2_2_to_linear (value);
    }
}

static INLINE long
u8_linear_to_float_linear (unsigned char *src,
                           unsigned char *dst,
                           long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    *d++ = lut_linear[*src++];

  return samples;
}


static INLINE long
u8_linear_to_float_linear_premul (unsigned char *src,
                                  unsigned char *dst,
                                  long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      float alpha = lut_linear[src[3]];
      d[0] = lut_linear[src[0]] * alpha;
      d[1] = lut_linear[src[1]] * alpha;
      d[2] = lut_linear[src[2]] * alpha;
      d[3] = alpha;
      src += 4;
      d += 4;
    }
  return samples;
}

static INLINE long
u8_gamma_2_2_to_float_linear (unsigned char *src,
                              unsigned char *dst,
                              long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    *d++ = lut_gamma_2_2[*src++];

  return samples;
}

static INLINE long
conv_rgba8_linear_rgbaF_linear (unsigned char *src,
                                unsigned char *dst,
                                long           samples)
{
  u8_linear_to_float_linear (src, dst, samples * 4);

  return samples;
}

static INLINE long
conv_rgba8_linear_ragabaaF_linear (unsigned char *src,
                                unsigned char *dst,
                                long           samples)
{
  u8_linear_to_float_linear_premul (src, dst, samples);

  return samples;
}


static INLINE long
conv_rgba8_gamma_2_2_rgbaF_linear (unsigned char *src,
                                   unsigned char *dst,
                                   long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      *d++ = lut_gamma_2_2[*src++];
      *d++ = lut_gamma_2_2[*src++];
      *d++ = lut_gamma_2_2[*src++];
      *d++ = lut_linear[*src++];
    }

  return samples;
}

static INLINE long
conv_rgb8_linear_rgbF_linear (unsigned char *src,
                              unsigned char *dst,
                              long           samples)
{
  u8_linear_to_float_linear (src, dst, samples * 3);

  return samples;
}

static INLINE long
conv_rgb8_gamma_2_2_rgbF_linear (unsigned char *src,
                                 unsigned char *dst,
                                 long           samples)
{
  u8_gamma_2_2_to_float_linear (src, dst, samples * 3);

  return samples;
}

static INLINE long
conv_rgb8_linear_rgbaF_linear (unsigned char *src,
                               unsigned char *dst,
                               long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      *d++ = lut_linear[*src++];
      *d++ = lut_linear[*src++];
      *d++ = lut_linear[*src++];
      *d++ = 1.0;
    }

  return samples;
}

#define conv_rgb8_linear_ragabaaF_linear conv_rgb8_linear_rgbaF_linear

static INLINE long
conv_rgb8_gamma_2_2_rgbaF_linear (unsigned char *src,
                                  unsigned char *dst,
                                  long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      *d++ = lut_gamma_2_2[*src++];
      *d++ = lut_gamma_2_2[*src++];
      *d++ = lut_gamma_2_2[*src++];
      *d++ = 1.0;
    }

  return samples;
}

static INLINE long
conv_ga8_linear_gaF_linear (unsigned char *src,
                            unsigned char *dst,
                            long           samples)
{
  u8_linear_to_float_linear (src, dst, samples * 2);

  return samples;
}

static INLINE long
conv_ga8_gamma_2_2_gaF_linear (unsigned char *src,
                               unsigned char *dst,
                               long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      *d++ = lut_gamma_2_2[*src++];
      *d++ = lut_linear[*src++];
    }

  return samples;
}

static INLINE long
conv_ga8_linear_rgbaF_linear (unsigned char *src,
                              unsigned char *dst,
                              long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      float value = lut_linear[*src++];

      *d++ = value;
      *d++ = value;
      *d++ = value;
      *d++ = lut_linear[*src++];
    }

  return samples;
}

static INLINE long
conv_ga8_gamma_2_2_rgbaF_linear (unsigned char *src,
                                 unsigned char *dst,
                                 long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      float value = lut_gamma_2_2[*src++];

      *d++ = value;
      *d++ = value;
      *d++ = value;
      *d++ = lut_linear[*src++];
    }

  return samples;
}

static INLINE long
conv_g8_linear_gF_linear (unsigned char *src,
                          unsigned char *dst,
                          long           samples)
{
  u8_linear_to_float_linear (src, dst, samples);

  return samples;
}

static INLINE long
conv_g8_gamma_2_2_gF_linear (unsigned char *src,
                             unsigned char *dst,
                             long           samples)
{
  u8_gamma_2_2_to_float_linear (src, dst, samples);

  return samples;
}

static INLINE long
conv_g8_linear_rgbaF_linear (unsigned char *src,
                             unsigned char *dst,
                             long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      float value = lut_linear[*src++];

      *d++ = value;
      *d++ = value;
      *d++ = value;
      *d++ = 1.0;
    }

  return samples;
}
static INLINE long
conv_g8_gamma_2_2_rgbaF_linear (unsigned char *src,
                                unsigned char *dst,
                                long           samples)
{
  float *d = (float *) dst;
  long   n = samples;

  while (n--)
    {
      float value = lut_gamma_2_2[*src++];

      *d++ = value;
      *d++ = value;
      *d++ = value;
      *d++ = 1.0;
    }

  return samples;
}

static INLINE long
conv_rgbaF_linear_rgb8_linear (unsigned char *src, 
                               unsigned char *dst, 
                               long           samples)
{
  float *fsrc = (float *) src;
  long n = samples;
  long int v;

  while (n--)
    {
      v = rint (*fsrc++ * 255.0);
      *dst++ = (v < 0) ? 0 : ((v > 255) ? 255 : v);

      v = rint (*fsrc++ * 255.0);
      *dst++ = (v < 0) ? 0 : ((v > 255) ? 255 : v);
     
      v = rint (*fsrc++ * 255.0);
      *dst++ = (v < 0) ? 0 : ((v > 255) ? 255 : v);

      fsrc++;
    }

  return samples;
}

static INLINE long
conv_rgbaF_linear_rgba8_linear (unsigned char *src, 
                                unsigned char *dst, 
                                long           samples)
{
  float *fsrc = (float *) src;
  long n = samples;
  long int v;

  while (n--)
    {
      v = rint (*fsrc++ * 255.0);
      *dst++ = (v < 0) ? 0 : ((v > 255) ? 255 : v);

      v = rint (*fsrc++ * 255.0);
      *dst++ = (v < 0) ? 0 : ((v > 255) ? 255 : v);
     
      v = rint (*fsrc++ * 255.0);
      *dst++ = (v < 0) ? 0 : ((v > 255) ? 255 : v);

      v = rint (*fsrc++ * 255.0);
      *dst++ = (v < 0) ? 0 : ((v > 255) ? 255 : v);
    }

  return samples;
}

int init (void);

int
init (void)
{
  Babl *ragabaaF_linear = babl_format_new (
    babl_model ("RaGaBaA"),
    babl_type ("float"),
    babl_component ("Ra"),
    babl_component ("Ga"),
    babl_component ("Ba"),
    babl_component ("A"),
    NULL);
  Babl *rgbaF_linear = babl_format_new (
    babl_model ("RGBA"),
    babl_type ("float"),
    babl_component ("R"),
    babl_component ("G"),
    babl_component ("B"),
    babl_component ("A"),
    NULL);
  Babl *rgba8_linear = babl_format_new (
    babl_model ("RGBA"),
    babl_type ("u8"),
    babl_component ("R"),
    babl_component ("G"),
    babl_component ("B"),
    babl_component ("A"),
    NULL);
  Babl *rgba8_gamma_2_2 = babl_format_new (
    babl_model ("R'G'B'A"),
    babl_type ("u8"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    babl_component ("A"),
    NULL);
  Babl *rgbF_linear = babl_format_new (
    babl_model ("RGB"),
    babl_type ("float"),
    babl_component ("R"),
    babl_component ("G"),
    babl_component ("B"),
    NULL);
  Babl *rgb8_linear = babl_format_new (
    babl_model ("RGB"),
    babl_type ("u8"),
    babl_component ("R"),
    babl_component ("G"),
    babl_component ("B"),
    NULL);
  Babl *rgb8_gamma_2_2 = babl_format_new (
    babl_model ("R'G'B'"),
    babl_type ("u8"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    NULL);
  Babl *gaF_linear = babl_format_new (
    babl_model ("YA"),
    babl_type ("float"),
    babl_component ("Y"),
    babl_component ("A"),
    NULL);
  Babl *ga8_linear = babl_format_new (
    babl_model ("YA"),
    babl_type ("u8"),
    babl_component ("Y"),
    babl_component ("A"),
    NULL);
  Babl *ga8_gamma_2_2 = babl_format_new (
    babl_model ("Y'A"),
    babl_type ("u8"),
    babl_component ("Y'"),
    babl_component ("A"),
    NULL);
  Babl *gF_linear = babl_format_new (
    babl_model ("Y"),
    babl_type ("float"),
    babl_component ("Y"),
    NULL);
  Babl *g8_linear = babl_format_new (
    babl_model ("Y"),
    babl_type ("u8"),
    babl_component ("Y"),
    NULL);
  Babl *g8_gamma_2_2 = babl_format_new (
    babl_model ("Y'"),
    babl_type ("u8"),
    babl_component ("Y'"),
    NULL);

  tables_init ();

#define o(src, dst) \
  babl_conversion_new (src, dst, "linear", conv_ ## src ## _ ## dst, NULL)

  o (rgba8_linear, ragabaaF_linear);
  o (rgba8_linear, rgbaF_linear);
  o (rgba8_gamma_2_2, rgbaF_linear);

  o (rgb8_linear, rgbF_linear);
  o (rgb8_gamma_2_2, rgbF_linear);
  o (rgb8_linear, rgbaF_linear);
  o (rgb8_linear, ragabaaF_linear);
  o (rgb8_gamma_2_2, rgbaF_linear);

  o (ga8_linear, gaF_linear);
  o (ga8_gamma_2_2, gaF_linear);
  o (ga8_linear, rgbaF_linear);
  o (ga8_gamma_2_2, rgbaF_linear);

  o (g8_linear, gF_linear);
  o (g8_gamma_2_2, gF_linear);
  o (g8_linear, rgbaF_linear);
  o (g8_gamma_2_2, rgbaF_linear);

  o (rgbaF_linear, rgb8_linear);
  o (rgbaF_linear, rgba8_linear);

  return 0;
}
