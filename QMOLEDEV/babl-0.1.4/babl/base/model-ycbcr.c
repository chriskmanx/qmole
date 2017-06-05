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
#include <math.h>
#include <assert.h>

#include "babl.h"
#include "babl-classes.h"
#include "babl-ids.h"
#include "babl-base.h"

#include "util.h"

static void components (void);
static void models (void);
static void conversions (void);
static void formats (void);

void
babl_base_model_ycbcr (void)
{
  components ();
  models ();
  conversions ();
  formats ();
}

static void
components (void)
{
  babl_component_new (
    "Cb",
    "id", BABL_CB,
    "chroma",
    NULL);

  babl_component_new (
    "Cr",
    "id", BABL_CR,
    "chroma",
    NULL);
}

static void
models (void)
{
  babl_model_new (
    "id", BABL_YCBCR,
    babl_component_from_id (BABL_LUMA),
    babl_component_from_id (BABL_CB),
    babl_component_from_id (BABL_CR),
    NULL);

  babl_model_new (
    "id", BABL_YCBCR_ALPHA,
    babl_component_from_id (BABL_LUMA),
    babl_component_from_id (BABL_CB),
    babl_component_from_id (BABL_CR),
    babl_component_from_id (BABL_ALPHA),
    NULL);
}

static long
rgba_to_ycbcra (char *src,
                char *dst,
                long  n)
{
  while (n--)
    {
      double red   = ((double *) src)[0];
      double green = ((double *) src)[1];
      double blue  = ((double *) src)[2];
      double alpha = ((double *) src)[3];

      double luminance, cb, cr;

      red   = linear_to_gamma_2_2 (red);
      green = linear_to_gamma_2_2 (green);
      blue  = linear_to_gamma_2_2 (blue);

      luminance = 0.299 * red + 0.587 * green + 0.114 * blue;
      cb        = -0.168736 * red - 0.331264 * green + 0.5 * blue;
      cr        = 0.5 * red - 0.418688 * green - 0.081312 * blue;

      ((double *) dst)[0] = luminance;
      ((double *) dst)[1] = cb;
      ((double *) dst)[2] = cr;
      ((double *) dst)[3] = alpha;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 4;
    }
  return n;
}


static long
rgba_to_ycbcr (char *src,
               char *dst,
               long  n)
{
  while (n--)
    {
      double red   = ((double *) src)[0];
      double green = ((double *) src)[1];
      double blue  = ((double *) src)[2];

      double luminance, cb, cr;

      red   = linear_to_gamma_2_2 (red);
      green = linear_to_gamma_2_2 (green);
      blue  = linear_to_gamma_2_2 (blue);

      luminance = 0.299 * red + 0.587 * green + 0.114 * blue;
      cb        = -0.168736 * red - 0.331264 * green + 0.5 * blue;
      cr        = 0.5 * red - 0.418688 * green - 0.081312 * blue;

      ((double *) dst)[0] = luminance;
      ((double *) dst)[1] = cb;
      ((double *) dst)[2] = cr;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 3;
    }
  return n;
}

static long
ycbcra_to_rgba (char *src,
                char *dst,
                long  n)
{
  while (n--)
    {
      double luminance = ((double *) src)[0];
      double cb        = ((double *) src)[1];
      double cr        = ((double *) src)[2];
      double alpha     = ((double *) src)[3];

      double red, green, blue;

      red   = 1.0 * luminance + 0.0 * cb + 1.40200 * cr;
      green = 1.0 * luminance - 0.344136 * cb - 0.71414136 * cr;
      blue  = 1.0 * luminance + 1.772 * cb + 0.0 * cr;

      red   = gamma_2_2_to_linear (red);
      green = gamma_2_2_to_linear (green);
      blue  = gamma_2_2_to_linear (blue);

      ((double *) dst)[0] = red;
      ((double *) dst)[1] = green;
      ((double *) dst)[2] = blue;
      ((double *) dst)[3] = alpha;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 4;
    }
  return n;
}


static long
ycbcr_to_rgba (char *src,
               char *dst,
               long  n)
{
  while (n--)
    {
      double luminance = ((double *) src)[0];
      double cb        = ((double *) src)[1];
      double cr        = ((double *) src)[2];

      double red, green, blue;

      red   = 1.0 * luminance + 0.0 * cb + 1.40200 * cr;
      green = 1.0 * luminance - 0.344136 * cb - 0.71414136 * cr;
      blue  = 1.0 * luminance + 1.772 * cb + 0.0 * cr;

      red   = gamma_2_2_to_linear (red);
      green = gamma_2_2_to_linear (green);
      blue  = gamma_2_2_to_linear (blue);

      ((double *) dst)[0] = red;
      ((double *) dst)[1] = green;
      ((double *) dst)[2] = blue;
      ((double *) dst)[3] = 1.0;

      src += sizeof (double) * 3;
      dst += sizeof (double) * 4;
    }
  return n;
}

static void
conversions (void)
{
  babl_conversion_new (
    babl_model_from_id (BABL_RGBA),
    babl_model_from_id (BABL_YCBCR),
    "linear", rgba_to_ycbcr,
    NULL
  );
  babl_conversion_new (
    babl_model_from_id (BABL_YCBCR),
    babl_model_from_id (BABL_RGBA),
    "linear", ycbcr_to_rgba,
    NULL
  );
  babl_conversion_new (
    babl_model_from_id (BABL_RGBA),
    babl_model_from_id (BABL_YCBCR_ALPHA),
    "linear", rgba_to_ycbcra,
    NULL
  );
  babl_conversion_new (
    babl_model_from_id (BABL_YCBCR_ALPHA),
    babl_model_from_id (BABL_RGBA),
    "linear", ycbcra_to_rgba,
    NULL
  );
}

static void
formats (void)
{
  babl_format_new (
    "name", "Y'CbCr u8",
    "planar",
    babl_model_from_id (BABL_YCBCR),
    babl_type_from_id (BABL_U8_LUMA),
    babl_sampling (1, 1),
    babl_component_from_id (BABL_LUMA),
    babl_type_from_id (BABL_U8_CHROMA),
    babl_sampling (2, 2),
    babl_component_from_id (BABL_CB),
    babl_sampling (2, 2),
    babl_component_from_id (BABL_CR),
    NULL);
  return;

  babl_format_new (
    "name", "y'cbcr420",
    "id", BABL_YCBCR420,
    "planar",
    babl_model_from_id (BABL_YCBCR),
    babl_type_from_id (BABL_U8_LUMA),
    babl_sampling (1, 1),
    babl_component_from_id (BABL_LUMA),
    babl_type_from_id (BABL_U8_CHROMA),
    babl_sampling (2, 2),
    babl_component_from_id (BABL_CB),
    babl_sampling (2, 2),
    babl_component_from_id (BABL_CR),
    NULL);


  babl_format_new (
    "name", "y'cbcr422",
    "id", BABL_YCBCR422,
    "planar",
    babl_model_from_id (BABL_YCBCR),
    babl_type_from_id (BABL_U8_LUMA),
    babl_sampling (1, 1),
    babl_component_from_id (BABL_LUMA),
    babl_type_from_id (BABL_U8_CHROMA),
    babl_sampling (2, 1),
    babl_component_from_id (BABL_CB),
    babl_sampling (2, 1),
    babl_component_from_id (BABL_CR),
    NULL);

  babl_format_new (
    "name", "y'cbcr411",
    "id", BABL_YCBCR411,
    "planar",
    babl_model_from_id (BABL_YCBCR),
    babl_type_from_id (BABL_U8_LUMA),
    babl_sampling (1, 1),
    babl_component_from_id (BABL_LUMA),
    babl_type_from_id (BABL_U8_CHROMA),
    babl_sampling (4, 1),
    babl_component_from_id (BABL_CB),
    babl_sampling (4, 1),
    babl_component_from_id (BABL_CR),
    NULL);
}
