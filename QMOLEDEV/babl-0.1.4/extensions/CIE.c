/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005, Øyvind Kolås.
 * Copyright (C) 2009, Martin Nordholts
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
#include <string.h>

#include "babl.h"
#include "extensions/util.h"

#define DEGREES_PER_RADIAN (180 / 3.14159265358979323846)
#define RADIANS_PER_DEGREE (1 / DEGREES_PER_RADIAN)

int init (void);

static void types (void);
static void components (void);
static void models (void);
static void conversions (void);
static void formats (void);

int
init (void)
{
  types ();
  components ();
  models ();
  formats ();
  conversions ();
  return 0;
}

static void
components (void)
{
  babl_component_new ("CIE L", NULL);
  babl_component_new ("CIE a", "chroma", NULL);
  babl_component_new ("CIE b", "chroma", NULL);
  babl_component_new ("CIE C(ab)", "chroma", NULL);
  babl_component_new ("CIE H(ab)", "chroma", NULL);
}

static void
models (void)
{
  babl_model_new (
    "name", "CIE Lab",
    babl_component ("CIE L"),
    babl_component ("CIE a"),
    babl_component ("CIE b"),
    NULL);

  babl_model_new (
    "name", "CIE Lab alpha",
    babl_component ("CIE L"),
    babl_component ("CIE a"),
    babl_component ("CIE b"),
    babl_component ("A"),
    NULL);

  babl_model_new (
    "name", "CIE LCH(ab)",
    babl_component ("CIE L"),
    babl_component ("CIE C(ab)"),
    babl_component ("CIE H(ab)"),
    NULL);

  babl_model_new (
    "name", "CIE LCH(ab) alpha",
    babl_component ("CIE L"),
    babl_component ("CIE C(ab)"),
    babl_component ("CIE H(ab)"),
    babl_component ("A"),
    NULL);
}

/***********    cpercep.h *********   */

/*
   Copyright (C) 1997-2002 Adam D. Moss (the "Author").  All Rights Reserved.

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is fur-
   nished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FIT-
   NESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CON-
   NECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the Author of the
   Software shall not be used in advertising or otherwise to promote the sale,
   use or other dealings in this Software without prior written authorization
   from the Author.
 */

/*
   cpercep.c: The CPercep Functions v0.9: 2002-02-10
   Adam D. Moss: adam@gimp.org <http://www.foxbox.org/adam/code/cpercep/>

   TODO: document functions, rename erroneously-named arguments
 */

static void  cpercep_init (void);

static void  cpercep_rgb_to_space (double  inr,
                                   double  ing,
                                   double  inb,
                                   double *outr,
                                   double *outg,
                                   double *outb);

static void  cpercep_space_to_rgb (double  inr,
                                   double  ing,
                                   double  inb,
                                   double *outr,
                                   double *outg,
                                   double *outb);

static void  ab_to_chab           (double  a,
                                   double  b,
                                   double *C,
                                   double *H);

static void  chab_to_ab           (double  C,
                                   double  H,
                                   double *a,
                                   double *b);


static long
rgba_to_lab (char *src,
             char *dst,
             long  n)
{
  while (n--)
    {
      double red   = ((double *) src)[0];
      double green = ((double *) src)[1];
      double blue  = ((double *) src)[2];

      double L, a, b;

      cpercep_rgb_to_space (red, green, blue, &L, &a, &b);

      ((double *) dst)[0] = L;
      ((double *) dst)[1] = a;
      ((double *) dst)[2] = b;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 3;
    }
  return n;
}

static long
lab_to_rgba (char *src,
             char *dst,
             long  n)
{
  while (n--)
    {
      double L = ((double *) src)[0];
      double a = ((double *) src)[1];
      double b = ((double *) src)[2];

      double red, green, blue;

      cpercep_space_to_rgb (L, a, b, &red, &green, &blue);

      ((double *) dst)[0] = red;
      ((double *) dst)[1] = green;
      ((double *) dst)[2] = blue;
      ((double *) dst)[3] = 1.0;

      src += sizeof (double) * 3;
      dst += sizeof (double) * 4;
    }
  return n;
}


static long
rgba_to_laba (char *src,
              char *dst,
              long  n)
{
  while (n--)
    {
      double red   = ((double *) src)[0];
      double green = ((double *) src)[1];
      double blue  = ((double *) src)[2];
      double alpha = ((double *) src)[3];

      double L, a, b;

      cpercep_rgb_to_space (red, green, blue, &L, &a, &b);

      ((double *) dst)[0] = L;
      ((double *) dst)[1] = a;
      ((double *) dst)[2] = b;
      ((double *) dst)[3] = alpha;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 4;
    }
  return n;
}

static long
laba_to_rgba (char *src,
              char *dst,
              long  n)
{
  while (n--)
    {
      double L     = ((double *) src)[0];
      double a     = ((double *) src)[1];
      double b     = ((double *) src)[2];
      double alpha = ((double *) src)[3];

      double red, green, blue;

      cpercep_space_to_rgb (L, a, b, &red, &green, &blue);

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
rgba_to_lchab (char *src,
               char *dst,
               long  n)
{
  while (n--)
    {
      double red   = ((double *) src)[0];
      double green = ((double *) src)[1];
      double blue  = ((double *) src)[2];
      double L, a, b, C, H;

      cpercep_rgb_to_space (red, green, blue, &L, &a, &b);
      ab_to_chab (a, b, &C, &H);

      ((double *) dst)[0] = L;
      ((double *) dst)[1] = C;
      ((double *) dst)[2] = H;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 3;
    }
  return n;
}

static long
lchab_to_rgba (char *src,
               char *dst,
               long  n)
{
  while (n--)
    {
      double L = ((double *) src)[0];
      double C = ((double *) src)[1];
      double H = ((double *) src)[2];
      double a, b, red, green, blue;

      chab_to_ab (C, H, &a, &b);
      cpercep_space_to_rgb (L, a, b, &red, &green, &blue);

      ((double *) dst)[0] = red;
      ((double *) dst)[1] = green;
      ((double *) dst)[2] = blue;
      ((double *) dst)[3] = 1.0;

      src += sizeof (double) * 3;
      dst += sizeof (double) * 4;
    }
  return n;
}


static long
rgba_to_lchaba (char *src,
                char *dst,
                long  n)
{
  while (n--)
    {
      double red   = ((double *) src)[0];
      double green = ((double *) src)[1];
      double blue  = ((double *) src)[2];
      double alpha = ((double *) src)[3];
      double L, a, b, C, H;

      cpercep_rgb_to_space (red, green, blue, &L, &a, &b);
      ab_to_chab (a, b, &C, &H);

      ((double *) dst)[0] = L;
      ((double *) dst)[1] = C;
      ((double *) dst)[2] = H;
      ((double *) dst)[3] = alpha;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 4;
    }
  return n;
}

static long
lchaba_to_rgba (char *src,
                char *dst,
                long  n)
{
  while (n--)
    {
      double L     = ((double *) src)[0];
      double C     = ((double *) src)[1];
      double H     = ((double *) src)[2];
      double alpha = ((double *) src)[3];
      double a, b, red, green, blue;

      chab_to_ab (C, H, &a, &b);
      cpercep_space_to_rgb (L, a, b, &red, &green, &blue);

      ((double *) dst)[0] = red;
      ((double *) dst)[1] = green;
      ((double *) dst)[2] = blue;
      ((double *) dst)[3] = alpha;

      src += sizeof (double) * 4;
      dst += sizeof (double) * 4;
    }
  return n;
}

static void
conversions (void)
{
  babl_conversion_new (
    babl_model ("RGBA"),
    babl_model ("CIE Lab"),
    "linear", rgba_to_lab,
    NULL
  );
  babl_conversion_new (
    babl_model ("CIE Lab"),
    babl_model ("RGBA"),
    "linear", lab_to_rgba,
    NULL
  );
  babl_conversion_new (
    babl_model ("RGBA"),
    babl_model ("CIE Lab alpha"),
    "linear", rgba_to_laba,
    NULL
  );
  babl_conversion_new (
    babl_model ("CIE Lab alpha"),
    babl_model ("RGBA"),
    "linear", laba_to_rgba,
    NULL
  );

  babl_conversion_new (
    babl_model ("RGBA"),
    babl_model ("CIE LCH(ab)"),
    "linear", rgba_to_lchab,
    NULL
  );
  babl_conversion_new (
    babl_model ("CIE LCH(ab)"),
    babl_model ("RGBA"),
    "linear", lchab_to_rgba,
    NULL
  );
  babl_conversion_new (
    babl_model ("RGBA"),
    babl_model ("CIE LCH(ab) alpha"),
    "linear", rgba_to_lchaba,
    NULL
  );
  babl_conversion_new (
    babl_model ("CIE LCH(ab) alpha"),
    babl_model ("RGBA"),
    "linear", lchaba_to_rgba,
    NULL
  );

  cpercep_init ();
}

static void
formats (void)
{
  babl_format_new (
    "name", "CIE Lab float",
    babl_model ("CIE Lab"),

    babl_type ("float"),
    babl_component ("CIE L"),
    babl_component ("CIE a"),
    babl_component ("CIE b"),
    NULL);

  babl_format_new (
    "name", "CIE Lab alpha float",
    babl_model ("CIE Lab alpha"),

    babl_type ("float"),
    babl_component ("CIE L"),
    babl_component ("CIE a"),
    babl_component ("CIE b"),
    babl_component ("A"),
    NULL);

  babl_format_new (
    "name", "CIE Lab u8",
    babl_model ("CIE Lab"),

    babl_type ("CIE u8 L"),
    babl_component ("CIE L"),
    babl_type ("CIE u8 ab"),
    babl_component ("CIE a"),
    babl_type ("CIE u8 ab"),
    babl_component ("CIE b"),
    NULL);

  babl_format_new (
    "name", "CIE Lab u16",
    babl_model ("CIE Lab"),

    babl_type ("CIE u16 L"),
    babl_component ("CIE L"),
    babl_type ("CIE u16 ab"),
    babl_component ("CIE a"),
    babl_type ("CIE u16 ab"),
    babl_component ("CIE b"),
    NULL);


  babl_format_new (
    "name", "CIE LCH(ab) float",
    babl_model ("CIE LCH(ab)"),

    babl_type ("float"),
    babl_component ("CIE L"),
    babl_component ("CIE C(ab)"),
    babl_component ("CIE H(ab)"),
    NULL);

  babl_format_new (
    "name", "CIE LCH(ab) alpha float",
    babl_model ("CIE LCH(ab) alpha"),

    babl_type ("float"),
    babl_component ("CIE L"),
    babl_component ("CIE C(ab)"),
    babl_component ("CIE H(ab)"),
    babl_component ("A"),
    NULL);
}


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
  convert_ ## name ## _double (char *src, \
                               char *dst, \
                               int src_pitch, \
                               int dst_pitch, \
                               long n)                               \
  { \
    return convert_u8_double_scaled (min_val, max_val, min, max, \
                                     src, dst, src_pitch, dst_pitch, n); \
  }                                                               \
  static long \
  convert_double_ ## name (char *src, \
                           char *dst, \
                           int src_pitch, \
                           int dst_pitch, \
                           long n)                                 \
  { \
    return convert_double_u8_scaled (min_val, max_val, min, max, \
                                     src, dst, src_pitch, dst_pitch, n); \
  }

/* source ICC.1:2004-10 */

MAKE_CONVERSIONS (u8_l, 0.0, 100.0, 0x00, 0xff)
MAKE_CONVERSIONS (u8_ab, -128.0, 127.0, 0x00, 0xff)

#undef MAKE_CONVERSIONS

static void
types_u8 (void)
{
  babl_type_new (
    "CIE u8 L",
    "integer",
    "unsigned",
    "bits", 8,
    "min_val", 0.0,
    "max_val", 100.0,
    NULL
  );

  babl_type_new (
    "CIE u8 ab",
    "integer",
    "unsigned",
    "bits", 8,
    "min_val", -50.0,
    "max_val", 50.0,
    NULL
  );

  babl_conversion_new (
    babl_type ("CIE u8 L"),
    babl_type ("double"),
    "plane", convert_u8_l_double,
    NULL
  );
  babl_conversion_new (
    babl_type ("double"),
    babl_type ("CIE u8 L"),
    "plane", convert_double_u8_l,
    NULL
  );

  babl_conversion_new (
    babl_type ("CIE u8 ab"),
    babl_type ("double"),
    "plane", convert_u8_ab_double,
    NULL
  );
  babl_conversion_new (
    babl_type ("double"),
    babl_type ("CIE u8 ab"),
    "plane", convert_double_u8_ab,
    NULL
  );
}

static inline long
convert_double_u16_scaled (double         min_val,
                           double         max_val,
                           unsigned short min,
                           unsigned short max,
                           char          *src,
                           char          *dst,
                           int            src_pitch,
                           int            dst_pitch,
                           long           n)
{
  while (n--)
    {
      double         dval = *(double *) src;
      unsigned short u16val;

      if (dval < min_val)
        u16val = min;
      else if (dval > max_val)
        u16val = max;
      else
        u16val = rint ((dval - min_val) / (max_val - min_val) * (max - min) + min);

      *(unsigned short *) dst = u16val;
      dst                    += dst_pitch;
      src                    += src_pitch;
    }
  return n;
}

static inline long
convert_u16_double_scaled (double         min_val,
                           double         max_val,
                           unsigned short min,
                           unsigned short max,
                           char          *src,
                           char          *dst,
                           int            src_pitch,
                           int            dst_pitch,
                           long           n)
{
  while (n--)
    {
      int    u16val = *(unsigned short *) src;
      double dval;

      if (u16val < min)
        dval = min_val;
      else if (u16val > max)
        dval = max_val;
      else
        dval = (u16val - min) / (double) (max - min) * (max_val - min_val) + min_val;

      (*(double *) dst) = dval;
      dst              += dst_pitch;
      src              += src_pitch;
    }
  return n;
}

#define MAKE_CONVERSIONS(name, min_val, max_val, min, max)      \
  static long \
  convert_ ## name ## _double (char *src, \
                               char *dst, \
                               int src_pitch, \
                               int dst_pitch, \
                               long n)                               \
  { \
    return convert_u16_double_scaled (min_val, max_val, min, max, \
                                      src, dst, src_pitch, dst_pitch, n); \
  }                                                               \
  static long \
  convert_double_ ## name (char *src, \
                           char *dst, \
                           int src_pitch, \
                           int dst_pitch, \
                           long n)                                 \
  { \
    return convert_double_u16_scaled (min_val, max_val, min, max, \
                                      src, dst, src_pitch, dst_pitch, n); \
  }

MAKE_CONVERSIONS (u16_l, 0.0, 100.0, 0x00, 0xffff)
MAKE_CONVERSIONS (u16_ab, -128.0, 127.0, 0x00, 0xffff)

#undef MAKE_CONVERSIONS

static void
types_u16 (void)
{
  babl_type_new (
    "CIE u16 L",
    "integer",
    "unsigned",
    "bits", 16,
    "min_val", 0.0,
    "max_val", 100.0,
    NULL
  );

  babl_type_new (
    "CIE u16 ab",
    "id", "CIE u8 ab",
    "integer",
    "unsigned",
    "bits", 16,
    "min_val", -50.0,
    "max_val", 50.0,
    NULL
  );


  babl_conversion_new (
    babl_type ("CIE u16 L"),
    babl_type ("double"),
    "plane", convert_u16_l_double,
    NULL
  );
  babl_conversion_new (
    babl_type ("double"),
    babl_type ("CIE u16 L"),
    "plane", convert_double_u16_l,
    NULL
  );

  babl_conversion_new (
    babl_type ("CIE u16 ab"),
    babl_type ("double"),
    "plane", convert_u16_ab_double,
    NULL
  );
  babl_conversion_new (
    babl_type ("double"),
    babl_type ("CIE u16 ab"),
    "plane", convert_double_u16_ab,
    NULL
  );
}

static void
types (void)
{
  types_u8 ();
  types_u16 ();
}




/***********   cpercep.c *********   */


/*
   Copyright (C) 1999-2002 Adam D. Moss (the "Author").  All Rights Reserved.

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is fur-
   nished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FIT-
   NESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CON-
   NECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the Author of the
   Software shall not be used in advertising or otherwise to promote the sale,
   use or other dealings in this Software without prior written authorization
   from the Author.
 */

/*
   cpercep.c: The CPercep Functions v0.9: 2002-02-10
   Adam D. Moss: adam@gimp.org <http://www.foxbox.org/adam/code/cpercep/>

   This code module concerns itself with conversion from a hard-coded
   RGB colour space (sRGB by default) to CIE L*a*b* and back again with
   (primarily) precision and (secondarily) speed, oriented largely
   towards the purposes of quantifying the PERCEPTUAL difference between
   two arbitrary RGB colours with a minimum of fuss.

   Motivation One: The author is disheartened at the amount of graphics
   processing software around which uses weighted or non-weighted
   Euclidean distance between co-ordinates within a (poorly-defined) RGB
   space as the basis of what should really be an estimate of perceptual
   difference to the human eye.  Certainly it's fast to do it that way,
   but please think carefully about whether your particular application
   should be tolerating sloppy results for the sake of real-time response.

   Motivation Two: Lack of tested, re-usable and free code available
   for this purpose.  The difficulty in finding something similar to
   CPercep with a free license motivated this project; I hope that this
   code also serves to illustrate how to perform the
   R'G'B'->XYZ->L*a*b*->XYZ->R'G'B' transformation correctly since I
   was distressed to note how many of the equations and code snippets
   on the net were omitting the reverse transform and/or were using
   incorrectly-derived or just plain wrong constants.

   TODO: document functions, rename erroneously-named arguments
 */

/* defines added to make it compile outside gimp */

#ifndef gboolean
#define gboolean    int
#endif
#ifndef FALSE
#define FALSE       0
#endif
#ifndef TRUE
#define TRUE        1
#endif


/* #include "config.h" */
#include <math.h>

#ifndef __GLIBC__
/* cbrt() is a GNU extension */
#define cbrt(x)    (pow (x, 1.0 / 3.0))
#endif


/* defines:

   SANITY: emits warnings when passed non-sane colours (and usually
   corrects them) -- useful when debugging.

   APPROX: speeds up the conversion from RGB to the colourspace by
   assuming that the RGB values passed in are integral and definitely
   in the range 0->255
 */

/* #define SANITY */
/* #define APPROX */


/* define characteristics of the source RGB space (and the space
   within which we try to behave linearly). */

/* Phosphor colours: */

/* sRGB/HDTV phosphor colours */
static const double pxr = 0.64F;
static const double pyr = 0.33F;
static const double pxg = 0.30F;
static const double pyg = 0.60F;
static const double pxb = 0.15F;
static const double pyb = 0.06F;

/* White point: */

/* D65 (6500K) (recommended but not a common display default) */
static const double lxn = 0.312713F;
static const double lyn = 0.329016F;

/* D50 (5000K) */
/*static const double lxn = 0.3457F; */
/*static const double lyn = 0.3585F; */

/* D55 (5500K) */
/*static const double lxn = 0.3324F; */
/*static const double lyn = 0.3474F; */

/* D93 (9300K) (a common monitor default, but poor colour reproduction) */
/* static const double lxn = 0.2848F; */
/* static const double lyn = 0.2932F; */

/* illum E (normalized) */
/*static const double lxn = 1.0/3.0F; */
/*static const double lyn = 1.0/3.0F; */

/* illum C (average sunlight) */
/*static const double lxn = 0.3101F; */
/*static const double lyn = 0.3162F; */

/* illum B (direct sunlight) */
/*static const double lxn = 0.3484F; */
/*static const double lyn = 0.3516F; */

/* illum A (tungsten lamp) */
/*static const double lxn = 0.4476F; */
/*static const double lyn = 0.4074F; */


static const double LRAMP = 7.99959199F;


static double xnn, znn;


#ifndef CLAMP
#define CLAMP(x, l, u)    ((x) < (l) ? (l) : ((x) > (u) ? (u) : (x)))
#endif


typedef double CMatrix[3][3];
typedef double CVector[3];

static CMatrix Mrgb_to_xyz, Mxyz_to_rgb;

static int
Minvert (CMatrix src, CMatrix dest)
{
  double det;

  dest[0][0] = src[1][1] * src[2][2] - src[1][2] * src[2][1];
  dest[0][1] = src[0][2] * src[2][1] - src[0][1] * src[2][2];
  dest[0][2] = src[0][1] * src[1][2] - src[0][2] * src[1][1];
  dest[1][0] = src[1][2] * src[2][0] - src[1][0] * src[2][2];
  dest[1][1] = src[0][0] * src[2][2] - src[0][2] * src[2][0];
  dest[1][2] = src[0][2] * src[1][0] - src[0][0] * src[1][2];
  dest[2][0] = src[1][0] * src[2][1] - src[1][1] * src[2][0];
  dest[2][1] = src[0][1] * src[2][0] - src[0][0] * src[2][1];
  dest[2][2] = src[0][0] * src[1][1] - src[0][1] * src[1][0];

  det =
    src[0][0] * dest[0][0] +
    src[0][1] * dest[1][0] +
    src[0][2] * dest[2][0];

  if (det <= 0.0F)
    {
#ifdef SANITY
      g_printerr ("\n\007 XXXX det: %f\n", det);
#endif
      return 0;
    }

  dest[0][0] /= det;
  dest[0][1] /= det;
  dest[0][2] /= det;
  dest[1][0] /= det;
  dest[1][1] /= det;
  dest[1][2] /= det;
  dest[2][0] /= det;
  dest[2][1] /= det;
  dest[2][2] /= det;

  return 1;
}


static void
rgbxyzrgb_init (void)
{
  /* The gamma related code has been removed since we do our
   * calculations in linear light. To revice that code, use version
   * control means
   */

  xnn = lxn / lyn;
  /* ynn taken as 1.0 */
  znn = (1.0F - (lxn + lyn)) / lyn;

  {
    CMatrix MRC, MRCi;
    double  C1, C2, C3;

    MRC[0][0] = pxr;
    MRC[0][1] = pxg;
    MRC[0][2] = pxb;
    MRC[1][0] = pyr;
    MRC[1][1] = pyg;
    MRC[1][2] = pyb;
    MRC[2][0] = 1.0F - (pxr + pyr);
    MRC[2][1] = 1.0F - (pxg + pyg);
    MRC[2][2] = 1.0F - (pxb + pyb);

    Minvert (MRC, MRCi);

    C1 = MRCi[0][0] * xnn + MRCi[0][1] + MRCi[0][2] * znn;
    C2 = MRCi[1][0] * xnn + MRCi[1][1] + MRCi[1][2] * znn;
    C3 = MRCi[2][0] * xnn + MRCi[2][1] + MRCi[2][2] * znn;

    Mrgb_to_xyz[0][0] = MRC[0][0] * C1;
    Mrgb_to_xyz[0][1] = MRC[0][1] * C2;
    Mrgb_to_xyz[0][2] = MRC[0][2] * C3;
    Mrgb_to_xyz[1][0] = MRC[1][0] * C1;
    Mrgb_to_xyz[1][1] = MRC[1][1] * C2;
    Mrgb_to_xyz[1][2] = MRC[1][2] * C3;
    Mrgb_to_xyz[2][0] = MRC[2][0] * C1;
    Mrgb_to_xyz[2][1] = MRC[2][1] * C2;
    Mrgb_to_xyz[2][2] = MRC[2][2] * C3;

    Minvert (Mrgb_to_xyz, Mxyz_to_rgb);
  }
}


static void
xyz_to_rgb (double *inx_outr,
            double *iny_outg,
            double *inz_outb)
{
  const double x = *inx_outr;
  const double y = *iny_outg;
  const double z = *inz_outb;

  *inx_outr = Mxyz_to_rgb[0][0] * x + Mxyz_to_rgb[0][1] * y + Mxyz_to_rgb[0][2] * z;
  *iny_outg = Mxyz_to_rgb[1][0] * x + Mxyz_to_rgb[1][1] * y + Mxyz_to_rgb[1][2] * z;
  *inz_outb = Mxyz_to_rgb[2][0] * x + Mxyz_to_rgb[2][1] * y + Mxyz_to_rgb[2][2] * z;
}


static void
rgb_to_xyz (double *inr_outx,
            double *ing_outy,
            double *inb_outz)
{
  const double r = *inr_outx;
  const double g = *ing_outy;
  const double b = *inb_outz;

  *inr_outx = Mrgb_to_xyz[0][0] * r + Mrgb_to_xyz[0][1] * g + Mrgb_to_xyz[0][2] * b;
  *ing_outy = Mrgb_to_xyz[1][0] * r + Mrgb_to_xyz[1][1] * g + Mrgb_to_xyz[1][2] * b;
  *inb_outz = Mrgb_to_xyz[2][0] * r + Mrgb_to_xyz[2][1] * g + Mrgb_to_xyz[2][2] * b;
}


static inline double
ffunc (const double t)
{
  if (t > 0.008856F)
    {
      return (cbrt (t));
    }
  else
    {
      return (7.787F * t + 16.0F / 116.0F);
    }
}


static inline double
ffunc_inv (const double t)
{
  if (t > 0.206893F)
    {
      return (t * t * t);
    }
  else
    {
      return ((t - 16.0F / 116.0F) / 7.787F);
    }
}


static void
xyz_to_lab (double *inx,
            double *iny,
            double *inz)
{
  double       L, a, b;
  double       ffuncY;
  const double X = *inx;
  const double Y = *iny;
  const double Z = *inz;

  if (Y > 0.0F)
    {
      if (Y > 0.008856F)
        {
          L = (116.0F * cbrt (Y)) - 16.0F;
        }
      else
        {
          L = (Y * 903.3F);
        }

#ifdef SANITY
      if (L < 0.0F)
        {
          g_printerr (" <eek1>%f \007", (float) L);
        }

      if (L > 100.0F)
        {
          g_printerr (" <eek2>%f \007", (float) L);
        }
#endif
    }
  else
    {
      L = 0.0;
    }

  ffuncY = ffunc (Y);
  a      = 500.0F * (ffunc (X / xnn) - ffuncY);
  b      = 200.0F * (ffuncY - ffunc (Z / znn));

  *inx = L;
  *iny = a;
  *inz = b;
}


static void
lab_to_xyz (double *inl,
            double *ina,
            double *inb)
{
  double       X, Y, Z;
  double       P;
  const double L = *inl;
  const double a = *ina;
  const double b = *inb;

  if (L > LRAMP)
    {
      P = Y = (L + 16.0F) / 116.0F;
      Y = Y * Y * Y;
    }
  else
    {
      Y = L / 903.3F;
      P = 7.787F * Y + 16.0F / 116.0F;
    }

  X = (P + a / 500.0F);
  X = xnn *ffunc_inv (X);
  Z = (P - b / 200.0F);
  Z = znn *ffunc_inv (Z);

#ifdef SANITY
  if (X < -0.00000F)
    {
      if (X < -0.0001F)
        g_printerr ("{badX %f {%f,%f,%f}}", X, L, a, b);
      X = 0.0F;
    }
  if (Y < -0.00000F)
    {
      if (Y < -0.0001F)
        g_printerr ("{badY %f}", Y);
      Y = 0.0F;
    }
  if (Z < -0.00000F)
    {
      if (Z < -0.1F)
        g_printerr ("{badZ %f}", Z);
      Z = 0.0F;
    }
#endif

  *inl = X;
  *ina = Y;
  *inb = Z;
}



/* call this before using the CPercep function */
static void
cpercep_init (void)
{
  static gboolean initialized = FALSE;

  if (!initialized)
    {
      rgbxyzrgb_init ();
      initialized = TRUE;
    }
}

static void
cpercep_rgb_to_space (double  inr,
                      double  ing,
                      double  inb,
                      double *outr,
                      double *outg,
                      double *outb)
{
#ifdef APPROX
#ifdef SANITY
  /* ADM extra sanity */
  if ((inr) > 255.0F ||
      (ing) > 255.0F ||
      (inb) > 255.0F ||
      (inr) < -0.0F ||
      (ing) < -0.0F ||
      (inb) < -0.0F
  )
    abort ();
#endif /* SANITY */
#endif /* APPROX */

#ifdef SANITY
  /* ADM extra sanity */
  if ((inr) > 1.0F ||
      (ing) > 1.0F ||
      (inb) > 1.0F ||
      (inr) < 0.0F ||
      (ing) < 0.0F ||
      (inb) < 0.0F
  )
    {
      g_printerr ("%%");
      /* abort(); */
    }
#endif /* SANITY */

  rgb_to_xyz (&inr, &ing, &inb);

#ifdef SANITY
  if (inr < 0.0F || ing < 0.0F || inb < 0.0F)
    {
      g_printerr (" [BAD2 XYZ: %f,%f,%f]\007 ",
                  inr, ing, inb);
    }
#endif /* SANITY */

  xyz_to_lab (&inr, &ing, &inb);

  *outr = inr;
  *outg = ing;
  *outb = inb;
}


static void
cpercep_space_to_rgb (double  inr,
                      double  ing,
                      double  inb,
                      double *outr,
                      double *outg,
                      double *outb)
{
  lab_to_xyz (&inr, &ing, &inb);

#ifdef SANITY
  if (inr < -0.0F || ing < -0.0F || inb < -0.0F)
    {
      g_printerr (" [BAD1 XYZ: %f,%f,%f]\007 ",
                  inr, ing, inb);
    }
#endif

  xyz_to_rgb (&inr, &ing, &inb);

  /* yes, essential.  :( */
  inr = CLAMP (inr, 0.0F, 1.0F);
  ing = CLAMP (ing, 0.0F, 1.0F);
  inb = CLAMP (inb, 0.0F, 1.0F);

  *outr = inr;
  *outg = ing;
  *outb = inb;
}

static void
ab_to_chab (double  a,
            double  b,
            double *C,
            double *H)
{
  *C = sqrt (a * a + b * b);
  *H = atan2 (b, a) * DEGREES_PER_RADIAN;

  /* Keep H within the range 0-360 */
  if (*H < 0.0)
    *H += 360;
}

static void
chab_to_ab (double  C,
            double  H,
            double *a,
            double *b)
{
  *a = cos (H * RADIANS_PER_DEGREE) * C;
  *b = sin (H * RADIANS_PER_DEGREE) * C;
}


#if 0
/* EXPERIMENTAL SECTION */

const double
xscaler (const double start, const double end,
         const double me, const double him)
{
  return start + ((end - start) * him) / (me + him);
}


void
mix_colours (const double L1, const double a1, const double b1,
             const double L2, const double a2, const double b2,
             double *rtnL, double *rtna, double *rtnb,
             double mass1, double mass2)
{
  double w1, w2;

#if 0
  *rtnL = xscaler (L1, L2, mass1, mass2);
  *rtna = xscaler (a1, a2, mass1, mass2);
  *rtnb = xscaler (b1, b2, mass1, mass2);
#else
#if 1
  w1 = mass1 * L1;
  w2 = mass2 * L2;
#else
  w1 = mass1 * (L1 * L1 * L1);
  w2 = mass2 * (L2 * L2 * L2);
#endif

  *rtnL = xscaler (L1, L2, mass1, mass2);

  if (w1 <= 0.0 &&
      w2 <= 0.0)
    {
      *rtna   =
        *rtnb = 0.0;
#ifdef SANITY
      /* g_printerr ("\007OUCH. "); */
#endif
    }
  else
    {
      *rtna = xscaler (a1, a2, w1, w2);
      *rtnb = xscaler (b1, b2, w1, w2);
    }
#endif
}
#endif /* EXPERIMENTAL SECTION */

/***********  /cpercep.c *********   */
