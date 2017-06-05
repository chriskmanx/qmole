/*
 * This file was part of gggl, it implements a variety of pixel conversion
 * functions that are usable with babl, the file needs more cleanup.
 *
 *    GGGL is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    GGGL is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with GGGL; if not, see <http://www.gnu.org/licenses/>.
 *
 *    Rights are granted to use this shared object in libraries covered by
 *    LGPL. (exception added, during import into babl CVS.)
 *
 *  Copyright 2003, 2004, 2005 Øyvind Kolås <pippin@gimp.org>
 */

#include "config.h"
#include <math.h>
#include <string.h>

#include "babl.h"
#include "extensions/util.h"


#define INLINE    inline

/*
 * Implemented according to information read from:
 *
 * http://www.cinenet.net/~spitzak/conversion/sketches_0265.pdf
 *
 * initially ignoring any diffusion, to keep the implementation
 * smaller, and interchangeable with the non optimized version.
 *
 * due to ability to be able to relicence gggl under a different
 * licence than GPL, I avoided the temptation to look at the
 * source files in the same location, in case I was going to
 * need this piece of code for projects where GPL compatibility
 * was a must.
 *
 * TODO: error diffusion,
 *       gamma correction  (not really,. gamma correction belongs in seperate ops,.
 */

#define USE_TABLES
#ifdef USE_TABLES

/* lookup tables used in conversion */

static float          table_8_F[1 << 8];
static float          table_16_F[1 << 16];
static unsigned char  table_F_8[1 << 16];
static unsigned short table_F_16[1 << 16];


static int table_inited = 0;

static void
table_init (void)
{
  if (table_inited)
    return;
  table_inited = 1;

  /* fill tables for conversion from integer to float */
  {
    int i;
    for (i = 0; i < 1 << 8; i++)
      {
        table_8_F[i] = (i * 1.0) / 255.0;
      }
    for (i = 0; i < 1 << 16; i++)
      table_16_F[i] = (i * 1.0) / 65535.0;
  }
  /* fill tables for conversion from float to integer */
  {
    union
    {
      float          f;
      unsigned short s[2];
    } u;
    u.f = 0.0;

    u.s[0] = 0.0;

    for (u.s[1] = 0; u.s[1] < 65535; u.s[1] += 1)
      {
        unsigned char  c;
        unsigned short s;

        if (u.f <= 0.0)
          {
            c = 0;
            s = 0;
          }
        else if (u.f >= 1.0)
          {
            c = 255;
            s = 65535;
          }
        else
          {
            c = rint (u.f * 255.0);
            s = rint (u.f * 65535.0);
          }

        /*fprintf (stderr, "%2.3f=%03i %05i ", f, c, (*hi));
           / if (! ((*hi)%9))
           /         fprintf (stderr, "\n"); */

        table_F_8[u.s[1]]  = c;
        table_F_16[u.s[1]] = s;
      }
  }
  /* fix tables to ensure 1:1 conversions back and forth */
  if (0)
    {                           /*FIXME: probably not the right way to do it,.. must sit down and scribble on paper */
      int i;
      for (i = 0; i < 256; i++)
        {
          float           f  = table_8_F[i];
          unsigned short *hi = ((unsigned short *) (void *) &f);
          unsigned short *lo = ((unsigned short *) (void *) &f);
          *lo              = 0;
          table_F_8[(*hi)] = i;
        }
    }
}

/* function to find the index in table for a float */
static unsigned int
gggl_float_to_index16 (float f)
{
  union
  {
    float          f;
    unsigned short s[2];
  } u;
  u.f = f;
  return u.s[1];
}

static INLINE long
conv_F_8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  if (!table_inited)
    table_init ();
  while (n--)
    {
      register float f = (*(float *) src);
      *(unsigned char *) dst = table_F_8[gggl_float_to_index16 (f)];
      dst                   += 1;
      src                   += 4;
    }
  return samples;
}

static INLINE long
conv_F_16 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  if (!table_inited)
    table_init ();
  while (n--)
    {
      register float f = (*(float *) src);
      *(unsigned short *) dst = table_F_16[gggl_float_to_index16 (f)];
      dst                    += 2;
      src                    += 4;
    }
  return samples;
}

static INLINE long
conv_8_F (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  if (!table_inited)
    table_init ();
  while (n--)
    {
      (*(float *) dst) = table_8_F[*(unsigned char *) src];
      dst             += 4;
      src             += 1;
    }
  return samples;
}

static INLINE long
conv_16_F (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  if (!table_inited)
    table_init ();
  while (n--)
    {
      (*(float *) dst) = table_16_F[*(unsigned short *) src];
      dst             += 4;
      src             += 2;
    }
  return samples;
}

#else

static INLINE long
conv_F_8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float f    = ((*(float *) src));
      int   uval = rint (f * 255.0);

      if (uval < 0) uval = 0;
      if (uval > 255) uval = 255;
      *(unsigned char *) dst = uval;

      dst += 1;
      src += 4;
    }
  return samples;
}

static INLINE long
conv_F_16 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float f = ((*(float *) src));
      if (f < 0.0)
        {
          *(unsigned short *) dst = 0;
        }
      else if (f > 1.0)
        {
          *(unsigned short *) dst = 65535;
        }
      else
        {
          *(unsigned short *) dst = rint (f * 65535.0);
        }
      dst += 2;
      src += 4;
    }
  return samples;
}

static INLINE long
conv_8_F (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      (*(float *) dst) = ((*(unsigned char *) src) / 255.0);
      dst             += 4;
      src             += 1;
    }
  return samples;
}

static INLINE long
conv_16_F (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      (*(float *) dst) = *(unsigned short *) src / 65535.0;
      dst             += 4;
      src             += 2;
    }
  return samples;
}


#endif


static INLINE long
conv_F_D (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      *(double *) dst = ((*(float *) src));
      dst            += 8;
      src            += 4;
    }
  return samples;
}

static INLINE long
conv_D_F (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      *(float *) dst = ((*(double *) src));
      dst           += 4;
      src           += 8;
    }
  return samples;
}

static INLINE long
conv_16_8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      (*(unsigned char *) dst) = (*(unsigned short *) src) >> 8;
      dst                     += 1;
      src                     += 2;
    }
  return samples;
}

static INLINE long
conv_8_16 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      (*(unsigned short *) dst) = (*(unsigned char *) src) << 8;
      dst                      += 2;
      src                      += 1;
    }
  return samples;
}

static INLINE long
conv_rgbaF_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      register float f = (*(float *) src);
      *(unsigned char *) dst = table_F_8[gggl_float_to_index16 (f)];
      src                   += 4;
      dst                   += 1;

      f                      = (*(float *) src);
      *(unsigned char *) dst = table_F_8[gggl_float_to_index16 (f)];
      src                   += 4;
      dst                   += 1;

      f                      = (*(float *) src);
      *(unsigned char *) dst = table_F_8[gggl_float_to_index16 (f)];
      src                   += 4;
      dst                   += 1;

      src += 4;
    }
  return samples;
}


/*********/
static INLINE long
conv_rgbaF_rgba8 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_F_8 (src, dst, samples * 4);
  return samples;
}

#define conv_rgbaF_rgbP8    conv_rgbaF_rgba8

static INLINE long
conv_rgbF_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_F_8 (src, dst, samples * 3);
  return samples;
}

static INLINE long
conv_gaF_ga8 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_F_8 (src, dst, samples * 2);
  return samples;
}

#define conv_rgbAF_rgbA8    conv_rgbaF_rgba8
#define conv_gF_g8          conv_F_8
#define conv_gAF_gA8        conv_gaF_ga8

static INLINE long
conv_rgbaF_rgba16 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_F_16 (src, dst, samples * 4);
  return samples;
}

static INLINE long
conv_rgbF_rgb16 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_F_16 (src, dst, samples * 3);
  return samples;
}

static INLINE long
conv_gaF_ga16 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_F_16 (src, dst, samples * 2);
  return samples;
}

#define conv_rgbAF_rgbA16    conv_rgbaF_rgba16
#define conv_gF_g16          conv_F_16
#define conv_gAF_gA16        conv_gaF_ga16

static INLINE long
conv_rgba8_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  return conv_8_F (src, dst, samples * 4) / 4;
}

static INLINE long
conv_rgb8_rgbF (unsigned char *src, unsigned char *dst, long samples)
{
  return conv_8_F (src, dst, samples * 3) / 3;
}

static INLINE long
conv_ga8_gaF (unsigned char *src, unsigned char *dst, long samples)
{
  return conv_8_F (src, dst, samples * 2) / 2;
}

#define conv_rgbA8_rgbAF    conv_rgba8_rgbaF
#define conv_gA8_gAF        conv_ga8_gaF
#define conv_g8_gF          conv_8_F

static INLINE long
conv_rgbaF_rgbaD (unsigned char *src, unsigned char *dst, long samples)
{
  conv_F_D (src, dst, samples * 4);
  return samples;
}

static INLINE long
conv_rgbaD_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  conv_D_F (src, dst, samples * 4);
  return samples;
}

static INLINE long
conv_rgba16_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  conv_16_F (src, dst, samples * 4);
  return samples;
}

static INLINE long
conv_rgb16_rgbF (unsigned char *src, unsigned char *dst, long samples)
{
  conv_16_F (src, dst, samples * 3);
  return samples;
}

static INLINE long
conv_ga16_gaF (unsigned char *src, unsigned char *dst, long samples)
{
  conv_16_F (src, dst, samples * 2);
  return samples;
}

#define conv_rgbA16_rgbAF    conv_rgba16_rgbaF
#define conv_gA16_gAF        conv_ga16_gaF
#define conv_g16_gF          conv_16_F

static INLINE long
conv_rgba16_rgba8 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_16_8 (src, dst, samples * 4);
  return samples;
}

static INLINE long
conv_rgb16_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_16_8 (src, dst, samples * 3);
  return samples;
}

static INLINE long
conv_ga16_ga8 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_16_8 (src, dst, samples * 2);
  return samples;
}

#define conv_rgbA16_rgbA8    conv_rgba16_rgba8
#define conv_gA16_gA8        conv_ga16_ga8
#define conv_g16_g8          conv_16_8

static INLINE long
conv_rgba8_rgba16 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_8_16 (src, dst, samples * 4);
  return samples;
}

static INLINE long
conv_rgb8_rgb16 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_8_16 (src, dst, samples * 3);
  return samples;
}

static INLINE long
conv_ga8_ga16 (unsigned char *src, unsigned char *dst, long samples)
{
  conv_8_16 (src, dst, samples * 2);
  return samples;
}

#define conv_rgbA8_rgbA16    conv_rgba8_rgba16
#define conv_gA8_gA16        conv_ga8_ga16
#define conv_g8_g16          conv_8_16

/* alpha conversions */

static INLINE long
conv_gaF_gAF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (*(float *) (src + 4));

      *(float *) dst = ((*(float *) src) * alpha);
      dst           += 4;
      src           += 4;
      *(float *) dst = alpha;
      dst           += 4;
      src           += 4;
    }
  return samples;
}

static INLINE long
conv_gAF_gaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (*(float *) (src + 4));

      *(float *) dst = ((*(float *) src) / alpha);
      dst           += 4;
      src           += 4;
      *(float *) dst = alpha;
      dst           += 4;
      src           += 4;
    }
  return samples;
}

static INLINE long
conv_rgbAF_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (((float *) src)[3]);
      int   c;
      if (alpha >= 1.0)
        {
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = *(float *) src;
              dst           += 4;
              src           += 4;
            }
        }
      else if (alpha <= 0.0)
        {
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = 0;
              dst           += 4;
              src           += 4;
            }
        }
      else
        {
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = ((*(float *) src) / alpha);
              dst           += 4;
              src           += 4;
            }
        }
      *(float *) dst = alpha;
      dst           += 4;
      src           += 4;
    }
  return samples;
}


static INLINE long
conv_rgbAF_rgbF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (((float *) src)[3]);
      if (alpha >= 1.0)
        {
          register int c;
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = *(float *) src;
              dst           += 4;
              src           += 4;
            }
        }
      else if (alpha <= 0.0)
        {
          register int c;
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = 0;
              dst           += 4;
              src           += 4;
            }
        }
      else
        {
          register int c;
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = ((*(float *) src) / alpha);
              dst           += 4;
              src           += 4;
            }
        }
      src += 4;
    }
  return samples;
}

static INLINE long
conv_rgbaF_rgbAF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (((float *) src)[3]);
      int   c;

      if (alpha >= 1.0)
        {
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = *(float *) src;
              dst           += 4;
              src           += 4;
            }
        }
      else if (alpha <= 0.0)
        {
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = 0;
              dst           += 4;
              src           += 4;
            }
        }
      else
        {
          for (c = 0; c < 3; c++)
            {
              *(float *) dst = ((*(float *) src) * alpha);
              dst           += 4;
              src           += 4;
            }
        }
      *(float *) dst = alpha;
      dst           += 4;
      src           += 4;
    }
  return samples;
}

/* alpha stripping and adding */

static INLINE long
conv_rgbaF_rgbF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      *(float *) dst = (*(float *) src);
      dst           += 4;
      src           += 4;
      *(float *) dst = (*(float *) src);
      dst           += 4;
      src           += 4;
      *(float *) dst = (*(float *) src);
      dst           += 4;
      src           += 4;
      src           += 4;
    }
  return samples;
}

static INLINE long
conv_rgbF_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      *(float *) dst = (*(float *) src);
      src           += 4;
      dst           += 4;
      *(float *) dst = (*(float *) src);
      src           += 4;
      dst           += 4;
      *(float *) dst = (*(float *) src);
      src           += 4;
      dst           += 4;
      *(float *) dst = 1.0;
      dst           += 4;
    }
  return samples;
}

static INLINE long
conv_gaF_gF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      *(int *) dst = (*(int *) src);
      dst         += 4;
      src         += 4;
      src         += 4;
    }
  return samples;
}

static INLINE long
conv_gF_gaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      *(float *) dst = (*(float *) src);
      dst           += 4;
      src           += 4;
      *(float *) dst = 1.0;
      dst           += 4;
    }
  return samples;
}

#define conv_gAF_gF        conv_gaF_gF
#define conv_gF_gAF        conv_gF_gaF

#define conv_rgbAF_rgbF    conv_rgbaF_rgbF
#define conv_rgbF_rgbAF    conv_rgbF_rgbaF

/* colorchannel dropping and adding */

static INLINE long
conv_gF_rgbF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int c;

      for (c = 0; c < 3; c++)
        {
          (*(float *) dst) = (*(float *) src);
          dst             += 4;
        }
      src += 4;
    }
  return samples;
}

static INLINE long
conv_rgbF_gF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int   c;
      float sum = 0;

      for (c = 0; c < 3; c++)
        {
          sum += (*(float *) src);
          src += 4;
        }
      sum             /= 3.0;
      (*(float *) dst) = sum;
      dst             += 4;
    }
  return samples;
}

static INLINE long
conv_gaF_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int c;

      for (c = 0; c < 3; c++)
        {
          (*(int *) dst) = (*(int *) src);
          dst           += 4;
        }
      src           += 4;
      (*(int *) dst) = (*(int *) src);
      dst           += 4;
      src           += 4;
    }
  return samples;
}

static INLINE long
conv_rgbaF_gaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float gray = 0;

      gray            += (*(float *) src) * 0.212671;
      src             += 4;
      gray            += (*(float *) src) * 0.715160;
      src             += 4;
      gray            += (*(float *) src) * 0.072169;
      src             += 4;
      (*(float *) dst) = gray;
      dst             += 4;
      (*(int *) dst)   = (*(int *) src);
      dst             += 4;
      src             += 4;
    }
  return samples;
}

#define conv_gAF_rgbAF    conv_gaF_rgbaF
#define conv_rgbAF_gAF    conv_rgbaF_gaF

/* other conversions coded for some optimisation reason or sumthin */

static INLINE long
conv_rgbA8_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (*(unsigned char *) src + (3)) / 255.0;
      int   c;

      for (c = 0; c < 3; c++)
        {
          (*(float *) dst) = (*(unsigned char *) src / 255.0) / alpha;
          dst             += 4;
          src             += 1;
        }
      *(float *) dst = alpha;
      dst           += 4;
      src           += 1;
    }
  return samples;
}

static INLINE long
conv_rgbaF_rgbA8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (*(float *) (src + (4 * 3)));
      int   c;

      for (c = 0; c < 3; c++)
        {
          *(unsigned char *) dst = ((*(float *) src) * alpha) * 255.0;
          dst                   += 1;
          src                   += 4;
        }
      *(unsigned char *) dst = alpha * 255.0;
      dst++;
      src += 4;
    }
  return samples;
}

static INLINE long
conv_rgbaF_rgbA16 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (*(float *) src + (4 * 3));
      int   c;

      for (c = 0; c < 3; c++)
        {
          *(unsigned short *) dst = ((*(float *) src) * alpha) * 65535.0;
          dst                    += 2;
          src                    += 4;
        }
      *(unsigned short *) dst = alpha * 65535.0;
      dst                    += 2;
      src                    += 4;
    }
  return samples;
}
#if 0
static INLINE long
conv_rgbaF_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int c;

      for (c = 0; c < 3; c++)
        {
          int val = rint ((*(float *) src) * 255.0);
          if (val < 0)
            *(unsigned char *) dst = 0;
          else if (val > 255)
            *(unsigned char *) dst = 255;
          else
            *(unsigned char *) dst = val;
          dst += 1;
          src += 4;
        }
      src += 4;
    }
  return samples;
}
#endif

static INLINE long
conv_rgbaF_g8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int   c;
      float sum = 0;

      for (c = 0; c < 3; c++)
        {
          sum += (*(float *) src);
          src += 4;
        }
      *(unsigned char *) dst = sum * 255.0 / 3;
      dst++;
      src += 4;
    }
  return samples;
}

static INLINE long
conv_rgbaF_rgb16 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int c;

      for (c = 0; c < 3; c++)
        {
          *(unsigned short *) dst = (*(float *) src) * 65535.0;
          dst                    += 2;
          src                    += 4;
        }
      src += 4;
    }
  return samples;
}

static INLINE long
conv_rgbP8_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int c;

      for (c = 0; c < 3; c++)
        {
          (*(float *) dst) = *(unsigned char *) src / 255.0;
          dst             += 4;
          src             += 1;
        }
      (*(float *) dst) = 1.0;
      dst             += 4;
      src             += 1;
    }
  return samples;
}

static INLINE long
conv_rgbA16_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      float alpha = (((unsigned short *) src)[3]) / 65535.0;
      int   c;

      for (c = 0; c < 3; c++)
        {
          (*(float *) dst) = (*(unsigned short *) src / 65535.0) / alpha;
          dst             += 4;
          src             += 2;
        }
      *(float *) dst = alpha;
      dst           += 4;
      src           += 2;
    }
  return samples;
}

/*
   static INLINE long
   conv_rgb8_rgbaF (unsigned char *src,
                 unsigned char *dst,
                 int samples)
   {
    long n=samples;
    while (n--) {
        int c;

        for (c = 0; c < 3; c++) {
            (*(float *) dst) = *(unsigned char *) src / 255.0;
            dst += 4;
            src += 1;
        }
        (*(float *) dst) = 1.0;
        dst += 4;
    }
   return samples;
   }

   static INLINE long
   conv_g8_rgbaF (unsigned char *src,
               unsigned char *dst,
               int samples)
   {
    long n=samples;
    while (n--) {
        int c;

        for (c = 0; c < 3; c++) {
            (*(float *) dst) = *(unsigned char *) src / 255.0;
            dst += 4;
        }
        src += 1;
        (*(float *) dst) = 1.0;
        dst += 4;
    }
   return samples;
   }

   static INLINE long
   conv_rgb16_rgbaF (unsigned char *src,
                  unsigned char *dst,
                  int samples)
   {
    long n=samples;
    while (n--) {
        int c;

        for (c = 0; c < 3; c++) {
 *(float *) dst = (*(unsigned short *) src) / 65535.0;
            src += 2;
            dst += 4;
        }
 *(float *) dst = 1.0;
        src += 2;
        dst += 4;
    }
   return samples;
   }

   static INLINE long
   conv_gF_rgbaF (unsigned char *src,
               unsigned char *dst,
               int samples)
   {
    long n=samples;
    while (n--) {
        (*(float *) dst) = (*(float *) src);
        dst += 4;
        (*(float *) dst) = (*(float *) src);
        dst += 4;
        (*(float *) dst) = (*(float *) src);
        dst += 4;
        (*(float *) dst) = 1.0;
        dst += 4;
        src += 4;

    }
   return samples;
   }
 */
static INLINE long
conv_rgba8_rgbA8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      if (src[3] == 255)
        {
          *(unsigned int *) dst = *(unsigned int *) src;
        }
      else if (src[3] == 0)
        {
          *(unsigned int *) dst = 0;
        }
      else
        {
          dst[0] = (src[0] * src[3]) >> 8;      /* FIXME: check if this faster */
          dst[1] = (src[1] * src[3]) >> 8;      /*        version (>>8 vs /255) */
          dst[2] = (src[2] * src[3]) >> 8;      /*        is accurate enough   */
          dst[3] = src[3];
        }
      dst += 4;
      src += 4;
    }
  return samples;
}

static INLINE long
conv_rgbA8_rgba8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      if (src[3] == 255)
        {
          *(unsigned int *) dst = *(unsigned int *) src;
          dst                  += 4;
        }
      else if (src[3] == 0)
        {
          *(unsigned int *) dst = 0;
          dst                  += 4;
        }
      else
        {
          unsigned int aa = (255 << 16) / src[3];
          *dst++ = (src[0] * aa) >> 16;
          *dst++ = (src[1] * aa) >> 16;
          *dst++ = (src[2] * aa) >> 16;
          *dst++ = src[3];
        }
      src += 4;
    }
  return samples;
}

static INLINE long
conv_argb8_rgba8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[2];
      dst[1] = src[1];
      dst[2] = src[0];
      dst[3] = src[3];
      src   += 4;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_rgba8_argb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[3];
      dst[1] = src[0];
      dst[2] = src[1];
      dst[3] = src[2];
      src   += 4;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_Argb8_rgbA8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[2];
      dst[1] = src[1];
      dst[2] = src[0];
      dst[3] = src[3];
      src   += 4;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_rgbA8_Argb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[2];
      dst[1] = src[1];
      dst[2] = src[0];
      dst[3] = src[3];
      src   += 4;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_Prgb8_rgbP8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[2];
      dst[1] = src[1];
      dst[2] = src[0];
      dst[3] = 255;
      src   += 4;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_rgbP8_Prgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[2];
      dst[1] = src[1];
      dst[2] = src[0];
      dst[3] = 255;
      src   += 4;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_Prgb8_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[2];
      dst[1] = src[1];
      dst[2] = src[0];
      /*dst[3] = src[3]; */
      src += 4;
      dst += 3;
    }
  return samples;
}


/* FIXME: this is actually BGRP */
static INLINE long
conv_rgb8_Prgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[2];          /* blue */
      dst[1] = src[1];          /* green */
      dst[2] = src[0];          /* red */
      dst[3] = 255;
      src   += 3;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_rgbA16_rgba16 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      if (src[3])
        {
          ((unsigned short *) dst)[0] =
            (((unsigned short *) src)[0] * 65535) / src[3];
          ((unsigned short *) dst)[1] =
            (((unsigned short *) src)[1] * 65535) / src[3];
          ((unsigned short *) dst)[2] =
            (((unsigned short *) src)[2] * 65535) / src[3];
        }
      ((unsigned short *) dst)[3] = ((unsigned short *) src)[3];
      dst                        += 8;
      src                        += 8;
    }
  return samples;
}

static INLINE long
conv_rgb8_rgbP8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[0];
      dst[1] = src[1];
      dst[2] = src[2];
      src   += 3;
      dst   += 4;
    }
  return samples;
}

static INLINE long
conv_rgb8_rgba8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      /**(unsigned int *) dst = *(unsigned int *) src;
         dst[3] = 255;*/

      dst[0] = src[0];
      dst[1] = src[1];
      dst[2] = src[2];
      dst[3] = 255;
      src   += 3;
      dst   += 4;
    }
  return samples;
}

#define conv_rgb8_rgbA8    conv_rgb8_rgba8

static INLINE long
conv_rgbP8_rgba8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      *(unsigned int *) dst = *(unsigned int *) src;
      dst[3]                = 255;
      src                  += 4;
      dst                  += 4;
    }
  return samples;
}

#define conv_rgbP8_rgbA8    conv_rgbP8_rgba8

static INLINE long
conv_rgba8_rgbP8 (unsigned char *src, unsigned char *dst, long samples)
{
  memcpy (dst, src, samples * 4);
  return samples;
}

#define conv_rgbA8_rgbP8    conv_rgba8_rgbP8

static INLINE long
conv_rgbP8_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[0];
      dst[1] = src[1];
      dst[2] = src[2];
      src   += 4;
      dst   += 3;
    }
  return samples;
}

static INLINE long
conv_rgba8_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      dst[0] = src[0];
      dst[1] = src[1];
      dst[2] = src[2];
      src   += 4;
      dst   += 3;
    }
  return samples;
}

static INLINE long
conv_rgbA8_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int alpha = src[3];
      if (alpha == 255)
        {
          *dst++ = src[0];
          *dst++ = src[1];
          *dst++ = src[2];
        }
      else if (alpha == 0)
        {
          *dst++ = 0;
          *dst++ = 0;
          *dst++ = 0;
        }
      else
        {
          unsigned int aa = (255 << 16) / alpha;
          *dst++ = (src[0] * aa) >> 16;
          *dst++ = (src[1] * aa) >> 16;
          *dst++ = (src[2] * aa) >> 16;
        }
      src += 4;
    }
  return samples;
}

#ifndef byteclamp
#define byteclamp(j)                   do { if (j < 0) j = 0;else if (j > 255) j = 255; } while (0)
#endif

#define YUV82RGB8(Y, U, V, R, G, B)    do { \
      R = ((Y << 15) + 37355 * (V - 128)) >> 15; \
      G = ((Y << 15) - 12911 * (U - 128) - 19038 * (V - 128)) >> 15; \
      B = ((Y << 15) + 66454 * (U - 128)) >> 15; \
      byteclamp (R); \
      byteclamp (G); \
      byteclamp (B); \
    } while (0)

#define RGB82YUV8(R, G, B, Y, U, V)    do { \
      Y = ((9798 * R + 19234 * G + 3736 * B) >> 15) + 000; \
      U = ((-4817 * R - 9470 * G + 14320 * B) >> 15) + 128; \
      V = ((20152 * R - 16875 * G - 3277 * B) >> 15) + 128; \
      byteclamp (Y); \
      byteclamp (U); \
      byteclamp (V); \
    } while (0)

static INLINE long
conv_yuv8_rgb8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int R, G, B;

      YUV82RGB8 (src[0], src[1], src[2], R, G, B);
      dst[0] = R;
      dst[1] = G;
      dst[2] = B;
      src   += 3;
      dst   += 3;
    }
  return samples;
}

static INLINE long
conv_rgb8_yuv8 (unsigned char *src, unsigned char *dst, long samples)
{
  long n = samples;

  while (n--)
    {
      int Y, U, V;

      YUV82RGB8 (src[0], src[1], src[2], Y, U, V);
      dst[0] = Y;
      dst[1] = U;
      dst[2] = V;
      src   += 3;
      dst   += 3;
    }
  return samples;
}

static INLINE long
conv_rgbaF_yuvaF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      float R, G, B;
      float Y, U, V;

      R = src_f[0];
      G = src_f[1];
      B = src_f[2];

      Y = 0.299 * R + 0.587 * B + 0.114 * B;
      U = (-0.1687) * R - 0.3313 * G + 0.5 * B /* +0.5 */;
      V = 0.5 * R - 0.4187 * G - 0.0813 * B /* +0.5 */;

      dst_f[0] = Y;
      dst_f[1] = U;
      dst_f[2] = V;
      dst_f[3] = src_f[3];

      dst_f += 4;
      src_f += 4;
    }
  return samples;
}

static INLINE long
conv_yuvaF_rgbaF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      float Y, U, V;
      float R, G, B;

      Y = src_f[0];
      U = src_f[1];
      V = src_f[2];

      R = Y + 1.40200 * (V /*-0.5*/);
      G = Y - 0.34414 * (U /*-0.5*/) -0.71414 * (V /*-0.5*/);
      B = Y + 1.77200 * (U /*-0.5*/);

      dst_f[0] = R;
      dst_f[1] = G;
      dst_f[2] = B;
      dst_f[3] = src_f[3];

      dst_f += 4;
      src_f += 4;
    }
  return samples;
}



static INLINE long
conv_rgbF_yuvF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      float R, G, B;
      float Y, U, V;

      R = src_f[0];
      G = src_f[1];
      B = src_f[2];

      Y = 0.299 * R + 0.587 * B + 0.114 * B;
      U = (-0.1687) * R - 0.3313 * G + 0.5 * B /* +0.5 */;
      V = 0.5 * R - 0.4187 * G - 0.0813 * B /* +0.5 */;

      dst_f[0] = Y;
      dst_f[1] = U;
      dst_f[2] = V;

      dst_f += 3;
      src_f += 3;
    }
  return samples;
}

static INLINE long
conv_yuvF_rgbF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      float Y, U, V;
      float R, G, B;

      Y = src_f[0];
      U = src_f[1];
      V = src_f[2];

      R = Y + 1.40200 * (V /*-0.5*/);
      G = Y - 0.34414 * (U /*-0.5*/) -0.71414 * (V /*-0.5*/);
      B = Y + 1.77200 * (U /*-0.5*/);

      dst_f[0] = R;
      dst_f[1] = G;
      dst_f[2] = B;

      dst_f += 3;
      src_f += 3;
    }
  return samples;
}


/******* lab, xyz and rgb interaction, lifted from cpercep by adam d. moss */

/*
   static const double Xn = 0.951;
   static const double Yn = 1.0;
   static const double Zn = 1.089;
 */

#define LXN    0.312713F
#define LYN    0.329016F

static const double lxn = LXN;
static const double lyn = LYN;
static double       xnn = LXN / LYN;
static double       znn = (1.0F - (LXN + LYN)) / LYN;


static const double LRAMP = 7.99959199F;

static INLINE long
conv_rgbF_xyzF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      float x, y, z;
      float red, green, blue;

      red   = src_f[0];
      green = src_f[1];
      blue  = src_f[2];

      x = 0.431 * red + 0.342 * green + 0.179 * blue;
      y = 0.222 * red + 0.707 * green + 0.071 * blue;
      z = 0.020 * red + 0.130 * green + 0.939 * blue;

      dst_f[0] = x;
      dst_f[1] = y;
      dst_f[2] = z;
      dst_f   += 3;
      src_f   += 3;
    }
  return samples;
}

static INLINE long
conv_xyzF_rgbF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      dst_f[0] = src_f[0];
      dst_f[1] = src_f[1];
      dst_f[2] = src_f[2];
      dst_f   += 3;
      src_f   += 3;
    }
  return samples;
}

static INLINE double
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


static INLINE double
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

static INLINE long
conv_labF_xyzF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      float P;
      float X, Y, Z;
      float L, a, b;

      L = src_f[0];
      a = src_f[1];
      b = src_f[2];

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


      dst_f[0] = X;
      dst_f[1] = Y;
      dst_f[2] = Z;
      dst_f   += 3;
      src_f   += 3;
    }
  return samples;
}

static INLINE long
conv_xyzF_labF (unsigned char *src, unsigned char *dst, long samples)
{
  float *src_f = (float *) src;
  float *dst_f = (float *) dst;
  long   n     = samples;

  while (n--)
    {
      double ffuncY;
      float  X, Y, Z;
      float  L, a, b;
      X = src_f[0];
      Y = src_f[1];
      Z = src_f[2];

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
        }
      else
        {
          L = 0.0;
        }

      ffuncY = ffunc (Y);
      a      = 500.0F * (ffunc (X / xnn) - ffuncY);
      b      = 200.0F * (ffuncY - ffunc (Z / znn));

      dst_f[0] = L;
      dst_f[1] = a;
      dst_f[2] = b;
      dst_f   += 3;
      src_f   += 3;
    }
  return samples;
}

/******* end of cpercep lift out **/


#define MAX_CONVERSIONS    100

typedef struct Conversion
{
  int from_fmt;
  int to_fmt;
  int cost;               /* cost of function,. calculated as ms to convert 1024*1024 samples */
  int loss;                     /* 0 maps ok, 1 precision loss, 2 alpha loss, 4 channel loss
                                   (should be dwelled further into) */
  void (*function)(unsigned char *src, unsigned char *dst, long samples);
} Conversion;

int init (void);

int
init (void)
{
  Babl *rgbaD = babl_format_new (
    babl_model ("R'G'B'A"),
    babl_type ("double"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    babl_component ("A"),
    NULL);
  Babl *rgbaF = babl_format_new (
    babl_model ("R'G'B'A"),
    babl_type ("float"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    babl_component ("A"),
    NULL);
  Babl *rgba16 = babl_format_new (
    babl_model ("R'G'B'A"),
    babl_type ("u16"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    babl_component ("A"),
    NULL);
  Babl *rgba8 = babl_format_new (
    babl_model ("R'G'B'A"),
    babl_type ("u8"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    babl_component ("A"),
    NULL);
  Babl *rgbAF = babl_format_new (
    babl_model ("R'aG'aB'aA"),
    babl_type ("float"),
    babl_component ("R'a"),
    babl_component ("G'a"),
    babl_component ("B'a"),
    babl_component ("A"),
    NULL);
  Babl *rgbA16 = babl_format_new (
    babl_model ("R'aG'aB'aA"),
    babl_type ("u16"),
    babl_component ("R'a"),
    babl_component ("G'a"),
    babl_component ("B'a"),
    babl_component ("A"),
    NULL);
  Babl *rgbA8 = babl_format_new (
    babl_model ("R'aG'aB'aA"),
    babl_type ("u8"),
    babl_component ("R'a"),
    babl_component ("G'a"),
    babl_component ("B'a"),
    babl_component ("A"),
    NULL);
  Babl *rgbF = babl_format_new (
    babl_model ("R'G'B'"),
    babl_type ("float"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    NULL);
  Babl *rgb16 = babl_format_new (
    babl_model ("R'G'B'"),
    babl_type ("u16"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    NULL);
  Babl *rgb8 = babl_format_new (
    babl_model ("R'G'B'"),
    babl_type ("u8"),
    babl_component ("R'"),
    babl_component ("G'"),
    babl_component ("B'"),
    NULL);
  Babl *gaF = babl_format_new (
    babl_model ("Y'A"),
    babl_type ("float"),
    babl_component ("Y'"),
    babl_component ("A"),
    NULL);
  Babl *gAF = babl_format_new (
    babl_model ("Y'aA"),
    babl_type ("float"),
    babl_component ("Y'a"),
    babl_component ("A"),
    NULL);
  Babl *gF = babl_format_new (
    babl_model ("Y'"),
    babl_type ("float"),
    babl_component ("Y'"),
    NULL);
  Babl *ga16 = babl_format_new (
    babl_model ("Y'A"),
    babl_type ("u16"),
    babl_component ("Y'"),
    babl_component ("A"),
    NULL);
  Babl *gA16 = babl_format_new (
    babl_model ("Y'aA"),
    babl_type ("u16"),
    babl_component ("Y'a"),
    babl_component ("A"),
    NULL);
  Babl *g16 = babl_format_new (
    babl_model ("Y'"),
    babl_type ("u16"),
    babl_component ("Y'"),
    NULL);
  Babl *ga8 = babl_format_new (
    babl_model ("Y'A"),
    babl_type ("u8"),
    babl_component ("Y'"),
    babl_component ("A"),
    NULL);
  Babl *gA8 = babl_format_new (
    babl_model ("Y'aA"),
    babl_type ("u8"),
    babl_component ("Y'a"),
    babl_component ("A"),
    NULL);
  Babl *g8 = babl_format_new (
    babl_model ("Y'"),
    babl_type ("u8"),
    babl_component ("Y'"),
    NULL);
  Babl *yuv8 = babl_format_new (
    "name", "Y'CbCr u8",
    "planar",
    babl_model ("Y'CbCr"),
    babl_type ("u8-luma"),
    babl_sampling (1, 1),
    babl_component ("Y'"),
    babl_type ("u8-chroma"),
    babl_sampling (2, 2),
    babl_component ("Cb"),
    babl_component ("Cr"),
    NULL);
  Babl *yuvF = babl_format_new (
    babl_model ("Y'CbCr"),
    babl_type ("float"),
    babl_component ("Y'"),
    babl_type ("float"),
    babl_component ("Cb"),
    babl_component ("Cr"),
    NULL);
  Babl *yuvaF = babl_format_new (
    babl_model ("Y'CbCrA"),
    babl_type ("float"),
    babl_component ("Y'"),
    babl_type ("float"),
    babl_component ("Cb"),
    babl_component ("Cr"),
    babl_component ("A"),
    NULL);

#define o(src, dst) \
  babl_conversion_new (src, dst, "linear", conv_ ## src ## _ ## dst, NULL)

  o (rgbaF, rgba8);
  o (rgba8, rgbaF);
  o (rgbaF, rgba16);
  o (rgba16, rgbaF);
  o (rgbAF, rgbA8);
  o (rgbA8, rgbAF);
  o (rgbAF, rgbA16);
  o (rgbA16, rgbAF);
  o (rgbF, rgb8);
  o (rgb8, rgbF);
  o (rgbF, rgb16);
  o (rgb16, rgbF);
  o (rgba8, rgba16);
  o (rgba16, rgba8);
  o (rgbA8, rgbA16);
  o (rgbA16, rgbA8);
  o (rgb8, rgb16);
  o (rgb16, rgb8);
  o (gaF, ga8);
  o (gAF, gA8);
  o (gF, g8);
  o (ga8, gaF);
  o (gA8, gAF);
  o (g8, gF);
  o (gaF, ga16);
  o (gAF, gA16);
  o (gF, g16);
  o (ga16, gaF);
  o (gA16, gAF);
  o (g16, gF);
  o (ga16, ga8);
  o (g16, g8);
  o (rgbaF, rgbAF);
  o (rgbAF, rgbaF);
  o (yuv8, rgb8);
  o (rgb8, yuv8);
  o (yuvF, rgbF);
  o (rgbF, yuvF);
  o (yuvaF, rgbaF);
  o (rgbA8, rgbA16);
  o (rgb8, rgb16);
  o (ga8, ga16);
  o (gA8, gA16);
  o (g8, g16);
  o (rgba8, rgbaF);
  o (rgbA8, rgbAF);
  o (rgb8, rgbF);
  o (ga8, gaF);
  o (gA8, gAF);
  o (g8, gF);
  o (rgba16, rgbaF);
  o (rgbA16, rgbAF);
  o (rgb16, rgbF);
  o (ga16, gaF);
  o (gA16, gAF);
  o (g16, gF);
  o (rgbaF, rgbAF);
  o (rgbAF, rgbaF);
  o (gaF, gAF);
  o (gAF, gaF);
  o (rgbaF, rgbF);
  o (rgbAF, rgbF);
  o (gaF, gF);
  o (gAF, gF);
  o (rgbF, rgbaF);
  o (rgbF, rgbAF);
  o (gF, gaF);
  o (gF, gAF);
  o (rgbF, gF);
  o (gF, rgbF);
  o (rgbaF, gaF);
  o (gaF, rgbaF);
  o (rgbAF, gAF);
  o (gAF, rgbAF);
  o (rgbaF, rgb8);
  o (rgbA8, rgbaF);
  o (rgbA8, rgbAF);
  o (ga8, gaF);
  o (gA8, gAF);
  o (rgbA8, rgba8);
  o (rgba8, rgbA8);
  o (rgbA16, rgba16);
  o (gAF, rgbAF);
  o (rgbaF, g8);
  o (rgbaF, rgb16);
  o (rgb8, rgba8);
  o (rgb8, rgbA8);
  o (rgbA8, rgb8);
  o (rgba8, rgb8);
  o (rgbaF, rgbA8);
  o (rgbaF, rgbA16);
  o (rgbA16, rgbaF);
  o (yuv8, rgb8);
  o (rgb8, yuv8);
  o (yuvF, rgbF);
  o (rgbF, yuvF);
  o (yuvaF, rgbaF);
  o (rgbaF, yuvaF);
  o (rgbaF, rgbaD);
  o (rgbaD, rgbaF);
  o (rgbaF, rgb8);
  o (rgbAF, rgbF);
#if 0
  o (rgbF, xyzF);
  o (xyzF, rgbF);
  o (labF, xyzF);
  o (xyzF, labF);
#endif

  return 0;
}
