/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005-2008, Øyvind Kolås and others.
 *
 * SSE optimized conversion routines.
 * Copyright (C) 2008, Jan Heller.
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

#if defined(__GNUC__) && (__GNUC__ >= 4) && defined(USE_SSE) && defined(USE_MMX)

#include <stdint.h>
#include <stdlib.h>

#include "babl.h"
#include "babl-cpuaccel.h"
#include "extensions/util.h"

#define INLINE inline

typedef float g4float __attribute__ ((vector_size (4*sizeof(float))));
typedef int   g2int   __attribute__ ((vector_size (2*sizeof(int))));

#define g4float(a,b,c,d)  ((g4float){a,b,c,d})
#define g4float_all(val)  g4float(val,val,val,val)
#define g4float_zero      g4float_all(0.0)
#define g4float_ff        g4float_all(255.0)

#define g4float_max(a,b)    __builtin_ia32_maxps(a, b)
#define g4float_min(a,b)    __builtin_ia32_minps(a, b)
#define g4float_cvt2pi(a)   __builtin_ia32_cvtps2pi(a)
#define g4float_movhl(a,b)  __builtin_ia32_movhlps(a, b)
#define g4float_emms        __builtin_ia32_emms


static INLINE long
conv_rgbaF_linear_rgb8_linear (unsigned char *src, 
                               unsigned char *dst, 
                               long           samples)
{
  long n = samples;

  if ((intptr_t) src & 0xF)
    {
      // nonaligned buffers, we have to use fallback x87 code
      float *fsrc = (float *) src;
      int v;

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
    }
  else   
    {
      // all is well, buffers are SSE compatible
      g4float *g4src = (g4float *) src;
      g4float v;

      union {
       g2int si; 
       unsigned char c[8];
      } u;

      while (n--)
        {
           v = *g4src++ * g4float_ff;
           v = g4float_min(v, g4float_ff);
           v = g4float_max(v, g4float_zero);
           u.si = g4float_cvt2pi (v);
           *dst++  = u.c[0];
           *dst++  = u.c[4];
           v = g4float_movhl (v, v);
           u.si = g4float_cvt2pi (v);  
           *dst++  = u.c[0];
        }

      g4float_emms ();
    }

  return samples;
}


static INLINE long
conv_rgbaF_linear_rgba8_linear (unsigned char *src, 
                                unsigned char *dst, 
                                long           samples)
{
  long n = samples;
  if ((intptr_t) src & 0xF)
    {
      // nonaligned buffers, we have to use fallback x87 code
      float *fsrc = (float *) src;
      int v;

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
    }
  else   
    {
      // all is well, buffers are SSE compatible
      g4float *g4src = (g4float *) src;
      g4float v;

      union {
       g2int si; 
       unsigned char c[8];
      } u;

      while (n--)
        {
           v = *g4src++ * g4float_ff;
           v = g4float_min(v, g4float_ff);
           v = g4float_max(v, g4float_zero);
           u.si = g4float_cvt2pi (v);
           *dst++  = u.c[0];
           *dst++  = u.c[4];
           v = g4float_movhl (v, v);
           u.si = g4float_cvt2pi (v);  
           *dst++  = u.c[0];
           *dst++  = u.c[4];
        }

      g4float_emms ();
    }

  return samples;
}

#endif

#define o(src, dst) \
  babl_conversion_new (src, dst, "linear", conv_ ## src ## _ ## dst, NULL)

int init (void);

int
init (void)
{
#if defined(__GNUC__) && (__GNUC__ >= 4) && defined(USE_SSE) && defined(USE_MMX)

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
  Babl *rgb8_linear = babl_format_new (
    babl_model ("RGB"),
    babl_type ("u8"),
    babl_component ("R"),
    babl_component ("G"),
    babl_component ("B"),
    NULL);

  if ((babl_cpu_accel_get_support () & BABL_CPU_ACCEL_X86_MMX) &&
      (babl_cpu_accel_get_support () & BABL_CPU_ACCEL_X86_SSE))
    {
      o (rgbaF_linear, rgb8_linear);
      o (rgbaF_linear, rgba8_linear);
    }

#endif

  return 0;
}

