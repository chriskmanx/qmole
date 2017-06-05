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

#ifndef _UTIL_H
#define _UTIL_H

#include <assert.h>
#include <math.h>

#ifndef BABL_LIBRARY
#error "config.h must be included prior to util.h"
#endif

#ifndef HAVE_RINT
# define rint(f)  (floor (((double) (f)) + 0.5))
#endif


#define BABL_PLANAR_SANITY  \
  {                         \
    assert(src_bands>0);    \
    assert(dst_bands>0);    \
    assert(src);            \
    assert(*src);           \
    assert(dst);            \
    assert(*dst);           \
    assert(n>0);            \
    assert(*src_pitch);     \
  }

#define BABL_PLANAR_STEP          \
  {                               \
    int i;                        \
    for (i=0; i< src_bands; i++)  \
      src[i]+=src_pitch[i];       \
    for (i=0; i< dst_bands; i++)  \
      dst[i]+=dst_pitch[i];       \
  }

#endif

