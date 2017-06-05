/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005-2008, Øyvind Kolås and others.
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

#ifndef _BABL_CONVERSION_H
#define _BABL_CONVERSION_H

BABL_CLASS_DECLARE (conversion);

Babl * babl_conversion (const char *name);



/* Signature of functions registered for reference type
 * conversions,
 */
typedef long (*BablFuncPlane)     (char  *src,
                                   char  *dst,
                                   int    src_pitch,
                                   int    dst_pitch,
                                   long   n);


typedef struct
BablConversion {
  BablInstance           instance;
  const Babl            *source;
  const Babl            *destination;
  long                   cost;
  double                 error;
  union
    {
      BablFuncLinear     linear;
      BablFuncPlane      plane;
      BablFuncPlanar     planar;
    } function;
  int                    processings;
  long                   pixels;
} BablConversion;

#endif
