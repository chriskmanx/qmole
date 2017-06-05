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

#ifndef _BABL_TYPES_H
#define _BABL_TYPES_H

#if !defined(BABL_INSIDE_BABL_H) && !defined(BABL_IS_BEING_COMPILED)
#error "babl-version.h must not be included directly, include babl.h instead."
#endif


/**
 * The babl API is based around polymorphism and almost everything is
 * a Babl object.
 */
typedef union _Babl Babl;

/* Conversion function between linear data of a either a data types or
 * color formats.
 */
typedef long (*BablFuncLinear)    (char  *src,
                                   char  *dst,
                                   long   n);

/* TypePlanar,ModelPlanar and FormatPlanar */
typedef long (*BablFuncPlanar)    (int    src_bands,
                                   char  *src[],
                                   int    src_pitch[],
                                   int    dst_bands,
                                   char  *dst[],
                                   int    dst_pitch[],
                                   long   n);

#endif
