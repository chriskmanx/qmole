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

#if 0
#define CONTEMPORARY_MONITOR
#endif

#ifdef CONTEMPORARY_MONITOR
  /* source: http://www.poynton.com/ColorFAQ.html */
  #define RGB_LUMINANCE_RED    (0.212671)
  #define RGB_LUMINANCE_GREEN  (0.715160)
  #define RGB_LUMINANCE_BLUE   (0.072169)
#else
  /* this is not correct, but the constants are kept around */
  #define RGB_LUMA_RED         (0.299)
  #define RGB_LUMA_GREEN       (0.587)
  #define RGB_LUMA_BLUE        (0.114)
  #define RGB_LUMINANCE_RED    RGB_LUMA_RED
  #define RGB_LUMINANCE_GREEN  RGB_LUMA_GREEN
  #define RGB_LUMINANCE_BLUE   RGB_LUMA_BLUE
#endif
