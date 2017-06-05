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

#ifndef _BABL_IDS_H
#define _BABL_IDS_H

enum {
  BABL_UNDEFINED = 0,
  BABL_TYPE_BASE = 100,
  BABL_U8,
  BABL_U16,
  BABL_U32,
  BABL_FLOAT,
  BABL_DOUBLE,
  BABL_HALF_FLOAT,

  BABL_U8_LUMA,
  BABL_U8_CHROMA,
  BABL_U16_CIE_L,
  BABL_U16_CIE_AB,
  BABL_U8_CIE_L,
  BABL_U8_CIE_AB,
  BABL_TYPE_LAST_INTERNAL,

  BABL_MODEL_BASE = 1000,
  BABL_GRAY,
  BABL_GRAY_ALPHA,
  BABL_GRAY_ALPHA_PREMULTIPLIED,
  BABL_RGB,
  BABL_RGBA,
  BABL_RGBA_PREMULTIPLIED,
  BABL_GRAY_GAMMA_2_2,
  BABL_GRAY_GAMMA_2_2_ALPHA,
  BABL_GRAY_GAMMA_2_2_ALPHA_PREMULTIPLIED,
  BABL_RGB_GAMMA_2_2,
  BABL_RGBA_GAMMA_2_2,
  BABL_RGBA_GAMMA_2_2_PREMULTIPLIED,
  BABL_YCBCR,
  BABL_YCBCR_ALPHA,
  BABL_CIE_LAB,
  BABL_CIE_LAB_ALPHA,
  BABL_MODEL_LAST_INTERNAL,

  BABL_COMPONENT_BASE = 10000,
  BABL_LUMINANCE,
  BABL_LUMINANCE_MUL_ALPHA,
  BABL_RED,
  BABL_GREEN,
  BABL_BLUE,
  BABL_ALPHA,
  BABL_RED_MUL_ALPHA,
  BABL_GREEN_MUL_ALPHA,
  BABL_BLUE_MUL_ALPHA,
  BABL_LUMA,
  BABL_LUMA_MUL_ALPHA,
  BABL_RED_GAMMA_2_2,
  BABL_GREEN_GAMMA_2_2,
  BABL_BLUE_GAMMA_2_2,
  BABL_RED_GAMMA_2_2_MUL_ALPHA,
  BABL_GREEN_GAMMA_2_2_MUL_ALPHA,
  BABL_BLUE_GAMMA_2_2_MUL_ALPHA,


  BABL_X,
  BABL_Y,
  BABL_Z,
  BABL_CIE_L,
  BABL_CIE_A,
  BABL_CIE_B,
  BABL_CB,
  BABL_CR,
  BABL_PADDING,
  BABL_COMPONENT_LAST_INTERNAL,

  BABL_FORMAT_BASE = 100000,
  BABL_SRGB,
  BABL_SRGBA,
  BABL_RGB_FLOAT,
  BABL_RGBA_FLOAT,
  BABL_RGBA_DOUBLE,
  BABL_LAB_FLOAT,
  BABL_LAB_U16,
  BABL_LAB_U8,
  BABL_RGB_U8,
  BABL_RGBA_U8,
  BABL_RGBA_U16,
  BABL_CMYK_FLOAT,
  BABL_CMYK_ALPHA_FLOAT,
  BABL_YCBCR411,
  BABL_YCBCR422,
  BABL_YCBCR420,
  BABL_FORMAT_LAST_INTERNAL,

  BABL_PIXEL_USER_BASE
};


#endif



