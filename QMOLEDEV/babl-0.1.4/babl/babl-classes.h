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

#ifndef _BABL_CLASSES_H
#define _BABL_CLASSES_H

#include "babl-class.h"
#include "babl-db.h"

/* magic number used at the start of all babl objects, used to do
 * differentiation in polymorphic functions. (as well as manual type
 * check assertions).
 */
#define BABL_MAGIC   0xbab100

/* class types */
enum {
  BABL_INSTANCE = BABL_MAGIC,
  BABL_TYPE,
  BABL_TYPE_INTEGER,
  BABL_TYPE_FLOAT,
  BABL_SAMPLING,
  BABL_COMPONENT,
  BABL_MODEL,
  BABL_FORMAT,

  BABL_CONVERSION,
  BABL_CONVERSION_LINEAR,
  BABL_CONVERSION_PLANE,
  BABL_CONVERSION_PLANAR,

  BABL_FISH,
  BABL_FISH_REFERENCE,
  BABL_FISH_SIMPLE,
  BABL_FISH_PATH,
  BABL_IMAGE,

  BABL_EXTENSION,

  BABL_SKY
};

#include "babl-type.h"
#include "babl-sampling.h"
#include "babl-component.h"
#include "babl-model.h"
#include "babl-format.h"
#include "babl-image.h"
#include "babl-conversion.h"
#include "babl-fish.h"
#include "babl-extension.h"


/* This union can be used for convenient access to any field without
 * the need to cast if the variable already is of the type Babl*
 */
typedef union _Babl
{
  BablClassType     class_type;
  BablInstance      instance;
  BablType          type;
  BablSampling      sampling;
  BablComponent     component;
  BablModel         model;
  BablFormat        format;
  BablConversion    conversion;
  BablImage         image;
  BablFish          fish;
  BablFishReference fish_reference;
  BablFishSimple    fish_simple;
  BablFishPath      fish_path;
  BablExtension     extension;
} _Babl;


#endif
