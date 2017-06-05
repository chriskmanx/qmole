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

#ifndef _BABL_IMAGE_H
#define _BABL_IMAGE_H


/****************************************************************/
/* BablImage */
BABL_CLASS_DECLARE (image);
/*
 * Babl images can be used for planar buffers instead of linear buffers for
 * babl_process(), BablImages are still experimental, for now BablImages can be
 * passed to babl_process, two different babl_process() functions will be
 * needed for this since the polymorphism cannot be trusted to work on linear
 * buffers that originate outside babl's control.
 *
 * Babl * babl_image_new (BablComponent *component1,
 *                        void          *data,
 *                        int            pitch,
 *                        int            stride,
 *                       [BablComponent *component1,
 *                        void          *data,
 *                        int            pitch,
 *                        int            stride,
 *                        ...]
 *                        NULL);
 */
Babl * babl_image_new  (void *first_component,
                        ...) BABL_ARG_NULL_TERMINATED;

typedef struct
{
  BablInstance    instance;
  BablFormat     *format;    /*< (if known) */
  BablModel      *model;     /*< (always known) */
  int             components;
  BablComponent **component;
  BablType      **type;
  BablSampling  **sampling;
  char          **data;
  int            *pitch;
  int            *stride;
} BablImage;

#endif
