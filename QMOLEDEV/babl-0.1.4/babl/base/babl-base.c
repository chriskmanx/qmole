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

#include "config.h"
#include "babl-base.h"

extern int babl_hmpf_on_name_lookups;

static void types (void);
static void models (void);

void
babl_base_init (void)
{
  babl_hmpf_on_name_lookups++;

  types ();
  models ();
  babl_formats_init ();

  babl_hmpf_on_name_lookups--;
}

void
babl_base_destroy (void)
{
  /* done by the destruction of the elemental babl clases */
}

/*
 * types
 */


static void
types (void)
{
  babl_base_type_float ();
  babl_base_type_u8 ();
  babl_base_type_u16 ();
  babl_base_type_u32 ();
}

/*
 * models
 */


static void
models (void)
{
  babl_base_model_rgb ();
  babl_base_model_gray ();
  babl_base_model_ycbcr ();
}
