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
#include <string.h>
#include <stdarg.h>

#include "babl-internal.h"

static int babl_image_destruct (void *babl)
{
  BablFormat *format = BABL (babl)->image.format;
  if (format && format->image_template == NULL)
    {
      format->image_template = babl;
      return -1; /* this should avoid freeing images created for formats,. */
    }
  return 0;
}

static Babl *
image_new (BablFormat     *format,
           BablModel      *model,
           int             components,
           BablComponent **component,
           BablSampling  **sampling,
           BablType      **type,
           char          **data,
           int            *pitch,
           int            *stride)
{
  Babl *babl;

  /* allocate all memory in one chunk */
  babl  = babl_malloc (sizeof (BablImage) +
                       sizeof (BablComponent*) * (components) +
                       sizeof (BablSampling*)  * (components) +
                       sizeof (BablType*)      * (components) +
                       sizeof (void*)          * (components) +
                       sizeof (int)            * (components) +
                       sizeof (int)            * (components));
  babl_set_destructor (babl, babl_image_destruct);
  babl->image.component = (void*)(((char *)babl)                  + sizeof (BablImage));
  babl->image.sampling  = (void*)(((char *)babl->image.component) + sizeof (BablComponent*) * (components));
  babl->image.type      = (void*)(((char *)babl->image.sampling)  + sizeof (BablSampling*)  * (components));
  babl->image.data      = (void*)(((char *)babl->image.type)      + sizeof (BablType*)      * (components));
  babl->image.pitch     = (void*)(((char *)babl->image.data)      + sizeof (void*)          * (components));
  babl->image.stride    = (void*)(((char *)babl->image.pitch)     + sizeof (int)            * (components));

  babl->class_type       = BABL_IMAGE;
  babl->instance.id      = 0;
  babl->instance.name    = "slaritbartfast";
  babl->image.format     = format;
  babl->image.model      = model;
  babl->image.components = components;

  memcpy (babl->image.component, component, components * sizeof (void *));
  memcpy (babl->image.type, type, components * sizeof (void *));
  memcpy (babl->image.data, data, components * sizeof (void *));
  memcpy (babl->image.pitch, pitch, components * sizeof (int));
  memcpy (babl->image.stride, stride, components * sizeof (int));

  return babl;
}

Babl *
babl_image_from_linear (char *buffer,
                        Babl *format)
{
  Babl          *babl;
  BablModel     *model      = NULL;
  int            components = 0;
  int            i;
  BablComponent *component [BABL_MAX_COMPONENTS];
  BablSampling  *sampling  [BABL_MAX_COMPONENTS];
  BablType      *type      [BABL_MAX_COMPONENTS];
  char          *data      [BABL_MAX_COMPONENTS];
  int            pitch     [BABL_MAX_COMPONENTS];
  int            stride    [BABL_MAX_COMPONENTS];

  int            offset     = 0;
  int            calc_pitch = 0;

  babl_assert (format);
  babl_assert (format->class_type == BABL_FORMAT ||
               format->class_type == BABL_MODEL);

  switch (format->class_type)
    {
      case BABL_FORMAT:
        components = format->format.components;
        if (format->format.image_template != NULL) /* single item cache for speeding
                                                      up subsequent use of linear buffers
                                                      for subsequent accesses
                                                    */
          {
            babl = format->format.image_template;
            format->format.image_template = NULL;
            for (i = 0; i < components; i++)
              {
                babl->image.data[i] = buffer + offset;
                offset   += (format->format.type[i]->bits / 8);
              }
            return babl;
          }
        model      = (BablModel *) format->format.model;

        memcpy (component, format->format.component, sizeof (Babl *) * components);
        memcpy (sampling, format->format.sampling, sizeof (Babl *) * components);
        memcpy (type, format->format.type, sizeof (Babl *) * components);

        for (i = 0; i < components; i++)
          {
            calc_pitch += (type[i]->bits / 8);
          }
        for (i = 0; i < components; i++)
          {
            pitch[i]  = calc_pitch;
            stride[i] = 0;
            data[i]   = buffer + offset;
            offset   += (type[i]->bits / 8);
          }
        break;

      case BABL_MODEL:
        model      = (BablModel *) format;
        components = format->format.components;
        for (i = 0; i < components; i++)
          {
            calc_pitch += (64 / 8);   /*< known to be double when we create from model */
          }
        memcpy (component, model->component, sizeof (Babl *) * components);
        for (i = 0; i < components; i++)
          {
            sampling[i] = (BablSampling *) babl_sampling (1, 1);
            type[i]     = (BablType *) babl_type_from_id (BABL_DOUBLE);
            pitch[i]    = calc_pitch;
            stride[i]   = 0;
            data[i]     = buffer + offset;
            offset     += (type[i]->bits / 8);
          }
        break;

      default:
        babl_log ("Eeeek!");
        break;
    }

  babl = image_new (
    ((void*)format!=(void*)model)?(BablFormat*)format:NULL,
    model, components,
    component, sampling, type, data, pitch, stride);
  return babl;
}

Babl *
babl_image_new (void *first,
                ...)
{
  va_list        varg;
  Babl          *babl;
  int            components = 0;
  BablFormat    *format     = NULL;
  BablModel     *model      = NULL;
  BablComponent *component [BABL_MAX_COMPONENTS];
  BablSampling  *sampling  [BABL_MAX_COMPONENTS];
  BablType      *type      [BABL_MAX_COMPONENTS];
  char          *data      [BABL_MAX_COMPONENTS];
  int            pitch     [BABL_MAX_COMPONENTS];
  int            stride    [BABL_MAX_COMPONENTS];

  const char    *arg = first;

  va_start (varg, first);

  while (1)
    {
      BablComponent *new_component = NULL;
      if (!arg)
        break;

      if (BABL_IS_BABL (arg))
        {
          Babl *babl = (Babl *) arg;

          if (babl->class_type == BABL_COMPONENT)
            {
              new_component = (BablComponent *) babl;
            }
          else
            {
              babl_log ("%s unexpected", babl_class_name (babl->class_type));
              va_end (varg);
              return NULL;
            }
        }
      else
        {
          new_component = (BablComponent *) babl_component (arg);
        }

      /* FIXME: add error checking */
      component [components] = new_component;
      sampling  [components] = NULL;
      type      [components] = NULL;
      data      [components] = va_arg (varg, void *);
      pitch     [components] = va_arg (varg, int);
      stride    [components] = va_arg (varg, int);
      components++;

      if (components >= BABL_MAX_COMPONENTS)
        {
          babl_log ("maximum number of components (%i) exceeded", BABL_MAX_COMPONENTS);
        }

      arg = va_arg (varg, char *);
    }

  va_end (varg);


  babl = image_new (format, model, components, component, sampling, type, data, pitch, stride);
  return babl;
}
