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
#include <math.h>

#include "babl-internal.h"
#include "babl-db.h"

static int babl_format_destruct (void *babl)
{
  BablFormat *format = babl;
  if (format->image_template != NULL)
    {
      babl_set_destructor (format->image_template, NULL);
      /* with no destructor set, the circular reference is no problem */
      babl_free (format->image_template);
      format->image_template = NULL;
    }
  if (format->from_list)
    babl_free (format->from_list);
  return 0;
}

static Babl *
format_new (const char     *name,
            int             id,
            int             planar,
            int             components,
            BablModel      *model,
            BablComponent **component,
            BablSampling  **sampling,
            BablType      **type)
{
  Babl *babl;

  /* i is destination position */
  int i, j, component_found = 0;
  for (i = 0; i < model->components; i++)
    {
      for (j = 0; j < components; j++)
        {
          if (component[j] == model->component[i])
            {
              component_found = 1;
              break;
            }
        }
      if (!component_found)
        {
          component_found = 0;
          babl_fatal ("matching source component for %s in model %s not found",
                      model->component[i]->instance.name, model->instance.name);
        }
    }

  /* allocate all memory in one chunk */
  babl = babl_malloc (sizeof (BablFormat) +
                      strlen (name) + 1 +
                      sizeof (BablComponent *) * (components) +
                      sizeof (BablSampling *) * (components) +
                      sizeof (BablType *) * (components) +
                      sizeof (int) * (components) +
                      sizeof (int) * (components));
  babl_set_destructor (babl, babl_format_destruct);

  babl->format.from_list = NULL;
  babl->format.component = (void *) (((char *) babl) + sizeof (BablFormat));
  babl->format.type      = (void *) (((char *) babl->format.component) + sizeof (BablComponent *) * (components));
  babl->format.sampling  = (void *) (((char *) babl->format.type) + sizeof (BablType *) * (components));
  babl->instance.name    = ((char *) babl->format.sampling) + sizeof (BablSampling *) * (components);

  babl->class_type  = BABL_FORMAT;
  babl->instance.id = id;

  strcpy (babl->instance.name, name);

  babl->format.model      = model;
  babl->format.components = components;

  memcpy (babl->format.component, component, sizeof (BablComponent *) * components);
  memcpy (babl->format.type, type, sizeof (BablType *) * components);
  memcpy (babl->format.sampling, sampling, sizeof (BablSampling *) * components);

  babl->format.planar = planar;

  babl->format.bytes_per_pixel = 0;
  {
    int i;
    for (i = 0; i < components; i++)
      babl->format.bytes_per_pixel += type[i]->bits / 8;
  }

  babl->format.loss = -1.0;
  babl->format.visited = 0;
  babl->format.image_template = NULL;

  return babl;
}


static char *
create_name (BablModel      *model,
             int             components,
             BablComponent **component,
             BablType      **type)
{
  char            buf[512] = "";
  char           *p = &buf[0];
  int             i;
  int             same_types = 1;
  BablType      **t          = type;
  BablType       *first_type = *type;
  BablComponent **c1         = component;
  BablComponent **c2         = model->component;


  sprintf (p, "%s ", model->instance.name);
  p += strlen (model->instance.name) + 1;

  i = components;
  while (i--)
    {
      if (first_type != *t)
        {
          same_types = 0;
          break;
        }
      t++;
    }

  if (same_types &&
      components != model->components)
    same_types = 0;

  i = components;
  while (same_types && i--)
    {
      if (*c1 != *c2)
        {
          same_types = 0;
          break;
        }
      c1++;
      c2++;
    }


  if (same_types)
    {
      sprintf (p, "%s", first_type->instance.name);
      return babl_strdup (buf);
    }

  i = components;

  while (i--)
    {
      sprintf (p, "(%s as %s) ",
               (*component)->instance.name,
               (*type)->instance.name);
      p += strlen ((*component)->instance.name) +
           strlen ((*type)->instance.name) + strlen ("( as ) ");
      component++;
      type++;
    }
  return babl_strdup (buf);
}


static char *
ncomponents_create_name (Babl *type,
                         int   components)
{
  char buf[512];
  sprintf (buf, "%s[%i] ", type->instance.name, components);
  return babl_strdup (buf);
}

Babl *
babl_format_n (Babl *btype,
               int   components)
{
  int            i;
  Babl          *babl;
  int            id         = 0;
  int            planar     = 0;
  BablModel     *model      = (BablModel *)babl_model ("Y");
  BablComponent *component [components];
  BablSampling  *sampling  [components];
  BablType      *type      [components];
  char          *name       = NULL;

  for (i = 0; i<components; i++)
    {
      component[i] = model->component[0];
      type[i] = &btype->type;
      sampling[i] = (BablSampling *) babl_sampling (1, 1);
    }

  name = ncomponents_create_name (btype, components);
  babl = babl_db_exist (db, id, name);
  if (babl)
    {
      /* There is an instance already registered by the required id/name,
       * returning the preexistent one instead.
       */
      babl_free (name);
      return babl;
    }

  babl = format_new (name,
                     id,
                     planar, components, model,
                     component, sampling, type);

  babl_db_insert (db, babl);
  babl_free (name);
  return babl;
}

static int
is_format_duplicate (Babl           *babl,
                     int             planar,
                     int             components,
                     BablModel      *model,
                     BablComponent **component,
                     BablSampling  **sampling,
                     BablType      **type)
{
  int i;

  if (babl->format.planar     != planar     ||
      babl->format.components != components ||
      babl->format.model      != model)
    return 0;

  for (i = 0; i < components; i++)
    {
      if (babl->format.component[i] != component[i] ||
          babl->format.sampling[i]  != sampling[i]  ||
          babl->format.type[i]      != type[i])
        return 0;
    }
  return 1;
}

Babl *
babl_format_new (void *first_arg,
                 ...)
{
  va_list        varg;
  Babl          *babl;
  int            id         = 0;
  int            planar     = 0;
  int            components = 0;
  BablModel     *model      = NULL;
  BablComponent *component [BABL_MAX_COMPONENTS];
  BablSampling  *sampling  [BABL_MAX_COMPONENTS];
  BablType      *type      [BABL_MAX_COMPONENTS];

  BablSampling  *current_sampling = (BablSampling *) babl_sampling (1, 1);
  BablType      *current_type     = (BablType *) babl_type_from_id (BABL_DOUBLE);
  char          *name             = NULL;
  void          *arg              = first_arg;

  va_start (varg, first_arg);

  while (1)
    {
      if (BABL_IS_BABL (arg))
        {
          Babl *babl = (Babl *) arg;

          switch (babl->class_type)
            {
              case BABL_TYPE:
              case BABL_TYPE_FLOAT:
              case BABL_TYPE_INTEGER:
                current_type = (BablType *) babl;
                break;

              case BABL_COMPONENT:
                if (!model)
                  {
                    babl_fatal ("no model specified before component %s",
                                babl->instance.name);
                  }
                component [components] = (BablComponent *) babl;
                type      [components] = current_type;
                sampling  [components] = current_sampling;
                components++;

                if (components >= BABL_MAX_COMPONENTS)
                  {
                    babl_fatal ("maximum number of components (%i) exceeded for %s",
                                BABL_MAX_COMPONENTS, name);
                  }
                break;

              case BABL_SAMPLING:
                current_sampling = (BablSampling *) arg;
                break;

              case BABL_MODEL:
                if (model)
                  {
                    babl_log ("args=(%s): model %s already requested",
                              babl->instance.name, model->instance.name);
                  }
                model = (BablModel *) arg;
                break;

              case BABL_INSTANCE:
              case BABL_FORMAT:
              case BABL_CONVERSION:
              case BABL_CONVERSION_LINEAR:
              case BABL_CONVERSION_PLANE:
              case BABL_CONVERSION_PLANAR:
              case BABL_FISH:
              case BABL_FISH_REFERENCE:
              case BABL_FISH_SIMPLE:
              case BABL_FISH_PATH:
              case BABL_IMAGE:
              case BABL_EXTENSION:
                babl_log ("%s unexpected",
                          babl_class_name (babl->class_type));
                break;

              case BABL_SKY: /* shut up compiler */
                break;
            }
        }
      /* if we didn't point to a babl, we assume arguments to be strings */
      else if (!strcmp (arg, "id"))
        {
          id = va_arg (varg, int);
        }

      else if (!strcmp (arg, "name"))
        {
          name = babl_strdup (va_arg (varg, char *));
        }

      else if (!strcmp (arg, "packed"))
        {
          planar = 0;
        }

      else if (!strcmp (arg, "planar"))
        {
          planar = 1;
        }

      else
        {
          babl_fatal ("unhandled argument '%s' for format '%s'", arg, name);
        }

      arg = va_arg (varg, char *);
      if (!arg)
        break;
    }

  va_end (varg);

  if (!name)
    name = create_name (model, components, component, type);

  babl = babl_db_exist (db, id, name);
  if (id && !babl && babl_db_exist (db, 0, name))
    babl_fatal ("Trying to reregister BablFormat '%s' with different id!", name);

  if (babl)
    {
      /* There is an instance already registered by the required id/name,
       * returning the preexistent one instead if it doesn't differ.
       */
      if (!is_format_duplicate (babl, planar, components, model,
                                component, sampling, type))
        babl_fatal ("BablFormat '%s' already registered "
                    "with different content!", name);

      babl_free (name);
      return babl;
    }

  babl = format_new (name,
                     id,
                     planar, components, model,
                     component, sampling, type);

  babl_db_insert (db, babl);
  babl_free (name);
  return babl;
}

int
babl_formats_count (void)
{
  return babl_db_count (db);
}

int
babl_format_has_alpha (const Babl *format)
{
  int n = babl_format_get_n_components (format);
  int i;

  for (i = 0; i < n; i++)
    {
      if (format->format.component[i]->alpha)
        {
          return 1;
        }
    }

  return 0;
}

int
babl_format_get_bytes_per_pixel (const Babl *format)
{
  if (format->class_type == BABL_FORMAT)
    {
      return format->format.bytes_per_pixel;
    }

  return 0;
}

int
babl_format_get_n_components (const Babl *format)
{
  if (format->class_type == BABL_FORMAT)
    {
      return format->format.components;
    }

  return 0;
}

Babl *
babl_format_get_type (const Babl *format,
                      int         component_index)
{
  if (format->class_type == BABL_FORMAT &&
      component_index >= 0 &&
      component_index < format->format.components)
    {
      return (Babl *)format->format.type[component_index];
    }

  return NULL;
}

Babl *
babl_format_with_model_as_type (Babl *model,
                                Babl *type)
{
  BablComponent *component[10];
  int            i;

  for (i = 0; i < model->model.components; i++)
    {
      component[i] = model->model.component[i];
    }
  component[i] = NULL;

  return babl_format_new (
           model,
           type,
           component[0],
           component[1],
           component[2],
           component[3],
           component[4],
           component[5],
           component[6],
           component[7],
           component[8],
           component[9],
           NULL
  );
}

#define test_pixels    256

static double *
test_create (void)
{
  double *test;
  int     i;

  srandom (20050728);

  test = babl_malloc (sizeof (double) * test_pixels * 4);

  for (i = 0; i < test_pixels * 4; i++)
    test [i] = (double) random () / RAND_MAX;

  return test;
}

double
babl_format_loss (Babl *babl)
{
  double  loss = 0.0;
  double *test;
  void   *original;
  double *clipped;
  void   *destination;
  double *transformed;

  Babl   *ref_fmt;
  Babl   *fmt;
  Babl   *fish_to;
  Babl   *fish_from;

  ref_fmt = babl_format_new (
    babl_model ("RGBA"),
    babl_type ("double"),
    babl_component ("R"),
    babl_component ("G"),
    babl_component ("B"),
    babl_component ("A"),
    NULL);

  if (babl->format.loss != -1.0)
    return babl->format.loss;

  fmt       = babl;
  fish_to   = babl_fish_reference (ref_fmt, fmt);
  fish_from = babl_fish_reference (fmt, ref_fmt);

  test        = test_create ();
  original    = babl_calloc (test_pixels, fmt->format.bytes_per_pixel);
  clipped     = babl_calloc (test_pixels, ref_fmt->format.bytes_per_pixel);
  destination = babl_calloc (test_pixels, fmt->format.bytes_per_pixel);
  transformed = babl_calloc (test_pixels, ref_fmt->format.bytes_per_pixel);

  babl_process (fish_to, test, original, test_pixels);
  babl_process (fish_from, original, clipped, test_pixels);
  babl_process (fish_to, clipped, destination, test_pixels);
  babl_process (fish_from, destination, transformed, test_pixels);

  loss = babl_rel_avg_error (clipped, test, test_pixels * 4);

  fish_to->fish.processings   -= 2;
  fish_from->fish.processings -= 2;
  fish_to->fish.pixels        -= test_pixels * 2;
  fish_from->fish.pixels      -= test_pixels * 2;

  babl_free (original);
  babl_free (clipped);
  babl_free (destination);
  babl_free (transformed);
  babl_free (test);

  babl->format.loss = loss;
  return loss;
}

BABL_CLASS_IMPLEMENT (format)
