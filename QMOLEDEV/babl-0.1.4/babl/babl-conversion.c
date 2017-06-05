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

static Babl *
conversion_new (const char    *name,
                int            id,
                Babl          *source,
                Babl          *destination,
                BablFuncLinear linear,
                BablFuncPlane  plane,
                BablFuncPlanar planar)
{
  Babl *babl = NULL;

  babl_assert (source->class_type ==
               destination->class_type);

  babl                = babl_malloc (sizeof (BablConversion) + strlen (name) + 1);
  babl->instance.name = (char *) babl + sizeof (BablConversion);
  strcpy (babl->instance.name, name);

  if (linear)
    {
      babl->class_type                 = BABL_CONVERSION_LINEAR;
      babl->conversion.function.linear = linear;
    }
  else if (plane)
    {
      babl->class_type                = BABL_CONVERSION_PLANE;
      babl->conversion.function.plane = plane;
    }
  else if (planar)
    {
      babl->class_type                 = BABL_CONVERSION_PLANAR;
      babl->conversion.function.planar = planar;
    }
  switch (source->class_type)
    {
      case BABL_TYPE:
        if (linear) /* maybe linear could take a special argument, passed as an
                       additional key/value pair in the constructor. To cast it
                       as a generic N-element conversion, thus making it applicable
                       to being generic for any within model conversion of plain
                       buffers.
                     */
          {
            babl_fatal ("linear conversions not supported for %s",
                        babl_class_name (source->class_type));
          }
        else if (planar)
          {
            babl_fatal ("planar conversions not supported for %s",
                        babl_class_name (source->class_type));
          }
        break;

      case BABL_MODEL:
        if (plane)
          {
            babl_fatal ("plane conversions not supported for %s",
                        babl_class_name (source->class_type));
          }
        break;

      case BABL_FORMAT:
        break;

      default:
        babl_fatal ("%s unexpected", babl_class_name (babl->class_type));
        break;
    }

  babl->instance.id            = id;
  babl->conversion.source      = source;
  babl->conversion.destination = destination;
  babl->conversion.error       = -1.0;
  babl->conversion.cost        = 69L;

  babl->conversion.pixels      = 0;
  babl->conversion.processings = 0;

  if (babl->class_type == BABL_CONVERSION_LINEAR &&
      BABL (babl->conversion.source)->class_type == BABL_MODEL)
    {
      Babl *src_format = NULL;
      Babl *dst_format = NULL;
      if (BABL (babl->conversion.source) == babl_model_from_id (BABL_RGBA))
        {
          src_format = babl_format_from_id (BABL_RGBA_DOUBLE);
          dst_format = babl_format_with_model_as_type (
            BABL (babl->conversion.destination),
            babl_type_from_id (BABL_DOUBLE));
        }
      else if (BABL (babl->conversion.destination) == babl_model_from_id (BABL_RGBA))
        {
          src_format = babl_format_with_model_as_type (
            BABL (babl->conversion.source),
            babl_type_from_id (BABL_DOUBLE));
          dst_format = babl_format_from_id (BABL_RGBA_DOUBLE);
        }
      else
        {
          babl_fatal ("neither source nor destination model is RGBA (requirement might be temporary)");
        }
      babl_conversion_new (
        src_format,
        dst_format,
        "linear", linear,
        NULL);
      babl->conversion.error = 0.0;
    }

  return babl;
}

static char buf[512] = "";
static char *
create_name (Babl *source, Babl *destination, int type)
{
  if (babl_extender ())
    {
      snprintf (buf, 512 - 1, "%s : %s%s to %s",
                BABL (babl_extender ())->instance.name,
                type == BABL_CONVERSION_LINEAR ? "" :
                type == BABL_CONVERSION_PLANE ? "plane " :
                type == BABL_CONVERSION_PLANAR ? "planar " : "Eeeek! ",
                source->instance.name,
                destination->instance.name);
      buf[511] = '\0';
    }
  else
    {
      snprintf (buf, 512 - 1, "%s %s to %s",
                type == BABL_CONVERSION_LINEAR ? "" :
                type == BABL_CONVERSION_PLANE ? "plane " :
                type == BABL_CONVERSION_PLANAR ? "planar " : "Eeeek! ",
                source->instance.name,
                destination->instance.name);
      buf[511] = '\0';
    }
  return buf;
}

Babl *
babl_conversion_new (void *first_arg,
                     ...)
{
  va_list        varg;
  Babl          *babl;

  int            id       = 0;
  BablFuncLinear linear   = NULL;
  BablFuncPlane  plane    = NULL;
  BablFuncPlanar planar   = NULL;
  int            type     = 0;
  int            got_func = 0;
  const char    *arg      = first_arg;

  Babl          *source;
  Babl          *destination;

  char          *name;

  va_start (varg, first_arg);
  source      = (Babl *) arg;
  destination = va_arg (varg, Babl *);
  arg         = va_arg (varg, char *);

  assert (BABL_IS_BABL (source));
  assert (BABL_IS_BABL (destination));


  while (arg)
    {
      if (!strcmp (arg, "id"))
        {
          id = va_arg (varg, int);
        }

      else if (!strcmp (arg, "linear"))
        {
          if (got_func++)
            {
              babl_fatal ("already got a conversion func\n");
            }
          linear = va_arg (varg, BablFuncLinear);
        }

      else if (!strcmp (arg, "plane"))
        {
          if (got_func++)
            {
              babl_fatal ("already got a conversion func\n");
            }
          plane = va_arg (varg, BablFuncPlane);
        }

      else if (!strcmp (arg, "planar"))
        {
          if (got_func++)
            {
              babl_fatal ("already got a conversion func\n");
            }
          planar = va_arg (varg, BablFuncPlanar);
        }

      else
        {
          babl_fatal ("unhandled argument '%s'", arg);
        }

      arg = va_arg (varg, char *);
    }

  va_end (varg);

  assert (source);
  assert (destination);

  if (linear)
    {
      type = BABL_CONVERSION_LINEAR;
    }
  else if (plane)
    {
      type = BABL_CONVERSION_PLANE;
    }
  else if (planar)
    {
      type = BABL_CONVERSION_PLANAR;
    }

  name = create_name (source, destination, type);

  babl = babl_db_exist (db, id, name);
  if (babl)
    {
      /* There is an instance already registered by the required id/name,
       * returning the preexistent one instead.
       */
      return babl;
    }

  babl = conversion_new (name, id, source, destination, linear, plane, planar);

  /* Since there is not an already registered instance by the required
   * id/name, inserting newly created class into database.
   */
  babl_db_insert (db, babl);
  if (!source->type.from_list)
    source->type.from_list = babl_list_init_with_size (BABL_CONVERSIONS);
  babl_list_insert_last (source->type.from_list, babl);
  return babl;
}

static long
babl_conversion_linear_process (BablConversion *conversion,
                                void           *source,
                                void           *destination,
                                long            n)
{
  return conversion->function.linear (source, destination, n);
}

static long
babl_conversion_plane_process (BablConversion *conversion,
                               void           *source,
                               void           *destination,
                               int             src_pitch,
                               int             dst_pitch,
                               long            n)
{
  return conversion->function.plane (source, destination,
                                     src_pitch, dst_pitch,
                                     n);
}

static long
babl_conversion_planar_process (BablConversion *conversion,
                                BablImage      *source,
                                BablImage      *destination,
                                long            n)
{
#ifdef USE_ALLOCA
  char **src_data = alloca (sizeof (void *) * source->components);
  char **dst_data = alloca (sizeof (void *) * destination->components);
#else
  char  *src_data[BABL_MAX_COMPONENTS];
  char  *dst_data[BABL_MAX_COMPONENTS];
#endif

  memcpy (src_data, source->data, sizeof (void *) * source->components);
  memcpy (dst_data, destination->data, sizeof (void *) * destination->components);

  return conversion->function.planar (source->components,
                                      src_data,
                                      source->pitch,
                                      destination->components,
                                      dst_data,
                                      destination->pitch,
                                      n);
}

long
babl_conversion_process (Babl *babl,
                         char *source,
                         char *destination,
                         long  n)
{
  BablConversion *conversion = (BablConversion *) babl;

  babl_assert (BABL_IS_BABL (conversion));

  switch (BABL (conversion)->class_type)
    {
      case BABL_CONVERSION_PLANE:
      {
        void *src_data  = NULL;
        void *dst_data  = NULL;
        int   src_pitch = 0;
        int   dst_pitch = 0;

        if (BABL_IS_BABL (source))
          {
            BablImage *img;

            img       = (BablImage *) source;
            src_data  = img->data[0];
            src_pitch = img->pitch[0];
          }
        if (BABL_IS_BABL (destination))
          {
            BablImage *img = (BablImage *) destination;

            dst_data  = img->data[0];
            dst_pitch = img->pitch[0];
          }

        if (!src_data)
          src_data = source;
        if (!src_pitch)
          src_pitch = BABL (conversion->source)->type.bits / 8;
        if (!dst_data)
          dst_data = destination;
        if (!dst_pitch)
          dst_pitch = BABL (conversion->destination)->type.bits / 8;

        babl_conversion_plane_process (conversion,
                                       src_data, dst_data,
                                       src_pitch, dst_pitch,
                                       n);
      }
        break;

      case BABL_CONVERSION_PLANAR:
        babl_assert (BABL_IS_BABL (source));
        babl_assert (BABL_IS_BABL (destination));

        babl_conversion_planar_process (conversion,
                                        (BablImage *) source,
                                        (BablImage *) destination,
                                        n);
        break;

      case BABL_CONVERSION_LINEAR:
        /* the assertions relied on a babl_malloc structure
         *
         * babl_assert (!BABL_IS_BABL (source));
           babl_assert (!BABL_IS_BABL (destination));*/

        babl_conversion_linear_process (conversion,
                                        source,
                                        destination,
                                        n);
        break;

      default:
        babl_log ("args=(%s, %p, %p, %li) unhandled conversion type: %s",
                  conversion->instance.name, source, destination, n,
                  babl_class_name (conversion->instance.class_type));
        return 0;
        break;
    }

  conversion->processings++;
  conversion->pixels += n;
  return n;
}

#define test_pixels    512


static double *
test_create (void)
{
  static double test[sizeof (double) * test_pixels * 4];
  int           i;
  static int done = 0;

  if (done)
    return test;

  srandom (20050728);

  for (i = 0; i < test_pixels * 4; i++)
    test [i] = (double) random () / RAND_MAX;

  done = 1;
  return test;
}

long
babl_conversion_cost (BablConversion *conversion)
{
  if (!conversion)
    return 100000000L;
  if (conversion->error == -1.0)
    babl_conversion_error (conversion);
  return conversion->cost;
}

double
babl_conversion_error (BablConversion *conversion)
{
  Babl *fmt_source;
  Babl *fmt_destination;

  Babl *fmt_rgba_double = babl_format_new (babl_model ("RGBA"),
                                           babl_type ("double"),
                                           babl_component ("R"),
                                           babl_component ("G"),
                                           babl_component ("B"),
                                           babl_component ("A"),
                                           NULL);

  double  error       = 0.0;
  long    ticks_start = 0;
  long    ticks_end   = 0;

  double *test;
  void   *source;
  void   *destination;
  double *destination_rgba_double;
  void   *ref_destination;
  double *ref_destination_rgba_double;

  Babl   *fish_rgba_to_source;
  Babl   *fish_reference;
  Babl   *fish_destination_to_rgba;

  if (!conversion)
    return 0.0;

  if (conversion->error != -1.0)  /* double conversion against a set value should work */
    {
      return conversion->error;
    }

  fmt_source      = BABL (conversion->source);
  fmt_destination = BABL (conversion->destination);

  fish_rgba_to_source      = babl_fish_reference (fmt_rgba_double, fmt_source);
  fish_reference           = babl_fish_reference (fmt_source, fmt_destination);
  fish_destination_to_rgba = babl_fish_reference (fmt_destination, fmt_rgba_double);

  if (fmt_source == fmt_destination)
    {
      conversion->error = 0.0;
      return 0.0;
    }

  if (!(fmt_source->instance.id != BABL_RGBA &&
        fmt_destination->instance.id != BABL_RGBA &&
        fmt_source->instance.id != BABL_DOUBLE &&
        fmt_destination->instance.id != BABL_DOUBLE &&
        fmt_source->class_type == BABL_FORMAT &&
        fmt_destination->class_type == BABL_FORMAT))
    {
      conversion->error = 0.000042;
    }

  test = test_create ();


  source                      = babl_calloc (test_pixels, fmt_source->format.bytes_per_pixel);
  destination                 = babl_calloc (test_pixels, fmt_destination->format.bytes_per_pixel);
  ref_destination             = babl_calloc (test_pixels, fmt_destination->format.bytes_per_pixel);
  destination_rgba_double     = babl_calloc (test_pixels, fmt_rgba_double->format.bytes_per_pixel);
  ref_destination_rgba_double = babl_calloc (test_pixels, fmt_rgba_double->format.bytes_per_pixel);

  babl_process (fish_rgba_to_source,
                test, source, test_pixels);

  ticks_start = babl_ticks ();
  babl_process (babl_fish_simple (conversion),
                source, destination, test_pixels);
  ticks_end = babl_ticks ();

  babl_process (fish_reference,
                source, ref_destination, test_pixels);

  babl_process (fish_destination_to_rgba,
                ref_destination, ref_destination_rgba_double, test_pixels);
  babl_process (fish_destination_to_rgba,
                destination, destination_rgba_double, test_pixels);

  error = babl_rel_avg_error (destination_rgba_double,
                              ref_destination_rgba_double,
                              test_pixels * 4);

  fish_rgba_to_source->fish.processings--;
  fish_reference->fish.processings--;
  fish_destination_to_rgba->fish.processings -= 2;

  fish_rgba_to_source->fish.pixels      -= test_pixels;
  fish_reference->fish.pixels           -= test_pixels;
  fish_destination_to_rgba->fish.pixels -= 2 * test_pixels;


  babl_free (source);
  babl_free (destination);
  babl_free (destination_rgba_double);
  babl_free (ref_destination);
  babl_free (ref_destination_rgba_double);

  conversion->error = error;
  conversion->cost  = babl_process_cost (ticks_start, ticks_end);

  return error;
}

BABL_CLASS_IMPLEMENT (conversion)
