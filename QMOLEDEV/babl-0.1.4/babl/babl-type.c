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

static int
babl_type_destroy (void *data)
{
  Babl *babl = data;
  if (babl->type.from_list)
    babl_free (babl->type.from_list);
  return 0; 
}

static Babl *
type_new (const char *name,
          int         id,
          int         bits)
{
  Babl *babl;

  babl_assert (bits != 0);
  babl_assert (bits % 8 == 0);

  babl                 = babl_malloc (sizeof (BablType) + strlen (name) + 1);
  babl_set_destructor (babl, babl_type_destroy);
  babl->instance.name  = (void *) ((char *) babl + sizeof (BablType));
  babl->class_type     = BABL_TYPE;
  babl->instance.id    = id;
  strcpy (babl->instance.name, name);
  babl->type.bits      = bits;
  babl->type.from_list = NULL;

  return babl;
}

static int
is_type_duplicate (Babl *babl, int bits)
{
  if (babl->type.bits != bits)
    return 0;

  return 1;
}

Babl *
babl_type_new (void *first_arg,
               ...)
{
  va_list     varg;
  Babl       *babl;
  int         id         = 0;
  int         is_integer = 0;
  int         bits       = 0;
  long        min        = 0;
  long        max        = 255;
  double      min_val    = 0.0;
  double      max_val    = 0.0;
  const char *name = first_arg;
  const char *arg;

  va_start (varg, first_arg);

  while (1)
    {
      arg = va_arg (varg, char *);
      if (!arg)
        break;

      if (BABL_IS_BABL (arg))
        {
#ifdef BABL_LOG
          Babl *babl = (Babl *) arg;

          babl_log ("%s unexpected", babl_class_name (babl->class_type));
#endif
        }
      /* if we didn't point to a babl, we assume arguments to be strings */
      else if (!strcmp (arg, "id"))
        {
          id = va_arg (varg, int);
        }

      else if (!strcmp (arg, "bits"))
        {
          bits = va_arg (varg, int);
          min  = 0;
          max  = 1 << bits;
        }
      else if (!strcmp (arg, "integer"))
        {
          is_integer = va_arg (varg, int);
        }
      else if (!strcmp (arg, "min"))
        {
          min = va_arg (varg, long);
        }
      else if (!strcmp (arg, "max"))
        {
          max = va_arg (varg, long);
        }
      else if (!strcmp (arg, "min_val"))
        {
          min_val = va_arg (varg, double);
        }
      else if (!strcmp (arg, "max_val"))
        {
          max_val = va_arg (varg, double);
        }

      else
        {
          babl_fatal ("unhandled argument '%s' for format '%s'", arg, name);
        }
    }

  va_end (varg);

  babl = babl_db_exist (db, id, name);
  if (id && !babl && babl_db_exist (db, 0, name))
    babl_fatal ("Trying to reregister BablType '%s' with different id!", name);

  if (babl)
    {
      /* There is an instance already registered by the required id/name,
       * returning the preexistent one instead if it doesn't differ.
       */

      if (!is_type_duplicate (babl, bits))
        babl_fatal ("BablType '%s' already registered "
                    "as different type!", name);

      return babl;
    }

  babl = type_new (name, id, bits);

  /* Since there is not an already registered instance by the required
   * id/name, inserting newly created class into database.
   */
  babl_db_insert (db, babl);
  return babl;
}


#define TOLERANCE    0.000000001
#define samples      512

static double test[samples];

static double r_interval (double min, double max)
{
  long int rand_i = random ();
  double   ret;

  ret  = (double) rand_i / RAND_MAX;
  ret *= (max - min);
  ret += min;
  return ret;
}

static void test_init (double min, double max)
{
  int i;

  srandom (20050728);
  for (i = 0; i < samples; i++)
    {
      test [i] = r_interval (min, max);
    }
}


static Babl *double_vector_format (void)
{
  static Babl *self = NULL;

  if (!self)
    self = babl_format_new (
      babl_model ("Y"),
      babl_type ("double"),
      babl_component ("Y"),
      NULL);
  return self;
}

int
babl_type_is_symmetric (Babl *babl)
{
  int     is_symmetrical = 1;
  void   *original;
  double *clipped;
  void   *destination;
  double *transformed;

  Babl   *ref_fmt;
  Babl   *fmt;
  Babl   *fish_to;
  Babl   *fish_from;

  test_init (0.0, 182.0);

  ref_fmt = double_vector_format ();
  fmt     = babl_format_new (babl_model ("Y"),
                             babl,
                             babl_component ("Y"),
                             NULL);
  fish_to   = babl_fish_reference (ref_fmt, fmt);
  fish_from = babl_fish_reference (fmt, ref_fmt);

  original    = babl_calloc (1, babl->type.bits / 8 * samples);
  clipped     = babl_calloc (1, 64 / 8 * samples);
  destination = babl_calloc (1, babl->type.bits / 8 * samples);
  transformed = babl_calloc (1, 64 / 8 * samples);

  babl_process (fish_to, test, original, samples);
  babl_process (fish_from, original, clipped, samples);
  babl_process (fish_to, clipped, destination, samples);
  babl_process (fish_from, destination, transformed, samples);

  fish_from->fish.processings -= 2;
  fish_to->fish.processings   -= 2;
  fish_from->fish.pixels      -= samples * 2;
  fish_to->fish.pixels        -= samples * 2;

  {
    int cnt = 0;
    int i;
    for (i = 0; i < samples; i++)
      {
        if (fabs (clipped[i] - transformed[i]) > TOLERANCE)
          {
            if (cnt++ < 4)
              babl_log ("%s:  %f %f %f)",
                        babl->instance.name, test[i], clipped[i], transformed[i]
              );
            is_symmetrical = 0;
          }
      }
  }

  babl_free (original);
  babl_free (clipped);
  babl_free (destination);
  babl_free (transformed);

  return is_symmetrical;
}

BABL_CLASS_IMPLEMENT (type)
