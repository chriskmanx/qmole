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
#include "babl-internal.h"
#include "babl-db.h"
#include <string.h>

#include <stdarg.h>

static Babl *
component_new (const char *name,
               int         id,
               int         luma,
               int         chroma,
               int         alpha)
{
  Babl *babl;

  babl                = babl_malloc (sizeof (BablComponent) + strlen (name) + 1);
  babl->instance.name = (char *) babl + sizeof (BablComponent);
  strcpy (babl->instance.name, name);

  babl->class_type       = BABL_COMPONENT;
  babl->instance.id      = id;
  babl->component.luma   = luma;
  babl->component.chroma = chroma;
  babl->component.alpha  = alpha;
  return babl;
}


static int
is_component_duplicate (Babl *babl, int luma, int chroma, int alpha)
{
  if (babl->component.luma   != luma   ||
      babl->component.chroma != chroma ||
      babl->component.alpha  != alpha)
    return 0;

  return 1;
}


Babl *
babl_component_new (void *first_arg,
                    ...)
{
  va_list     varg;
  Babl       *babl;
  int         id     = 0;
  int         luma   = 0;
  int         chroma = 0;
  int         alpha  = 0;
  const char *name   = first_arg;
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

      else if (!strcmp (arg, "luma"))
        {
          luma = 1;
        }

      else if (!strcmp (arg, "chroma"))
        {
          chroma = 1;
        }

      else if (!strcmp (arg, "alpha"))
        {
          alpha = 1;
        }

      else
        {
          babl_fatal ("unhandled argument '%s' for component '%s'", arg, name);
        }
    }

  va_end (varg);

  babl = babl_db_exist (db, id, name);
  if (id && !babl && babl_db_exist (db, 0, name))
    babl_fatal ("Trying to reregister BablComponent '%s' with different id!",
                name);

  if (babl)
    {
      /* There is an instance already registered by the required id/name,
       * returning the preexistent one instead if it doesn't differ.
       */
      if (!is_component_duplicate (babl, luma, chroma, alpha))
        babl_fatal ("BablComponent '%s' already registered "
                    "with different attributes!", name);
      return babl;
    }

  babl = component_new (name, id, luma, chroma, alpha);

  /* Since there is not an already registered instance by the required
   * id/name, inserting newly created class into database.
   */
  babl_db_insert (db, babl);
  return babl;
}

BABL_CLASS_IMPLEMENT (component)
