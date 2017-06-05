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

/* Implementation of list data structure.
 * Copyright (C) 2008, Jan Heller
 */

#include "config.h"
#include "babl-internal.h"

#define BABL_LIST_INITIAL_SIZE 0x7F

BablList *
babl_list_init (void)
{
  return babl_list_init_with_size (BABL_LIST_INITIAL_SIZE);
}

static int
babl_list_destroy (void *data)
{
  BablList *list = data;
  babl_free (list->items);
  return 0;
}

BablList *
babl_list_init_with_size (int initial_size)
{
  BablList *list = babl_calloc (sizeof (BablList), 1);

  babl_set_destructor (list, babl_list_destroy);

  if (initial_size == 0)
    initial_size = 1;
  list->size = initial_size;
  list->count = 0;
  list->items = NULL;
  if (list->size)
    {
      list->items = babl_calloc (sizeof (BablInstance *), list->size);
    }

  return list;
}

void
babl_list_insert_last (BablList *list,
                       Babl     *item)
{
  babl_assert(list);
  babl_assert(BABL_IS_BABL(item));

  if (list->size < list->count + 1)
    {
      Babl **new_items;

      new_items = babl_realloc (list->items, (list->size * 2) * sizeof (BablInstance *));
      babl_assert (new_items);
      list->items = new_items;
      memset (list->items + list->size, 0, list->size * sizeof (BablInstance *));
      list->size *= 2;
    }
    list->items[list->count++] = item;
}

void
babl_list_remove_last (BablList *list)
{
  babl_assert (list);
  babl_assert (list->count > 0);

  list->count--;
}

void
babl_list_copy (BablList *from,
                BablList *to)
{
  babl_assert (from);
  babl_assert (to);

  if (to->size < from->count)
    {
      Babl **new_items;

      new_items = babl_realloc (to->items, from->count * sizeof (BablInstance *));
      babl_assert (new_items);
      to->items = new_items;
      to->size = from->count;
    }

    memcpy (to->items, from->items, from->count * sizeof (BablInstance *));
    to->count = from->count;
}

void
babl_list_each (BablList         *list,
                BablEachFunction each_fun,
                void             *user_data)
{
  int i;

  babl_assert(list);
  babl_assert(each_fun);

  for (i = 0; i < list->count; i++)
    {
      if (list->items[i])
        {
          if (each_fun ((Babl *) list->items[i], user_data))
            break;
        }
    }
}

