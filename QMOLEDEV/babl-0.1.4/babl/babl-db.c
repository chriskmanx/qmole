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

/* Reimplementation of database code using redundant hash tables
 * for faster searching by id/name and using list for fast item enumeration.
 * Copyright (C) 2008, Jan Heller
 */

#define _BABL_DB_C

#include "config.h"
#include <string.h>
#include "babl-internal.h"

static int
db_find_by_name (Babl *item, void *data)
{
  if (!strcmp (item->instance.name, (char *) data))
    return 1;
  return 0;
}

static int
db_find_by_id (Babl *item, void *data)
{
  if (item->instance.id == *((int *) data))
    return 1;
  return 0;
}

static int
db_hash_by_name (BablHashTable *htab, Babl *item)
{
  return babl_hash_by_str (htab, item->instance.name);
}

static int
db_hash_by_id (BablHashTable *htab, Babl *item)
{
  return babl_hash_by_int (htab, item->instance.id);
}

static int
each_free (Babl *data,
           void *foo)
{
  babl_free (data);
  return 0;
}

static int
babl_db_destroy (void *data)
{
  BablDb *db = data;
  babl_assert (db);

  babl_db_each (db, each_free, NULL);
  babl_mutex_destroy (db->mutex);
  babl_free (db->name_hash);
  babl_free (db->id_hash);
  babl_free (db->babl_list);
  return 0;
}

BablDb *
babl_db_init (void)
{
  BablDb *db = babl_calloc (sizeof (BablDb), 1);
  babl_set_destructor (db, babl_db_destroy);

  db->name_hash = babl_hash_table_init (db_hash_by_name, db_find_by_name);
  db->id_hash = babl_hash_table_init (db_hash_by_id, db_find_by_id);
  db->babl_list = babl_list_init_with_size (512);
  db->mutex = babl_mutex_new ();

  return db;
}


Babl *
babl_db_find (BablDb     *db,
              const char *name)
{
  return babl_hash_table_find (db->name_hash, babl_hash_by_str (db->name_hash, name),
                              NULL, (void *) name);
}

int
babl_db_count (BablDb *db)
{
  return db->babl_list->count;
}

Babl *
babl_db_insert (BablDb *db,
                Babl   *item)
{
  babl_mutex_lock (db->mutex);
  if (item->instance.id)
    babl_hash_table_insert (db->id_hash, item);
  babl_hash_table_insert (db->name_hash, item);
  babl_list_insert_last (db->babl_list, item);

  /* this point all registered items pass through, a nice
  * place to brand them with where the item came from. */
  item->instance.creator = babl_extender ();
  babl_mutex_unlock (db->mutex);
  return item;
}

void
babl_db_each (BablDb          *db,
              BablEachFunction each_fun,
              void            *user_data)
{
  babl_list_each (db->babl_list, each_fun, user_data);
}


Babl *
babl_db_exist (BablDb     *db,
               int        id,
               const char *name)
{
  Babl *ret;
  if (id)
    ret = babl_hash_table_find (db->id_hash, babl_hash_by_int (db->id_hash, id), NULL, &id);
  else 
    ret = babl_hash_table_find (db->name_hash, babl_hash_by_str (db->name_hash, name), NULL, (void *) name);
  return ret;
}

Babl *
babl_db_exist_by_id (BablDb *db,
                     int    id)
{
  Babl *ret;
  ret = babl_hash_table_find (db->id_hash, babl_hash_by_int (db->id_hash, id), NULL, &id);
  return ret;
}

Babl *
babl_db_exist_by_name (BablDb     *db,
                       const char *name)
{
  Babl *ret;
  ret = babl_hash_table_find (db->name_hash, babl_hash_by_str (db->name_hash, name),
                              NULL, (void *) name);
  return ret;
}
