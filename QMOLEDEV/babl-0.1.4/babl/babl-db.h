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

#ifndef _BABL_DB_H
#define _BABL_DB_H

#ifndef _BABL_H
#error  babl-db.h is only to be included after babl.h
#endif

#include "babl-list.h"
#include "babl-hash-table.h"
#include "babl-memory.h"
#include "babl-mutex.h"

typedef struct _BablDb BablDb;

typedef struct _BablDb
{
  BablHashTable *name_hash;
  BablHashTable *id_hash;
  BablList      *babl_list;
  BablMutex     *mutex;
} _BablDb;


BablDb *
babl_db_init (void);

void
babl_db_each (BablDb           *db,
              BablEachFunction  each_fun,
              void             *user_data);

int
babl_db_count (BablDb *db);

Babl *
babl_db_insert (BablDb *db,
                Babl   *entry);

Babl *
babl_db_exist (BablDb     *db,
               int        id,
               const char *name);

Babl *
babl_db_exist_by_name (BablDb     *db,
                       const char *name);
Babl *
babl_db_exist_by_id (BablDb *db,
                     int    id);

Babl *
babl_db_find (BablDb     *db,
              const char *name);

#endif
