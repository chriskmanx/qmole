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

#ifndef _BABL_HASH_TABLE_H
#define _BABL_HASH_TABLE_H

#ifndef _BABL_H
#error  babl-hash-table.h is only to be included after babl.h
#endif


typedef struct _BablHashTable BablHashTable;

typedef int  (*BablHashValFunction) (BablHashTable *htab, Babl *item);
typedef int  (*BablHashFindFunction) (Babl *item, void *data);

typedef struct _BablHashTable
{
  Babl                 **data_table;
  int                  *chain_table;
  int                  mask;
  int                  count;
  BablHashValFunction  hash_func;
  BablHashFindFunction find_func;
} _BablHashTable;


BablHashTable *
babl_hash_table_init (BablHashValFunction  hfunc,
                      BablHashFindFunction ffunc);

int
babl_hash_by_str (BablHashTable *htab,
                  const char    *str);

int
babl_hash_by_int (BablHashTable *htab,
                  int           id);

int
babl_hash_table_size (BablHashTable *htab);

int
babl_hash_table_insert (BablHashTable *htab,
                        Babl          *item);

Babl *
babl_hash_table_find (BablHashTable       *htab,
                      int                  hash,
                      BablHashFindFunction find_func,
                      void                *data);

#endif
