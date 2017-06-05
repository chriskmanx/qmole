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

/* Implementation of hash table data structure based on coalesced hashing.
 * Copyright (C) 2008, Jan Heller
 */

#include "config.h"
#include "babl-internal.h"

#define BABL_HASH_TABLE_INITIAL_MASK   0x1FF  /* 511 */

/* static functions declarations */
static inline int
hash_insert (BablHashTable *htab,
             Babl          *item);

static void
hash_rehash (BablHashTable *htab);


int
babl_hash_by_str (BablHashTable *htab,
                  const char    *str)
{
  int   hash = 0;

  while (*str)
  {
    hash += *str++;
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  return (hash & htab->mask);
}

int
babl_hash_by_int (BablHashTable *htab,
                  int           id)
{
  int   hash = 0;
  int   i;

  for (i = 0; i < sizeof (int); i++)
  {
    hash +=  id & 0xFF;
    hash += (hash << 10);
    hash ^= (hash >> 6);
    id >>= 8;
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  return (hash & htab->mask);
}

static inline int
hash_insert (BablHashTable *htab,
             Babl          *item)
{
  int hash = htab->hash_func(htab, item);

  if (htab->data_table[hash] == NULL)
    {
      /* create new chain */
      htab->data_table[hash] = item;
    }
  else
    {
      int it, oit, cursor = 0;

      while ((cursor < (htab->mask + 1)) && (htab->data_table[cursor] != NULL))
        ++cursor;

      htab->data_table[cursor] = item;

      for (oit = hash, it = htab->chain_table[oit]; it != -1; oit = it, it = htab->chain_table[oit])
        ;
      htab->chain_table[oit] = cursor;
    }

  htab->count++;
  return 0;
}

static void
hash_rehash (BablHashTable *htab)
{
  Babl *item;
  int  i;
  BablHashTable *nhtab = babl_calloc (sizeof (BablHashTable), 1);

  nhtab->data_table = NULL;
  nhtab->chain_table = NULL;
  nhtab->mask = (htab->mask << 1) + 1;
  nhtab->count = 0;
  nhtab->hash_func = htab->hash_func;
  nhtab->find_func = htab->find_func;
  if (nhtab->mask)
    {
      nhtab->data_table = babl_calloc (sizeof (BablInstance *), babl_hash_table_size(nhtab));
      nhtab->chain_table = babl_malloc (sizeof (int *) * babl_hash_table_size(nhtab));
      memset (nhtab->chain_table, -1, sizeof (int) * babl_hash_table_size(nhtab));
    }

  for (i = 0; i < babl_hash_table_size (htab); i++)
    {
      item = htab->data_table[i];
      babl_hash_table_insert (nhtab, item);
    }

  htab->mask = nhtab->mask;
  babl_free (htab->data_table);
  babl_free (htab->chain_table);
  htab->data_table = nhtab->data_table;
  htab->chain_table = nhtab->chain_table;
  babl_free (nhtab);
}

int
babl_hash_table_size (BablHashTable *htab)
{
    return htab->mask + 1;
}


static int
babl_hash_table_destroy (void *data)
{
  BablHashTable *htab = data;
  babl_free (htab->data_table);
  babl_free (htab->chain_table);
  return 0;
}


BablHashTable *
babl_hash_table_init (BablHashValFunction  hfunc,
                      BablHashFindFunction ffunc)
{
  BablHashTable *htab;

  babl_assert(hfunc);
  babl_assert(ffunc);

  htab = babl_calloc (sizeof (BablHashTable), 1);
  babl_set_destructor (htab, babl_hash_table_destroy);

  htab->data_table = NULL;
  htab->chain_table = NULL;
  htab->mask = BABL_HASH_TABLE_INITIAL_MASK;
  htab->count = 0;
  htab->hash_func = hfunc;
  htab->find_func = ffunc;
  if (htab->mask)
  {
      htab->data_table = babl_calloc (sizeof (BablInstance *), babl_hash_table_size(htab));
      htab->chain_table = babl_malloc (sizeof (int *) * babl_hash_table_size(htab));
      memset (htab->chain_table, -1, sizeof (int) * babl_hash_table_size(htab));
  }

  return htab;
}

int
babl_hash_table_insert (BablHashTable *htab,
                        Babl          *item)
{
  babl_assert (htab);
  babl_assert (BABL_IS_BABL(item));

  if (babl_hash_table_size (htab) < htab->count + 1)
    hash_rehash (htab);
  return hash_insert (htab, item);
}

Babl *
babl_hash_table_find (BablHashTable       *htab,
                      int                  hash,
                      BablHashFindFunction find_func,
                      void                *data)
{
  int  it;
  Babl *item;

  babl_assert (htab);

  it = hash;
  item = htab->data_table[it];

  if (!item)
    return NULL;

  for (;;)
    {
      if (find_func)
        {
          if (find_func (item, data))
            return item;
        }
      else if (htab->find_func (item, data))
        return item;
      it = htab->chain_table[it];
      if (it == -1)
        break;
      item = htab->data_table[it];
    }

  return NULL;
}

