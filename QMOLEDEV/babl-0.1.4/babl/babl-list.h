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

#ifndef _BABL_LIST_H
#define _BABL_LIST_H

#ifndef _BABL_H
#error  babl-list.h is only to be included after babl.h
#endif

struct _BablList
{
  int  count;
  int  size;
  Babl **items;
};

BablList *
babl_list_init (void);

BablList *
babl_list_init_with_size (int initial_size);

int
babl_list_size (BablList *list);

void
babl_list_insert_last (BablList *list,
                       Babl     *item);

void
babl_list_remove_last (BablList *list);

#define babl_list_get_first(list) (list->items[0])
#define babl_list_get_last(list)  (list->items[list->count-1])
#define babl_list_size(list)      (list->count)

void
babl_list_copy (BablList *from,
                BablList *to);

void
babl_list_each (BablList      *list,
                BablEachFunction each_fun,
                void             *user_data);


#endif
