/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005-2008, Øyvind Kolås and others.
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

#ifndef _BABL_CLASS_H
#define _BABL_CLASS_H

#include "babl.h"


typedef struct _BablList BablList;

typedef int BablClassType;

typedef int  (*BablEachFunction) (Babl *entry,
                                  void *data);

/* All Classes in babl have common functionality like the ability
 * to be iterated over, common functionality is defined through these
 * macros.
 */
#define BABL_CLASS_DECLARE(klass)                                    \
                                                                     \
BablDb * babl_##klass##_db (void);                                   \
Babl   * babl_##klass##_from_id        (int id);                     \
void     babl_##klass##_class_for_each (BablEachFunction  each_fun,  \
                                        void             *user_data)

/* common header for any item inserted into database, the actual
 * implementation of babl-instance is in babl-internal
 */
typedef struct
{
  BablClassType  class_type;
  int            id;      /*< a numerical id, look at 'babl-ids.h' for the reserved
                              ones */
  void          *creator;
  char          *name;    /*< the name this type exists under         */
} BablInstance;


#endif
