/*
 * orbit-imodule-utils.h:
 *
 * Copyright (C) 2002 Sun Microsystems, Inc.
 *                    Ximian, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * Authors:
 *	Mark McLoughlin <mark@skynet.ie>
 */
#ifndef __ORBIT_IMODULE_UTILS_H__
#define __ORBIT_IMODULE_UTILS_H__

#include <glib.h>
#include <libIDL/IDL.h>

G_BEGIN_DECLS

typedef struct {
	IDL_tree get_op;
	IDL_tree set_op;
} ORBit_imodule_fakeops;

GHashTable      *ORBit_imodule_new_typecodes         (void);
void             ORBit_imodule_free_typecodes        (GHashTable     *typecodes);

CORBA_sequence_CORBA_TypeCode *
                 ORBit_imodule_get_typecodes_seq     (GHashTable     *typecodes);


CORBA_TypeCode   ORBit_imodule_get_typecode          (GHashTable     *typecodes,
						      IDL_tree        tree);
CORBA_TypeCode   ORBit_imodule_create_alias_typecode (GHashTable     *typecodes,
						      IDL_tree        tree,
						      CORBA_TypeCode  original_type);

IDL_tree         ORBit_imodule_get_typespec          (IDL_tree        tree);
gboolean         ORBit_imodule_type_is_fixed_length  (IDL_tree        tree);
void             ORBit_imodule_traverse_parents      (IDL_tree        tree,
						      GFunc           callback,
						      gpointer        user_data);

G_END_DECLS

#endif /* __ORBIT_IMODULE_UTILS_H__ */
