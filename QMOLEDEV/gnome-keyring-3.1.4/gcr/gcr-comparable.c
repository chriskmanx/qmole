/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gcr-comparable.h"

#include <string.h>

/**
 * SECTION:gcr-comparable
 * @title: GcrComparable
 * @short_description: Interface for comparing objects
 *
 * The #GcrComparable interface is implemented by objects when they should be
 * comparable against one another.
 */

/**
 * GcrComparable:
 *
 * The #GcrComparable interface is implemented by comparable objects.
 */

/**
 * GcrComparableIface:
 *
 * The interface to implement for #GcrComparable
 */

/* ---------------------------------------------------------------------------------
 * INTERFACE
 */

static void
gcr_comparable_base_init (gpointer g_class)
{
	static volatile gsize initialized = 0;

	if (g_once_init_enter (&initialized)) {
		/* Add properties and signals to the interface */
		g_once_init_leave (&initialized, 1);
	}
}

GType
gcr_comparable_get_type (void)
{
	static GType type = 0;
	if (!type) {
		static const GTypeInfo info = {
			sizeof (GcrComparableIface),
			gcr_comparable_base_init,               /* base init */
			NULL,             /* base finalize */
			NULL,             /* class_init */
			NULL,             /* class finalize */
			NULL,             /* class data */
			0,
			0,                /* n_preallocs */
			NULL,             /* instance init */
		};
		type = g_type_register_static (G_TYPE_INTERFACE, "GcrComparableIface", &info, 0);
		g_type_interface_add_prerequisite (type, G_TYPE_OBJECT);
	}

	return type;
}


/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_comparable_compare:
 * @self: The comparable object
 * @other: Another comparable object
 *
 * Compare whether two objects represent the same thing. The return value can
 * also be used to sort the objects.
 *
 * Returns: Zero if the two objects represent the same thing, non-zero if not.
 */
gint
gcr_comparable_compare (GcrComparable *self, GcrComparable *other)
{
	g_return_val_if_fail (GCR_IS_COMPARABLE (self), -1);
	g_return_val_if_fail (GCR_COMPARABLE_GET_INTERFACE (self)->compare, -1);
	g_return_val_if_fail (G_IS_OBJECT (self), -1);
	return GCR_COMPARABLE_GET_INTERFACE (self)->compare (self, other);
}

/**
 * gcr_comparable_memcmp:
 * @mem1: First block of memory
 * @size1: Length of first block
 * @mem2: Second lock of memory
 * @size2: Length of second block
 *
 * Compare two blocks of memory. The return value can be used to sort
 * the blocks of memory.
 *
 * Returns: Zero if the blocks are identical, non-zero if not.
 */
gint
gcr_comparable_memcmp (gconstpointer mem1, gsize size1,
                       gconstpointer mem2, gsize size2)
{
	gint result;

	if (mem1 == mem2 && size1 == size2)
		return 0;

	if (!mem1)
		return 1;
	if (!mem2)
		return -1;

	result = memcmp (mem1, mem2, MIN (size1, size2));
	if (result != 0)
		return result;

	if (size1 == size2)
		return 0;
	if (size1 < size2)
		return -1;
	return 1;
}
