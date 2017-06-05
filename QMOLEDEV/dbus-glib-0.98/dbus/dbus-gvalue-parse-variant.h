/* GVariant to dbus-glib escape hatch
 *
 * Copyright © 2010 Collabora Ltd. <http://www.collabora.co.uk/>
 *
 * Licensed under the Academic Free License version 2.1
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Alternatively, at your option, you can redistribute and/or modify
 * this single file under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * that license, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#ifndef __DBUS_GVALUE_PARSE_VARIANT_H__
#define __DBUS_GVALUE_PARSE_VARIANT_H__

#include <glib-object.h>

G_BEGIN_DECLS

void dbus_g_value_parse_g_variant (GVariant *variant, GValue *value);

G_END_DECLS

#endif
