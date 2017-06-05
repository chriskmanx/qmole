/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 Match Grun and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * 		(at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/*
 * General functions for saving properties to an XML file.
 */

#ifndef __XMLSAVER_H__
#define __XMLSAVER_H__

#include <stdio.h>
#include <glib.h>

/* XML property data */
typedef struct _XmlProperty XmlProperty;

struct _XmlProperty {
	gchar      *path;
	gchar      *encoding;
	GHashTable *propertyTable;
	gint       retVal;
};

/* Function prototypes */

XmlProperty *xmlprops_create	( void );
void xmlprops_free		( XmlProperty *props );
void xmlprops_set_path		( XmlProperty *props, const gchar *value );
gint xmlprops_load_file		( XmlProperty *props );
gint xmlprops_save_file		( XmlProperty *props );
void xmlprops_set_property	( XmlProperty *props, const gchar *name,
				  const gchar *value );
void xmlprops_set_property_i	( XmlProperty *props, const gchar *name,
				  const gint value );
void xmlprops_set_property_b	( XmlProperty *props, const gchar *name,
				  const gboolean value );
void xmlprops_get_property_s	( XmlProperty *props, const gchar *name,
				  gchar *buffer );
gint xmlprops_get_property_i	( XmlProperty *props, const gchar *name );
gboolean xmlprops_get_property_b( XmlProperty *props, const gchar *name );

#endif /* __XMLSAVER_H__ */
