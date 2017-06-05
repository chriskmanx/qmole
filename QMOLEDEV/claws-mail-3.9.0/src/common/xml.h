/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
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

#ifndef __XML_H__
#define __XML_H__

#include <glib.h>
#include <stdio.h>

#define XMLBUFSIZE	8192

typedef struct _XMLAttr		XMLAttr;
typedef struct _XMLTag		XMLTag;
typedef struct _XMLNode		XMLNode;
typedef struct _XMLFile		XMLFile;

struct _XMLAttr
{
	gchar *name;
	gchar *value;
};

struct _XMLTag
{
	gchar *tag;
	GList *attr;
};

struct _XMLNode
{
	XMLTag *tag;
	gchar *element;
};

struct _XMLFile
{
	FILE *fp;

	GString *buf;
	gchar *bufp;

	gchar *dtd;
	gchar *encoding;
	gboolean need_codeconv;

	GList *tag_stack;
	guint level;

	gboolean is_empty_element;
};

XMLFile *xml_open_file		(const gchar	*path);
void     xml_close_file		(XMLFile	*file);
GNode   *xml_parse_file		(const gchar	*path);

gint xml_get_dtd		(XMLFile	*file);
gint xml_parse_next_tag		(XMLFile	*file);

XMLTag *xml_get_current_tag	(XMLFile	*file);
GList  *xml_get_current_tag_attr(XMLFile	*file);
gchar  *xml_get_element		(XMLFile	*file);

gboolean  xml_compare_tag	(XMLFile	*file,
				 const gchar	*name);

XMLNode *xml_node_new		(XMLTag		*tag,
				 const gchar	*text);

XMLTag	*xml_tag_new		(const gchar	*tag);
XMLAttr *xml_attr_new		(const gchar	*name,
				 const gchar	*value);
XMLAttr *xml_attr_new_int	(const gchar	*name,
				 const gint	 value);
void xml_tag_add_attr		(XMLTag		*tag,
				 XMLAttr*	attr);


gint xml_file_put_escape_str	(FILE		*fp,
				 const gchar	*str);

gint xml_file_put_xml_decl	(FILE		*fp);

void xml_free_tree		(GNode		*node);

int  xml_write_tree		(GNode		*node,
				 FILE		*fp);
GNode *xml_copy_tree		(GNode 		*node);

#endif /* __XML_H__ */
