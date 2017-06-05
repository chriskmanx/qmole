/*
 * Sylpheed templates subsystem 
 * Copyright (C) 2001 Alexander Barinov
 * Copyright (C) 2001-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __TEMPLATE_H__
#define __TEMPLATE_H__

#include <glib.h>

typedef struct _Template	Template;

struct _Template {
	gchar *load_filename;
	gchar *name;
	gchar *subject;
	gchar *from;
	gchar *to;
	gchar *cc;
	gchar *bcc;		
	gchar *value;
};

void template_free		(Template	*tmpl);

GSList *template_read_config	(void);

GSList *template_get_config	(void);
void template_set_config	(GSList		*tmpl_list);

#endif /* __TEMPLATE_H__ */
