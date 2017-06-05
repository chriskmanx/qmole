/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999,2000 Hiroyuki Yamamoto
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

#ifndef __ERTF_H__
#define __ERTF_H__

#include <glib.h>
#include <stdio.h>

#include "codeconv.h"

typedef enum
{
	ERTF_NORMAL,
	ERTF_NOFILL,
	ERTF_UNKNOWN,
	ERTF_ERR,
	ERTF_EOF
} ERTFState;

typedef struct _ERTFParser	ERTFParser;

struct _ERTFParser
{
	FILE *fp;
	CodeConverter *conv;

	GHashTable *symbol_table;

	GString *str;
	GString *buf;
	
	gchar *bufp;

	ERTFState state;

	gboolean newline;
	gboolean empty_line;
	gboolean space;
	gboolean pre;
};

ERTFParser *ertf_parser_new	(FILE		*fp,
				 CodeConverter	*conv);
void ertf_parser_destroy	(ERTFParser	*parser);
gchar *ertf_parse		(ERTFParser	*parser);

#endif /* __ERTF_H__ */
				 
	
	
