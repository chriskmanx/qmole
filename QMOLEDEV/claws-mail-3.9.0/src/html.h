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

#ifndef __HTML_H__
#define __HTML_H__

#include <glib.h>
#include <stdio.h>

#include "codeconv.h"

typedef enum
{
	SC_HTML_NORMAL,
	SC_HTML_PAR,
	SC_HTML_BR,
	SC_HTML_HR,
	SC_HTML_HREF,
	SC_HTML_IMG,
	SC_HTML_FONT,
	SC_HTML_PRE,
	SC_HTML_UNKNOWN,
	SC_HTML_CONV_FAILED,
	SC_HTML_ERR,
	SC_HTML_EOF,
	SC_HTML_HREF_BEG
} SC_HTMLState;

typedef struct _SC_HTMLParser	SC_HTMLParser;
typedef struct _SC_HTMLAttr		SC_HTMLAttr;
typedef struct _SC_HTMLTag		SC_HTMLTag;

struct _SC_HTMLParser
{
	FILE *fp;
	CodeConverter *conv;

	GHashTable *symbol_table;
	GHashTable *alt_symbol_table;

	GString *str;
	GString *buf;

	gchar *bufp;

	SC_HTMLState state;

	gchar *href;

	gboolean newline;
	gboolean empty_line;
	gboolean space;
	gboolean pre;
};

struct _SC_HTMLAttr
{
	gchar *name;
	gchar *value;
};

struct _SC_HTMLTag
{
	gchar *name;
	GList *attr;
};

SC_HTMLParser *sc_html_parser_new	(FILE		*fp,
				 CodeConverter	*conv);
void sc_html_parser_destroy	(SC_HTMLParser	*parser);
gchar *sc_html_parse		(SC_HTMLParser	*parser);

#endif /* __HTML_H__ */
