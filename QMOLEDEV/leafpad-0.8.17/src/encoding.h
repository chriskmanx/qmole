/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef _ENCODING_H
#define _ENCODING_H

#include <glib.h> // TODO: ?????  It must to -> .c

enum {
	IANA = 0,
	OPENI18N,
	CODEPAGE,
	ENCODING_MAX_ITEM_NUM
};

typedef struct {
	const gchar *item[ENCODING_MAX_ITEM_NUM];
} EncArray;

enum {
	LF = 0x0A,
	CR = 0x0D,
};

guint get_encoding_code(void);
EncArray *get_encoding_items(guint code);
const gchar *get_default_charset(void);
gint detect_line_ending(const gchar *text);
void convert_line_ending_to_lf(gchar *text);
void convert_line_ending(gchar **text, gint retcode);
const gchar *detect_charset(const gchar *text);

#endif  /* _ENCODING_H */
