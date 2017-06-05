/*
 * Sylpheed -- a gtk+ based, lightweight, and fast e-mail client
 * Copyright (c) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the gnu general public license as published by
 * the free software foundation; either version 3 of the license, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but without any warranty; without even the implied warranty of
 * merchantability or fitness for a particular purpose.  see the
 * gnu general public license for more details.
 *
 * You should have received a copy of the Gnu General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef STRINGTABLE_H__
#define STRINGTABLE_H__

#include <glib.h>

typedef struct {
	GHashTable *hash_table;
} StringTable;

StringTable *string_table_new     (void);
void         string_table_free    (StringTable *table);

gchar *string_table_insert_string (StringTable *table, const gchar *str);
void   string_table_free_string   (StringTable *table, const gchar *str);

void   string_table_get_stats     (StringTable *table);

#endif /* STRINGTABLE_H__ */
