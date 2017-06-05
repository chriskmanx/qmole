/* $Id: e2_cache.h 2779 2013-10-09 04:56:05Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>
Copyright (C) 2004 Florian Zaehringer (flo.zaehringer@web.de)

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_CACHE_H__
#define __E2_CACHE_H__

#include "emelfm2.h"

typedef enum
{
	//simple types
	E2_CACHE_TYPE_BOOL,
	E2_CACHE_TYPE_INT,
	E2_CACHE_TYPE_LONG,
	E2_CACHE_TYPE_TIME,	//could be int or long
	E2_CACHE_TYPE_FLOAT,
	E2_CACHE_TYPE_DOUBLE,
	E2_CACHE_TYPE_STR,
	E2_CACHE_TYPE_LIST,
	E2_CACHE_TYPE_TREE,
	E2_CACHE_TYPE_ARRAY,	//treated as longs
	E2_CACHE_TYPE_STORE,
/*	//these are for things that don't like being set directly ...
	E2_CACHE_TYPE_BOOLPTR,
	E2_CACHE_TYPE_INTPTR,
	E2_CACHE_TYPE_DBLPTR, */
} E2_CacheType;

typedef struct _E2_Cache
{
	E2_CacheType type;
	gchar *name;
	gpointer *data;
	gpointer sync_func;
	gpointer sync_data;
} E2_Cache;

gboolean e2_cache_check (gchar *name);
#define e2_cache_bool_register(name, val, def) e2_cache_int_register (name, val, def)
void e2_cache_int_register (gchar *name, gint *value, gint def);
gint e2_cache_ROint_register (gchar *name, gint *value, gint def);  //#w = gint **intwatch,  for debugging
void e2_cache_time_register (gchar *name, time_t *value, time_t def);
void e2_cache_double_register (gchar *name, gdouble *value, gdouble def);
void e2_cache_str_register (gchar *name, gchar **str, gchar *def);
void e2_cache_store_register (gchar *name, gpointer *store,
// gpointer fillfunc, gpointer syncfunc);
	void (*fillfunc) (gpointer*, GList*), GList*(*syncfunc) (gpointer, gpointer),
	gpointer syncdata);
E2_Cache *e2_cache_list_register (gchar *name, GList **list);
void e2_cache_array_register (gchar *name, guint size, gint *values, gint *defs);
void e2_cache_unregister (gchar *name);
void e2_cache_file_write (void);
void e2_cache_init (gboolean config_dir_ready);
void e2_cache_clean (void);
void e2_cache_clean1 (const gchar *name);

#endif //ndef __E2_CACHE_H__
