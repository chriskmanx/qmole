/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (c) 2001-2002 by Hiroyuki Yamamoto & The Claws Mail Team.
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

#ifndef MATCHER_PARSER_H
#define MATCHER_PARSER_H

#include "filtering.h"
#include <glib.h>

extern FILE *matcher_parserin;
extern int matcher_parserlineno;

void matcher_parser_disable_warnings	(const gboolean disable);
void matcher_parser_start_parsing	(FILE *f);
int matcher_parserparse			(void);

MatcherList *matcher_parser_get_cond	(gchar *str, gboolean *is_fast);
MatcherProp *matcher_parser_get_prop	(gchar *str);
FilteringProp *matcher_parser_get_filtering	(gchar *str);
GSList *matcher_parser_get_action_list(gchar *str);

#endif
