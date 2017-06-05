/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gparser.h parse DBus description files
 *
 * Copyright (C) 2003  Red Hat, Inc.
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
#ifndef DBUS_GLIB_PARSER_H
#define DBUS_GLIB_PARSER_H

#include <dbus/dbus.h>
#include <glib.h>
#include "dbus-gidl.h"

G_BEGIN_DECLS

typedef struct Parser Parser;

Parser*  parser_new           (void);
Parser*  parser_ref           (Parser      *parser);
void     parser_unref         (Parser      *parser);
gboolean parser_check_doctype (Parser      *parser,
                               const char  *doctype,
                               GError     **error);
gboolean parser_start_element (Parser      *parser,
                               const char  *element_name,
                               const char **attribute_names,
                               const char **attribute_values,
                               GError     **error);
gboolean parser_end_element   (Parser      *parser,
                               const char  *element_name,
                               GError     **error);
gboolean parser_content       (Parser      *parser,
                               const char  *content,
                               int          len,
                               GError     **error);
gboolean parser_finished      (Parser      *parser,
                               GError     **error);

NodeInfo* description_load_from_file   (const char  *filename,
                                        GError     **error);
NodeInfo* description_load_from_string (const char  *str,
                                        int          len,
                                        GError     **error);

NodeInfo* parser_get_nodes (Parser *parser);

G_END_DECLS

#endif /* DBUS_GLIB_GPARSER_H */
