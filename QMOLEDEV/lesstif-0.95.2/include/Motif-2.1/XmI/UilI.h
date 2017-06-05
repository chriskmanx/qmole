/**
 *
 * $Id: UilI.h,v 1.1 2004/08/28 19:23:36 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */ 

#ifndef _XMI_UILI_H
#define _XMI_UILI_H

#include <uil/Uil.h>
#include <uil/UilDef.h>

#define MAX_UIL_LINE		132
#define MAX_INCLUDE_DEPTH	10
#define PATHLENMAX		4096
#define PARSE_BUF_SIZE		4096

#ifdef DEBUG
#define TRACE(x)		printf x
#else
#define TRACE(x)
#endif

/*
 * lexemes
 */
#define INT	1
#define REAL	2
#define STRING	3
#define SYMBOL	4
#define KEYWORD 5

/*
 * symbol table management
 */
typedef struct _sym_entry {
    struct _sym_entry	*next;
    int type;
    int len;
    char *text;
    union {
	sym_entry_type *entry;
	key_keytable_entry_type *keyword;
    } e;
} sym_entry;

typedef struct _src_source_record_type
{
    int lineno;
    char data[MAX_UIL_LINE];
}
src_source_record_type;

typedef struct Uil_parse_data
{
    Uil_command_type		*command;
    Uil_compile_desc_type	*desc;
    sym_entry_type		*root;
    sym_entry			**hash;
    int				include_depth;
    char			**files;
    int				num_files;
    int				max_files;
    int				cur_file_name_idx;
    src_source_record_type	*lines;
    int				num_lines;
    int				max_lines;
    struct _file_data {
	int			fd;
	char			pbuf[PARSE_BUF_SIZE];
	char			*forward;
	char			*lexeme;
	int			lineno;
	int			colno;
    } file_data[MAX_INCLUDE_DEPTH];
    int				cur_file;
    char			curtok[PARSE_BUF_SIZE];
    struct _value {
	int			integer;
	double			real;
	char			*string;
	sym_entry		*sym;
    } value;
}
Uil_parse_data;

/* Prototypes, local data */
void _uilmsg(Uil_parse_data *pd, int severity, const char *fmt, ...);
void _uil_parse_destroy(Uil_parse_data *pd);
int _uil_next_lexeme(Uil_parse_data *pd);
void _uil_dump_node(sym_entry_type *node_entry);
Uil_parse_data *_uil_parse_init(Uil_command_type *cmd,
				  Uil_compile_desc_type *desc);
Uil_status_type _uil_parse_module(Uil_parse_data *pd);

#endif /* _XMI_UILI_H */
