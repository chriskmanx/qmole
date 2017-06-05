/**
 *
 * $Id: Uil.c,v 1.1 2004/08/28 19:22:42 dannybackx Exp $
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
 */

static const char rcsid[] = "$Id: Uil.c,v 1.1 2004/08/28 19:22:42 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include <uil/UilDef.h>
#include <uil/UilSymDef.h>
#include <XmI/UilI.h>

#include <XmI/DebugUtil.h>


/*
 * compile a UIL file
 */
 
/* The Motif docs specify the callbacks in an ANSI violating
   incomplete type. We define the callbacks to be of 
      Uil_continue_type(*)(void) instead of
      Uil_continue_type(*)()
 */
Uil_status_type
Uil(Uil_command_type *command_desc, Uil_compile_desc_type *compile_desc,
    Uil_continue_type(*message_cb)(void), char *message_data,
    Uil_continue_type(*status_cb) (void), char *status_data)
{
    Uil_parse_data *pd;
    Uil_status_type status;

    if ((pd = _uil_parse_init(command_desc, compile_desc)) == NULL)
    {
	return Uil_k_severe_status;
    }

    status = _uil_parse_module(pd);

    _uil_parse_destroy(pd);

    return status;
}

/*
 * dump a Uil generated symbol table
 */
void
UilDumpSymbolTable(sym_entry_type *node_entry)
{
    _uil_dump_node(node_entry);
}
