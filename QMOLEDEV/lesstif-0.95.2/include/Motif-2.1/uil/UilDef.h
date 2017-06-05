/**
 *
 * $Id: UilDef.h,v 1.1 2004/08/28 19:23:37 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
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
 **/

#ifndef _UIL_UILDEF_H
#define _UIL_UILDEF_H

#include <X11/Intrinsic.h>
#include <uil/Uil.h>
#include <uil/UilDBDef.h>
#include <uil/UilSymGl.h>
#include <uil/UilSymDef.h>

#ifdef __cplusplus
extern "C"
{
#endif

/* The Motif docs specify the callbacks in an ANSI violating
   incomplete type. We define the callbacks to be of 
      Uil_continue_type(*)(void) instead of
      Uil_continue_type(*)()
 */
Uil_status_type Uil(Uil_command_type * comand_desc,
                    Uil_compile_desc_type * compile_desc,
                    Uil_continue_type(*message_cb) (void),
                    char *message_data,
                    Uil_continue_type(*status_cb) (void),
                    char *status_data);

void UilDumpSymbolTable(sym_entry_type * node_entry);

#ifdef __cplusplus
}
#endif

#endif /* _UIL_UILDEF_H */
