/**
 *
 * $Id: uilparse.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
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

static const char rcsid[] = "$Id: uilparse.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";


#include <LTconfig.h>

#include <string.h>

/* Enable this for verbose lexing */
/* #define DEBUG */

#ifdef HAVE_LIBGEN_H
#include <libgen.h>    /* for basename() */
#endif

#include <X11/Intrinsic.h>
#include <XmI/LTmisc.h>
#include <XmI/UilI.h>

#include <uil/UilSymGl.h>

#include <XmI/DebugUtil.h>


static int
get_token(Uil_parse_data *pd)
{
    return 0;
}


/* for lookahead */
static void
unget_token(Uil_parse_data *pd)
{
}


static Uil_status_type
parse_module_hdr(Uil_parse_data *pd, sym_root_entry_type *root)
{
  return Uil_k_error_status;
}


static Uil_status_type
parse_sections(Uil_parse_data *pd, sym_root_entry_type *root)
{
  return Uil_k_error_status;
}


static Uil_status_type
dump_uid(Uil_parse_data *pd, sym_root_entry_type *root)
{
  return Uil_k_error_status;
}


extern Uil_status_type
_uil_parse_module(Uil_parse_data *pd)
{
    sym_root_entry_type *root;
    int ltype, ret;
    
    root = (sym_root_entry_type *)XtCalloc(1, sizeof(sym_root_entry_type));

    ltype = _uil_next_lexeme(pd);
    /* first token MUST be "module" */
    if (ltype != KEYWORD)
    {
	_uilmsg(pd, Uil_k_severe_status,
	       "Missing module keyword: check module syntax.\n");

	return Uil_k_severe_status;
    }

    pd->desc->parse_tree_root = (char *)root;

    strncpy(root->full_file_name, pd->command->source_file, sizeof(root->full_file_name)-1);
    root->full_file_name[sizeof(root->full_file_name)-1]='\0';
    strncpy(root->file_name, basename(pd->command->source_file), sizeof(root->file_name)-1);
    root->file_name[sizeof(root->file_name)-1]='\0';

    root->module_hdr =
        (sym_module_entry_type *)XtCalloc(1, sizeof(sym_module_entry_type));

    if ((ret = parse_module_hdr(pd, root)) != Uil_k_success_status)
    {
	return ret;
    }

    if ((ret = parse_sections(pd, root)) != Uil_k_success_status)
    {
	return ret;
    }

    dump_uid(pd, root);

     return Uil_k_success_status;
}
