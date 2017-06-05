/**************************************************************************

    orbit-idl-driver.c (Dispatch parsed tree to various backends)

    Copyright (C) 1999 Elliot Lee

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    $Id$

***************************************************************************/

#include "config.h"

#include "orbit-idl2.h"
#include "orbit-idl-backend.h"
#include "orbit-idl-c-backend.h"

#include <string.h>

static void
orbit_idl_tree_fake_ops (IDL_tree tree, IDL_ns ns)
{
	IDL_tree node;

	if (!tree)
		return;

	switch(IDL_NODE_TYPE(tree)) {
	case IDLN_MODULE:
		orbit_idl_tree_fake_ops (IDL_MODULE (tree).definition_list, ns);
		break;
	case IDLN_INTERFACE:
		orbit_idl_tree_fake_ops (IDL_INTERFACE (tree).body, ns);
		break;
	case IDLN_LIST:
		for (node = tree; node; node = IDL_LIST (node).next)
			orbit_idl_tree_fake_ops (IDL_LIST (node).data, ns);
		break;
	case IDLN_ATTR_DCL:
		orbit_idl_attr_fake_ops (tree, ns);
		break;
	default:
		break;
	}
}

gboolean
orbit_idl_to_backend (const char    *filename,
		      OIDL_Run_Info *rinfo)
{
	IDL_ns   ns;
	IDL_tree tree;
	int      errcode;
	gboolean retval;

	errcode = IDL_parse_filename (
			filename, rinfo->cpp_args, NULL,
			&tree, &ns,
			(rinfo->show_cpp_errors ? IDLF_SHOW_CPP_ERRORS : 0) |
			(rinfo->is_pidl ? IDLF_XPIDL : 0) |
			(rinfo->onlytop ? IDLF_INHIBIT_INCLUDES : 0) |
			IDLF_TYPECODES |
			IDLF_SRCFILES |
			IDLF_CODEFRAGS,
			rinfo->idl_warn_level);

	rinfo->ns = ns;

	if (rinfo->debug_level > 3)
		orbit_idl_print_node (tree, 0);

	if (errcode != IDL_SUCCESS) {
		if (errcode == -1)
			g_warning ("Parse of %s failed: %s", filename, g_strerror (errno));

		return 0;
	}

	orbit_idl_tree_fake_ops (tree, ns);

	if (!strcmp (rinfo->output_language, "c")) 
		retval = orbit_idl_output_c (tree, rinfo);
	else
		retval = orbit_idl_backend_output (rinfo, tree);

	return retval;
}
