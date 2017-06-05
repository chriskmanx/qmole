/*
 * Copyright (C) 2001 Maciej Stachowiak, Ximian Inc.
 */

#include "config.h"
#include "orbit-idl-c-backend.h"

#include <string.h>
#include <ctype.h>

static void
output_deps (IDL_tree tree, 
	     OIDL_Run_Info *rinfo, 
	     OIDL_C_Info *ci)
{
	if (!tree)
		return;

	switch (IDL_NODE_TYPE (tree)) {
	case IDLN_SRCFILE: {
		char *idlfn = IDL_SRCFILE (tree).filename;
		fprintf (ci->fh, " \\\n\t%s", idlfn);
		break;
	}

	case IDLN_MODULE:
		output_deps (IDL_MODULE (tree).definition_list, rinfo, ci);
		break;

	case IDLN_LIST: {
		IDL_tree sub;

		for (sub = tree; sub; sub = IDL_LIST (sub).next)
			output_deps (IDL_LIST (sub).data, rinfo, ci);
		break;
	}

	case IDLN_INTERFACE:
		output_deps (IDL_INTERFACE (tree).body, rinfo, ci);
		break;

	default:
		break;
	}
}

void
orbit_idl_output_c_deps (IDL_tree       tree,
			 OIDL_Run_Info *rinfo, 
			 OIDL_C_Info   *ci)
{
	int i;

	g_return_if_fail (ci->fh != NULL);

	for (i = 0; i < OUTPUT_NUM_PASSES - 1; i++) {
		char *name = orbit_idl_c_filename_for_pass (
			rinfo->input_filename, 1 << i);
		fprintf (ci->fh, "%s ", name);
		g_free (name);
	}

	fprintf (ci->fh, ": ");
  
	output_deps (tree, rinfo, ci);

	fprintf (ci->fh, "\n");
}
