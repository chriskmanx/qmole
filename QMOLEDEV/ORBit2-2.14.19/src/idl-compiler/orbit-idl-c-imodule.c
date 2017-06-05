#include "config.h"

#include "orbit-idl-c-backend.h"

#include <string.h>

static void
ci_build_interfaces (OIDL_C_Info *ci,
		     IDL_tree     tree)
{
	if (!tree)
		return;

	switch (IDL_NODE_TYPE (tree)) {
	case IDLN_MODULE:
		ci_build_interfaces (
			ci, IDL_MODULE (tree).definition_list);
		break;
	case IDLN_LIST: {
		IDL_tree sub;
		for (sub = tree; sub; sub = IDL_LIST (sub).next)
			ci_build_interfaces (
				ci, IDL_LIST (sub).data);
		break;
	}
	case IDLN_INTERFACE: {
		char *id;

		id = IDL_ns_ident_to_qstring (IDL_IDENT_TO_NS (
			IDL_INTERFACE (tree).ident), "_", 0);

		fprintf (ci->fh, "\t&%s__iinterface,\n", id);

		g_free (id);

		ci_build_interfaces (
			ci, IDL_INTERFACE(tree).body);
		break;
	}
	default:
		break;
	}
}

static void
ci_build_types (OIDL_C_Info *ci,
		IDL_tree     tree,
		guint       *count)
{
	if (!tree)
		return;

	switch (IDL_NODE_TYPE (tree)) {
	case IDLN_MODULE:
		ci_build_types (
			ci, IDL_MODULE (tree).definition_list, count);
		break;
	case IDLN_LIST: {
		IDL_tree sub;
		for (sub = tree; sub; sub = IDL_LIST (sub).next)
			ci_build_types (
				ci, IDL_LIST (sub).data, count);
		break;
	}
	case IDLN_INTERFACE:
		ci_build_types (
			ci, IDL_INTERFACE(tree).body, count);
		break;
	case IDLN_TYPE_DCL: {
	    IDL_tree sub;
	    for (sub = IDL_TYPE_DCL (tree).dcls; sub; sub = IDL_LIST (sub).next) {
		IDL_tree ent = IDL_LIST (sub).data;
		gchar *id;

		id = orbit_cbe_get_typespec_str (ent);

		fprintf (ci->fh, "\tTC_%s,\n", id);
		(*count)++;

		g_free (id);
	    }

	    break;
	}
	case IDLN_TYPE_STRUCT: {
		gchar *id;
		IDL_tree l;

		id = orbit_cbe_get_typespec_str (tree);

		fprintf (ci->fh, "\tTC_%s,\n", id);
		(*count)++;

		g_free (id);

		/* check for nested structs/enums */
		for (l = IDL_TYPE_STRUCT (tree).member_list; l; l = IDL_LIST (l).next) {
			IDL_tree dcl;

			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_MEMBER);
			dcl = IDL_MEMBER (IDL_LIST (l).data).type_spec;

			/* skip straight declarations */
			if (IDL_NODE_TYPE(dcl) == IDLN_TYPE_STRUCT ||
			    IDL_NODE_TYPE(dcl) == IDLN_TYPE_UNION ||
			    IDL_NODE_TYPE(dcl) == IDLN_TYPE_ENUM)
				ci_build_types (ci, dcl, count);
		}
		break;
	};
	case IDLN_TYPE_UNION: {
		gchar *id;
		IDL_tree l;

		id = orbit_cbe_get_typespec_str (tree);

		fprintf (ci->fh, "\tTC_%s,\n", id);
		(*count)++;

		g_free (id);

		/* if discriminator is an enum, register it */
		if (IDL_NODE_TYPE (IDL_TYPE_UNION (tree).switch_type_spec) == IDLN_TYPE_ENUM)
			ci_build_types (
				ci, IDL_TYPE_UNION (tree).switch_type_spec, count);

		/* check for nested structs/enums */
		for (l = IDL_TYPE_UNION (tree).switch_body; l; l = IDL_LIST (l).next) {
			IDL_tree dcl;

			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_CASE_STMT);
			dcl = IDL_MEMBER (
                                IDL_CASE_STMT (IDL_LIST (l).data).element_spec).type_spec;

			if (IDL_NODE_TYPE(dcl) == IDLN_TYPE_STRUCT ||
			    IDL_NODE_TYPE(dcl) == IDLN_TYPE_UNION ||
			    IDL_NODE_TYPE(dcl) == IDLN_TYPE_ENUM)
				ci_build_types (ci, dcl, count);
		}
		break;
	}
	case IDLN_EXCEPT_DCL: {
		gchar *id;
		IDL_tree l;

		id = orbit_cbe_get_typespec_str (tree);

		fprintf (ci->fh, "\tTC_%s,\n", id);
		(*count)++;

		g_free (id);

		/* check for nested structs/enums */
		for (l = IDL_EXCEPT_DCL (tree).members; l; l = IDL_LIST (l).next) {
			IDL_tree dcl;

			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_MEMBER);
			dcl = IDL_MEMBER (IDL_LIST (l).data).type_spec;

			/* skip straight declarations */
			if (IDL_NODE_TYPE(dcl) == IDLN_TYPE_STRUCT ||
			    IDL_NODE_TYPE(dcl) == IDLN_TYPE_UNION ||
			    IDL_NODE_TYPE(dcl) == IDLN_TYPE_ENUM)
				ci_build_types (ci, dcl, count);
		}
		break;
	}
	case IDLN_TYPE_INTEGER:
	case IDLN_TYPE_ANY:
	case IDLN_TYPE_STRING:
	case IDLN_TYPE_WIDE_STRING:
	case IDLN_TYPE_CHAR:
	case IDLN_TYPE_WIDE_CHAR:
	case IDLN_TYPE_FLOAT:
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
	case IDLN_TYPE_SEQUENCE:
	case IDLN_TYPE_ENUM:
	case IDLN_IDENT:
	case IDLN_FORWARD_DCL:
	case IDLN_TYPE_OBJECT: {
		gchar *id;

		id = orbit_cbe_get_typespec_str (tree);

		fprintf (ci->fh, "\tTC_%s,\n", id);
		(*count)++;

		g_free (id);

		break;
	}
	default:
		break;
	}
}

void
orbit_idl_output_c_imodule (IDL_tree       tree,
			    OIDL_Run_Info *rinfo,
			    OIDL_C_Info   *ci)
{
	guint count;

	fprintf (ci->fh, OIDL_C_WARNING);
	fprintf (ci->fh, "#include <string.h>\n");
	fprintf (ci->fh, "#define ORBIT_IDL_C_IMODULE_%s\n\n",ci->c_base_name);

	fprintf (ci->fh, "#include \"%s-common.c\"\n\n", ci->base_name);

	fprintf (ci->fh, "#include <orbit/orb-core/orbit-small.h>\n\n");

	fprintf (ci->fh, "static CORBA_TypeCode %s__itypes[] = {\n",
		 ci->c_base_name);

	count = 0;
	ci_build_types (ci, tree, &count);

	fprintf (ci->fh, "\tNULL\n};\n\n");

	fprintf (ci->fh, "static ORBit_IInterface *%s__iinterfaces[] = {\n",
		 ci->c_base_name);

	ci_build_interfaces (ci, tree);

	fprintf (ci->fh, "\tNULL\n};\n");

	fprintf (ci->fh, "ORBit_IModule orbit_imodule_data = {\n");
	fprintf (ci->fh, "   %d,\n", ORBIT_CONFIG_SERIAL);
	fprintf (ci->fh, "   %s__iinterfaces,\n", ci->c_base_name);
	fprintf (ci->fh, " { %u, %u, %s__itypes, FALSE }\n",
		 count, count, ci->c_base_name);
	fprintf (ci->fh, "};\n\n");
}
