#include "config.h"

#include "orbit-idl-c-backend.h"

#include <string.h>

static void
cs_output_stub (IDL_tree     tree,
		OIDL_C_Info *ci,
		int         *idx)
{
	FILE     *of = ci->fh;
	char     *iface_id;
	char     *opname;
	gboolean  has_retval, has_args;

	g_return_if_fail (idx != NULL);

	iface_id = IDL_ns_ident_to_qstring (
			IDL_IDENT_TO_NS (IDL_INTERFACE (
				IDL_get_parent_node (tree, IDLN_INTERFACE, NULL)
					).ident), "_", 0);
	opname = IDL_ns_ident_to_qstring (IDL_IDENT_TO_NS (IDL_OP_DCL (tree).ident), "_", 0);

	has_retval = IDL_OP_DCL (tree).op_type_spec != NULL;
	has_args   = IDL_OP_DCL (tree).parameter_dcls != NULL;

	orbit_cbe_op_write_proto (of, tree, "", FALSE);

	fprintf (of, "{\n");

	if (has_retval) {
		orbit_cbe_write_param_typespec (of, tree);
		fprintf (of, " " ORBIT_RETVAL_VAR_NAME ";\n");
	}
#if 0
	fprintf (ci->fh, "POA_%s__epv *%s;\n", iface_id, ORBIT_EPV_VAR_NAME);
	fprintf (ci->fh, "gpointer _ORBIT_servant;\n");

	/* in-proc part */
	fprintf (ci->fh, "if ((%s = ORBit_c_stub_invoke\n", ORBIT_EPV_VAR_NAME);
	fprintf (ci->fh, "		(_obj, %s__classid, &_ORBIT_servant,\n", iface_id);
	fprintf (ci->fh, "               G_STRUCT_OFFSET (POA_%s__epv, %s)))) {\n",
		 iface_id, IDL_IDENT (IDL_OP_DCL (tree).ident).str);

	fprintf (ci->fh, "if (ORBit_small_flags & ORBIT_SMALL_FAST_LOCALS && \n");
	fprintf (ci->fh, "    ORBIT_STUB_IsBypass (_obj, %s__classid) && \n", iface_id);
	fprintf (ci->fh, "    (%s = (POA_%s__epv*) ORBIT_STUB_GetEpv (_obj, %s__classid))->%s) {\n",
		 ORBIT_EPV_VAR_NAME, iface_id, iface_id, IDL_IDENT (IDL_OP_DCL (tree).ident).str);

	fprintf (ci->fh, "ORBIT_STUB_PreCall (_obj);\n");

	fprintf (ci->fh, "%s%s->%s (_ORBIT_servant, ",
		 IDL_OP_DCL (tree).op_type_spec? ORBIT_RETVAL_VAR_NAME " = ":"",
		 ORBIT_EPV_VAR_NAME,
		 IDL_IDENT (IDL_OP_DCL (tree).ident).str);

	for (node = IDL_OP_DCL (tree).parameter_dcls; node; node = IDL_LIST (node).next)
		fprintf (ci->fh, "%s, ",
			 IDL_IDENT (IDL_PARAM_DCL (IDL_LIST (node).data).simple_declarator).str);

	if (IDL_OP_DCL (tree).context_expr)
		fprintf (ci->fh, "_ctx, ");

	fprintf (ci->fh, "ev);\n");

	fprintf (ci->fh, "ORBit_stub_post_invoke (_obj, %s);\n", ORBIT_EPV_VAR_NAME);

	fprintf (of, " } else { /* remote marshal */\n");
#endif

	/* remote invocation part */
	if (has_args)
		orbit_cbe_flatten_args (tree, of, "_args");

	fprintf (of, "ORBit_c_stub_invoke (_obj, "
		 "&%s__iinterface.methods, %d, ", iface_id, *idx);

	if (has_retval)
		fprintf (of, "&_ORBIT_retval, ");
	else
		fprintf (of, "NULL, ");

	if (has_args)
		fprintf (of, "_args, ");
	else
		fprintf (of, "NULL, ");

	if (IDL_OP_DCL (tree).context_expr)
		fprintf (ci->fh, "_ctx, ");
	else
		fprintf (ci->fh, "NULL, ");
		
	fprintf (of, "ev, ");

	fprintf (of, "%s__classid, G_STRUCT_OFFSET (POA_%s__epv, %s),\n",
		 iface_id, iface_id, IDL_IDENT (IDL_OP_DCL (tree).ident).str);
	fprintf (of, "(ORBitSmallSkeleton) _ORBIT_skel_small_%s);\n\n", opname);

	if (has_retval)
		fprintf (of, "return " ORBIT_RETVAL_VAR_NAME ";\n");

	fprintf (of, "}\n");

	g_free (iface_id);

	(*idx)++;
}

static void
cs_output_stubs (IDL_tree     tree,
		 OIDL_C_Info *ci,
		 int         *idx)
{
	if (!tree)
		return;

	switch (IDL_NODE_TYPE (tree)) {
	case IDLN_MODULE:
		cs_output_stubs (IDL_MODULE (tree).definition_list, ci, idx);
		break;
	case IDLN_LIST: {
		IDL_tree sub;

		for (sub = tree; sub; sub = IDL_LIST (sub).next)
			cs_output_stubs (IDL_LIST (sub).data, ci, idx);
		break;
		}
	case IDLN_ATTR_DCL: {
		IDL_tree node;
      
		for (node = IDL_ATTR_DCL (tree).simple_declarations; node; node = IDL_LIST (node).next) {
			OIDL_Attr_Info *ai;

			ai = IDL_LIST (node).data->data;
	
			cs_output_stubs (ai->op1, ci, idx);

			if (ai->op2)
				cs_output_stubs (ai->op2, ci, idx);
		}
		break;
		}
	case IDLN_INTERFACE: {
		int real_idx = 0;

		cs_output_stubs (IDL_INTERFACE (tree).body, ci, &real_idx);
		break;
		}
	case IDLN_OP_DCL:
		cs_output_stub (tree, ci, idx);
		break;
	default:
		break;
	}
}

void
orbit_idl_output_c_stubs (IDL_tree       tree,
			  OIDL_Run_Info *rinfo,
			  OIDL_C_Info   *ci)
{
	fprintf (ci->fh, OIDL_C_WARNING);
	fprintf (ci->fh, "#include <string.h>\n");
	fprintf (ci->fh, "#define ORBIT2_STUBS_API\n");
	fprintf (ci->fh, "#include \"%s.h\"\n\n", ci->base_name);

	cs_output_stubs (tree, ci, NULL);
}
