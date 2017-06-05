#include "orbit-idl-c-backend.h"

#include <string.h>

typedef struct {
	IDL_tree     ts;
	char        *structname;
	char        *substructname;
	int          array_gen_ctr;
} CBETCGenInfo;

static int random_id = 0;

static char *orbit_generate_tcstruct_name (IDL_tree     ts);
static void  cbe_tc_generate              (OIDL_C_Info *ci, CBETCGenInfo *tci);

void
orbit_output_typecode (OIDL_C_Info *ci,
		       IDL_tree     node)
{
	CBETCGenInfo tci;

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_DCL:
	case IDLN_TYPE_STRUCT:
	case IDLN_TYPE_UNION:
	case IDLN_TYPE_ENUM:
	case IDLN_EXCEPT_DCL:
	case IDLN_TYPE_SEQUENCE:
	case IDLN_INTERFACE:
		break;
	default:
		g_error ("You can't produce a typecode for a %s", 
			 IDL_tree_type_names[IDL_NODE_TYPE (node)]);
	}

	tci.ts            = node;
	tci.structname    = orbit_generate_tcstruct_name (node);
	tci.substructname = NULL;
	tci.array_gen_ctr = 0;

	cbe_tc_generate (ci, &tci);

	g_free (tci.structname);
}

static char *
orbit_generate_tcstruct_name (IDL_tree node)
{
	GString *tmpstr;
	char    *retval;

	tmpstr = g_string_new (NULL);

	switch (IDL_NODE_TYPE (node)) {
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
	case IDLN_TYPE_STRUCT:
	case IDLN_TYPE_UNION:
	case IDLN_TYPE_ENUM:
	case IDLN_IDENT:
	case IDLN_EXCEPT_DCL:
	case IDLN_INTERFACE:
	case IDLN_FORWARD_DCL:
	case IDLN_TYPE_OBJECT: {
		char *typespec = orbit_cbe_get_typespec_str (node);

		g_string_printf (tmpstr, "TC_%s", typespec);

		g_free (typespec);
		}
		break;
	default:
		g_string_printf (tmpstr, "anonTC_%d", random_id++);
		break;
	}

	retval = tmpstr->str;
	g_string_free (tmpstr, FALSE);

	return retval;
}

static void
orbit_output_tcstruct_anon_subnames_array (FILE *fh, IDL_tree node, int subnames_id)
{
	IDL_tree l;

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_ENUM:
		if (!IDL_TYPE_ENUM (node).enumerator_list)
			break;

		fprintf (fh, "static const char *anon_subnames_array%d[] = {", subnames_id);

		for (l = IDL_TYPE_ENUM (node).enumerator_list; l; l = IDL_LIST (l).next) {
			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_IDENT);

			fprintf (fh, "\"%s\"", IDL_IDENT (IDL_LIST (l).data).str );
			if (IDL_LIST (l).next)
				fprintf (fh, ", ");
		}

		fprintf (fh, "};\n");
		break;
	case IDLN_EXCEPT_DCL:
	case IDLN_TYPE_STRUCT:
		if (!IDL_TYPE_STRUCT (node).member_list)
			break;

		fprintf (fh, "static const char *anon_subnames_array%d[] = {", subnames_id);

		for (l = IDL_TYPE_STRUCT (node).member_list; l; l = IDL_LIST (l).next) {
			IDL_tree dcl;

			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_MEMBER);

			for (dcl = IDL_MEMBER (IDL_LIST (l).data).dcls; dcl;
			     dcl = IDL_LIST (dcl).next) {
				IDL_tree p = IDL_LIST (dcl).data;

				g_assert (IDL_NODE_TYPE (p) == IDLN_IDENT ||
					  IDL_NODE_TYPE (p) == IDLN_TYPE_ARRAY);

				if (IDL_NODE_TYPE (p) == IDLN_IDENT)
					fprintf (fh, "\"%s\"", IDL_IDENT (p).str);

				else /* IDLN_TYPE_ARRAY */
					fprintf (fh, "\"%s\"", 
						 IDL_IDENT (IDL_TYPE_ARRAY (p).ident).str);
	
				if (IDL_LIST (dcl).next || IDL_LIST (l).next)
					fprintf (fh, ", ");
			}
		}

		fprintf (fh, "};\n");
		break;
	case IDLN_TYPE_UNION:
		if (!IDL_TYPE_UNION (node).switch_body)
			break;

		fprintf (fh, "static const char * anon_subnames_array%d[] = {", subnames_id);

		for (l = IDL_TYPE_UNION (node).switch_body; l; l = IDL_LIST (l).next) {
			IDL_tree dcl, label;
			const char *subname;

			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_CASE_STMT);

			dcl = IDL_LIST (IDL_MEMBER (
				IDL_CASE_STMT (IDL_LIST (l).data).element_spec).dcls).data;

			g_assert (IDL_NODE_TYPE (dcl) == IDLN_IDENT ||
				  IDL_NODE_TYPE (dcl) == IDLN_TYPE_ARRAY);

			if (IDL_NODE_TYPE (dcl) == IDLN_IDENT)
				subname = IDL_IDENT (dcl).str;
			else /* IDLN_TYPE_ARRAY */
				subname = IDL_IDENT (IDL_TYPE_ARRAY (dcl).ident).str;

			/* output the name once for each label */
			for (label = IDL_CASE_STMT (IDL_LIST (l).data).labels;
			     label != NULL; label = IDL_LIST (label).next) {
				fprintf (fh, "\"%s\"", subname);
				if (IDL_LIST (label).next)
					fprintf (fh, ", ");
			}

			if (IDL_LIST (l).next)
				fprintf (fh, ", ");
		}
		fprintf (fh, "};\n");
		break;
	default:
		break;
	}
}

static void
orbit_output_tcstruct_anon_subtypes_array (FILE     *fh,
					   IDL_tree  node,
					   int       subtypes_id,
					   char     *substructname)
{
	IDL_tree l;

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_EXCEPT_DCL:
	case IDLN_TYPE_STRUCT:
		if (!IDL_TYPE_STRUCT (node).member_list)
			break;

		fprintf (fh, "static ORBIT2_MAYBE_CONST CORBA_TypeCode anon_subtypes_array%d[] = {",
			 subtypes_id);

		for (l = IDL_TYPE_STRUCT (node).member_list; l; l = IDL_LIST (l).next) {
			IDL_tree  dcl;
			char     *tmpstr;

			dcl = IDL_MEMBER (IDL_LIST (l).data).type_spec;

			switch (IDL_NODE_TYPE (dcl)) {
			case IDLN_IDENT:
			case IDLN_INTERFACE:
			case IDLN_TYPE_OBJECT:
			case IDLN_FORWARD_DCL:
				dcl = orbit_cbe_get_typespec (dcl);

				if (IDL_NODE_DECLSPEC (dcl) & IDLF_DECLSPEC_PIDL &&
				   (IDL_NODE_TYPE (dcl) == IDLN_INTERFACE ||
				    IDL_NODE_TYPE (dcl) == IDLN_FORWARD_DCL))
					tmpstr = g_strdup ("Object");
				else
					tmpstr = orbit_cbe_get_typespec_str (
							IDL_MEMBER (IDL_LIST (l).data).type_spec);
				break;
			default:
				tmpstr = orbit_cbe_get_typespec_str (
						IDL_MEMBER (IDL_LIST (l).data).type_spec);
				break;
			}

			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_MEMBER);

			for (dcl = IDL_MEMBER (IDL_LIST (l).data).dcls; dcl;
			     dcl = IDL_LIST (dcl).next) {

				fprintf (fh, "(CORBA_TypeCode)&TC_%s_struct", tmpstr);

				if (IDL_LIST (dcl).next || IDL_LIST (l).next)
					fprintf (fh, ", ");
			}

			g_free (tmpstr);
		}

		fprintf (fh, "};\n");

		break;
	case IDLN_TYPE_UNION:
		if (!IDL_TYPE_UNION (node).switch_body)
			break;

		fprintf (fh, "static ORBIT2_MAYBE_CONST CORBA_TypeCode anon_subtypes_array%d[] = {", subtypes_id);

		for (l = IDL_TYPE_UNION (node).switch_body; l; l = IDL_LIST (l).next) {
			IDL_tree  label, dcl;
			char     *tmpstr;

			g_assert (IDL_NODE_TYPE (IDL_LIST (l).data) == IDLN_CASE_STMT);

			dcl = IDL_MEMBER (IDL_CASE_STMT (IDL_LIST (l).data).element_spec).type_spec;

			switch (IDL_NODE_TYPE (orbit_cbe_get_typespec (dcl))) {
			case IDLN_INTERFACE:
			case IDLN_FORWARD_DCL:
				if (IDL_NODE_DECLSPEC (dcl) & IDLF_DECLSPEC_PIDL)
					tmpstr = g_strdup ( "Object");
				else
					tmpstr = orbit_cbe_get_typespec_str (dcl);
				break;
			default:
				tmpstr = orbit_cbe_get_typespec_str (dcl);
				break;
			}

			for (label = IDL_CASE_STMT (IDL_LIST (l).data).labels; label;
			     label = IDL_LIST (label).next) {
				fprintf (fh, "(CORBA_TypeCode)&TC_%s_struct", tmpstr);

				if (IDL_LIST (label).next || IDL_LIST (l).next)
					fprintf (fh, ", ");
			}

			g_free (tmpstr);
		}

		fprintf (fh, "};\n");

		break;
	case IDLN_TYPE_SEQUENCE: {
		IDL_tree  seqts;
		char     *tmpstr;

		seqts = orbit_cbe_get_typespec (IDL_TYPE_SEQUENCE (node).simple_type_spec);

		fprintf (fh, "static ORBIT2_MAYBE_CONST CORBA_TypeCode anon_subtypes_array%d[] = ", subtypes_id);

		switch (IDL_NODE_TYPE (seqts)) {
		case IDLN_INTERFACE:
		case IDLN_FORWARD_DCL:
			if (IDL_NODE_DECLSPEC (seqts) && IDLF_DECLSPEC_PIDL)
				tmpstr = g_strdup ("Object");
			else
				tmpstr = orbit_cbe_get_typespec_str (
						IDL_TYPE_SEQUENCE (node).simple_type_spec);
			break;
		default:
			tmpstr = orbit_cbe_get_typespec_str (
						IDL_TYPE_SEQUENCE (node).simple_type_spec);
			break;
		}

		fprintf (fh, "{(CORBA_TypeCode)&TC_%s_struct};\n", tmpstr);
		g_free (tmpstr);

		}
		break;
	case IDLN_TYPE_ARRAY:
	case IDLN_IDENT:
		fprintf (fh, "static ORBIT2_MAYBE_CONST CORBA_TypeCode anon_subtypes_array%d[] = ", subtypes_id);
		fprintf (fh, "{(CORBA_TypeCode)&%s_struct};\n", substructname);
		break;
	default:
		break;
	}
}

static int
orbit_output_tcstruct_anon_sublabels_array (FILE     *fh, 
					    IDL_tree  node,
					    int       sublabels_id)
{
	IDL_tree l, label;
	int      index = 0;
	int      default_index = -1;

	if (IDL_NODE_TYPE (node) != IDLN_TYPE_UNION ||
	    !IDL_TYPE_UNION (node).switch_body)
		return default_index;

	fprintf (fh, "static ORBIT2_MAYBE_CONST CORBA_long anon_sublabels_array%d[] = {", sublabels_id);

	for (l = IDL_TYPE_UNION (node).switch_body; l; l = IDL_LIST (l).next)
		for (label = IDL_CASE_STMT (IDL_LIST (l).data).labels; label;
		     label = IDL_LIST (label).next, index++) {

			if (IDL_LIST (label).data) {
				fprintf (fh, "(CORBA_long) ");

				orbit_cbe_write_const (fh, IDL_LIST (label).data);

			} else { /* default case */
				fprintf (fh, "-1");
				default_index = index;
			}

			if (IDL_LIST (label).next || IDL_LIST (l).next)
				fprintf (fh, ", ");
		}

	fprintf (fh, "};\n");

	return default_index;
}

static void
orbit_output_tcstruct_parent (FILE *fh)
{
	fprintf (fh, "{&ORBit_TypeCode_epv, ORBIT_REFCOUNT_STATIC}");
}

static void
orbit_output_tcstruct_kind (FILE *fh, IDL_tree node, int array_gen_ctr)
{
	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_ARRAY:
		if (array_gen_ctr)
			fprintf (fh, "CORBA_tk_array");
		else
			fprintf (fh, "CORBA_tk_alias");
		break;
	case IDLN_IDENT:
		fprintf (fh, "CORBA_tk_alias");
		break;
	case IDLN_TYPE_STRUCT:
		fprintf (fh, "CORBA_tk_struct");
		break;
	case IDLN_TYPE_UNION:
		fprintf (fh, "CORBA_tk_union");
		break;
	case IDLN_TYPE_ENUM:
		fprintf (fh, "CORBA_tk_enum");
		break;
	case IDLN_TYPE_OBJECT:
	case IDLN_INTERFACE:
	case IDLN_FORWARD_DCL:
		fprintf (fh, "CORBA_tk_objref");
		break;
	case IDLN_EXCEPT_DCL:
		fprintf (fh, "CORBA_tk_except");
		break;
	case IDLN_TYPE_INTEGER:
		fprintf (fh, "CORBA_tk_");

		if (!IDL_TYPE_INTEGER (node).f_signed)
			fprintf (fh, "u");

		switch (IDL_TYPE_INTEGER (node).f_type) {
		case IDL_INTEGER_TYPE_SHORT:
			fprintf (fh, "short");
			break;
		case IDL_INTEGER_TYPE_LONG:
			fprintf (fh, "long");
			break;
		case IDL_INTEGER_TYPE_LONGLONG:
			fprintf (fh, "longlong");
			break;
		}
		break;
	case IDLN_TYPE_FLOAT:
		fprintf (fh, "CORBA_tk_");

		switch (IDL_TYPE_FLOAT (node).f_type) {
		case IDL_FLOAT_TYPE_FLOAT:
			fprintf (fh, "float");
			break;
		case IDL_FLOAT_TYPE_DOUBLE:
			fprintf (fh, "double");
			break;
		case IDL_FLOAT_TYPE_LONGDOUBLE:
			fprintf (fh, "longdouble");
			break;
		}
		break;
	case IDLN_TYPE_BOOLEAN:
		fprintf (fh, "CORBA_tk_boolean");
		break;
	case IDLN_TYPE_OCTET:
		fprintf (fh, "CORBA_tk_octet");
		break;
	case IDLN_TYPE_STRING:
		fprintf (fh, "CORBA_tk_string");
		break;
	case IDLN_TYPE_WIDE_STRING:
		fprintf (fh, "CORBA_tk_wstring");
		break;
	case IDLN_TYPE_CHAR:
		fprintf (fh, "CORBA_tk_char");
		break;
	case IDLN_TYPE_WIDE_CHAR:
		fprintf (fh, "CORBA_tk_wchar");
		break;
	case IDLN_TYPE_ANY:
		fprintf (fh, "CORBA_tk_any");
		break;
	case IDLN_TYPE_SEQUENCE:
		fprintf (fh, "CORBA_tk_sequence");
		break;
	default:
		g_message ("Type %s has no tk constant!",
			   IDL_tree_type_names[IDL_NODE_TYPE (node)]);
	}
}

static void
orbit_output_tcstruct_name (FILE *fh, IDL_tree node, int array_gen_ctr)
{
	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_STRUCT:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_TYPE_STRUCT (node).ident).str);
		break;
	case IDLN_TYPE_UNION:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_TYPE_UNION (node).ident).str);
		break;
	case IDLN_TYPE_ENUM:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_TYPE_ENUM (node).ident).str);
		break;
	case IDLN_INTERFACE:
	case IDLN_FORWARD_DCL:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_INTERFACE (node).ident).str);
		break;
	case IDLN_EXCEPT_DCL:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_EXCEPT_DCL (node).ident).str);
		break;
	case IDLN_IDENT:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (node).str);
		break;
	case IDLN_TYPE_ARRAY:
		if (!array_gen_ctr)
			fprintf (fh, "(char *)\"%s\"", 
				 IDL_IDENT (IDL_TYPE_ARRAY (node).ident).str);
		else
			fprintf (fh, "NULL");
		break;
	default:
		fprintf (fh, "NULL");
		break;
	}
}

static void
orbit_output_tcstruct_repo_id (FILE *fh, IDL_tree node, int array_gen_ctr)
{
	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_STRUCT:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_TYPE_STRUCT (node).ident).repo_id);
		break;
	case IDLN_TYPE_UNION:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_TYPE_UNION (node).ident).repo_id);
		break;
	case IDLN_TYPE_ENUM:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_TYPE_ENUM (node).ident).repo_id);
		break;
	case IDLN_INTERFACE:
	case IDLN_FORWARD_DCL:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_INTERFACE (node).ident).repo_id);
		break;
	case IDLN_EXCEPT_DCL:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_EXCEPT_DCL (node).ident).repo_id);
		break;
	case IDLN_IDENT:
		fprintf (fh, "(char *)\"%s\"", IDL_IDENT (node).repo_id);
		break;
	case IDLN_TYPE_ARRAY:
		if (!array_gen_ctr)
			fprintf (fh, "(char *)\"%s\"", IDL_IDENT (IDL_TYPE_ARRAY (node).ident).repo_id);
		else
			fprintf (fh, "NULL");
		break;
	default:
		fprintf (fh, "NULL");
		break;
	}
}

static void
orbit_output_tcstruct_length (FILE *fh, IDL_tree node, int array_gen_ctr)
{
	int length = 0;

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_SEQUENCE:
		if (IDL_TYPE_SEQUENCE (node).positive_int_const)
			length = IDL_INTEGER (IDL_TYPE_SEQUENCE (node).positive_int_const).value;
		break;
	case IDLN_TYPE_STRING:
		if (IDL_TYPE_STRING (node).positive_int_const)
			length = IDL_INTEGER (IDL_TYPE_STRING (node).positive_int_const).value;
		break;
	case IDLN_TYPE_WIDE_STRING:
		if (IDL_TYPE_WIDE_STRING (node).positive_int_const)
			length = IDL_INTEGER (IDL_TYPE_STRING (node).positive_int_const).value;
		break;
	case IDLN_TYPE_ARRAY:
		if (array_gen_ctr) {
			IDL_tree sizer;

			sizer = IDL_list_nth (IDL_TYPE_ARRAY (node).size_list,
					      array_gen_ctr - 1);

			g_assert (IDL_NODE_TYPE (IDL_LIST (sizer).data) == IDLN_INTEGER);

			length = IDL_INTEGER (IDL_LIST (sizer).data).value;
		}
		break;
	default:
		length = 0;
		break;
	}

	fprintf (fh, "%d", length);
}

static void
orbit_output_tcstruct_sub_parts (FILE *fh, IDL_tree node)
{
	int length = 0;

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_STRUCT:
	case IDLN_EXCEPT_DCL: {
		IDL_tree l;

		for (l = IDL_TYPE_STRUCT (node).member_list; l; l = IDL_LIST (l).next) {
			IDL_tree member;

			member = IDL_LIST (l).data;

			g_assert (IDL_NODE_TYPE (member) == IDLN_MEMBER);

			length += IDL_list_length (IDL_MEMBER (member).dcls);
		}
		}
		break;
	case IDLN_TYPE_UNION: {
		IDL_tree l;

		for (l = IDL_TYPE_UNION (node).switch_body; l; l = IDL_LIST (l).next) {
			IDL_tree case_stmt;

			case_stmt = IDL_LIST (l).data;

			g_assert (IDL_NODE_TYPE (case_stmt) == IDLN_CASE_STMT);

			length += IDL_list_length (IDL_CASE_STMT (case_stmt).labels);
		}
		}
		break;
	case IDLN_TYPE_ENUM:
		length = IDL_list_length (IDL_TYPE_ENUM (node).enumerator_list);
		break;
	case IDLN_IDENT:
	case IDLN_TYPE_SEQUENCE:
	case IDLN_TYPE_ARRAY:
		length = 1;
		break;
	default:
		length = 0;
		break;
	}

	fprintf (fh, "%d\n", length);
}

static void
orbit_output_tcstruct_subnames (FILE *fh, IDL_tree node, int subnames_id)
{
	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_ENUM:
		if (IDL_TYPE_ENUM (node).enumerator_list)
			fprintf (fh, "(char **)anon_subnames_array%d", subnames_id);
		else
			fprintf (fh, "NULL");
		break;
	case IDLN_TYPE_UNION:
		if (IDL_TYPE_UNION (node).switch_body)
			fprintf (fh, "(char **)anon_subnames_array%d", subnames_id);
		else
			fprintf (fh, "NULL");
		break;
	case IDLN_TYPE_STRUCT:
	case IDLN_EXCEPT_DCL:
		if (IDL_TYPE_STRUCT (node).member_list)
			fprintf (fh, "(char **)anon_subnames_array%d", subnames_id);
		else
			fprintf (fh, "NULL");
		break;
	default:
		fprintf (fh, "NULL");
		break;
	}
}

static void
orbit_output_tcstruct_subtypes (FILE *fh, IDL_tree node, int subtypes_id)
{
	switch (IDL_NODE_TYPE (node)) {
	case IDLN_EXCEPT_DCL:
	case IDLN_TYPE_STRUCT:
		if (IDL_TYPE_STRUCT (node).member_list)
			fprintf (fh, "(CORBA_TypeCode *)anon_subtypes_array%d", subtypes_id);
		else
			fprintf (fh, "NULL");
		break;
	case IDLN_TYPE_UNION:
		if (IDL_TYPE_UNION (node).switch_body)
			fprintf (fh, "(CORBA_TypeCode *)anon_subtypes_array%d", subtypes_id);
		else
			fprintf (fh, "NULL");
		break;
	case IDLN_TYPE_SEQUENCE:
	case IDLN_TYPE_ARRAY:
	case IDLN_IDENT:
		fprintf (fh, "(CORBA_TypeCode *)anon_subtypes_array%d", subtypes_id);
		break;
	default:
		fprintf (fh, "NULL");
		break;
	}
}

static void
orbit_output_tcstruct_sublabels (FILE *fh, IDL_tree node, int sublabels_id)
{
	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_UNION:
		fprintf (fh, "(CORBA_long *)anon_sublabels_array%d", sublabels_id);
		break;
	default:
		fprintf (fh, "NULL");
		break;
	}
}

static void
orbit_output_tcstruct_discriminator (FILE *fh, IDL_tree node)
{
	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_UNION: {
		char *switch_type_spec_str;

		switch_type_spec_str = 
			orbit_cbe_get_typespec_str (IDL_TYPE_UNION (node).switch_type_spec);

		fprintf (fh, "(CORBA_TypeCode)&TC_%s_struct", switch_type_spec_str);

		g_free (switch_type_spec_str);
		}
		break;
	default:
		fprintf (fh, "CORBA_OBJECT_NIL");
		break;
	}
}

static void
orbit_output_tcstruct_recurse_depth (FILE *fh)
{
	fprintf (fh, "0");
}

static void
orbit_output_tcstruct_default_index (FILE *fh, int union_default_index)
{
	fprintf (fh, "%d", union_default_index);
}

static void
orbit_output_tcstruct_digits_scale (FILE *fh, IDL_tree node)
{
	if (IDL_NODE_TYPE (node) == IDLN_TYPE_FIXED) {
		fprintf (fh, "%" IDL_LL "d, %" IDL_LL "d" ,
			 IDL_INTEGER (IDL_TYPE_FIXED (node).positive_int_const).value,
			 IDL_INTEGER (IDL_TYPE_FIXED (node).integer_lit).value);
	}
	else
		fprintf (fh, "0, 0");
}

static void
orbit_add_align (GSList **max, const char *str)
{
	GSList *l;

	for (l = *max; l; l = l->next) {
		if (!strcmp (l->data, str))
			return;
	}
	*max = g_slist_prepend (*max, (gpointer) str);
}

static GSList *
orbit_find_c_align (GSList *max, IDL_tree node)
{
	node = orbit_cbe_get_typespec (node);	

	switch (IDL_NODE_TYPE (node)) {
	case IDLN_TYPE_INTEGER:
		switch (IDL_TYPE_INTEGER (node).f_type) {
		case IDL_INTEGER_TYPE_SHORT:
			orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_SHORT");
			break;
		case IDL_INTEGER_TYPE_LONG:
			orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_LONG");
			break;
		case IDL_INTEGER_TYPE_LONGLONG:
			orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_LONG_LONG");
			break;
		}
		break;
	case IDLN_TYPE_FLOAT:
		switch (IDL_TYPE_FLOAT (node).f_type) {
		case IDL_FLOAT_TYPE_FLOAT:
			orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_FLOAT");
			break;
		case IDL_FLOAT_TYPE_DOUBLE:
			orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_DOUBLE");
			break;
		case IDL_FLOAT_TYPE_LONGDOUBLE:
			orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_LONG_DOUBLE");
			break;
		}
		break;
	case IDLN_TYPE_ENUM:
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_LONG");
		break;
	case IDLN_TYPE_CHAR: /* drop through */
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_CHAR");
		break;
	case IDLN_TYPE_WIDE_CHAR:
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_SHORT");
		break;
	case IDLN_TYPE_UNION: {
		IDL_tree l = IDL_TYPE_UNION (node).switch_body;

		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_STRUCT");

		for (; l; l = IDL_LIST (l).next) {
			IDL_tree subtype = IDL_MEMBER (IDL_CASE_STMT (
				IDL_LIST (l).data).element_spec).type_spec;
			max = orbit_find_c_align (max, subtype);
		}
		break;
	}
	case IDLN_EXCEPT_DCL: /* drop through */
	case IDLN_TYPE_STRUCT: {
		IDL_tree l = IDL_TYPE_STRUCT (node).member_list;
					
		for (; l; l = IDL_LIST (l).next) {
			IDL_tree member = IDL_MEMBER (IDL_LIST (l).data).type_spec;

			max = orbit_find_c_align (max, member);
		}
		break;
	}
	case IDLN_TYPE_STRING: /* drop through */
	case IDLN_TYPE_WIDE_STRING:
	case IDLN_TYPE_OBJECT:
	case IDLN_TYPE_TYPECODE:
	case IDLN_FORWARD_DCL:
	case IDLN_INTERFACE:
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_POINTER");
		break;
	case IDLN_TYPE_ARRAY: {
		IDL_tree subtype = IDL_TYPE_DCL (
			IDL_get_parent_node (node, IDLN_TYPE_DCL, NULL)).type_spec;
		max = orbit_find_c_align (max, subtype);
		break;
	}
	case IDLN_TYPE_SEQUENCE:
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_STRUCT");
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_LONG");
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_POINTER");
		break;
	case IDLN_TYPE_ANY:
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_STRUCT");
		orbit_add_align (&max, "ORBIT_ALIGNOF_CORBA_POINTER");
		break;
	default:
		g_error ("Can't find alignment %s\n", 
			 IDL_tree_type_names[IDL_NODE_TYPE (node)]);
		break;
	}

	return max;
}

static void
orbit_output_tcstruct_c_align (FILE *fh, IDL_tree node)
{
	GSList *max;
	GString *str = g_string_sized_new (120);

	max = orbit_find_c_align (NULL, node);

	if (!max)
		g_string_append (str, "1");

	else if (!max->next)
		g_string_append (str, max->data);

	else {
		int i = 0;
		GSList *l;

		for (l = max; l; l = l->next) {
			g_string_append (str, "MAX (");
			g_string_append (str, l->data);
			g_string_append (str, ", ");
			i++;
		}
		
		g_string_append (str, "1");
		for (; i > 0; i--)
			g_string_append_c (str, ')');
	}

	fprintf (fh, "%s", str->str);

	g_string_free (str, TRUE);
}

static void
cbe_tc_generate (OIDL_C_Info  *ci,
		 CBETCGenInfo *tci)
{
	CBETCGenInfo  subtci;
	IDL_tree      curitem;
	char         *ctmp;
	int           union_default_index = -1,
		      subnames_id  = random_id++,
		      subtypes_id  = random_id++,
		      sublabels_id = random_id++;

	if (strncmp (tci->structname, "anon", 4)) {
		fprintf (ci->fh, "#if ");
		orbit_cbe_id_cond_hack (ci->fh, "TC_IMPL",
					tci->structname, ci->c_base_name);
		fprintf (ci->fh, " && !defined(TC_DEF_%s)\n", tci->structname);
		fprintf (ci->fh, "#define TC_DEF_%s 1\n", tci->structname);
	}

	if (IDL_NODE_TYPE (tci->ts) == IDLN_TYPE_DCL) {
		subtci = *tci;

		curitem = IDL_TYPE_DCL (tci->ts).type_spec;
		subtci.substructname = ctmp = orbit_generate_tcstruct_name (curitem);

		/* 
		 * The only type not already defined elsewhere
		 * that can be in the left half of a TypeCode.
		 */
		if (IDL_NODE_TYPE (curitem) == IDLN_TYPE_SEQUENCE) {
			subtci.structname = ctmp;
			subtci.ts         = curitem;
			cbe_tc_generate (ci, &subtci);
		}

		for (curitem = IDL_TYPE_DCL (tci->ts).dcls; curitem;
		     curitem = IDL_LIST (curitem).next) {
			subtci.ts = IDL_LIST (curitem).data;

			if (IDL_NODE_TYPE (subtci.ts) == IDLN_TYPE_ARRAY)
				subtci.structname = orbit_generate_tcstruct_name (
							IDL_TYPE_ARRAY (subtci.ts).ident);
			else
				subtci.structname = orbit_generate_tcstruct_name (subtci.ts);

			cbe_tc_generate (ci, &subtci);
			g_free (subtci.structname);
		}

	g_free (ctmp);
	return;
	}

	/* Do magic here - nesting of typecodes for arrays */
	if (IDL_NODE_TYPE (tci->ts) == IDLN_TYPE_ARRAY && 
            (IDL_list_length (IDL_TYPE_ARRAY (tci->ts).size_list) > tci->array_gen_ctr)) {

		curitem = IDL_list_nth (IDL_TYPE_ARRAY (tci->ts).size_list,
					tci->array_gen_ctr - 1);

		subtci = *tci;
		subtci.structname = ctmp = orbit_generate_tcstruct_name (curitem);
		subtci.array_gen_ctr++;

		cbe_tc_generate (ci, &subtci);

		tci->substructname = ctmp; /* FIXME: memory leak */
	}

	orbit_output_tcstruct_anon_subnames_array  (ci->fh, tci->ts, subnames_id);
	orbit_output_tcstruct_anon_subtypes_array  (ci->fh, tci->ts, subtypes_id,
						    tci->substructname);

	union_default_index = orbit_output_tcstruct_anon_sublabels_array (
					ci->fh, tci->ts, sublabels_id);

	if (!strncmp (tci->structname, "anon", 4))
		fprintf (ci->fh, "static ");
	else {
		fprintf (ci->fh, "#ifdef ORBIT_IDL_C_IMODULE_%s\n",
			 ci->c_base_name);
		fprintf (ci->fh, "static\n");
		fprintf (ci->fh, "#endif\n");
	}

	fprintf (ci->fh, "ORBIT2_MAYBE_CONST struct CORBA_TypeCode_struct %s_struct = {\n",
				tci->structname);

	orbit_output_tcstruct_parent (ci->fh);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_kind (ci->fh, tci->ts, tci->array_gen_ctr);

	fprintf (ci->fh, ",\n");

	/* flags */
	fprintf (ci->fh, "0,\n");

	/* c_length */
	fprintf (ci->fh, "0,\n");

	orbit_output_tcstruct_c_align (ci->fh, tci->ts);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_length (ci->fh, tci->ts, tci->array_gen_ctr);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_sub_parts (ci->fh, tci->ts);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_subtypes (ci->fh, tci->ts, subtypes_id);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_discriminator (ci->fh, tci->ts);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_name (ci->fh, tci->ts, tci->array_gen_ctr);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_repo_id (ci->fh, tci->ts, tci->array_gen_ctr);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_subnames (ci->fh, tci->ts, subnames_id);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_sublabels (ci->fh, tci->ts, sublabels_id);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_default_index (ci->fh, union_default_index);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_recurse_depth (ci->fh);

	fprintf (ci->fh, ",\n");

	orbit_output_tcstruct_digits_scale (ci->fh, tci->ts);

	fprintf (ci->fh, "\n};\n");

	if (strncmp (tci->structname, "anon", 4))
		fprintf (ci->fh, "#endif\n");
}
