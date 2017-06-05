#include "config.h"

#include "orbit-idl-c-backend.h"

#include <string.h>

char *
orbit_cbe_get_typecode_name (IDL_tree tree)
{
	if (!tree)
		return g_strdup ("TC_FIXME");
	else
		return g_strconcat ("TC_", orbit_cbe_get_typespec_str (tree), NULL);
}

gboolean
orbit_cbe_type_is_builtin(IDL_tree tree)
{ 
  return FALSE;
  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_LIST:
  case IDLN_GENTREE:
  case IDLN_MEMBER:
  case IDLN_NATIVE:
  case IDLN_CASE_STMT:
  case IDLN_MODULE:
  case IDLN_BINOP:
  case IDLN_UNARYOP:
  case IDLN_CODEFRAG:
    g_error("Strange type for being a builtin");
    break;
  case IDLN_INTEGER:
  case IDLN_STRING:
  case IDLN_WIDE_STRING:
  case IDLN_CHAR:
  case IDLN_WIDE_CHAR:
  case IDLN_FIXED:
  case IDLN_FLOAT:
  case IDLN_BOOLEAN:
  case IDLN_CONST_DCL:
  case IDLN_TYPE_INTEGER:
  case IDLN_TYPE_FLOAT:
  case IDLN_TYPE_CHAR:
  case IDLN_TYPE_WIDE_CHAR:
  case IDLN_TYPE_STRING:
  case IDLN_TYPE_WIDE_STRING:
  case IDLN_TYPE_BOOLEAN:
  case IDLN_TYPE_OCTET:
  case IDLN_TYPE_ANY:
  case IDLN_TYPE_OBJECT:
  case IDLN_TYPE_TYPECODE:
  case IDLN_TYPE_ENUM:
    return TRUE;
    break;
  case IDLN_TYPE_DCL:
  case IDLN_EXCEPT_DCL:
  case IDLN_ATTR_DCL:
  case IDLN_OP_DCL:
  case IDLN_PARAM_DCL:
  case IDLN_TYPE_FIXED:
  case IDLN_TYPE_SEQUENCE:
  case IDLN_TYPE_ARRAY:
  case IDLN_TYPE_STRUCT:
  case IDLN_TYPE_UNION:
  case IDLN_IDENT:
  case IDLN_INTERFACE:
  case IDLN_FORWARD_DCL:
  default:
    return FALSE;
    break;
  }

  return FALSE;
}

/**
    Gets the "type" of {tree} as known in C.
    The return value was alloc'd via g_malloc, and must be g_free'd.
**/
char *
orbit_cbe_get_typespec_str(IDL_tree tree)
{
  char *retval = NULL;
  GString *tmpstr = NULL;

  if(!tree) {
    return g_strdup("void");
  }

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_MEMBER:
    return orbit_cbe_get_typespec_str(IDL_MEMBER(tree).type_spec);
    break;
  case IDLN_TYPE_ANY:
    retval = "CORBA_any";
    break;
  case IDLN_TYPE_FLOAT:
    switch(IDL_TYPE_FLOAT(tree).f_type) {
    case IDL_FLOAT_TYPE_FLOAT:
      retval = "CORBA_float";
      break;
    case IDL_FLOAT_TYPE_DOUBLE:
      retval = "CORBA_double";
      break;
    case IDL_FLOAT_TYPE_LONGDOUBLE:
      retval = "CORBA_long_double";
      break;
    }
    break;
  case IDLN_TYPE_FIXED:
    return g_strdup_printf( "CORBA_fixed_%" IDL_LL "d_%" IDL_LL "d",
		     IDL_INTEGER(IDL_TYPE_FIXED(tree).positive_int_const).value,
		     IDL_INTEGER(IDL_TYPE_FIXED(tree).integer_lit).value);
    break;
  case IDLN_TYPE_INTEGER:
    tmpstr = g_string_new(NULL);
    g_string_append(tmpstr, "CORBA_");
    if(!IDL_TYPE_INTEGER(tree).f_signed)
	g_string_append(tmpstr, "unsigned_");

    switch(IDL_TYPE_INTEGER(tree).f_type) {
    case IDL_INTEGER_TYPE_SHORT:
	g_string_append(tmpstr, "short");
	break;
    case IDL_INTEGER_TYPE_LONGLONG:
	g_string_append(tmpstr, "long_");
    	/* FALLTHROUGH */
    case IDL_INTEGER_TYPE_LONG:
	g_string_append(tmpstr, "long");
	break;
    }
    break;
  case IDLN_TYPE_STRING:
    retval = "CORBA_string";	/* this is non-standard! */
    break;
  case IDLN_TYPE_OCTET:
    retval = "CORBA_octet";
    break;
  case IDLN_TYPE_WIDE_STRING:
    retval = "CORBA_wstring";	/* this is non-standard! */
    break;
  case IDLN_TYPE_CHAR:
    retval = "CORBA_char";
    break;
  case IDLN_TYPE_WIDE_CHAR:
    retval = "CORBA_wchar";
    break;
  case IDLN_TYPE_BOOLEAN:
    retval = "CORBA_boolean";
    break;
  case IDLN_TYPE_STRUCT:
    return orbit_cbe_get_typespec_str(IDL_TYPE_STRUCT(tree).ident);
  case IDLN_EXCEPT_DCL:
    return orbit_cbe_get_typespec_str(IDL_EXCEPT_DCL(tree).ident);
  case IDLN_TYPE_ARRAY:
    return orbit_cbe_get_typespec_str(IDL_TYPE_ARRAY(tree).ident);
  case IDLN_TYPE_UNION:
    return orbit_cbe_get_typespec_str(IDL_TYPE_UNION(tree).ident);
  case IDLN_TYPE_ENUM:
    return orbit_cbe_get_typespec_str(IDL_TYPE_ENUM(tree).ident);
  case IDLN_IDENT:
    return IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(tree), "_", 0);
  case IDLN_PARAM_DCL:
    return orbit_cbe_get_typespec_str(IDL_PARAM_DCL(tree).param_type_spec);
  case IDLN_TYPE_SEQUENCE:
    {
	IDL_tree subtype = IDL_TYPE_SEQUENCE(tree).simple_type_spec;
        char *ctmp, *base;
        ctmp = orbit_cbe_get_typespec_str(subtype);
	/* We should have built-in alias to make this next line not needed */
	base = orbit_cbe_type_is_builtin(subtype)
	  ? ctmp + strlen("CORBA_") : ctmp;
	retval = g_strdup_printf( "CORBA_sequence_%s", base);
        g_free(ctmp);
        return retval;
    }
    break;
  case IDLN_NATIVE:
    retval = "gpointer";
    break;
  case IDLN_FORWARD_DCL:
  case IDLN_INTERFACE:
    return orbit_cbe_get_typespec_str(IDL_INTERFACE(tree).ident);
  case IDLN_TYPE_OBJECT:
    retval = "CORBA_Object";
    break;
  case IDLN_TYPE_TYPECODE:
    retval = "CORBA_TypeCode";
    break;
  default:
    g_error("We were asked to get a typename for a %s",
	    IDL_tree_type_names[IDL_NODE_TYPE(tree)]);
    break;
  }

  if (retval)
    return g_strdup (retval);
  else
    return g_string_free (tmpstr, FALSE);
}

void
orbit_cbe_write_typespec(FILE *of, IDL_tree tree)
{
    char *name = orbit_cbe_get_typespec_str(tree);
    fprintf(of, "%s", name);
    g_free(name);
}

/**
    Parameters (e.g., arguments to methods, and the return value
    have some complicated rules in the C mapping for how many
    levels of pointer and the exact type involved. This function
    generates the externally visible parameter type.

    Note the hack below because "const CORBA_string" is not
    promotable to "const CORBA_char*" (the later the standard
    to which apps are written, while the former is what would
    be produced without the hack).
**/
static char *
orbit_cbe_write_param_typespec_str(IDL_tree ts, IDL_ParamRole role)
{
	int      i, n;
	gboolean isSlice;
	char    *name;
	GString *str = g_string_sized_new (23);
	IDL_tree typedef_spec;
	char *typedef_name;

	n = oidl_param_info (ts, role, &isSlice);
	name = orbit_cbe_get_typespec_str (ts);

	if ( role == DATA_IN ) {
	        /* We want to check if this is a typedef for CORBA_string so we can do special handling 
		 * in that case. 
		 */
	        typedef_spec = orbit_cbe_get_typespec (ts);
		typedef_name = orbit_cbe_get_typespec_str (typedef_spec);

		g_string_printf (str, "const %s", 
				 !strcmp (typedef_name, "CORBA_string") ?
				 "CORBA_char *" : name);

		g_free (typedef_name);
	} else
		g_string_printf (str, "%s", name);

	g_free (name);

	if ( isSlice )
		g_string_append (str, "_slice");

	for (i = 0; i < n; i++)
		g_string_append_c (str, '*');

	return g_string_free (str, FALSE);
}

static void
orbit_cbe_write_param_typespec_raw (FILE *of, IDL_tree ts, IDL_ParamRole role)
{
    char *str;
    str = orbit_cbe_write_param_typespec_str (ts, role);
    fprintf (of, "%s", str);
    g_free (str);
}

void
orbit_cbe_write_param_typespec(FILE *of, IDL_tree tree) {
    IDL_tree		ts = NULL /* Quiet gcc */;
    IDL_ParamRole	role = 0 /* Quiet gcc */;

    switch ( IDL_NODE_TYPE(tree) ) {
    case IDLN_OP_DCL: /* means return value of method */
        ts = IDL_OP_DCL(tree).op_type_spec;
	role = DATA_RETURN;
        break;
    case IDLN_PARAM_DCL: /* one of the parameters */
        ts = IDL_PARAM_DCL(tree).param_type_spec;
    	role = oidl_attr_to_paramrole(IDL_PARAM_DCL(tree).attr);
        break;
    default:
        g_assert_not_reached();
    }
    orbit_cbe_write_param_typespec_raw(of, ts, role);
}

void
orbit_cbe_op_write_proto (FILE       *of,
			  IDL_tree    op,
			  const char *nom_prefix,
			  gboolean    for_epv)
{
	IDL_tree  sub;
	char     *id;

	g_assert (IDL_NODE_TYPE(op) == IDLN_OP_DCL);

	orbit_cbe_write_param_typespec (of, op);

	id = IDL_ns_ident_to_qstring (
		IDL_IDENT_TO_NS (IDL_INTERFACE (
			IDL_get_parent_node (op, IDLN_INTERFACE, NULL)).ident), "_", 0);

	if (for_epv)
		fprintf (of, " (*%s%s)", nom_prefix ? nom_prefix : "",
			 IDL_IDENT(IDL_OP_DCL(op).ident).str);
	else 
		fprintf (of, " %s%s_%s", nom_prefix ? nom_prefix : "",
			 id, IDL_IDENT (IDL_OP_DCL (op).ident).str);

	fprintf (of, "(");

	if (for_epv)
		fprintf (of, "PortableServer_Servant _servant, ");
	else
		fprintf (of, "%s _obj, ", id);

	g_free (id);

	for (sub = IDL_OP_DCL (op).parameter_dcls; sub; sub = IDL_LIST (sub).next) {
		IDL_tree parm = IDL_LIST (sub).data;

		orbit_cbe_write_param_typespec (of, parm);

		fprintf (of, " %s, ", IDL_IDENT (IDL_PARAM_DCL (parm).simple_declarator).str);
	}

	if (IDL_OP_DCL (op).context_expr)
		fprintf (of, "CORBA_Context _ctx, ");

	fprintf (of, "CORBA_Environment *ev)");
}

/* Writes the value of the constant in 'tree' to file handle 'of' */
static char *
orbit_cbe_get_const(IDL_tree tree)
{
  char *opc = NULL, *retval, *ctmp;
  GString *tmpstr = g_string_new(NULL);

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_BOOLEAN:
    g_string_printf(tmpstr, "%s", IDL_BOOLEAN(tree).value?"CORBA_TRUE":"CORBA_FALSE");
    break;
  case IDLN_CHAR:
    g_string_printf(tmpstr, "'\\x%X'", *(unsigned char *)IDL_CHAR(tree).value);
    break;
  case IDLN_FLOAT:
    g_string_printf(tmpstr, "%f", IDL_FLOAT(tree).value);
    break;
  case IDLN_INTEGER:
    g_string_printf(tmpstr, "%" IDL_LL "d", IDL_INTEGER(tree).value);
    break;
  case IDLN_STRING:
    g_string_printf(tmpstr, "\"%s\"", IDL_STRING(tree).value);
    break;
  case IDLN_WIDE_CHAR:
    g_string_printf(tmpstr, "L'%ls'", IDL_WIDE_CHAR(tree).value);
    break;
  case IDLN_WIDE_STRING:
    g_string_printf(tmpstr, "L\"%ls\"", IDL_WIDE_STRING(tree).value);
    break;
  case IDLN_BINOP:
    g_string_printf(tmpstr, "(");
    ctmp = orbit_cbe_get_const(IDL_BINOP(tree).left);
    g_string_append(tmpstr, ctmp);
    g_free(ctmp);
    switch(IDL_BINOP(tree).op) {
    case IDL_BINOP_OR:
      opc = "|";
      break;
    case IDL_BINOP_XOR:
      opc = "^";
      break;
    case IDL_BINOP_AND:
      opc = "&";
      break;
    case IDL_BINOP_SHR:
      opc = ">>";
      break;
    case IDL_BINOP_SHL:
      opc = "<<";
      break;
    case IDL_BINOP_ADD:
      opc = "+";
      break;
    case IDL_BINOP_SUB:
      opc = "-";
      break;
    case IDL_BINOP_MULT:
      opc = "*";
      break;
    case IDL_BINOP_DIV:
      opc = "/";
      break;
    case IDL_BINOP_MOD:
      opc = "%";
      break;
    }
    g_string_append_printf(tmpstr, " %s ", opc);
    ctmp = orbit_cbe_get_const(IDL_BINOP(tree).right);
    g_string_append_printf(tmpstr, "%s)", ctmp);
    g_free(ctmp);
    break;
  case IDLN_UNARYOP:
    switch(IDL_UNARYOP(tree).op) {
    case IDL_UNARYOP_PLUS: opc = "+"; break;
    case IDL_UNARYOP_MINUS: opc = "-"; break;
    case IDL_UNARYOP_COMPLEMENT: opc = "~"; break;
    }
    ctmp = orbit_cbe_get_const(IDL_UNARYOP(tree).operand);
    g_string_printf(tmpstr, "%s%s", opc, ctmp);
    g_free(ctmp);
    break;
  case IDLN_IDENT:
    {
      char *id;
      id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(tree), "_", 0);
      g_string_printf(tmpstr, "%s", id);
      g_free(id);
    }
    break;
  default:
    g_error("We were asked to print a constant for %s", IDL_tree_type_names[tree->_type]);
    break;
  }

  retval = tmpstr->str;

  g_string_free(tmpstr, FALSE);

  return retval;
}

void
orbit_cbe_write_const(FILE *of, IDL_tree tree)
{
  char *ctmp;

  ctmp = orbit_cbe_get_const(tree);
  fprintf(of, "%s", ctmp);
  g_free(ctmp);
}

/* This is the WORST HACK in the WORLD, really truly, but the C preprocessor doesn't allow us to use
   strings, so we have to work around it by using individual characters. */
void
orbit_cbe_id_define_hack(FILE *fh, const char *def_prefix, const char *def_name, const char *def_value)
{
  int i, n;
  n = strlen(def_value);
  for(i = 0; i < n; i++)
    fprintf(fh, "#define %s_%s_%d '%c'\n", def_prefix, def_name, i, def_value[i]);
}

void
orbit_cbe_id_cond_hack(FILE *fh, const char *def_prefix, const char *def_name, const char *def_value)
{
  int i, n;
  n = strlen(def_value);
  if(n <= 0)
    return;

  fprintf(fh, "(");

  for(i = 0; i < n; i++)
    fprintf(fh, "%s (%s_%s_%d == '%c') \\\n", i?"&&":"", def_prefix, def_name, i, def_value[i]);
  fprintf(fh, ")");
}

#define BASE_TYPES \
	     IDLN_TYPE_INTEGER: \
	case IDLN_TYPE_FLOAT: \
	case IDLN_TYPE_ENUM: \
        case IDLN_TYPE_BOOLEAN: \
	case IDLN_TYPE_CHAR: \
	case IDLN_TYPE_WIDE_CHAR: \
	case IDLN_TYPE_OCTET

#define STRING_TYPES \
	     IDLN_TYPE_STRING: \
	case IDLN_TYPE_WIDE_STRING

#define OBJREF_TYPES \
	     IDLN_TYPE_OBJECT: \
	case IDLN_INTERFACE: \
	case IDLN_FORWARD_DCL

static const char *
orbit_cbe_flatten_ref (IDL_ParamRole role, IDL_tree typespec)
{
	gboolean is_fixed;

	is_fixed = orbit_cbe_type_is_fixed_length (typespec);

	switch (role) {
	case DATA_IN:
		switch (IDL_NODE_TYPE (typespec)) {
		case BASE_TYPES:
		case STRING_TYPES:
		case OBJREF_TYPES:
		case IDLN_TYPE_TYPECODE:
		case IDLN_NATIVE:
			return "(gpointer)&";

		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_SEQUENCE:
		case IDLN_TYPE_ARRAY:
			return "(gpointer)";
			
		default:
		case IDLN_TYPE_FIXED:
			g_error ("Hit evil type %d", IDL_NODE_TYPE (typespec));
		};
		return NULL;

	case DATA_INOUT:
		switch (IDL_NODE_TYPE (typespec)) {
		case BASE_TYPES:
		case STRING_TYPES:
		case OBJREF_TYPES:
		case IDLN_TYPE_TYPECODE:
		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ARRAY:
		case IDLN_NATIVE:
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_SEQUENCE:
			return "";

		default:
		case IDLN_TYPE_FIXED:
			g_error ("Hit evil type %d", IDL_NODE_TYPE (typespec));
		};
		return NULL;

	case DATA_OUT:
		switch (IDL_NODE_TYPE (typespec)) {
		case BASE_TYPES:
		case STRING_TYPES:
		case OBJREF_TYPES:
		case IDLN_TYPE_TYPECODE:
		case IDLN_NATIVE:
			return "&";

		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ARRAY:
			if (is_fixed)
				return "&";
			else /* drop through */

		case IDLN_TYPE_SEQUENCE:
		case IDLN_TYPE_ANY:
			return "";

		default:
		case IDLN_TYPE_FIXED:
			g_error ("Hit evil type %d", IDL_NODE_TYPE (typespec));
		};
		return NULL;

	case DATA_RETURN:
		g_error ("No data return handler");
		return NULL;
	}

	return NULL;
}

void
orbit_cbe_flatten_args (IDL_tree tree, FILE *of, const char *name)
{
	int i = 0;
	IDL_tree l;

	for (l = IDL_OP_DCL(tree).parameter_dcls; l;
	     l = IDL_LIST(l).next)
		i++;

	fprintf (of, "gpointer %s[%d];\n", name, i);
	
	i = 0;
	for (l = IDL_OP_DCL(tree).parameter_dcls; l;
	     l = IDL_LIST(l).next) {
		IDL_tree decl = IDL_LIST (l).data;
		IDL_tree tspec = orbit_cbe_get_typespec (decl);
		IDL_ParamRole r = 0;

		switch(IDL_PARAM_DCL(decl).attr) {
		case IDL_PARAM_IN:    r = DATA_IN;    break;
		case IDL_PARAM_INOUT: r = DATA_INOUT; break;
		case IDL_PARAM_OUT:   r = DATA_OUT;   break;
		default:
			g_error("Unknown IDL_PARAM type");
		}
		
		fprintf (of, "%s[%d] = %s%s;\n",
			 name, i,
			 orbit_cbe_flatten_ref (r, tspec),
			 IDL_IDENT (IDL_PARAM_DCL (decl).simple_declarator).str);
		i++;
	}
}

static char *
orbit_cbe_unflatten_ref (IDL_ParamRole role, IDL_tree typespec)
{
	gboolean is_fixed;
	char    *typestr;
	char    *retval;

	is_fixed = orbit_cbe_type_is_fixed_length (typespec);

	typestr = orbit_cbe_write_param_typespec_str (typespec, role);

	switch (role) {
	case DATA_IN:
		switch (IDL_NODE_TYPE (typespec)) {
		case BASE_TYPES:
		case STRING_TYPES:
		case OBJREF_TYPES:
		case IDLN_TYPE_TYPECODE:
		case IDLN_NATIVE:
			retval = g_strdup_printf ("*(%s *)", typestr);
			break;


		case IDLN_TYPE_ARRAY:
			retval = g_strdup_printf ("(%s_slice *)", typestr);
			break;

		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_SEQUENCE:
			retval = g_strdup_printf ("(%s)", typestr);
			break;
			
		default:
		case IDLN_TYPE_FIXED:
			g_error ("Hit evil type %d", IDL_NODE_TYPE (typespec));
			retval = NULL;
			break;
		};
		break;

	case DATA_INOUT:
		switch (IDL_NODE_TYPE (typespec)) {
		case IDLN_TYPE_ARRAY:
			retval = g_strdup_printf ("(%s_slice *)", typestr);
			break;

		case BASE_TYPES:
		case STRING_TYPES:
		case OBJREF_TYPES:
		case IDLN_TYPE_TYPECODE:
		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
		case IDLN_NATIVE:
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_SEQUENCE:
			retval = g_strdup_printf ("(%s)", typestr);
			break;

		default:
		case IDLN_TYPE_FIXED:
			g_error ("Hit evil type %d", IDL_NODE_TYPE (typespec));
			retval = NULL;
			break;
		};
		break;

	case DATA_OUT:
		switch (IDL_NODE_TYPE (typespec)) {
		case BASE_TYPES:
		case STRING_TYPES:
		case OBJREF_TYPES:
		case IDLN_TYPE_TYPECODE:
		case IDLN_NATIVE:
			retval = g_strdup_printf ("*(%s *)", typestr);
			break;

		case IDLN_TYPE_ARRAY:
			if (is_fixed) {
				retval = g_strdup_printf ("*(%s_slice **)", typestr);
				break;
			}
			/* drop through */

		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
			if (is_fixed) {
				retval = g_strdup_printf ("*(%s *)", typestr);
				break;
			}
			/* drop through */

		case IDLN_TYPE_SEQUENCE:
		case IDLN_TYPE_ANY:
			retval = g_strdup_printf ("(%s)", typestr);
			break;

		default:
		case IDLN_TYPE_FIXED:
			g_error ("Hit evil type %d", IDL_NODE_TYPE (typespec));
			retval = NULL;
			break;
		};
		break;

	case DATA_RETURN:
	default:
		g_error ("No data return handler");
		retval = NULL;
		break;
	}

	g_free (typestr);

	return retval;
}

void
orbit_cbe_unflatten_args (IDL_tree tree, FILE *of, const char *name)
{
	IDL_tree l;
	int      i = 0;

	for (l = IDL_OP_DCL(tree).parameter_dcls; l;
	     l = IDL_LIST(l).next) {
		IDL_tree decl = IDL_LIST (l).data;
		IDL_tree tspec = orbit_cbe_get_typespec (decl);
		IDL_ParamRole r = 0;
		char *unflatten;

		switch(IDL_PARAM_DCL(decl).attr) {
		case IDL_PARAM_IN:    r = DATA_IN;    break;
		case IDL_PARAM_INOUT: r = DATA_INOUT; break;
		case IDL_PARAM_OUT:   r = DATA_OUT;   break;
		default:
			g_error("Unknown IDL_PARAM type");
		}

		unflatten = orbit_cbe_unflatten_ref (r, tspec);
		fprintf (of, "%s%s[%d], ", unflatten, name, i++);
		g_free (unflatten);
	}
}
