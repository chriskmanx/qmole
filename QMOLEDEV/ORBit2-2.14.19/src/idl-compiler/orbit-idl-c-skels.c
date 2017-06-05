#include "config.h"

#include "orbit-idl-c-backend.h"

#include <string.h>

typedef struct {
	OIDL_C_Info *ci;
	GSList      *oplist;
	int          curlevel;
} CBESkelInterfaceTraverseInfo;

typedef struct {
	char     *iface_id;
	char     *opname;
	IDL_tree  op;
	int       idx;
} CBESkelOpInfo;

static void cbe_skel_do_interface(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci);

static void
ck_output_poastuff(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  if( !tree || (tree->declspec & IDLF_DECLSPEC_PIDL)!=0 ) 
    return;

  switch(IDL_NODE_TYPE(tree)) {
  case IDLN_MODULE:
    ck_output_poastuff(IDL_MODULE(tree).definition_list, rinfo, ci);
    break;
  case IDLN_LIST:
    {
      IDL_tree sub;
      for(sub = tree; sub; sub = IDL_LIST(sub).next) {
	ck_output_poastuff(IDL_LIST(sub).data, rinfo, ci);
      }
    }
    break;
  case IDLN_INTERFACE:
    cbe_skel_do_interface(tree, rinfo, ci);
    break;
  default:
    break;
  }
}

static gint
cbe_skel_compare_op_dcls(CBESkelOpInfo *op1, CBESkelOpInfo *op2)
{
  return strcmp(op1->opname, op2->opname);
}

static void
cbe_skel_free_op_info(CBESkelOpInfo *op)
{
  g_free(op->opname);
  g_free(op->iface_id);
  g_free(op);
}

static void
cbe_skel_interface_add_relayer (IDL_tree intf, CBESkelInterfaceTraverseInfo *iti)
{
  CBESkelOpInfo *newopi;
  IDL_tree curitem, curdcl, curattr, curattrdcl;
  char *iface_id;
  int   idx = 0;

  iface_id =
    IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(intf).ident),
			    "_", 0);

  for(curitem = IDL_INTERFACE(intf).body; curitem;
      curitem = IDL_LIST(curitem).next) {
    curdcl = IDL_LIST(curitem).data;

    switch(IDL_NODE_TYPE(curdcl)) {
    case IDLN_OP_DCL:
      newopi = g_new0(CBESkelOpInfo, 1);
      newopi->iface_id = g_strdup(iface_id);
      newopi->opname = g_strdup(IDL_IDENT(IDL_OP_DCL(curdcl).ident).str);
      newopi->idx = idx++;
      iti->oplist = g_slist_insert_sorted(iti->oplist, newopi,
					  (GCompareFunc)cbe_skel_compare_op_dcls);
      break;
    case IDLN_ATTR_DCL:
      for(curattr = IDL_ATTR_DCL(curdcl).simple_declarations;
	  curattr; curattr = IDL_LIST(curattr).next) {
	curattrdcl = IDL_LIST(curattr).data;

	newopi = g_new0(CBESkelOpInfo, 1);
	newopi->iface_id = g_strdup(iface_id);
	newopi->opname = g_strdup_printf("_get_%s", IDL_IDENT(curattrdcl).str);
	newopi->idx = idx++;
	iti->oplist = g_slist_insert_sorted(iti->oplist, newopi,
					    (GCompareFunc)cbe_skel_compare_op_dcls);
	if(!IDL_ATTR_DCL(curdcl).f_readonly) {
	  newopi = g_new0(CBESkelOpInfo, 1);
	  newopi->iface_id = g_strdup(iface_id);
	  newopi->opname = g_strdup_printf("_set_%s", IDL_IDENT(curattrdcl).str);
	  newopi->idx = idx++;
	  iti->oplist = g_slist_insert_sorted(iti->oplist, newopi,
					      (GCompareFunc)cbe_skel_compare_op_dcls);
	}
      }
      break;
    default:
      break;
    }
  }

  g_free(iface_id);
}

static void
cbe_skel_interface_print_relayers(const CBESkelInterfaceTraverseInfo *iti)
{
  CBESkelInterfaceTraverseInfo subiti = *iti;
  GSList *curnode;
  CBESkelOpInfo *opi;
  char curchar;

  curnode = iti->oplist;
  subiti.curlevel = iti->curlevel+1;
  fprintf(iti->ci->fh, "switch(opname[%d]) {\n", iti->curlevel);
  while(curnode) {
    opi = (CBESkelOpInfo *)curnode->data;
    if(iti->curlevel > strlen(opi->opname)) {
      curnode = g_slist_next(curnode);
      continue;
    }
    curchar = opi->opname[iti->curlevel];
    if(curchar)
      fprintf(iti->ci->fh, "case '%c':\n", curchar);
    else
      fprintf(iti->ci->fh, "case '\\0':\n");
    subiti.oplist = NULL;
    while(curnode && ((CBESkelOpInfo *)curnode->data)->opname[iti->curlevel]
	  == curchar) {
      subiti.oplist = g_slist_append(subiti.oplist, curnode->data);
      curnode = g_slist_next(curnode);
    }

    if(g_slist_length(subiti.oplist) > 1) {
      if(curchar)
	cbe_skel_interface_print_relayers(&subiti);
      else
	g_error("two ops with same name!!!!");
    } else {
      if(strlen(opi->opname + iti->curlevel))
	fprintf(iti->ci->fh, "if(strcmp((opname + %d), \"%s\")) break;\n",
		iti->curlevel + 1, opi->opname + iti->curlevel+1);
      fprintf(iti->ci->fh, "*impl = (gpointer)servant->vepv->%s_epv->%s;\n",
	      opi->iface_id, opi->opname);
      fprintf(iti->ci->fh, "{ORBit_IInterface *volatile _t_=&%s__iinterface;*m_data = (gpointer)&_t_->methods._buffer [%d];}\n",
	      opi->iface_id, opi->idx);
      fprintf(iti->ci->fh, "return (ORBitSmallSkeleton)_ORBIT_skel_small_%s_%s;\n",
	      opi->iface_id, opi->opname);
    }
    fprintf(iti->ci->fh, "break;\n");
    g_slist_free(subiti.oplist);
  }
  fprintf(iti->ci->fh, "default: break; \n}\n");
}

static void
cbe_skel_interface_print_relayer(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  char *id;
  CBESkelInterfaceTraverseInfo iti;

  id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(tree).ident), "_", 0);
  fprintf(ci->fh, "static ORBitSmallSkeleton get_skel_small_%s(POA_%s *servant,\nconst char *opname,"
	  "gpointer *m_data, gpointer *impl)\n{\n", id, id);

  iti.ci = ci;
  iti.oplist = NULL;
  iti.curlevel = 0;

  IDL_tree_traverse_parents(tree,
			    (GFunc)cbe_skel_interface_add_relayer, &iti);

  cbe_skel_interface_print_relayers(&iti);

  g_slist_foreach(iti.oplist, (GFunc)cbe_skel_free_op_info, NULL);
  g_slist_free(iti.oplist);

  fprintf(ci->fh, "return NULL;\n");
  fprintf(ci->fh, "}\n\n");

  g_free(id);
}

typedef struct {
	OIDL_C_Info *ci;
	const char  *for_id;
} CBESkelVEPVMapTraverseInfo;

static void
cbe_skel_interface_print_vepvmap_offsets (IDL_tree node, CBESkelVEPVMapTraverseInfo *ti)
{
	char *id;
	id = IDL_ns_ident_to_qstring (IDL_IDENT_TO_NS (IDL_INTERFACE (node).ident), "_", 0);
	fprintf (ti->ci->fh, "(CORBA_unsigned_long) %s__classid,\n", id);
	fprintf (ti->ci->fh, "ORBIT_VEPV_OFFSET (POA_%s__vepv, %s_epv),\n", ti->for_id, id);
	g_free (id);
}

static void
cbe_skel_do_interface(IDL_tree tree, OIDL_Run_Info *rinfo, OIDL_C_Info *ci)
{
  char *id, *id2;
  IDL_tree curitem, pnt;
  int i;
  CBESkelVEPVMapTraverseInfo ti;

  /* PIDL methods dont have normal skel functions. */
  for ( pnt=tree; pnt; pnt=IDL_NODE_UP(pnt) ) {
      if ( pnt->declspec & IDLF_DECLSPEC_PIDL )
	return;
  }

  id = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_INTERFACE(tree).ident), "_", 0);

  cbe_skel_interface_print_relayer(tree, rinfo, ci);

  fprintf(ci->fh,
	  "void POA_%s__init(PortableServer_Servant servant,\nCORBA_Environment *env)\n",
	  id);
  fprintf(ci->fh, "{\n");
  fprintf(ci->fh,"  static PortableServer_ClassInfo class_info = {");

  fprintf(ci->fh, "NULL, (ORBit_small_impl_finder)&get_skel_small_%s, ", id);

  fprintf(ci->fh,"\"%s\", &%s__classid, NULL, &%s__iinterface};\n",
	  IDL_IDENT(IDL_INTERFACE(tree).ident).repo_id, id, id);

  fprintf(ci->fh,"  PortableServer_ServantBase__init ("
	         "       ((PortableServer_ServantBase *)servant), env);\n");

  for(curitem = IDL_INTERFACE(tree).inheritance_spec; curitem;
      curitem = IDL_LIST(curitem).next) {
    id2 = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_LIST(curitem).data),
				  "_", 0);
    fprintf(ci->fh, "  POA_%s__init(servant, env);\n", id2);
    g_free(id2);
  }

  /* registering after other __inits() makes the classids increment nicely. */
  fprintf (ci->fh, "   ORBit_skel_class_register (&class_info,\n");
  fprintf (ci->fh, "   (PortableServer_ServantBase *)servant, POA_%s__fini,\n", id);
  fprintf (ci->fh, "   ORBIT_VEPV_OFFSET (POA_%s__vepv, %s_epv),\n", id, id);
  ti.for_id = id; ti.ci = ci;
  IDL_tree_traverse_parents_full (tree, (GFunc) cbe_skel_interface_print_vepvmap_offsets, &ti, FALSE);
  fprintf (ci->fh, "   (CORBA_unsigned_long) 0);");

  fprintf(ci->fh, "}\n\n");

  fprintf(ci->fh,
	  "void POA_%s__fini(PortableServer_Servant servant,\nCORBA_Environment *env)\n",
	  id);
  fprintf(ci->fh, "{\n");
  if(IDL_INTERFACE(tree).inheritance_spec)
    {
      for(i = IDL_list_length(IDL_INTERFACE(tree).inheritance_spec) - 1;
	  i >= 0; i--) {
	curitem = IDL_list_nth(IDL_INTERFACE(tree).inheritance_spec, i);
	id2 = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_LIST(curitem).data),
				      "_", 0);
	/* XXX fixme - this is going to call ServantBase__fini multiple times */
	fprintf(ci->fh, "  POA_%s__fini(servant, env);\n",
		id2);
	g_free(id2);
      }
    }
  fprintf(ci->fh, "  PortableServer_ServantBase__fini(servant, env);\n");
  fprintf(ci->fh, "}\n\n");

  g_free(id);
}

void
orbit_idl_output_c_skeletons (IDL_tree       tree,
			      OIDL_Run_Info *rinfo,
			      OIDL_C_Info   *ci)
{
	fprintf (ci->fh, OIDL_C_WARNING);
	fprintf (ci->fh, "#include <string.h>\n");
	fprintf (ci->fh, "#define ORBIT2_STUBS_API\n");
	fprintf (ci->fh, "#include \"%s.h\"\n\n", ci->base_name);

	ck_output_poastuff (tree, rinfo, ci);
}
