/* 
 *  orbit-name-server : a CORBA CosNaming server
 *
 *  Copyright (C) 1998 Elliot Lee, Sebastian Wilhelmi
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301, USA.
 */

#include "CosNaming.h"
#include "CosNaming_impl.h"
#include "name-support.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>

/*** App-specific servant structures ***/

typedef struct
{
  CosNaming_NameComponent nc;
  CORBA_Object obj;
  gint refcount;
  CORBA_boolean is_subctx;
} RegisteredName;

typedef struct
{
  POA_CosNaming_NamingContextExt servant;
  PortableServer_POA poa;

  GHashTable *names;
  CORBA_Object objref;
} impl_POA_CosNaming_NamingContextExt;

typedef struct
{
  POA_CosNaming_BindingIterator servant;
  PortableServer_POA poa;

  GList *items;
} impl_POA_CosNaming_BindingIterator;

/*** Stub implementations ***/

static void
NameComponent__copy (CosNaming_NameComponent * dest,
		     CosNaming_NameComponent * src)
{
  dest->id = CORBA_string_dup (src->id);
  dest->kind = CORBA_string_dup (src->kind);
}

static void
Name__copy (CosNaming_Name * dest, CosNaming_Name * src, int from_pos)
{
  int i;
  dest->_length = MAX (src->_length - from_pos, 0);
  dest->_buffer = dest->_length ? 
    CORBA_sequence_CosNaming_NameComponent_allocbuf (dest->_length) : NULL;

  for (i = 0; i < dest->_length; i++)
    NameComponent__copy (dest->_buffer + i, src->_buffer + i + from_pos);
}

static guint
nc_hash (const CosNaming_NameComponent * nom)
{
  return g_str_hash (nom->id) ^ g_str_hash (nom->kind);
}

static gint
nc_compare (const CosNaming_NameComponent * n1,
	    const CosNaming_NameComponent * n2)
{
  return !strcmp (n1->id, n2->id) && !strcmp (n1->kind, n2->kind);
}

static RegisteredName *
rn_new (CosNaming_NameComponent * nc,
	CORBA_Object obj, CORBA_boolean is_subctx)
{
  RegisteredName *retval;

  retval = g_new0 (RegisteredName, 1);

  NameComponent__copy (&retval->nc, nc);

  retval->obj = CORBA_Object_duplicate (obj, /* cheat */ NULL);
  retval->is_subctx = is_subctx;

  return retval;
}

#if 0
static void
rn_print (CosNaming_NameComponent * nc, RegisteredName * nom)
{
  g_print ("id %s kind %s ptr %p [%p]\n", nc->id, nc->kind,
	   nom->obj, nom->obj ? nom->obj->pobj : NULL);
}
#endif

static RegisteredName *
rn_ref (RegisteredName * nom)
{
  nom->refcount++;

  return nom;
}

static void
rn_unref (RegisteredName * nom)
{
  nom->refcount--;

  if (nom->refcount <= 0)
    {
      CORBA_free (nom->nc.id);
      CORBA_free (nom->nc.kind);
      CORBA_Object_release (nom->obj, /* cheat */ NULL);
      g_free (nom);
    }
}

static void
l_rn_unref(gpointer data, gpointer user_data)
{
  rn_unref((RegisteredName *)data);
}

static void
impl_NamingContext_unbind (impl_POA_CosNaming_NamingContextExt *
			   servant, CosNaming_Name * n,
			   CORBA_Environment * ev)
{
  RegisteredName *nom = NULL;

  if (n->_length < 1)
    goto raise_exc;
  nom = g_hash_table_lookup (servant->names, n->_buffer);

  if (n->_length == 1 && nom)
    {
      g_hash_table_remove (servant->names, &nom->nc);
      rn_unref (nom);
    }
  else if (nom && nom->is_subctx)
    {
      CosNaming_Name n2 = *n;

      n2._length--;
      n2._buffer++;
      CosNaming_NamingContext_unbind (nom->obj, &n2, ev);
    }
  else
    {
      CosNaming_NamingContext_NotFound *exdata;
    raise_exc:

      exdata = CosNaming_NamingContext_NotFound__alloc ();
      exdata->why =
	nom ? CosNaming_NamingContext_not_context :
	CosNaming_NamingContext_missing_node;
      Name__copy (&exdata->rest_of_name, n, 0);

      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			   ex_CosNaming_NamingContext_NotFound, exdata);
    }
}

static void
impl_NamingContext_bind (impl_POA_CosNaming_NamingContextExt *
			 servant, CosNaming_Name * n,
			 CORBA_Object obj, CORBA_Environment * ev)
{
  RegisteredName *nom = NULL;

  if (n->_length < 1)
    goto raise_exc;

  nom = g_hash_table_lookup (servant->names, n->_buffer);

  if (n->_length == 1)
    {
      if (nom)
	{
	  if (!CORBA_Object_non_existent (nom->obj, ev))
	    {
	      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				   ex_CosNaming_NamingContext_AlreadyBound,
				   NULL);
	      return;
	    }
	  else
	    impl_NamingContext_unbind (servant, n, ev);
	}

      nom = rn_new (&n->_buffer[0], obj, CORBA_FALSE);
      g_hash_table_insert (servant->names, &nom->nc, rn_ref (nom));

    }
  else if (nom && nom->is_subctx)
    {
      CosNaming_Name n2 = *n;

      n2._length--;
      n2._buffer++;
      CosNaming_NamingContext_bind (nom->obj, &n2, obj, ev);
      return;
    }
  else
    {

      CosNaming_NamingContext_NotFound *exdata;
    raise_exc:

      exdata = CosNaming_NamingContext_NotFound__alloc ();
      exdata->why =
	nom ? CosNaming_NamingContext_not_context :
	CosNaming_NamingContext_missing_node;
      Name__copy (&exdata->rest_of_name, n, 0);

      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			   ex_CosNaming_NamingContext_NotFound, exdata);
    }
}

static void
impl_NamingContext_rebind (impl_POA_CosNaming_NamingContextExt *
			   servant, CosNaming_Name * n,
			   CORBA_Object obj, CORBA_Environment * ev)
{
  RegisteredName *nom = NULL;

  if (n->_length < 1)
    goto raise_exc;

  nom = g_hash_table_lookup (servant->names, n->_buffer);

  if (n->_length == 1)
    {
      if (nom)
	{
	  g_hash_table_remove (servant->names, &nom->nc);
	  rn_unref (nom);
	}

      nom = rn_new (&n->_buffer[0], obj, CORBA_FALSE);
      g_hash_table_insert (servant->names, &nom->nc, rn_ref (nom));

    }
  else if (nom && nom->is_subctx)
    {
      CosNaming_Name n2 = *n;

      n2._length--;
      n2._buffer++;
      CosNaming_NamingContext_rebind (nom->obj, &n2, obj, ev);
    }
  else
    {
      CosNaming_NamingContext_NotFound *exdata;
    raise_exc:

      exdata = CosNaming_NamingContext_NotFound__alloc ();
      exdata->why =
	nom ? CosNaming_NamingContext_not_context :
	CosNaming_NamingContext_missing_node;
      Name__copy (&exdata->rest_of_name, n, 0);

      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			   ex_CosNaming_NamingContext_NotFound, exdata);
    }
}

static void
impl_NamingContext_bind_context (impl_POA_CosNaming_NamingContextExt
				 * servant, CosNaming_Name * n,
				 CosNaming_NamingContext nc,
				 CORBA_Environment * ev)
{
  RegisteredName *nom = NULL;

  if (n->_length < 1)
    goto raise_exc;

  nom = g_hash_table_lookup (servant->names, n->_buffer);

  if (n->_length == 1)
    {
      if (nom)
	{
	  if (!CORBA_Object_non_existent (nom->obj, ev))
	    {
	      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				   ex_CosNaming_NamingContext_AlreadyBound,
				   NULL);
	      return;
	    }
	  else
	    impl_NamingContext_unbind (servant, n, ev);
	}

      nom = rn_new (&n->_buffer[0], nc, CORBA_TRUE);
      g_hash_table_insert (servant->names, &nom->nc, rn_ref (nom));

    }
  else if (nom && nom->is_subctx)
    {
      CosNaming_Name n2 = *n;

      n2._length--;
      n2._buffer++;
      CosNaming_NamingContext_bind (nom->obj, &n2, nc, ev);
      return;
    }
  else
    {

      CosNaming_NamingContext_NotFound *exdata;
    raise_exc:

      exdata = CosNaming_NamingContext_NotFound__alloc ();
      exdata->why =
	nom ? CosNaming_NamingContext_not_context :
	CosNaming_NamingContext_missing_node;
      Name__copy (&exdata->rest_of_name, n, 0);

      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			   ex_CosNaming_NamingContext_NotFound, exdata);
    }
}

static void
impl_NamingContext_rebind_context (impl_POA_CosNaming_NamingContextExt * 
				   servant, CosNaming_Name * n,
				   CosNaming_NamingContext nc, 
				   CORBA_Environment * ev)
{
  RegisteredName *nom = NULL;

  if (n->_length < 1)
    goto raise_exc;
  nom = g_hash_table_lookup (servant->names, n->_buffer);

  if (n->_length == 1)
    {
      if (nom)
	{
	  g_hash_table_remove (servant->names, &nom->nc);
	  rn_unref (nom);
	}

      nom = rn_new (&n->_buffer[0], nc, CORBA_TRUE);
      g_hash_table_insert (servant->names, &nom->nc, rn_ref (nom));

    }
  else if (nom && nom->is_subctx)
    {
      CosNaming_Name n2 = *n;

      n2._length--;
      n2._buffer++;
      CosNaming_NamingContext_rebind_context (nom->obj, &n2, nc, ev);
    }
  else
    {
      CosNaming_NamingContext_NotFound *exdata;
    raise_exc:

      exdata = CosNaming_NamingContext_NotFound__alloc ();
      exdata->why =
	nom ? CosNaming_NamingContext_not_context :
	CosNaming_NamingContext_missing_node;
      Name__copy (&exdata->rest_of_name, n, 0);

      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			   ex_CosNaming_NamingContext_NotFound, exdata);
    }
}

static CORBA_Object
impl_NamingContext_resolve (impl_POA_CosNaming_NamingContextExt *
			    servant, CosNaming_Name * n,
			    CORBA_Environment * ev)
{
  RegisteredName *nom = NULL;

  if (n->_length < 1)
    goto raise_exc;
  nom = g_hash_table_lookup (servant->names, n->_buffer);
  if (!nom)
    {
      CosNaming_NamingContext_NotFound *exdata;

      exdata = CosNaming_NamingContext_NotFound__alloc ();
      exdata->why = CosNaming_NamingContext_missing_node;
      Name__copy (&exdata->rest_of_name, n, 0);

      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			   ex_CosNaming_NamingContext_NotFound, exdata);
    }
  else if (n->_length == 1)
    {
      if (!CORBA_Object_non_existent (nom->obj, ev))
	return CORBA_Object_duplicate (nom->obj, ev);
      else
	{
	  CosNaming_NamingContext_NotFound *exdata;

	  impl_NamingContext_unbind (servant, n, ev);
	raise_exc:
	  exdata = CosNaming_NamingContext_NotFound__alloc ();
	  exdata->why = CosNaming_NamingContext_missing_node;
	  Name__copy (&exdata->rest_of_name, n, 0);

	  CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			       ex_CosNaming_NamingContext_NotFound, exdata);
	}
    }
  else
    {
      CosNaming_Name n2 = *n;

      n2._length--;
      n2._buffer++;
      return CosNaming_NamingContext_resolve (nom->obj, &n2, ev);
    }

  return CORBA_OBJECT_NIL;
}

static CosNaming_NamingContext
impl_NamingContext_new_context (impl_POA_CosNaming_NamingContextExt
				* servant, CORBA_Environment * ev)
{
  return ORBit_CosNaming_NamingContextExt_create (servant->poa, ev);
}

static CosNaming_NamingContext
impl_NamingContext_bind_new_context (impl_POA_CosNaming_NamingContextExt * 
				     servant, CosNaming_Name * n,
				     CORBA_Environment * ev)
{
  RegisteredName *nom = NULL;
  CosNaming_NamingContext retval;

  if (n->_length < 1)
    goto raise_exc;
  nom = g_hash_table_lookup (servant->names, n->_buffer);

  if (n->_length == 1)
    {
      if (nom)
	{
	  if (!CORBA_Object_non_existent (nom->obj, ev))
	    {
	      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				   ex_CosNaming_NamingContext_AlreadyBound,
				   NULL);
	      return CORBA_OBJECT_NIL;
	    }
	  else
	    impl_NamingContext_unbind (servant, n, ev);
	}

      retval = impl_NamingContext_new_context (servant, ev);
      impl_NamingContext_bind_context (servant, n, retval, ev);

      return retval;
    }
  else if (nom && nom->is_subctx)
    {
      CosNaming_Name n2 = *n;

      n2._length--;
      n2._buffer++;

      return CosNaming_NamingContext_bind_new_context (nom->obj, &n2, ev);
    }
  else
    {
      CosNaming_NamingContext_NotFound *exdata;
    raise_exc:

      exdata = CosNaming_NamingContext_NotFound__alloc ();
      exdata->why =
	nom ? CosNaming_NamingContext_not_context :
	CosNaming_NamingContext_missing_node;
      Name__copy (&exdata->rest_of_name, n, 0);

      CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			   ex_CosNaming_NamingContext_NotFound, exdata);
    }

  return CORBA_OBJECT_NIL;
}

static void
h_rn_unref (gpointer key, gpointer val, gpointer user_data)
{
  rn_unref (val);
}

static void
impl_NamingContextExt__destroy (impl_POA_CosNaming_NamingContextExt *servant,
				CORBA_Environment                   *ev)
{
  CORBA_Object_release ((CORBA_Object) servant->poa, ev);

  g_hash_table_foreach_remove (servant->names, (GHRFunc) h_rn_unref,
			       servant);
  g_hash_table_destroy (servant->names);

  CORBA_Object_release (servant->objref, ev);
  POA_CosNaming_NamingContextExt__fini ((PortableServer_Servant) servant, ev);
  g_free (servant);
}

static void
impl_NamingContext_destroy (impl_POA_CosNaming_NamingContextExt *
			    servant, CORBA_Environment * ev)
{
  impl_NamingContextExt__destroy (servant, ev);
}

static CosNaming_BindingList *
n_names_into_bindinglist (GList ** itemlist, int n)
{
  CosNaming_BindingList *bl = CosNaming_BindingList__alloc ();
  int i;
  RegisteredName *curnom;

  bl->_length = n;
  bl->_buffer =
    n ? CORBA_sequence_CosNaming_Binding_allocbuf (bl->_length) : NULL;
  for (i = 0; i < n; i++)
    {
      curnom = (*itemlist)->data;
      *itemlist = g_list_remove_link (*itemlist, *itemlist);

      bl->_buffer[i].binding_name._length = 1;
      bl->_buffer[i].binding_name._buffer =
	CORBA_sequence_CosNaming_NameComponent_allocbuf (1);
      NameComponent__copy (&bl->_buffer[i].binding_name._buffer[0],
			   &curnom->nc);
      bl->_buffer[i].binding_type =
	curnom->is_subctx ? CosNaming_ncontext : CosNaming_nobject;

      rn_unref (curnom);
    }

  return bl;
}

static void
list_ptr_append (gpointer key, gpointer val, GList ** listptr)
{
  *listptr = g_list_append (*listptr, rn_ref (val));
}

static GList *
nc_names_into_itemlist (impl_POA_CosNaming_NamingContextExt * servant)
{
  GList *retval = NULL;
  g_hash_table_foreach (servant->names, (GHFunc) list_ptr_append, &retval);

  return retval;
}

static void
impl_BindingIterator__destroy (impl_POA_CosNaming_BindingIterator *
			       servant, CORBA_Environment * ev)
{
  CORBA_Object_release ((CORBA_Object) servant->poa, ev);

  g_list_foreach (servant->items, (GFunc) l_rn_unref, NULL);
  g_list_free (servant->items);

  POA_CosNaming_BindingIterator__fini ((PortableServer_Servant) servant, ev);
  g_free (servant);
}

static CORBA_boolean
impl_BindingIterator_next_one (impl_POA_CosNaming_BindingIterator *
			       servant, CosNaming_Binding ** b,
			       CORBA_Environment * ev)
{
  RegisteredName *curnom;

  if (servant->items)
    {
      *b = CosNaming_Binding__alloc ();
      curnom = servant->items->data;
      servant->items = g_list_remove_link (servant->items, servant->items);

      (*b)->binding_name._length = 1;

      (*b)->binding_name._buffer =
	CORBA_sequence_CosNaming_NameComponent_allocbuf (1);
      NameComponent__copy (&(*b)->binding_name._buffer[0], &curnom->nc);
      (*b)->binding_type = 
	curnom->is_subctx ? CosNaming_ncontext : CosNaming_nobject;

      rn_unref (curnom);
    }
  else
    *b = NULL;

  return servant->items ? CORBA_TRUE : CORBA_FALSE;
}

static CORBA_boolean
impl_BindingIterator_next_n (impl_POA_CosNaming_BindingIterator *
			     servant, CORBA_unsigned_long how_many,
			     CosNaming_BindingList ** bl,
			     CORBA_Environment * ev)
{
  *bl =
    n_names_into_bindinglist (&servant->items,
			      MIN (g_list_length (servant->items), how_many));

  return ((*bl)->_length > 0);
}

static void
impl_BindingIterator_destroy (impl_POA_CosNaming_BindingIterator *
			      servant, CORBA_Environment * ev)
{
  impl_BindingIterator__destroy (servant, ev);
}


static CosNaming_NamingContextExt_StringName
impl_NamingContextExt_to_string (impl_POA_CosNaming_NamingContextExt
				 * servant, CosNaming_Name * n,
				 CORBA_Environment * ev)
{
  return ORBit_CosNaming_Name_to_string (n);
}

static CosNaming_Name *
impl_NamingContextExt_to_name (impl_POA_CosNaming_NamingContextExt *
			       servant,
			       CosNaming_NamingContextExt_StringName
			       sn, CORBA_Environment * ev)
{
  return ORBit_string_to_CosNaming_Name (sn, ev);
}

static CosNaming_NamingContextExt_URLString
impl_NamingContextExt_to_url (impl_POA_CosNaming_NamingContextExt *
			      servant,
			      CosNaming_NamingContextExt_Address
			      addr,
			      CosNaming_NamingContextExt_StringName
			      sn, CORBA_Environment * ev)
{
  return CORBA_string_dup ("FIXME: Not implemented yet, what should that "
			   "do BTW? Just a concatanation?");
}

static CORBA_Object
impl_NamingContextExt_resolve_str (impl_POA_CosNaming_NamingContextExt * 
				   servant,
				   CosNaming_NamingContextExt_StringName n, 
				   CORBA_Environment * ev)
{
  CosNaming_Name *name = ORBit_string_to_CosNaming_Name (n, ev);
  if (ev->_major != CORBA_NO_EXCEPTION)
    return CORBA_OBJECT_NIL;
  if (name->_length == 0)
    return CORBA_Object_duplicate (servant->objref, ev);
  else
    return impl_NamingContext_resolve (servant, name, ev);
}

/*** epv structures ***/

static PortableServer_ServantBase__epv impl_BindingIterator_base_epv = {
  NULL,				/* _private data */
  (gpointer) & impl_BindingIterator__destroy,	/* finalize routine */
  NULL,				/* default_POA routine */
};

static POA_CosNaming_BindingIterator__epv impl_BindingIterator_epv = {
  NULL,				/* _private */
  (gpointer) & impl_BindingIterator_next_one,
  (gpointer) & impl_BindingIterator_next_n,
  (gpointer) & impl_BindingIterator_destroy
};

static POA_CosNaming_NamingContextExt__epv impl_NamingContextExt_epv = {
  NULL,				/* _private */
  (gpointer) & impl_NamingContextExt_to_string,
  (gpointer) & impl_NamingContextExt_to_name,
  (gpointer) & impl_NamingContextExt_to_url,
  (gpointer) & impl_NamingContextExt_resolve_str
};

/*** vepv structures ***/

static POA_CosNaming_BindingIterator__vepv impl_BindingIterator_vepv = {
  &impl_BindingIterator_base_epv,
  &impl_BindingIterator_epv
};

static CosNaming_BindingIterator
impl_BindingIterator__create (PortableServer_POA                   poa,
			      impl_POA_CosNaming_BindingIterator **ns,
			      CORBA_Environment                   *ev)
{
  CosNaming_BindingIterator retval;
  impl_POA_CosNaming_BindingIterator *newservant;
  PortableServer_ObjectId *objid;

  newservant = *ns = g_new0 (impl_POA_CosNaming_BindingIterator, 1);
  newservant->servant.vepv = &impl_BindingIterator_vepv;
  newservant->poa = (PortableServer_POA) CORBA_Object_duplicate ((CORBA_Object) poa, ev);
  newservant->items = NULL;

  POA_CosNaming_BindingIterator__init ((PortableServer_Servant) newservant,
				       ev);
  objid = PortableServer_POA_activate_object (poa, newservant, ev);
  CORBA_free (objid);
  retval = PortableServer_POA_servant_to_reference (poa, newservant, ev);

  return retval;
}

static void
impl_NamingContext_list (impl_POA_CosNaming_NamingContextExt *servant, 
			 CORBA_unsigned_long                  how_many,
			 CosNaming_BindingList              **bl,
			 CosNaming_BindingIterator           *bi,
			 CORBA_Environment                   *ev)
{
  GList *itemlist;

  itemlist = nc_names_into_itemlist (servant);

  if (how_many > 0)
    {
      *bl =
	n_names_into_bindinglist (&itemlist,
				  MIN (how_many,
				       g_hash_table_size (servant->names)));
    }
  else
    {
      *bl = CosNaming_BindingList__alloc ();
      (*bl)->_length = 0;
    }

  if (itemlist)
    {
      impl_POA_CosNaming_BindingIterator *newservant;
      *bi = impl_BindingIterator__create (servant->poa, &newservant, ev);
      newservant->items = itemlist;
    }
  else
    *bi = CORBA_OBJECT_NIL;
}

static PortableServer_ServantBase__epv impl_NamingContextExt_base_epv = {
  NULL,				/* _private data */
  (gpointer) & impl_NamingContextExt__destroy,	/* finalize routine */
  NULL,				/* default_POA routine */
};

static POA_CosNaming_NamingContext__epv impl_NamingContext_epv = {
  NULL,				/* _private */

  (gpointer) & impl_NamingContext_bind,
  (gpointer) & impl_NamingContext_rebind,
  (gpointer) & impl_NamingContext_bind_context,
  (gpointer) & impl_NamingContext_rebind_context,
  (gpointer) & impl_NamingContext_resolve,
  (gpointer) & impl_NamingContext_unbind,
  (gpointer) & impl_NamingContext_new_context,
  (gpointer) & impl_NamingContext_bind_new_context,
  (gpointer) & impl_NamingContext_destroy,
  (gpointer) & impl_NamingContext_list
};

static POA_CosNaming_NamingContextExt__vepv impl_NamingContextExt_vepv = {
	&impl_NamingContextExt_base_epv,
	&impl_NamingContext_epv,
	&impl_NamingContextExt_epv
};

CosNaming_NamingContextExt
ORBit_CosNaming_NamingContextExt_create (PortableServer_POA poa, 
					 CORBA_Environment *ev)
{
	CosNaming_NamingContext retval;
	impl_POA_CosNaming_NamingContextExt *newservant;
	PortableServer_ObjectId *objid;

	newservant = g_new0 (impl_POA_CosNaming_NamingContextExt, 1);
	newservant->servant.vepv = &impl_NamingContextExt_vepv;
	newservant->poa = (PortableServer_POA) CORBA_Object_duplicate ((CORBA_Object) poa, ev);
	newservant->names =
		g_hash_table_new ((GHashFunc) nc_hash, (GCompareFunc) nc_compare);

	POA_CosNaming_NamingContextExt__init ((PortableServer_Servant) newservant, ev);
	objid = PortableServer_POA_activate_object (poa, newservant, ev);
	CORBA_free (objid);
	retval = PortableServer_POA_servant_to_reference (poa, newservant, ev);
	newservant->objref = CORBA_Object_duplicate (retval, ev);

	return retval;
}
