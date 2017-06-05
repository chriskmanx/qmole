#include "config.h"
#include <orbit/orbit.h>
#include <string.h>
#include <stdio.h>
#include "corba-ops.h"
#include "orb-core-private.h"
#include "orb-core-export.h"
#include "orbit-debug.h"
#include "../util/orbit-purify.h"

static GMutex *object_lock = NULL;

#define OBJECT_LOCK(obj)   LINK_MUTEX_LOCK   (object_lock)
#define OBJECT_UNLOCK(obj) LINK_MUTEX_UNLOCK (object_lock)

/*
 * obj->orb -> construct time property ...
 * obj->type_qid -> construct time property ?
 *
 * obj->profile_list -> needs locking
 */

static guint
g_CORBA_Object_hash (gconstpointer key)
{
	CORBA_Object obj = (gpointer) key;

	/* type_id is not reliable: cf. corbaloc */
	g_assert (obj->object_key != NULL);

        return IOP_ObjectKey_hash (obj->object_key);
}

static gboolean
g_CORBA_Object_equal (gconstpointer a, gconstpointer b)
{
	GSList *cur1, *cur2;
	CORBA_Object _obj = (CORBA_Object) a;
	CORBA_Object other_object = (CORBA_Object) b;

	g_assert (_obj->object_key && other_object->object_key);

	if (!IOP_ObjectKey_equal (_obj->object_key, other_object->object_key))
		return FALSE;

	for (cur1 = _obj->profile_list; cur1; cur1 = cur1->next) {
		for (cur2 = other_object->profile_list; cur2; cur2 = cur2->next) {
			if (IOP_profile_equal (_obj, other_object,
					       cur1->data, cur2->data)) {
#ifdef G_ENABLE_DEBUG
			        if (_orbit_debug_flags & ORBIT_DEBUG_OBJECTS) {
					char *a, *b;
					a = IOP_profile_dump (_obj, cur1->data);
					b = IOP_profile_dump (other_object, cur2->data);
					fprintf (stderr, "Profiles match:\n'%s':%s\n'%s':%s\n",
						           g_quark_to_string (_obj->type_qid), a,
						           g_quark_to_string (other_object->type_qid), b);
					g_free (a);
					g_free (b);
				}
#endif /* G_ENABLE_DEBUG */
				return TRUE;
			}
		}
	}

	return FALSE;
}

void
ORBit_register_objref (CORBA_Object obj)
{
	CORBA_ORB orb = obj->orb;

	g_assert (orb != NULL);
	g_assert (obj->object_key != NULL);
	g_assert (obj->profile_list != NULL);

	LINK_MUTEX_LOCK (orb->lock);

	if (!orb->objrefs)
		orb->objrefs = g_hash_table_new (
			g_CORBA_Object_hash, g_CORBA_Object_equal);
	g_hash_table_insert (orb->objrefs, obj, obj);

	LINK_MUTEX_UNLOCK (orb->lock);
}

static CORBA_Object
ORBit_lookup_objref (CORBA_Object obj)
{
	CORBA_Object result;
	CORBA_ORB orb = obj->orb;

	g_assert (orb != NULL);

	LINK_MUTEX_LOCK (orb->lock);
	if (!orb->objrefs || !obj->profile_list)
		result = NULL;
	else
		result = g_hash_table_lookup (orb->objrefs, obj);
	LINK_MUTEX_UNLOCK (orb->lock);

	return result;
}

static void
CORBA_Object_release_cb (ORBit_RootObject robj)
{
	CORBA_Object obj = (CORBA_Object) robj;
	CORBA_ORB    orb = obj->orb;

	if (orb && obj->profile_list) {
		LINK_MUTEX_LOCK (orb->lock);
		g_hash_table_remove (orb->objrefs, obj);
		LINK_MUTEX_UNLOCK (orb->lock);
	}

	ORBit_free_T (obj->object_key);

	IOP_delete_profiles (obj->orb, &obj->profile_list);
	IOP_delete_profiles (obj->orb, &obj->forward_locations);

	if (obj->adaptor_obj) {
		obj->adaptor_obj->objref = NULL;

		ORBit_RootObject_release_T (obj->adaptor_obj);
	}

	
	if (obj->connection) {
		LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);
		giop_connection_unref (obj->connection);
		LINK_MUTEX_LOCK   (ORBit_RootObject_lifecycle_lock);
	}

	p_free (obj, struct CORBA_Object_type);
}

static ORBit_RootObject_Interface objref_if = {
	ORBIT_ROT_OBJREF,
	CORBA_Object_release_cb
};

CORBA_Object
ORBit_objref_new (CORBA_ORB      orb,
		  ORBit_OAObject adaptor_obj,
		  GQuark         type_id)
{
	CORBA_Object retval;

	retval = g_new0 (struct CORBA_Object_type, 1);

	ORBit_RootObject_init ((ORBit_RootObject) retval, &objref_if);

	retval->type_qid = type_id;
	retval->orb = orb;
	retval->adaptor_obj = ORBit_RootObject_duplicate (adaptor_obj);

	return retval;
}

static CORBA_Object
ORBit_objref_find (CORBA_ORB   orb,
		   const char *type_id,
		   GSList     *profiles)
{
	CORBA_Object retval = CORBA_OBJECT_NIL;
	struct CORBA_Object_type fakeme = {{NULL}};

	fakeme.orb = orb;
	fakeme.type_qid = g_quark_from_string (type_id);
	fakeme.profile_list = profiles;
	fakeme.object_key = IOP_profiles_sync_objkey (profiles);

	LINK_MUTEX_LOCK (ORBit_RootObject_lifecycle_lock);

	retval = ORBit_lookup_objref (&fakeme);

	dprintf (OBJECTS, "Lookup '%s' (%p) == %p\n", type_id, profiles, retval);

#ifdef G_ENABLE_DEBUG
	if (_orbit_debug_flags & ORBIT_DEBUG_OBJECTS) {
		GSList *l;
		fprintf (stderr, "Profiles: ");
		for (l = profiles; l; l = l->next) {
			char *str;
			fprintf (stderr, "%s", (str = IOP_profile_dump (&fakeme, l->data)));
			g_free (str);
		}
		fprintf (stderr, "\n");
	}
#endif /* G_ENABLE_DEBUG */

	if (!retval) {
		retval = ORBit_objref_new (orb, NULL, fakeme.type_qid);
		retval->profile_list = profiles;
		retval->object_key   = fakeme.object_key;
		ORBit_register_objref (retval);
	} else {
		ORBit_free_T (fakeme.object_key);
		IOP_delete_profiles (orb, &profiles);
	}

	retval = ORBit_RootObject_duplicate_T (retval);

	LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);

	return retval;
}

static gboolean
is_localhost (const char *host)
{
	return (host && !strcmp (link_get_local_hostname (), host)) ? TRUE : FALSE;
}

/**
 * ORBit_objref_get_proxy:
 * @obj: the local object
 * 
 *  Creates a 'remote' alike object for the fully local
 * object reference, so that we can deal with it
 * asynchronously. Defeats the orb cleverness to stop this,
 * doesn't register in the global object / profile hash.
 *
 *  You almost certainly don't want to use this routine.
 *
 * FIXME: we should have an ORB wide 'socketpair' profile
 * that is private for purely in-proc local loopback support.
 * 
 * Return value: a proxy object
 **/
CORBA_Object
ORBit_objref_get_proxy (CORBA_Object obj)
{
	CORBA_Object iobj;

	OBJECT_LOCK (obj);
	if (!obj->profile_list) {
		IOP_generate_profiles (obj);
		ORBit_register_objref (obj);
	}
	OBJECT_UNLOCK (obj);

	/* We need a pseudo-remote reference */
	iobj = ORBit_objref_new (obj->orb, NULL, obj->type_qid);
	iobj->profile_list = IOP_profiles_copy (obj->profile_list);
	iobj->object_key = IOP_ObjectKey_copy (obj->object_key);

	return ORBit_RootObject_duplicate (iobj);
}

static gboolean
ORBit_try_connection_T (CORBA_Object obj)
{
	gboolean retval = FALSE;
	LinkConnectionStatus status;
	LinkConnection *cnx = LINK_CONNECTION (obj->connection);

	OBJECT_UNLOCK (obj);

	status = link_connection_wait_connected (cnx);

	switch (status) {
	case LINK_CONNECTING:
		g_assert_not_reached();
		break;
	case LINK_CONNECTED:
		retval = TRUE;
		break;
	case LINK_DISCONNECTED:
	case LINK_TIMEOUT:
		/* Have a go at reviving it */
		dprintf (MESSAGES, "re-connecting dropped cnx %p: ", cnx);
		if (giop_connection_try_reconnect (GIOP_CONNECTION (cnx)) == LINK_CONNECTED)
			retval = TRUE;
		dprintf (MESSAGES, retval ? "connected\n" : "not connected\n" );
		break;
	}

	OBJECT_LOCK (obj);

	g_assert (LINK_CONNECTION (obj->connection) == cnx);

	return retval;
}

GIOPConnection *
ORBit_object_peek_connection (CORBA_Object obj)
{
	GIOPConnection *cnx;

	OBJECT_LOCK (obj);
	
	if ((cnx = obj->connection))
		giop_connection_ref (cnx);

	OBJECT_UNLOCK (obj);

	return cnx;
}

GIOPConnection *
ORBit_object_get_connection (CORBA_Object obj)
{
	GSList *plist, *cur;
	char tbuf[20];
	/* Information we have to come up with */
	ORBit_ObjectKey *objkey;
	char *proto = NULL, *host, *service;
	gboolean is_ssl = FALSE;
	GIOPVersion iiop_version = GIOP_1_2;
	GIOPConnection *cnx = NULL;
	gboolean unix_socket_enabled = FALSE;
	gboolean ipv4_ipv6_enabled = FALSE;
	gboolean unix_socket_failed = FALSE;

	OBJECT_LOCK (obj);

	unix_socket_enabled = ORBit_proto_use ("UNIX");
	ipv4_ipv6_enabled = (ORBit_proto_use ("IPv4") || ORBit_proto_use ("IPv6"));

	if (obj->connection) {
		if (ORBit_try_connection_T (obj)) {
			cnx = obj->connection;
			giop_connection_ref (cnx);
			OBJECT_UNLOCK (obj);
			return cnx;
		} else {
			OBJECT_UNLOCK (obj);
			return NULL;
		}
	}
  
	g_assert (obj->connection == NULL);

	if (!obj->forward_locations) {
		plist = obj->profile_list;
		objkey = obj->object_key;
	} else {
		plist = obj->forward_locations;
		objkey = IOP_profiles_sync_objkey (plist);
	}

	for (cur = plist; cur; cur = cur->next) {
		gpointer *pinfo = cur->data;

		if (IOP_profile_get_info (obj, pinfo, &iiop_version, &proto,
					  &host, &service, &is_ssl, tbuf)) {

			if (unix_socket_failed && ipv4_ipv6_enabled && is_localhost (host))
				continue;

			obj->connection = giop_connection_initiate (
				obj->orb, proto, host, service,
				is_ssl ? LINK_CONNECTION_SSL : 0, iiop_version);

			if (!obj->connection && unix_socket_enabled && ipv4_ipv6_enabled) {
				if (!strcmp (proto,"UNIX")) 
					unix_socket_failed = TRUE;
			}
	
			if (obj->connection && ORBit_try_connection_T (obj)) {
				if (!IOP_ObjectKey_equal (obj->object_key, objkey)) {
					/* We need to remove and re-add the object from the
					 * objrefs hash table since changing the object key will
					 * change the object's hash value.
					 */
					LINK_MUTEX_LOCK (obj->orb->lock);
					g_hash_table_remove (obj->orb->objrefs, obj);
					obj->object_key = objkey;
					g_hash_table_insert (obj->orb->objrefs, obj, obj);
					LINK_MUTEX_UNLOCK (obj->orb->lock);
				} else {
					obj->object_key = objkey;
				}

				obj->connection->orb_data = obj->orb;

				dprintf (OBJECTS, "Initiated a connection to '%s' '%s' '%s'\n",
					 proto, host, service);

				cnx = obj->connection;
				giop_connection_ref (cnx);
				break;
			}
		}
	}
	OBJECT_UNLOCK (obj);

	return cnx;
}

GIOPConnection *
ORBit_handle_location_forward (GIOPRecvBuffer *buf,
			       CORBA_Object    obj)
{
	GSList         *profiles = NULL;
	GIOPConnection *old_connection;

	if (ORBit_demarshal_IOR (obj->orb, buf, NULL, &profiles))
	  return NULL;

	OBJECT_LOCK (obj);
	IOP_delete_profiles (obj->orb, &obj->forward_locations);
	obj->forward_locations = profiles;

	/* We need to clear the connection because 
	   forwarding results in a different connection */
	old_connection = obj->connection;
	obj->connection = NULL;
	OBJECT_UNLOCK (obj);

	giop_connection_unref (old_connection);

	return ORBit_object_get_connection (obj);
}

CORBA_InterfaceDef
CORBA_Object_get_interface (CORBA_Object       obj,
			    CORBA_Environment *ev)
{
	/* FIXME: we can use the IInterface info for this */
	return CORBA_OBJECT_NIL;
}

CORBA_boolean
CORBA_Object_is_nil (CORBA_Object       obj,
		     CORBA_Environment *ev)
{
	return obj ? CORBA_FALSE : CORBA_TRUE;
}

CORBA_Object
CORBA_Object_duplicate (CORBA_Object       obj,
			CORBA_Environment *ev)
{
	return ORBit_RootObject_duplicate (obj);
}

void
CORBA_Object_release (CORBA_Object        obj,
		      CORBA_Environment *ev)
{
	ORBit_RootObject_release (obj);
}

CORBA_boolean
CORBA_Object_non_existent (CORBA_Object       obj,
			   CORBA_Environment *ev)
{
	gboolean        retval;
	GIOPConnection *cnx;
	ORBit_OAObject  adaptor_obj;

	if (obj == CORBA_OBJECT_NIL)
		return TRUE;

	adaptor_obj = obj->adaptor_obj;
	if (adaptor_obj && adaptor_obj->interface->is_active (adaptor_obj))
		return FALSE;

	cnx = ORBit_object_get_connection (obj);

	if (cnx) {
		LinkConnectionStatus status;
		status = link_connection_wait_connected (LINK_CONNECTION (cnx));
		retval = (status == LINK_CONNECTED) ? FALSE : TRUE;
		giop_connection_unref (cnx);
	} else
		retval = TRUE;

	return retval;
}

/*
 * We already ensure uniqueness of the CORBA_Object structure
 */
CORBA_boolean
CORBA_Object_is_equivalent (CORBA_Object       obj,
			    const CORBA_Object other_object,
			    CORBA_Environment *ev)
{
	return obj == other_object;
}

/*
 * We already ensure uniqueness of the CORBA_Object structure
 */
CORBA_unsigned_long
CORBA_Object_hash (CORBA_Object              obj,
		   const CORBA_unsigned_long maximum,
		   CORBA_Environment        *ev)
{
	CORBA_unsigned_long retval;

	retval = GPOINTER_TO_UINT (obj);

	return maximum ? (retval % maximum) : retval;
}

void
CORBA_Object_create_request (CORBA_Object         _obj,
			     const CORBA_Context  ctx,
			     const CORBA_char    *operation,
			     const CORBA_NVList   arg_list,
			     CORBA_NamedValue    *result,
			     CORBA_Request       *request,
			     const CORBA_Flags    req_flag,
			     CORBA_Environment   *ev)
{
	CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);
}

CORBA_Policy
CORBA_Object_get_policy (CORBA_Object           obj,
			 const CORBA_PolicyType policy_type,
			 CORBA_Environment     *ev)
{
	CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);
	return CORBA_OBJECT_NIL;
}

CORBA_DomainManagersList *
CORBA_Object_get_domain_managers (CORBA_Object       obj,
				  CORBA_Environment *ev)
{
	CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);
	return NULL;
}

CORBA_Object
CORBA_Object_set_policy_overrides (CORBA_Object                obj,
				   const CORBA_PolicyList     *policies,
				   const CORBA_SetOverrideType set_add,
				   CORBA_Environment          *ev)
{
	CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);
	return CORBA_OBJECT_NIL;
}

void
ORBit_marshal_object (GIOPSendBuffer *buf, CORBA_Object obj)
{
	GSList             *cur;
	const char         *typeid;
	CORBA_unsigned_long num_profiles = 0;

	if (!obj) {
		dprintf (OBJECTS, "Marshal NIL object\n");
		giop_send_buffer_append_string (buf, "");
		giop_send_buffer_append_aligned (buf, &num_profiles, 4);
		return;
	}

	typeid = g_quark_to_string (obj->type_qid);
	if (!typeid)
		g_error ("Attempted to marshal a bogus / "
			 "dead object %p type", obj);

	giop_send_buffer_append_string (buf, typeid);

	OBJECT_LOCK (obj);

	if (!obj->profile_list) {
		IOP_generate_profiles (obj);
		ORBit_register_objref (obj);
	}
	num_profiles = g_slist_length (obj->profile_list);
	g_assert (num_profiles > 0);

	giop_send_buffer_append_aligned (buf, &num_profiles, 4);

	dprintf (OBJECTS, "Marshal object '%p'\n", obj);

	for (cur = obj->profile_list; cur; cur = cur->next) {
#ifdef G_ENABLE_DEBUG
		if (_orbit_debug_flags & ORBIT_DEBUG_OBJECTS) {
			char *str;
			fprintf (stderr, "%s\n",
				 (str = IOP_profile_dump (obj, cur->data)));
			g_free (str);
		}
#endif /* G_ENABLE_DEBUG */
		IOP_profile_marshal (obj, buf, cur->data);
	}

	OBJECT_UNLOCK (obj);
}

gboolean
ORBit_demarshal_object (CORBA_Object   *obj,
			GIOPRecvBuffer *buf,
			CORBA_ORB       orb)
{
	gchar  *type_id = NULL;
	GSList *profiles = NULL;

	g_return_val_if_fail (orb != CORBA_OBJECT_NIL, TRUE);

	if (ORBit_demarshal_IOR (orb, buf, &type_id, &profiles))
		return TRUE;

	if (type_id)
		*obj = ORBit_objref_find (orb, type_id, profiles);
	else
		*obj = CORBA_OBJECT_NIL;

	return FALSE;
}

CORBA_char*
ORBit_object_to_corbaloc (CORBA_Object       obj,
                          CORBA_Environment *ev)
{
        CORBA_char      *retval = NULL;

        if (!obj) {
                dprintf (OBJECTS, "Corbaloc NIL object\n");
                return CORBA_string_dup ("corbaloc::/");
        }

        OBJECT_LOCK (obj);

        if (!obj->profile_list) {
                IOP_generate_profiles (obj);
                ORBit_register_objref (obj);
        }

        if (!(retval = ORBit_corbaloc_from (obj->profile_list,
                                            obj->object_key))) {
                CORBA_exception_set_system (
                        ev, ex_CORBA_BAD_PARAM, CORBA_COMPLETED_NO);
                /* FIXME, set minor code with vendor specific id */
        }
 
        OBJECT_UNLOCK (obj);
         
        return retval;
}


CORBA_Object
ORBit_object_by_corbaloc  (CORBA_ORB          orb,
                           const gchar       *corbaloc,
                           CORBA_Environment *ev)
{
        CORBA_Object  retval       = CORBA_OBJECT_NIL;
        GSList       *profile_list = NULL;

        g_return_val_if_fail (orb!=NULL,      CORBA_OBJECT_NIL);
        g_return_val_if_fail (corbaloc!=NULL, CORBA_OBJECT_NIL);
        g_return_val_if_fail (ev!=NULL,       CORBA_OBJECT_NIL);

	if (!strncmp (corbaloc, "corbaloc::/", 1 + strlen ("corbaloc::/")))
		return CORBA_OBJECT_NIL;  

        if (!(profile_list = ORBit_corbaloc_parse (corbaloc)))  {
                CORBA_exception_set_system (
                        ev, ex_CORBA_BAD_PARAM, CORBA_COMPLETED_NO);
                /* FIXME, set minor code with vendor specific id */
                return CORBA_OBJECT_NIL;
        }

        if (!(retval = ORBit_objref_find (orb, "", profile_list))) {
                CORBA_exception_set_system (
                        ev, ex_CORBA_BAD_PARAM, CORBA_COMPLETED_NO);
                /* FIXME, set minor code with vendor specific id */

                IOP_delete_profiles (orb, &profile_list);
                return CORBA_OBJECT_NIL;
        }

        return retval;
}

static gboolean
ORBit_IInterface_is_a (ORBit_IInterface *idata, const char *type_id)
{
	int i;

	if (!strcmp (idata->tc->repo_id, type_id))
		return TRUE;

	for (i = 0; i < idata->base_interfaces._length; i++ )
		if (!strcmp (idata->base_interfaces._buffer [i], type_id))
			return TRUE;

	return FALSE;
}

static void
ORBit_impl_CORBA_Object_is_a(PortableServer_ServantBase *servant,
                             gpointer ret, gpointer *args,
                             gpointer ctx, CORBA_Environment *ev,
                             gpointer imp)
{
	PortableServer_ClassInfo *ci = ORBIT_SERVANT_TO_CLASSINFO (servant);
	const char               *type_id = *(const char **)args[0];

	*(CORBA_boolean *)ret = ORBit_IInterface_is_a (ci->idata, type_id);
}

CORBA_boolean
CORBA_Object_is_a (CORBA_Object       obj,
		   const CORBA_char  *logical_type_id,
		   CORBA_Environment *ev)
{
	static GQuark  corba_object_quark = 0;
	static GQuark  omg_corba_object_quark = 0;
	CORBA_boolean  retval;
	gpointer       servant;
	gpointer       args[] = { NULL };
	GQuark         logical_type_quark;

	args[0] = (gpointer *)&logical_type_id;

	if (!corba_object_quark)
		corba_object_quark = g_quark_from_static_string (
			"IDL:CORBA/Object:1.0");

	if (!omg_corba_object_quark)
		omg_corba_object_quark = g_quark_from_static_string (
			"IDL:omg.org/CORBA/Object:1.0");

	logical_type_quark = g_quark_from_string (logical_type_id);

	if (logical_type_quark == corba_object_quark)
		return CORBA_TRUE;

	if (logical_type_quark == omg_corba_object_quark)
		return CORBA_TRUE;

	if (!obj)
		return CORBA_FALSE;

	if (logical_type_quark == obj->type_qid)
		return CORBA_TRUE;

	if ((servant = ORBit_small_get_servant (obj)))
		ORBit_impl_CORBA_Object_is_a (servant, &retval, args, NULL, ev, NULL);

	else /* warning: obeys POA policies */
		ORBit_small_invoke_stub (obj, &CORBA_Object__imethods[4],
					 &retval, args, NULL, ev);

	return retval;
}

static void
ORBit_impl_ORBit_get_type_id (PortableServer_ServantBase *servant,
			      gpointer ret, gpointer *args,
			      gpointer ctx, CORBA_Environment *ev,
			      gpointer imp)
{
	PortableServer_ClassInfo *ci = ORBIT_SERVANT_TO_CLASSINFO (servant);

	*(CORBA_char **)ret = CORBA_string_dup (ci->idata->tc->repo_id);
}

static void
ORBit_impl_ORBit_get_iinterface (PortableServer_ServantBase *servant,
				 gpointer ret, gpointer *args,
				 gpointer ctx, CORBA_Environment *ev,
				 gpointer imp)
{
	const char *repo_id = *(const char **)args[0];

	*(ORBit_IInterface **)ret = ORBit_small_get_iinterface (
		CORBA_OBJECT_NIL, repo_id, ev);
}

ORBitSmallSkeleton
get_small_skel_CORBA_Object(PortableServer_Servant servant, const char *opname, 
			    gpointer * m_data, gpointer * impl)
{
	if (!strcmp (opname, "_is_a")) {
		*impl = *m_data = (gpointer)&CORBA_Object__imethods [4];
		return (ORBitSmallSkeleton) ORBit_impl_CORBA_Object_is_a;

	} else if (!strcmp (opname, "ORBit_get_type_id")) {
		*impl = *m_data = (gpointer)&CORBA_Object__imethods [
			CORBA_OBJECT_SMALL_GET_TYPE_ID];
		return (ORBitSmallSkeleton) ORBit_impl_ORBit_get_type_id;

	} else if (!strcmp (opname, "ORBit_get_iinterface")) {
		*impl = *m_data = (gpointer)&CORBA_Object__imethods [
			CORBA_OBJECT_SMALL_GET_IINTERFACE];
		return (ORBitSmallSkeleton) ORBit_impl_ORBit_get_iinterface;
	}		

	return NULL;
}

/* 
 * Arguments' Definitions.
 */

static ORBit_IArg
CORBA_Object_is_a__arginfo[] = {
   {
   TC_CORBA_string, ORBit_I_ARG_IN, "logical_type_id"
   },
   {NULL, 0, NULL}
};

static ORBit_IArg
CORBA_Object_is_equivalent__arginfo[] = {
   {
    TC_CORBA_Object, ORBit_I_ARG_IN, "other_object"
   },
   {NULL, 0, NULL}
};

static ORBit_IArg
CORBA_Object_hash__arginfo[] = {
   {
    TC_CORBA_unsigned_long, 
    ORBit_I_ARG_IN | ORBit_I_COMMON_FIXED_SIZE,
    "maximum"
   },
   {NULL, 0, NULL}
};

static ORBit_IArg
CORBA_Object_create_request__arginfo[] = {
   {
    TC_CORBA_Context, ORBit_I_ARG_IN, "ctx"
   },
   {
    TC_CORBA_Identifier, ORBit_I_ARG_IN, "operation"
   },
   {
    TC_CORBA_NVList, ORBit_I_ARG_IN, "arg_list"
   },
   {
    TC_CORBA_NamedValue, ORBit_I_ARG_INOUT, "result"
   },
   {
    TC_CORBA_Request, ORBit_I_ARG_OUT, "request"
   },
   {
    TC_CORBA_Flags, 
    ORBit_I_ARG_IN | ORBit_I_COMMON_FIXED_SIZE, 
    "req_flag"
   },
   {NULL, 0, NULL}
};

static ORBit_IArg
CORBA_Object_get_policy__arginfo[] = {
   {
    TC_CORBA_PolicyType,
    ORBit_I_ARG_IN | ORBit_I_COMMON_FIXED_SIZE,
    "policy_type"
   },
   {NULL, 0, NULL}
};

static ORBit_IArg
CORBA_Object_ORBit_get_iinterface__arginfo[] = {
   {
    TC_CORBA_string,
    ORBit_I_ARG_IN,
    "type_id"
   },
   {NULL, 0, NULL}
};

static CORBA_TypeCode
CORBA_Object_ORBit_get_iinterface__exceptinfo[] = {
	TC_ORBit_NoIInterface, NULL
};

static ORBit_IArg
CORBA_Object_set_policy_overrides__arginfo[] = {
   {
    TC_CORBA_PolicyList, ORBit_I_ARG_IN, "policies"
   },
   {
    TC_CORBA_SetOverrideType, 
    ORBit_I_ARG_IN | ORBit_I_COMMON_FIXED_SIZE,
    "set_add"
   },
   {NULL, 0, NULL}
};

/* 
 * Methods' Definitions.
 */

ORBit_IMethod
CORBA_Object__imethods[] = {
   {
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_InterfaceDef, "_interface", 10, 0
   },
   {
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_boolean, "is_nil", 6,
    0 | ORBit_I_COMMON_FIXED_SIZE
   },
   {
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_Object, "duplicate", 9, 0
   },
   {
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    CORBA_OBJECT_NIL, "release", 7, 0
   },
   {
    {1, 1, CORBA_Object_is_a__arginfo, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_boolean, "_is_a", 5,
    0 | ORBit_I_COMMON_FIXED_SIZE
   },
   {
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_boolean, "_non_existent", 13,
    0 | ORBit_I_COMMON_FIXED_SIZE
   },
   {
    {1, 1, CORBA_Object_is_equivalent__arginfo, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_boolean, "is_equivalent", 13,
    0 | ORBit_I_COMMON_FIXED_SIZE
   },
   {
    {1, 1, CORBA_Object_hash__arginfo, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_unsigned_long, "hash", 4,
    0 | ORBit_I_COMMON_FIXED_SIZE
   },
   {
    {6, 6, CORBA_Object_create_request__arginfo, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    CORBA_OBJECT_NIL, "create_request", 14, 0
   },
   {
    {1, 1, CORBA_Object_get_policy__arginfo, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_Policy, "get_policy", 10, 0
   },
   {
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_DomainManagersList, "get_domain_managers", 19, 0
   },
   {
    {2, 2, CORBA_Object_set_policy_overrides__arginfo, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_Object, "set_policy_overrides", 20, 0
   },
   /* ORBit-small bits */
   {
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    {0, 0, NULL, FALSE},
    TC_CORBA_string, "ORBit_get_type_id", 17, 0
   },
   {
    {1, 1, CORBA_Object_ORBit_get_iinterface__arginfo, FALSE},
    {0, 0, NULL, FALSE},
    {1, 1, CORBA_Object_ORBit_get_iinterface__exceptinfo, FALSE},
    TC_ORBit_IInterface, "ORBit_get_iinterface", 20, 0
   }
};

/* 
 * Interface Definition.
 */

ORBit_IInterface
CORBA_Object__iinterface = {
  TC_CORBA_Object, 
  {12, 12, CORBA_Object__imethods, FALSE},
  {0, 0, NULL, FALSE}
};

void
_ORBit_object_init (void)
{
	object_lock = link_mutex_new();
}
