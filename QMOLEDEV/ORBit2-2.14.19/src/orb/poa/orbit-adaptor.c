#include <config.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include <orbit/orbit.h>
#include <orbit/poa/orbit-adaptor.h>

#include "orbit-poa.h"
#include "../orb-core/orbit-debug.h"
#include "../GIOP/giop-debug.h"

void
ORBit_ObjectAdaptor_set_thread_hintv (ORBit_ObjectAdaptor adaptor,
				      ORBitThreadHint     thread_hint,
				      va_list             args)
{
	g_return_if_fail (adaptor != NULL);
	g_return_if_fail (thread_hint >= ORBIT_THREAD_HINT_NONE &&
			  thread_hint <= ORBIT_THREAD_HINT_ON_CONTEXT);

	adaptor->thread_hint = thread_hint;
	switch (thread_hint) {
	case ORBIT_THREAD_HINT_PER_OBJECT:
	case ORBIT_THREAD_HINT_PER_REQUEST:
	case ORBIT_THREAD_HINT_PER_POA:
	case ORBIT_THREAD_HINT_PER_CONNECTION:
	case ORBIT_THREAD_HINT_ON_CONTEXT:
		if (link_thread_safe ())
			link_set_io_thread (TRUE);
		break;
	case ORBIT_THREAD_HINT_NONE:
	case ORBIT_THREAD_HINT_ONEWAY_AT_IDLE:
	case ORBIT_THREAD_HINT_ALL_AT_IDLE:
		break;
	}

	if (thread_hint == ORBIT_THREAD_HINT_ON_CONTEXT) {
		adaptor->context = va_arg (args, GMainContext*);
		if (adaptor->context)
			g_main_context_ref (adaptor->context);
		else
			g_warning ("POA thread policy of ORBIT_THREAD_HINT_ON_CONTEXT chosen, "
				   "but NULL context supplied.  will dispatch to default context.");
	}
}

void
ORBit_ObjectAdaptor_set_thread_hint (ORBit_ObjectAdaptor adaptor,
				     ORBitThreadHint     thread_hint,
				     ...)
{
	va_list args;

	va_start (args, thread_hint);
	ORBit_ObjectAdaptor_set_thread_hintv (adaptor, thread_hint, args);
	va_end (args);
}


ORBitThreadHint
ORBit_ObjectAdaptor_get_thread_hint (ORBit_ObjectAdaptor adaptor)
{
	g_return_val_if_fail (adaptor != NULL, ORBIT_THREAD_HINT_NONE);

	return adaptor->thread_hint;
}


/*
 * Tie a object adaptor object to the current thread. This means that
 * incoming requests to the servant will be handled by this thread only.
 * The object must be created by a POA with the ORBIT_THREAD_HINT_PER_OBJECT
 * hint set, and if the thread exits the behaviour will fallback to the
 * default for HINT_PER_OBJECT, i.e. a new thread will be spawned to handle
 * all calls for the object. You need to call this function before any
 * requests for the object arrive, since otherwise a thread for the
 * object will be created.
 */
void
ORBit_ObjectAdaptor_object_bind_to_current_thread (CORBA_Object obj)
{
	ORBit_OAObject adaptor_obj;
	ORBit_POAObject pobj;
	ORBit_ObjectAdaptor adaptor;
	
	g_return_if_fail (obj != NULL);

	adaptor_obj = obj->adaptor_obj;
	
	g_return_if_fail (adaptor_obj != NULL);
	g_return_if_fail (adaptor_obj->interface != NULL);
	g_return_if_fail (adaptor_obj->interface->adaptor_type & ORBIT_ADAPTOR_POA);

	pobj = (ORBit_POAObject) adaptor_obj;
	
	adaptor = (ORBit_ObjectAdaptor) pobj->poa;

	if (adaptor->thread_hint != ORBIT_THREAD_HINT_PER_OBJECT) {
		g_warning ("POA thread policy must be ORBIT_THREAD_HINT_PER_OBJECT for thread binding to work");
	}

	/* The GIOPThread will allocated in a user thread will be freed by
	 * the TLS destructor, which will also release the key to the pobj.
	 * If the pobj is destroyed before this ORBit_POAObject_release_cb
	 * will release the key.
	 */
	giop_thread_key_add (giop_thread_self (), pobj);
}


CORBA_long
ORBit_adaptor_setup (ORBit_ObjectAdaptor adaptor,
		     CORBA_ORB           orb)
{
	int adaptor_id;
	CORBA_long *tptr;

	LINK_MUTEX_LOCK (ORBit_RootObject_lifecycle_lock);
	{
		adaptor_id = orb->adaptors->len;

		g_ptr_array_set_size (orb->adaptors, adaptor_id + 1);
		g_ptr_array_index    (orb->adaptors, adaptor_id) = adaptor;
	}
	LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);

	adaptor->thread_hint = ORBIT_THREAD_HINT_NONE;

	adaptor->adaptor_key._length  = ORBIT_ADAPTOR_PREFIX_LEN;
	adaptor->adaptor_key._buffer  =
		CORBA_sequence_CORBA_octet_allocbuf (adaptor->adaptor_key._length);
	adaptor->adaptor_key._release = CORBA_TRUE;

	ORBit_genuid_buffer (adaptor->adaptor_key._buffer + sizeof (CORBA_long),
			     ORBIT_ADAPTOR_KEY_LEN, ORBIT_GENUID_COOKIE);

	tptr = (CORBA_long *) adaptor->adaptor_key._buffer;
	*tptr = adaptor_id;

	return *tptr;
}

static ORBit_ObjectAdaptor
ORBit_adaptor_find (CORBA_ORB orb, ORBit_ObjectKey *objkey)
{
	gint32 adaptorId;
	ORBit_ObjectAdaptor adaptor;

	if (!objkey)
		return NULL;

	if (objkey->_length < ORBIT_ADAPTOR_PREFIX_LEN)
		return NULL;

	memcpy (&adaptorId, objkey->_buffer, sizeof (gint32));

	if (adaptorId < 0 || adaptorId >= orb->adaptors->len)
		return NULL;

	LINK_MUTEX_LOCK (ORBit_RootObject_lifecycle_lock);
	{
		if ((adaptor = g_ptr_array_index (orb->adaptors, adaptorId))) {
			if (memcmp (objkey->_buffer,
				    adaptor->adaptor_key._buffer,
				    ORBIT_ADAPTOR_PREFIX_LEN))
				adaptor = NULL;
			else
				ORBit_RootObject_duplicate_T (adaptor);
		}
	}
	LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);

	return adaptor;
}

static CORBA_Object
ORBit_forw_bind_find (CORBA_ORB orb, ORBit_ObjectKey *objkey)
{
	CORBA_Object object = NULL;
	gchar *objectId = NULL; 

	if (!objkey)
		return NULL;

	objectId = (gchar *) g_malloc0 (sizeof(gchar)*objkey->_length+1);

	memcpy (objectId, objkey->_buffer, objkey->_length);

	LINK_MUTEX_LOCK (ORBit_RootObject_lifecycle_lock);
	{
		const gchar *typeid;
		object = g_hash_table_lookup (orb->forw_binds, objectId);
		
		if (object) {
			typeid = g_quark_to_string (object->type_qid);
			if (!typeid) {
				gboolean removed;
				removed = g_hash_table_remove (orb->forw_binds,
							       objectId);
				g_assert (removed == TRUE);
				object = NULL;
			}
		}
	}
	LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);
	
	g_free (objectId);
	
	return object;
}
 
void
ORBit_handle_locate_request (CORBA_ORB orb, GIOPRecvBuffer *recv_buffer)
{
	ORBit_ObjectKey *objkey;
	ORBit_ObjectAdaptor adaptor;

	objkey = giop_recv_buffer_get_objkey (recv_buffer);

	/*
	 * FIXME: this only checks that the adaptor is available,
	 * needs more work in the POA.
	 */
	if (objkey && (adaptor = ORBit_adaptor_find (orb, objkey))) {
		GIOPSendBuffer *send_buffer = 
			giop_send_buffer_use_locate_reply
				(recv_buffer->giop_version,
				 giop_recv_buffer_get_request_id (recv_buffer),
				 GIOP_OBJECT_HERE);
 
		giop_send_buffer_write (send_buffer, recv_buffer->connection, FALSE);
		giop_send_buffer_unuse (send_buffer);

		ORBit_RootObject_release (adaptor);
	} else { /* can't find adaptor */
		GIOPSendBuffer *send_buffer;

		send_buffer = giop_send_buffer_use_locate_reply
			(recv_buffer->giop_version,
			 giop_recv_buffer_get_request_id (recv_buffer),
			 GIOP_UNKNOWN_OBJECT);

		giop_send_buffer_write (send_buffer, recv_buffer->connection, FALSE);
		giop_send_buffer_unuse (send_buffer);
	}
  
	giop_recv_buffer_unuse (recv_buffer);
}

void
ORBit_handle_request (CORBA_ORB orb, GIOPRecvBuffer *recv_buffer)
{
	ORBit_ObjectKey *objkey;
	ORBit_ObjectAdaptor adaptor;

	objkey = giop_recv_buffer_get_objkey (recv_buffer);
	adaptor = ORBit_adaptor_find (orb, objkey);

	if (!adaptor || !objkey) {
		CORBA_Object forw_obj = ORBit_forw_bind_find (orb, objkey);
		if (forw_obj) {			
			GIOPSendBuffer *send_buffer = 
				giop_send_buffer_use_reply
				(recv_buffer->giop_version,
				 giop_recv_buffer_get_request_id (recv_buffer),
				 GIOP_LOCATION_FORWARD);
			
			ORBit_marshal_object(send_buffer, forw_obj);

			giop_send_buffer_write (send_buffer, recv_buffer->connection, FALSE);
			giop_send_buffer_unuse (send_buffer);

			giop_recv_buffer_unuse (recv_buffer);
		}
		else {
			
			CORBA_Environment env;
			
			CORBA_exception_init (&env);

			tprintf ("Error: failed to find adaptor or objkey for "
				 "object while invoking method '%s'",
				 giop_recv_buffer_get_opname (recv_buffer));
		
			CORBA_exception_set_system (
				&env, ex_CORBA_OBJECT_NOT_EXIST, 
				CORBA_COMPLETED_NO);
			ORBit_recv_buffer_return_sys_exception (recv_buffer, &env);
			
			CORBA_exception_free (&env);
		}
		
	} else {
		dprintf (MESSAGES, "p %d: handle request '%s'\n",
			 getpid (),
			 giop_recv_buffer_get_opname (recv_buffer));

		adaptor->handle_request (adaptor, recv_buffer, objkey);
	}

	ORBit_RootObject_release (adaptor);
}

void
ORBit_small_handle_request (ORBit_OAObject     adaptor_obj,
			    CORBA_Identifier   opname,
			    gpointer           ret,
			    gpointer          *args,
			    CORBA_Context      ctx,
			    GIOPRecvBuffer    *recv_buffer,
	 		    CORBA_Environment *ev)
{
	adaptor_obj->interface->handle_request (adaptor_obj, opname, ret, 
						args, ctx, recv_buffer, ev);
}

gboolean
ORBit_OAObject_is_active (ORBit_OAObject adaptor_obj) 
{
	return adaptor_obj->interface->is_active (adaptor_obj);

}

ORBit_ObjectKey*
ORBit_OAObject_object_to_objkey (ORBit_OAObject adaptor_obj)
{
	return adaptor_obj->interface->object_to_objkey (adaptor_obj);
}

void
ORBit_OAObject_invoke (ORBit_OAObject     adaptor_obj,
		       gpointer           ret,
		       gpointer          *args,
		       CORBA_Context      ctx,
		       gpointer           data,
		       CORBA_Environment *ev)
{
	adaptor_obj->interface->invoke(adaptor_obj, ret, args, ctx, data, ev);
}
