/*
 * Warning - before reading this file, and while hacking
 * it, it is advisable to examine:
 *
 *    docs/internals/interface-indirection.gnumeric
 *
 * FIXME: We need some global I/F -> m_data lookup action
 * FIXME: We need to map interface inheritance.
 * FIXME: Add #ifdef ORBIT_PURIFY support.
 *
 * FIXME: 'Obvious' optimizations
 *  Here:
 *    * 2 demarshalers - 1 straight, 1 endianness switching.
 *    * do more alloca's for basic things
 *    * more IDL compiler help for allocation and indirection
 *    decisions - these are closely tied
 *    * No alias types in the structures ... nice :-)
 *  Elsewhere:
 *    * store object profiles in GIOP format for fast marshaling
 *    * make locking more chunky _T everything.
 */

#include <config.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <stdio.h>
#include <string.h>

#include <gmodule.h>
#include <glib.h>

#include <orbit/orbit.h>

#include "../poa/orbit-poa-export.h"
#include "../GIOP/giop-debug.h"
#include "orb-core-private.h"
#include "orbit-debug.h"

int ORBit_small_flags = 0;

gpointer
ORBit_small_alloc (CORBA_TypeCode tc)
{
	return ORBit_alloc_by_tc (tc);
}

gpointer
ORBit_small_allocbuf (CORBA_TypeCode tc, CORBA_unsigned_long length)
{
	/* see above */
	return ORBit_alloc_tcval (tc->subtypes [0], length);
}

void
ORBit_small_freekids (CORBA_TypeCode tc, gpointer p, gpointer d)
{
	/* see above */
	ORBit_freekids_via_TypeCode (tc, p);
}

static void
ORBit_handle_exception_array (GIOPRecvBuffer     *rb,
			      CORBA_Environment  *ev,
			      const ORBit_ITypes *types,
			      CORBA_ORB           orb)
{
	CORBA_SystemException *new;
	CORBA_unsigned_long len, completion_status, reply_status;
	CORBA_char *my_repoid;

	g_return_if_fail (rb->msg.header.message_type == GIOP_REPLY);

	CORBA_exception_free (ev);

	rb->cur = ALIGN_ADDRESS (rb->cur, sizeof (len));
	if ((rb->cur + 4) > rb->end)
		goto errout;

	len = *(CORBA_unsigned_long *)rb->cur;
	rb->cur += 4;
	if (giop_msg_conversion_needed (rb))
		len = GUINT32_SWAP_LE_BE (len);

	if (len) {
		my_repoid = (char *) rb->cur;
		rb->cur += len;
	} else
		my_repoid = NULL;

	reply_status = giop_recv_buffer_reply_status (rb);

	dprintf (MESSAGES, "Received exception %d: '%s'\n",
		 reply_status, my_repoid ? my_repoid : "<Null>");

	if (reply_status == CORBA_SYSTEM_EXCEPTION) {
		CORBA_unsigned_long minor;

		dprintf (MESSAGES, "system exception\n");
		
		ev->_major = CORBA_SYSTEM_EXCEPTION;

		rb->cur = ALIGN_ADDRESS (rb->cur, sizeof (minor));
		if ((rb->cur + sizeof (minor)) > rb->end)
			goto errout;
		minor = *(CORBA_unsigned_long *) rb->cur;
		rb->cur += 4;
		if (giop_msg_conversion_needed (rb))
			minor = GUINT32_SWAP_LE_BE (minor);

		rb->cur = ALIGN_ADDRESS (rb->cur, sizeof (completion_status));
		if ((rb->cur + sizeof (completion_status)) > rb->end)
			goto errout;
		completion_status = *(CORBA_unsigned_long *) rb->cur;
		rb->cur += 4;
		if (giop_msg_conversion_needed (rb))
			completion_status = GUINT32_SWAP_LE_BE (completion_status);

		new = CORBA_SystemException__alloc ();
		new->minor = minor;
		new->completed = completion_status;
			
		/* FIXME: check what should the repo ID be? */
		CORBA_exception_set (ev, CORBA_SYSTEM_EXCEPTION,
				     my_repoid, new);
		/* FIXME: might be fixed one day by cunning detection
		   in CORBA_exception_set ... */
		if (!ev->_any._type)
			ev->_any._type = ORBit_RootObject_duplicate (
				TC_CORBA_SystemException);
		
		dprintf (MESSAGES, "system exception de-marshaled\n");
		return;

	} else if (reply_status == CORBA_USER_EXCEPTION) {
		int i;

		dprintf (MESSAGES, "user exception\n");

		for (i = 0; my_repoid && i < types->_length; i++) {
			if (!strcmp (types->_buffer[i]->repo_id, my_repoid))
				break;
		}

		if (!types || types->_length == 0 || i >= types->_length) {
			/* weirdness; they raised an exception that we don't
			   know about */
			CORBA_exception_set_system (
				ev, ex_CORBA_MARSHAL,
				CORBA_COMPLETED_MAYBE);
		} else {
			gpointer data;
			
			dprintf (MESSAGES, "de-marshal user exception\n");

			data = ORBit_demarshal_arg (
				rb, types->_buffer [i], orb);

			/* FIXME: might be fixed one day by cunning detection
			   in CORBA_exception_set ... */
			CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
					     types->_buffer [i]->repo_id, data);
			if (!ev->_any._type)
				ev->_any._type = ORBit_RootObject_duplicate (
					types->_buffer [i]);
		}
	}
  
	if (ev->_major != CORBA_NO_EXCEPTION)
		return;

 errout:
	/* ignore LOCATION_FORWARD here, that gets handled in the stub */
	CORBA_exception_set_system (ev, ex_CORBA_MARSHAL,
				    CORBA_COMPLETED_MAYBE);
}

static gboolean
ORBit_small_send_user_exception (GIOPSendBuffer     *send_buffer,
				 CORBA_Environment  *ev,
				 const ORBit_ITypes *types)
{
	int i;

	for (i = 0; i < types->_length; i++) {
		if(!strcmp (types->_buffer[i]->repo_id, ev->_id))
			break;
	}

	if (i >= types->_length) {
		g_warning ("Some clown returned undeclared "
			   "exception '%s' ", ev->_id);

		CORBA_exception_free (ev);
		CORBA_exception_set_system (
			ev, ex_CORBA_UNKNOWN,
			CORBA_COMPLETED_MAYBE);

		giop_send_buffer_unuse (send_buffer);

		return FALSE;
	} else {
		giop_send_buffer_append_string (send_buffer, ev->_id);

		dprintf (MESSAGES, "Returning exception of type '%s'\n", ev->_id);

		ORBit_marshal_arg (send_buffer, ev->_any._value,
				   types->_buffer[i]);

		return TRUE;
	}
}

static void
ORBit_small_marshal_context (GIOPSendBuffer *send_buffer,
			     ORBit_IMethod  *m_data,
			     CORBA_Context   ctx)
{
	int i;
	/* Horrible inefficiency to get round the 'lete
	   efficiency of the current impl */
	ORBit_ContextMarshalItem *mlist;

	mlist = g_alloca (sizeof (ORBit_ContextMarshalItem) *
			m_data->contexts._length);

	tprintf (" context { ");

	for (i = 0; i < m_data->contexts._length; i++) {
		char *val;

		mlist [i].str = m_data->contexts._buffer [i];

		val = g_hash_table_lookup (ctx->mappings, mlist [i].str);
		tprintf ("( %s: '%s' )%s", mlist [i].str, val,
			 i < m_data->contexts._length - 1 ? ", ": "");

		mlist [i].len = strlen (mlist [i].str) + 1;
	}

	tprintf (" }");

	/* Assumption, this doesn't whack mlist pointers into
	   the send_buffer: verified */
	ORBit_Context_marshal (
		ctx, mlist, m_data->contexts._length, send_buffer);
}

#define BASE_TYPES \
	     CORBA_tk_short: \
	case CORBA_tk_long: \
	case CORBA_tk_enum: \
	case CORBA_tk_ushort: \
	case CORBA_tk_ulong: \
	case CORBA_tk_float: \
	case CORBA_tk_double: \
	case CORBA_tk_boolean: \
	case CORBA_tk_char: \
	case CORBA_tk_octet: \
	case CORBA_tk_longlong: \
	case CORBA_tk_ulonglong: \
	case CORBA_tk_longdouble: \
	case CORBA_tk_wchar

#define STRUCT_UNION_TYPES \
	     CORBA_tk_struct: \
	case CORBA_tk_union: \
	case CORBA_tk_except

#define OBJ_STRING_TYPES \
	     CORBA_tk_objref: \
	case CORBA_tk_TypeCode: \
	case CORBA_tk_string: \
	case CORBA_tk_wstring

#define SEQ_ANY_TYPES \
	     CORBA_tk_sequence: \
	case CORBA_tk_any

typedef struct {
	CORBA_unsigned_long len;
	char                opname[1];
} OpData;

#define do_marshal_value(a,b,c)     \
	ORBit_marshal_value   ((a),(gconstpointer *)(b),(c))
#define do_demarshal_value(a,b,c,e) \
	if (ORBit_demarshal_value ((c),(b),(a),(e))) \
		goto demarshal_exception

static gboolean
orbit_small_marshal (CORBA_Object           obj,
		     GIOPConnection        *cnx,
		     GIOPMessageQueueEntry *mqe,
		     CORBA_unsigned_long    request_id,
		     ORBit_IMethod         *m_data,
		     gpointer              *args,
		     CORBA_Context          ctx)
{
	GIOPSendBuffer          *send_buffer;
	struct iovec             op_vec;
	CORBA_TypeCode           tc;
	int                      i;

	tprintf_header (obj, m_data);

	{
		int          align;
		int          len = sizeof (CORBA_unsigned_long) + m_data->name_len + 1;
		guchar      *header = g_alloca (len + sizeof (CORBA_unsigned_long));

		*(CORBA_unsigned_long *) header = m_data->name_len + 1;
		memcpy (header + sizeof (CORBA_unsigned_long),
			m_data->name, m_data->name_len + 1);
	       
		align = len + (sizeof (CORBA_unsigned_long) - 1);
		align &= ~(sizeof (CORBA_unsigned_long) - 1);
		memset (header + len, 0, align - len);

		dprintf (MESSAGES, "Align = %d\n", align);
		op_vec.iov_len  = align;
		op_vec.iov_base = header;
	}

	send_buffer = giop_send_buffer_use_request (
		cnx->giop_version, request_id, 
		(m_data->flags & ORBit_I_METHOD_1_WAY) == 0,
		obj->object_key, &op_vec, NULL);

	if (!send_buffer)
		return FALSE;

	dprintf (MESSAGES, "Marshal: id 0x%x\n", request_id);

	for (i = 0; i < m_data->arguments._length; i++) {

		ORBit_IArg *a = &m_data->arguments._buffer [i];
		gpointer    p;

		if (!(a->flags & (ORBit_I_ARG_IN |
				  ORBit_I_ARG_INOUT)))
			continue;
		tc = a->tc;

		while (tc->kind == CORBA_tk_alias)
			tc = tc->subtypes [0];

		dump_arg (a, tc);

		p = args [i];
		tprintf_trace_value (&p, tc);

		p = args [i];
		do_marshal_value (send_buffer, &p, tc);

		if (i < m_data->arguments._length - 1)
			tprintf (", ");
	}

	tprintf (")");

	if (m_data->contexts._length > 0)
		ORBit_small_marshal_context (send_buffer, m_data, ctx);

	do_giop_dump_send (send_buffer);

	if (giop_send_buffer_write (send_buffer, cnx, FALSE)) {
		g_warning ("Failed to send buffer");
		giop_recv_list_destroy_queue_entry (mqe);
		return FALSE;
	}

	giop_send_buffer_unuse (send_buffer);

	return TRUE;
}

typedef enum {
	MARSHAL_SYS_EXCEPTION_INCOMPLETE,
	MARSHAL_SYS_EXCEPTION_COMPLETE,
	MARSHAL_EXCEPTION_COMPLETE,
	MARSHAL_RETRY,
	MARSHAL_CLEAN
} DeMarshalRetType;

static DeMarshalRetType
orbit_small_demarshal (CORBA_Object           obj,
		       GIOPConnection       **cnx,
		       GIOPRecvBuffer        *recv_buffer,
		       CORBA_Environment     *ev,
		       gpointer               ret,
		       ORBit_IMethod         *m_data,
		       gpointer              *args)
{
	gpointer        data, p;
	CORBA_TypeCode  tc;
	CORBA_ORB       orb = obj->orb;

	if (!recv_buffer) {
		dprintf (MESSAGES, "No recv buffer ...\n");
		return MARSHAL_SYS_EXCEPTION_INCOMPLETE;
	}

	if (giop_recv_buffer_reply_status (recv_buffer) != GIOP_NO_EXCEPTION)
 		goto msg_exception;

	if ((tc = m_data->ret) && tc->kind != CORBA_tk_void) {
		tprintf (" =>: ");

		g_assert (ret != NULL);

		while (tc->kind == CORBA_tk_alias)
			tc = tc->subtypes [0];

		switch (tc->kind) {
		case BASE_TYPES:
		case OBJ_STRING_TYPES:
			p = ret;
			do_demarshal_value (recv_buffer, &ret, tc, orb);
			tprintf_trace_value (&p, tc);
			break;

		case STRUCT_UNION_TYPES:
			if (m_data->flags & ORBit_I_COMMON_FIXED_SIZE) {
				p = ret;
				do_demarshal_value (recv_buffer, &ret, tc, orb);
				tprintf_trace_value (&p, tc);
				break;
			} /* drop through */

		case SEQ_ANY_TYPES:
		case CORBA_tk_array:
		default:
			data = ORBit_demarshal_arg (recv_buffer, tc, orb);
			if (!data)
				return MARSHAL_SYS_EXCEPTION_COMPLETE;

			p = data;
			tprintf_trace_value (&p, tc);

			*((gpointer *)ret) = data;
			break;
		}
	}

	{
		int i;
		int trace_have_out = 0;

		for (i = 0; i < m_data->arguments._length; i++) {
			const ORBit_IArg *a;
			gpointer          arg;

			a = &m_data->arguments._buffer [i];

			if (!(a->flags & (ORBit_I_ARG_OUT |
					  ORBit_I_ARG_INOUT)))
				continue;

			tc = a->tc;

			while (tc->kind == CORBA_tk_alias)
				tc = tc->subtypes [0];

			dump_arg (a, tc);

			if (a->flags & ORBit_I_ARG_OUT)
				/* this may read (&discard) uninitialized memory,
				 * see 'foo' below. This is for simplicity. */
				arg = *(gpointer *)args [i];
			else
				arg = args [i];

			switch (tc->kind) {
			case OBJ_STRING_TYPES: {
				if (a->flags & ORBit_I_ARG_INOUT) {
					if (tc->kind == CORBA_tk_TypeCode ||
					    tc->kind == CORBA_tk_objref)
						CORBA_Object_release (*(gpointer *)arg, ev);
					else if (tc->kind == CORBA_tk_string ||
						 tc->kind == CORBA_tk_wstring)
						ORBit_free (*(gpointer *) arg);
				}
				/* drop through */
			case BASE_TYPES:
				if (!trace_have_out++)
					tprintf (" out: (");
				p = arg;
				do_demarshal_value (recv_buffer, &arg, tc, orb);
				tprintf_trace_value (&p, tc);
				break;
			}

			case STRUCT_UNION_TYPES:
			case SEQ_ANY_TYPES:
			case CORBA_tk_array:
			default:
				p = arg;
				if (a->flags & ORBit_I_COMMON_FIXED_SIZE) {
					do_demarshal_value (recv_buffer, &arg, tc, orb);
				} else if (a->flags & ORBit_I_ARG_INOUT) {
					ORBit_freekids_via_TypeCode (tc, arg);
					do_demarshal_value (recv_buffer, &arg, tc, orb);
				} else /* 'foo' - don't use the bogus 'arg' contents */
					*(gpointer *)args [i] = p = ORBit_demarshal_arg (
						recv_buffer, tc, obj->orb);

				if (!trace_have_out++)
					tprintf (" out: (");
				tprintf_trace_value (&p, tc);
				break;
			}
			if (trace_have_out &&
			    i < m_data->arguments._length - 1)
				tprintf (", ");
		}
		if (trace_have_out)
			tprintf (" )");
	}

	return MARSHAL_CLEAN;

 demarshal_exception:
	/* FIXME: may well leak */
	CORBA_exception_set_system(ev, ex_CORBA_MARSHAL,
				   CORBA_COMPLETED_MAYBE);
	return MARSHAL_EXCEPTION_COMPLETE;

 msg_exception:
	if (giop_recv_buffer_reply_status (recv_buffer) ==
	    GIOP_LOCATION_FORWARD) {
		
		*cnx = ORBit_handle_location_forward (recv_buffer, obj);
		tprintf (" Exception: forward (%p)", *cnx);
		if (!*cnx) {
			CORBA_exception_set_system(ev, ex_CORBA_MARSHAL,
						   CORBA_COMPLETED_MAYBE);
			return MARSHAL_SYS_EXCEPTION_INCOMPLETE;
		}
		return MARSHAL_RETRY;
	} else {
		ORBit_handle_exception_array (
			recv_buffer, ev, &m_data->exceptions, obj->orb);

#ifdef G_ENABLE_DEBUG
		if (_orbit_debug_flags & ORBIT_DEBUG_TRACES) {
			if (ev->_major == CORBA_SYSTEM_EXCEPTION)
				tprintf (" System Exception: '%s' ", ev->_id);
			else {
				tprintf (" User Exception: '%s' ", ev->_id);
				ORBit_trace_any (&ev->_any);
			}
		}
#endif /* G_ENABLE_DEBUG */

		return MARSHAL_EXCEPTION_COMPLETE;
	}
}

void
ORBit_small_invoke_stub_n (CORBA_Object        object,
			   ORBit_IMethods     *methods,
			   glong               index,
			   gpointer            ret,
			   gpointer           *args,
			   CORBA_Context       ctx,
			   CORBA_Environment  *ev)
{
	if (index < 0 || index > methods->_length) {
		dprintf (MESSAGES, "Cannot invoke OOB method (%ld,%lu)\n",
			 index, (gulong)methods->_length);
		CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT,
					    CORBA_COMPLETED_NO);

	} else
		ORBit_small_invoke_stub (object, &methods->_buffer[index], ret, args, ctx, ev);
}

void
ORBit_small_invoke_stub (CORBA_Object       obj,
			 ORBit_IMethod     *m_data,
			 gpointer           ret,
			 gpointer          *args,
			 CORBA_Context      ctx,
			 CORBA_Environment *ev)
{
	CORBA_unsigned_long     request_id;
	CORBA_completion_status completion_status;
	GIOPConnection         *cnx = NULL;
	GIOPMessageQueueEntry   mqe;
	ORBit_OAObject          adaptor_obj;
	GIOPRecvBuffer         *recv_buffer = NULL;
	CORBA_Object            xt_proxy = CORBA_OBJECT_NIL;
	ORBitPolicy            *invoke_policy = CORBA_OBJECT_NIL;
	gboolean                timeout = FALSE;

	CORBA_exception_init (ev);

	if (!obj) {
		dprintf (MESSAGES, "Cannot invoke method on null object\n");
		CORBA_exception_set_system (ev, ex_CORBA_INV_OBJREF,
					    CORBA_COMPLETED_NO);
		goto clean_out;
	}

	if ((invoke_policy = ORBit_object_get_policy (obj)))
		ORBit_policy_push (invoke_policy);

	adaptor_obj = obj->adaptor_obj;

	if (adaptor_obj) {
		/* FIXME: unchecked cast */
		if (ORBit_poa_allow_cross_thread_call ((ORBit_POAObject) adaptor_obj,
						       m_data->flags)) {
			tprintf_header (obj, m_data);
			tprintf ("[in-proc]");
			ORBit_small_handle_request (adaptor_obj, m_data->name, ret,
						    args, ctx, NULL, ev);
			goto clean_out;
		} else {
			tprintf ("[in-proc-XT]");
			/*
			 * FIXME: this is _really_ slow, can easily be optimised
			 * by shoving the GIOP data straight on the incoming
			 * queue
			 */
			xt_proxy = ORBit_objref_get_proxy (obj);
			obj = xt_proxy;
		}
	} else
		giop_thread_new_check (NULL);

	cnx = ORBit_object_get_connection (obj);

	if (!cnx) {
		dprintf (MESSAGES, "Null connection on object '%p'\n", obj);
		completion_status = CORBA_COMPLETED_NO;
		goto system_exception;
	}

 retry_request:
	request_id = GPOINTER_TO_UINT (&obj);
	completion_status = CORBA_COMPLETED_NO;

	giop_recv_list_setup_queue_entry (&mqe, cnx, GIOP_REPLY, request_id);

	if (!orbit_small_marshal (obj, cnx, &mqe, request_id,
				  m_data, args, ctx))
		goto system_exception;

	completion_status = CORBA_COMPLETED_MAYBE;

	if (m_data->flags & ORBit_I_METHOD_1_WAY) {
		tprintf ("[ one way ]");
		giop_recv_list_destroy_queue_entry (&mqe);
		goto clean_out;
	}

	recv_buffer = giop_recv_buffer_get (&mqe, &timeout);
	if (timeout)
		goto timeout_exception;

	switch (orbit_small_demarshal (obj, &cnx, recv_buffer, ev,
				       ret, m_data, args))
	{
	case MARSHAL_SYS_EXCEPTION_COMPLETE:
		completion_status = CORBA_COMPLETED_YES;
		dprintf (MESSAGES, "Sys exception completed on id 0x%x\n\n", request_id);
		goto system_exception;

	case MARSHAL_SYS_EXCEPTION_INCOMPLETE:
		dprintf (MESSAGES, "Sys exception incomplete on id 0x%x\n\n", request_id);
		goto system_exception;

	case MARSHAL_EXCEPTION_COMPLETE:
		dprintf (MESSAGES, "Clean demarshal of exception on id 0x%x\n\n", request_id);
		break;

	case MARSHAL_RETRY:
		dprintf (MESSAGES, "Retry demarshal on id 0x%x\n\n", request_id);
		goto retry_request;

	case MARSHAL_CLEAN:
		dprintf (MESSAGES, "Clean demarshal on id 0x%x\n\n", request_id);
		break;
	};

 clean_out:
	ORBit_RootObject_release (xt_proxy);
	giop_recv_buffer_unuse (recv_buffer);

	tprintf_end_method ();
	if (cnx)
		giop_connection_unref (cnx);
	if (invoke_policy) {
		ORBit_policy_pop ();
		ORBit_policy_unref (invoke_policy);
	}
	return;

 system_exception:
	tprintf ("[System exception comm failure] )");
	CORBA_exception_set_system (ev, ex_CORBA_COMM_FAILURE,
				    completion_status);
	goto clean_out;

 timeout_exception:
	tprintf ("[System exception timeout] )");
	CORBA_exception_set_system (ev, ex_CORBA_TIMEOUT,
				    CORBA_COMPLETED_NO);
	goto clean_out;
}

void
ORBit_small_invoke_adaptor (ORBit_OAObject     adaptor_obj,
			    GIOPRecvBuffer    *recv_buffer,
			    ORBit_IMethod     *m_data,
			    gpointer           data,
			    CORBA_Environment *ev)
{
	struct CORBA_Context_type  ctx;
	gpointer                  *args = NULL;
	gpointer                  *scratch = NULL;
	gpointer                   pretval = NULL;
	gpointer                   retval = NULL;
	GIOPSendBuffer            *send_buffer;
	CORBA_ORB                  orb;
	CORBA_TypeCode             tc;
	gboolean                   has_context;
	int                        i;

	orb = ((ORBit_POAObject)adaptor_obj)->poa->orb;

	has_context = (m_data->contexts._length > 0);

	tprintf_header (adaptor_obj->objref, m_data);

	if ((tc = m_data->ret) && tc->kind != CORBA_tk_void) {
		
		while (tc->kind == CORBA_tk_alias)
			tc = tc->subtypes [0];

		switch (tc->kind) {
		case BASE_TYPES:
		case OBJ_STRING_TYPES:
			retval = g_alloca (ORBit_gather_alloc_info (tc));
			break;
		case STRUCT_UNION_TYPES:
			if (m_data->flags & ORBit_I_COMMON_FIXED_SIZE) {
				retval = ORBit_alloc_by_tc (tc);
				break;
			} /* drop through */
		default:
			retval = &pretval;
			pretval = NULL;
			break;
		}
	}

	if (m_data->arguments._length > 0) {
		int len = m_data->arguments._length *
			sizeof (gpointer);

		args = g_alloca (len);
		memset (args, 0, len);
		scratch = g_alloca (len);
		memset (scratch, 0, len);
	}		

	for (i = 0; i < m_data->arguments._length; i++) {
		ORBit_IArg *a = &m_data->arguments._buffer [i];
		
		if (a->flags & ORBit_I_ARG_IN ||
		    a->flags & ORBit_I_ARG_INOUT) {
			gpointer p;

			tc = a->tc;

			while (tc->kind == CORBA_tk_alias)
				tc = tc->subtypes [0];

			switch (tc->kind) {
			case BASE_TYPES:
			case OBJ_STRING_TYPES:
				p = args [i] = g_alloca (ORBit_gather_alloc_info (tc));
				do_demarshal_value (recv_buffer, &p, tc, orb);

				p = args [i];
				tprintf_trace_value (&p, tc);
				break;
			case STRUCT_UNION_TYPES:
			case CORBA_tk_array:
				if (a->flags & ORBit_I_COMMON_FIXED_SIZE) {
					p = args [i] = g_alloca (ORBit_gather_alloc_info (tc));
					do_demarshal_value (recv_buffer, &p, tc, orb);
					p = args [i];
					tprintf_trace_value (&p, tc);
					break;
				} /* drop through */
			default:
				args [i] = ORBit_demarshal_arg (recv_buffer, a->tc, orb);
				p = args [i];
				tprintf_trace_value (&p, tc);
				break;
			}

		} else { /* Out */
			tc = a->tc;

			while (tc->kind == CORBA_tk_alias)
				tc = tc->subtypes [0];

			args [i] = &scratch [i];

			switch (tc->kind) {
			case BASE_TYPES:
			case OBJ_STRING_TYPES:
				scratch [i] = g_alloca (ORBit_gather_alloc_info (tc));
				break;
			case STRUCT_UNION_TYPES:
			case CORBA_tk_array:
				if (a->flags & ORBit_I_COMMON_FIXED_SIZE) {
					scratch [i] = ORBit_alloc_by_tc (tc);
					break;
				} /* drop through */
			default:
				scratch [i] = NULL;
				break;
			}

		}

		if (i < m_data->arguments._length - 1)
			tprintf (", ");
	}

	tprintf (")");

	if (has_context) {
		tprintf ("[FIXME:context]");
		if (ORBit_Context_demarshal (NULL, &ctx, recv_buffer))
			g_warning ("FIXME: handle context demarshaling failure");
	}

	ORBit_OAObject_invoke (adaptor_obj, retval, args, &ctx, data, ev);

	if (has_context)
		ORBit_Context_server_free (&ctx);

	if (m_data->flags & ORBit_I_METHOD_1_WAY)
		goto clean_out;
	else
		goto handle_possible_exception;
	
 demarshal_exception:
	CORBA_exception_set_system (ev, ex_CORBA_MARSHAL,
				    CORBA_COMPLETED_NO);

 handle_possible_exception:
	/* FIXME: should we be using the connection's GIOP version ? */
	send_buffer = giop_send_buffer_use_reply (
		recv_buffer->giop_version,
		giop_recv_buffer_get_request_id (recv_buffer),
		ev->_major);

	if (!send_buffer) {
		dprintf (MESSAGES, "Weird, no send_buffer");
		return;

	} else if (ev->_major == CORBA_USER_EXCEPTION) {
		if (!ORBit_small_send_user_exception (
			send_buffer, ev, &m_data->exceptions)) {
			/* Tried to marshal an unknown exception,
			   so we throw a system exception next */
			dprintf (MESSAGES, "Re-sending an exception, this time %d: '%s'",
				 ev->_major, ev->_id);
			goto handle_possible_exception;
		}
		tprintf ("User exception '%s'", ev->_id);

	} else if (ev->_major != CORBA_NO_EXCEPTION) {
		ORBit_send_system_exception (send_buffer, ev);

		tprintf ("System exception");

	} else { /* Marshal return values */
		int trace_have_out = 0;

		if ((tc = m_data->ret) && tc->kind != CORBA_tk_void) {
			gpointer p = retval;

			tprintf (" =>; ");

			while (tc->kind == CORBA_tk_alias)
				tc = tc->subtypes [0];

			switch (tc->kind) {
				
			case BASE_TYPES:
			case OBJ_STRING_TYPES:
				ORBit_marshal_arg (send_buffer, retval, m_data->ret);
				tprintf_trace_value (&p, tc);
				break;

			case STRUCT_UNION_TYPES:
				if (m_data->flags & ORBit_I_COMMON_FIXED_SIZE) {
					ORBit_marshal_arg (send_buffer, retval, m_data->ret);
					tprintf_trace_value (&p, tc);
					break;
				} /* drop through */

			case CORBA_tk_any:
			case CORBA_tk_sequence:
			case CORBA_tk_array:
			default:
				p = *(gpointer *) retval;
				ORBit_marshal_arg (send_buffer, *(gpointer *)retval, m_data->ret);
				tprintf_trace_value (&p, tc);
				break;
			}
		}

		for (i = 0; i < m_data->arguments._length; i++) {
			ORBit_IArg *a = &m_data->arguments._buffer [i];
			gpointer    p;
			
			tc = a->tc;
			while (tc->kind == CORBA_tk_alias)
				tc = tc->subtypes [0];
			
			if (a->flags & ORBit_I_ARG_INOUT) {
				if (!trace_have_out++)
					tprintf (" out: (");

				ORBit_marshal_arg (send_buffer, args [i], tc);
				p = args [i];
				tprintf_trace_value (&p, tc);
			}

			else if (a->flags & ORBit_I_ARG_OUT) {
				if (!trace_have_out++)
					tprintf (" out: (");

				ORBit_marshal_arg (send_buffer, scratch [i], tc);

				p = scratch [i];
				tprintf_trace_value (&p, tc);
			}

			if (trace_have_out &&
			    i < m_data->arguments._length - 1 &&
			    m_data->arguments._buffer [i + 1].flags &
			    (ORBit_I_ARG_OUT | ORBit_I_ARG_INOUT))
				tprintf (", ");
		}
		if (trace_have_out)
			tprintf (" )");
	}

	do_giop_dump_send (send_buffer);

	giop_send_buffer_write (send_buffer, recv_buffer->connection, FALSE);
	giop_send_buffer_unuse (send_buffer);

	if (m_data->ret && tc->kind != CORBA_tk_void) {
		switch (m_data->ret->kind) {
		case BASE_TYPES:
			break;
		case CORBA_tk_objref:
		case CORBA_tk_TypeCode:
			if (ev->_major == CORBA_NO_EXCEPTION)
				CORBA_Object_release (*(CORBA_Object *) retval, ev);
			break;
		case CORBA_tk_string:
		case CORBA_tk_wstring:
			if (ev->_major == CORBA_NO_EXCEPTION)
				ORBit_free (*(char **) retval);
			break;
		case STRUCT_UNION_TYPES:
			if (m_data->flags & ORBit_I_COMMON_FIXED_SIZE) {
				ORBit_free (retval);
				break;
			} /* drop through */
		default:
			if (ev->_major == CORBA_NO_EXCEPTION)
				ORBit_free (pretval);
			break;
		}
	}

 clean_out:
	tprintf_end_method ();

	for (i = 0; i < m_data->arguments._length; i++) {
		ORBit_IArg *a = &m_data->arguments._buffer [i];
		
		tc = a->tc;
			
		while (tc->kind == CORBA_tk_alias)
			tc = tc->subtypes [0];
			
		if (a->flags & ORBit_I_ARG_IN ||
		    a->flags & ORBit_I_ARG_INOUT) {
			
			switch (tc->kind) {
			case BASE_TYPES:
				break;
			case CORBA_tk_objref:
			case CORBA_tk_TypeCode:
				CORBA_Object_release (*(CORBA_Object *) args [i], ev);
				break;
			case CORBA_tk_string:
			case CORBA_tk_wstring:
				ORBit_free (*(char **) args [i]);
				break;
			case STRUCT_UNION_TYPES:
			case CORBA_tk_array:
				if (a->flags & ORBit_I_COMMON_FIXED_SIZE) {
					ORBit_freekids_via_TypeCode (tc, args [i]);
					break;
				}
				/* drop through */
			default:
				ORBit_free (args [i]);
				break;
			}
		} else { /* Out */
			switch (tc->kind) {
			case BASE_TYPES:
				break;
			case CORBA_tk_objref:
			case CORBA_tk_TypeCode:
				if (ev->_major == CORBA_NO_EXCEPTION)
					CORBA_Object_release (*(CORBA_Object *) scratch [i], ev);
				break;
			case CORBA_tk_string:
			case CORBA_tk_wstring:
				if (ev->_major == CORBA_NO_EXCEPTION)
					ORBit_free (*(char **) scratch [i]);
				break;
			case STRUCT_UNION_TYPES:
			case CORBA_tk_array:
				if (a->flags & ORBit_I_COMMON_FIXED_SIZE) {
					ORBit_free (scratch [i]);
					break;
				}
				/* drop through */
			default:
				if (ev->_major == CORBA_NO_EXCEPTION)
					ORBit_free (scratch [i]);
				break;
			}
		}
	}
	
	CORBA_exception_free (ev);
}

#ifdef DEBUG
gpointer
ORBit_small_getepv (CORBA_Object obj, CORBA_unsigned_long class_id)
{
	PortableServer_ServantBase *servant;
	PortableServer_ClassInfo   *class_info;
	CORBA_unsigned_long         offset;
	ORBit_POAObject             pobj;

	if (obj->adaptor_obj->interface->adaptor_type != ORBIT_ADAPTOR_POA)
		return NULL;

	pobj        = (ORBit_POAObject)obj->adaptor_obj;
	servant     = pobj->servant;
	class_info  = servant->vepv[0]->_private;
	g_assert (class_info != NULL);
	g_assert (class_id < class_info->vepvlen);
	offset     = class_info->vepvmap [class_id];

	return servant->vepv [offset];
}
#endif

struct _ORBitAsyncQueueEntry {
	GIOPMessageQueueEntry   mqe;
	CORBA_Object            obj;
	ORBitAsyncInvokeFunc    fn;
	gpointer                user_data;
	ORBit_IMethod          *m_data;
	CORBA_completion_status completion_status;
};

void
ORBit_small_demarshal_async (ORBitAsyncQueueEntry *aqe,
			     gpointer              ret,
			     gpointer             *args,
			     CORBA_Environment    *ev)
{
	g_return_if_fail (aqe->mqe.buffer != NULL);

	switch (orbit_small_demarshal (aqe->obj, &aqe->mqe.cnx, aqe->mqe.buffer, ev,
				       ret, aqe->m_data, args)) {
	case MARSHAL_SYS_EXCEPTION_COMPLETE:
		aqe->completion_status = CORBA_COMPLETED_YES;
		dprintf (MESSAGES, "Sys exception completed on id 0x%x\n\n",
			 aqe->mqe.request_id);
		goto system_exception;

	case MARSHAL_SYS_EXCEPTION_INCOMPLETE:
		dprintf (MESSAGES, "Sys exception incomplete on id 0x%x\n\n",
			 aqe->mqe.request_id);
		goto system_exception;

	case MARSHAL_EXCEPTION_COMPLETE:
		dprintf (MESSAGES, "Clean demarshal of exception on id 0x%x\n\n",
			 aqe->mqe.request_id);
		break;

	case MARSHAL_RETRY:
		g_warning ("Retry demarshal failed on id 0x%x\n\n", aqe->mqe.request_id);
		return;

	case MARSHAL_CLEAN:
		dprintf (MESSAGES, "Clean demarshal on id 0x%x\n\n",
			 aqe->mqe.request_id);
		break;
	};
	goto clean_out;

 system_exception:
	tprintf ("[System exception comm failure] )");
	CORBA_exception_set_system (ev, ex_CORBA_COMM_FAILURE,
				    aqe->completion_status);

 clean_out:
	tprintf_end_method ();
}

static void
async_recv_cb (ORBitAsyncQueueEntry *aqe)
{
	CORBA_Environment *ev, real_ev;

	ev = &real_ev;
	CORBA_exception_init (ev);

	/* So we don't get invoked again */
	aqe->mqe.async_cb = NULL;

	if (!aqe->mqe.cnx ||
	    aqe->mqe.cnx->parent.status == LINK_DISCONNECTED)
		CORBA_exception_set_system (ev, ex_CORBA_COMM_FAILURE,
					    aqe->completion_status);

	if (aqe->mqe.cnx &&
	    aqe->mqe.cnx->parent.status == LINK_TIMEOUT)
		CORBA_exception_set_system (ev, ex_CORBA_TIMEOUT,
					    aqe->completion_status);

	if (aqe->fn)
		aqe->fn (aqe->obj, aqe->m_data, aqe, aqe->user_data, ev);

	ORBit_RootObject_release (aqe->obj);
/*	ORBit_RootObject_release (aqe->m_data); */
	giop_recv_list_destroy_queue_entry (&aqe->mqe);
	g_free (aqe);
	CORBA_exception_free (ev);
}

/**
 * ORBit_small_invoke_async:
 * @obj: 
 * @m_data: 
 * @fn: 
 * @user_data: 
 * @args: 
 * @ctx: 
 * @ev:
 * 
 *     This method is used to invoke a remote (or local) method
 * asynchronously. @fn is called back on return - either with an empty
 * CORBA_Environment indicating success, or with the error.
 **/
void
ORBit_small_invoke_async (CORBA_Object         obj,
			  ORBit_IMethod       *m_data,
			  ORBitAsyncInvokeFunc fn,
			  gpointer             user_data,
			  gpointer            *args,
			  CORBA_Context        ctx,
			  CORBA_Environment   *ev)
{
	CORBA_unsigned_long     request_id;
	GIOPConnection         *cnx;
	ORBitAsyncQueueEntry   *aqe = g_new (ORBitAsyncQueueEntry, 1);

	if (obj->adaptor_obj) /* a local object, we need a remote handle */
		aqe->obj = ORBit_objref_get_proxy (obj);
	else
		aqe->obj = ORBit_RootObject_duplicate (obj);

	cnx = ORBit_object_get_connection (aqe->obj);

	if (!cnx) {
		tprintf ("Null connection on object '%p'\n", aqe->obj);
		aqe->completion_status = CORBA_COMPLETED_NO;
		goto system_exception;
	}

	request_id = GPOINTER_TO_UINT (aqe);
	aqe->completion_status = CORBA_COMPLETED_NO;

	giop_recv_list_setup_queue_entry (&aqe->mqe, cnx, GIOP_REPLY, request_id);

	if (! (m_data->flags & ORBit_I_METHOD_1_WAY))
		giop_recv_list_setup_queue_entry_async (
			&aqe->mqe, (GIOPAsyncCallback) async_recv_cb);
	else if (fn)
		g_warning ("oneway method being invoked async with a callback");

	if (!orbit_small_marshal (aqe->obj, cnx, &aqe->mqe, request_id,
				  m_data, args, ctx))
		goto system_exception;

	if (m_data->flags & ORBit_I_METHOD_1_WAY)
		giop_recv_list_destroy_queue_entry (&aqe->mqe);

	aqe->completion_status = CORBA_COMPLETED_MAYBE;
	aqe->fn = fn;
	aqe->user_data = user_data;
	/* FIXME: perenial ORBit_IMethod lifecycle issues */
	aqe->m_data = /* ORBit_RootObject_duplicate */ (m_data);

 clean_out:
	if (cnx)
		giop_connection_unref (cnx);
	tprintf_end_method ();
	return;

 system_exception:
	tprintf ("[System exception comm failure] )");
	CORBA_exception_set_system (ev, ex_CORBA_COMM_FAILURE,
				    aqe->completion_status);
	g_free (aqe);
	goto clean_out;
}

gpointer
ORBit_small_get_servant (CORBA_Object obj)
{
	ORBit_POAObject pobj;

	if (!obj || !obj->adaptor_obj || !obj->adaptor_obj->interface)
		return NULL;

	if (obj->adaptor_obj->interface->adaptor_type != ORBIT_ADAPTOR_POA) {
		g_warning ("Not a poa object !");
		return NULL;
	}

	pobj = (ORBit_POAObject)obj->adaptor_obj;

	return pobj ? pobj->servant : NULL;
}

static ORBitConnectionStatus
get_status (GIOPConnection *cnx)
{
	ORBitConnectionStatus ret;

	g_return_val_if_fail (cnx != NULL, ORBIT_CONNECTION_DISCONNECTED);

	switch (link_connection_get_status (LINK_CONNECTION (cnx))) {
	case LINK_CONNECTED:
		ret = ORBIT_CONNECTION_CONNECTED;
		break;
	case LINK_CONNECTING:
		ret = ORBIT_CONNECTION_CONNECTED;
		break;
	default:
		ret = ORBIT_CONNECTION_DISCONNECTED;
		break;
	}
	return ret;
}

ORBitConnectionStatus
ORBit_small_get_connection_status (CORBA_Object obj)
{
	ORBitConnectionStatus ret;

	g_return_val_if_fail (obj != CORBA_OBJECT_NIL,
			      ORBIT_CONNECTION_DISCONNECTED);

	if (ORBit_small_get_servant (obj))
		ret = ORBIT_CONNECTION_IN_PROC;
	else {
		GIOPConnection *cnx;

		cnx = ORBit_object_get_connection (obj);

		if (cnx) {
			ret = get_status (cnx);
			giop_connection_unref (cnx);
		} else
			ret = ORBIT_CONNECTION_DISCONNECTED;
	}

	return ret;
}

ORBitConnectionStatus
ORBit_small_listen_for_broken (CORBA_Object obj,
			       GCallback    fn,
			       gpointer     user_data)
{
	ORBitConnectionStatus ret;

	if (!obj)
		ret = ORBIT_CONNECTION_DISCONNECTED;

	else if (ORBit_small_get_servant (obj))
		ret = ORBIT_CONNECTION_IN_PROC;

	else {
		GIOPConnection *cnx;

		cnx = ORBit_object_get_connection (obj);

		if (cnx) {
			ret = get_status (cnx);
			link_connection_add_broken_cb
				(LINK_CONNECTION (cnx),
				 (LinkBrokenCallback)fn, user_data);
			giop_connection_unref (cnx);
		} else
			ret = ORBIT_CONNECTION_DISCONNECTED;
	}

	return ret;
}

ORBitConnectionStatus
ORBit_small_unlisten_for_broken_full (CORBA_Object obj,
				      GCallback    fn,
				      gpointer     user_data)
{
	ORBitConnectionStatus ret;

	if (!obj)
		ret = ORBIT_CONNECTION_DISCONNECTED;

	else if (ORBit_small_get_servant (obj))
		ret = ORBIT_CONNECTION_IN_PROC;

	else {
		GIOPConnection *cnx;

		cnx = ORBit_object_peek_connection (obj);

		if (cnx) {
			ret = get_status (cnx);
			link_connection_remove_broken_cb
				(LINK_CONNECTION (cnx),
				 (LinkBrokenCallback)fn, user_data);
			giop_connection_unref (cnx);
		} else
			ret = ORBIT_CONNECTION_DISCONNECTED;
	}

	return ret;
}

ORBitConnectionStatus
ORBit_small_unlisten_for_broken (CORBA_Object obj,
				 GCallback    fn)
{
	return ORBit_small_unlisten_for_broken_full (obj, fn, NULL);
}

ORBitConnection *
ORBit_small_get_connection_ref (CORBA_Object obj)
{
	return (ORBitConnection *) ORBit_object_get_connection (obj);
}

ORBitConnection *
ORBit_small_get_connection (CORBA_Object obj)
{
	ORBitConnection *cnx;

	cnx = ORBit_small_get_connection_ref (obj);

	/* This sucks but compatibily */
	ORBit_small_connection_unref (cnx);
	
	return cnx;
}

void
ORBit_small_connection_unref (ORBitConnection *cnx)
{
	if (cnx)
		giop_connection_unref (GIOP_CONNECTION (cnx));
}

void
ORBit_connection_set_max_buffer (ORBitConnection *cnx,
				 gulong           max_buffer_bytes)
{
	LinkConnection *lcnx = (LinkConnection *) cnx;

	g_return_if_fail (LINK_IS_CONNECTION (lcnx));

	link_connection_set_max_buffer (lcnx, max_buffer_bytes);
}
