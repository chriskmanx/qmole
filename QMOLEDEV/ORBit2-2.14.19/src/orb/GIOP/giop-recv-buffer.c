#include <config.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <glib.h>
#include <glib/gprintf.h>

#include "giop-private.h"
#include "giop-debug.h"
#include <orbit/GIOP/giop-types.h>
#include <orbit/GIOP/giop-recv-buffer.h>

#undef DEBUG

#ifdef G_ENABLE_DEBUG
void (*giop_debug_hook_unexpected_reply) (GIOPRecvBuffer        *buf) = NULL;
void (*giop_debug_hook_spoofed_reply)    (GIOPRecvBuffer        *buf,
					  GIOPMessageQueueEntry *ent) = NULL;
void (*giop_debug_hook_incoming_mangler) (GIOPRecvBuffer        *buf) = NULL;
#endif


#define MORE_FRAGMENTS_FOLLOW(buf) ((buf)->msg.header.flags & GIOP_FLAG_FRAGMENTED)

/*
 * FIXME: pretty much this whole module,
 * and all the giop-types headers should be
 * auto-generated and generic ...
 */

/* A list of GIOPMessageQueueEntrys */
static GList  *giop_queued_messages = NULL;
static GMutex *giop_queued_messages_lock = NULL;

/* Don't do this genericaly, union's suck genericaly */
static gboolean
giop_GIOP_TargetAddress_demarshal (GIOPRecvBuffer     *buf,
				   GIOP_TargetAddress *value)
{
	gboolean do_bswap = giop_msg_conversion_needed (buf);

	buf->cur = ALIGN_ADDRESS(buf->cur, 2);
	if ((buf->cur + 2) > buf->end)
		return TRUE;
	if (do_bswap)
		value->_d = GUINT16_SWAP_LE_BE (
			*(guint16 *) buf->cur);
	else
		value->_d = *(guint16 *) buf->cur;
	buf->cur += 2;

	switch (value->_d) {
	case GIOP_KeyAddr:
		buf->cur = ALIGN_ADDRESS (buf->cur, 4);

		if ((buf->cur + 4) > buf->end)
			return TRUE;
		value->_u.object_key._release = CORBA_FALSE;
		if (do_bswap)
			value->_u.object_key._length = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
		else
			value->_u.object_key._length = *((guint32 *)buf->cur);
		buf->cur += 4;
		if ((buf->cur + value->_u.object_key._length) > buf->end ||
		    (buf->cur + value->_u.object_key._length) < buf->cur)
			return TRUE;
		value->_u.object_key._buffer = buf->cur;
		buf->cur += value->_u.object_key._length;

		break;
	case GIOP_ProfileAddr:
		g_warning ("XXX FIXME GIOP_ProfileAddr not handled");
		return TRUE;
		break;
	case GIOP_ReferenceAddr:
		g_warning ("XXX FIXME GIOP_ReferenceAddr not handled");
		return TRUE;
		break;
	}

	return FALSE;
}

static gboolean
giop_IOP_ServiceContextList_demarshal (GIOPRecvBuffer         *buf,
				       IOP_ServiceContextList *value)
{
	return ORBit_demarshal_value (
		TC_IOP_ServiceContextList,
		(gpointer *)&value, buf, NULL);
}

static void
giop_IOP_ServiceContextList_free (IOP_ServiceContextList *value)
{
	if (value)
		CORBA_free (value->_buffer);
}

static gboolean
giop_recv_buffer_demarshal_request_1_1(GIOPRecvBuffer *buf)
{
  gboolean do_bswap = giop_msg_conversion_needed(buf);
  CORBA_unsigned_long oplen;

  buf->msg.u.request_1_1.service_context._buffer = NULL;
  if(giop_IOP_ServiceContextList_demarshal(buf, &buf->msg.u.request_1_1.service_context))
    return TRUE;

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);

  if((buf->cur + 12) > buf->end)
    return TRUE;

  if(do_bswap)
    buf->msg.u.request_1_1.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
  else
    buf->msg.u.request_1_1.request_id = *((guint32 *)buf->cur);
  buf->cur += 4;
  buf->msg.u.request_1_1.response_expected = *buf->cur;
  buf->cur += 4;
  if(do_bswap)
    buf->msg.u.request_1_1.object_key._length = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
  else
    buf->msg.u.request_1_1.object_key._length = *((guint32 *)buf->cur);
  buf->cur += 4;
  
  if((buf->cur + buf->msg.u.request_1_1.object_key._length) > buf->end
     || (buf->cur + buf->msg.u.request_1_1.object_key._length) < buf->cur)
    return TRUE;

  buf->msg.u.request_1_1.object_key._buffer = buf->cur;
  buf->msg.u.request_1_1.object_key._release = CORBA_FALSE;

  buf->cur += buf->msg.u.request_1_1.object_key._length;
  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    return TRUE;
  if(do_bswap)
    oplen = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
  else
    oplen = *((guint32 *)buf->cur);
  buf->cur += 4;

  if((buf->cur + oplen) > buf->end
     || (buf->cur + oplen) < buf->cur)
    return TRUE;

  buf->msg.u.request_1_1.operation = (CORBA_char *) buf->cur;
  buf->cur += oplen;
  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    return TRUE;

  if(do_bswap)
    buf->msg.u.request_1_1.requesting_principal._length = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
  else
    buf->msg.u.request_1_1.requesting_principal._length = *((guint32 *)buf->cur);

  buf->cur += 4;
  if((buf->cur + buf->msg.u.request_1_1.requesting_principal._length) > buf->end
     || (buf->cur + buf->msg.u.request_1_1.requesting_principal._length) < buf->cur)
    return TRUE;

  buf->msg.u.request_1_1.requesting_principal._buffer = buf->cur;
  buf->msg.u.request_1_1.requesting_principal._release = CORBA_FALSE;
  buf->cur += buf->msg.u.request_1_1.requesting_principal._length;

  return FALSE;
}

static gboolean
giop_recv_buffer_demarshal_request_1_2(GIOPRecvBuffer *buf)
{
  gboolean do_bswap = giop_msg_conversion_needed(buf);
  CORBA_unsigned_long oplen;

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);

  if((buf->cur + 8) > buf->end)
    return TRUE;

  if(do_bswap)
    buf->msg.u.request_1_2.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
  else
    buf->msg.u.request_1_2.request_id = *((guint32 *) buf->cur);
  buf->cur += 4;
  buf->msg.u.request_1_2.response_flags = *buf->cur;
  buf->cur += 4;

  if(giop_GIOP_TargetAddress_demarshal(buf, &buf->msg.u.request_1_2.target))
    return TRUE;

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    return TRUE;

  if(do_bswap)
    oplen = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
  else
    oplen = *((guint32 *)buf->cur);
  buf->cur += 4;

  if((buf->cur + oplen) > buf->end
     || (buf->cur + oplen) < buf->cur)
    return TRUE;

  buf->msg.u.request_1_2.operation = (CORBA_char *) buf->cur;
  buf->cur += oplen;

  buf->msg.u.request_1_2.service_context._buffer = NULL;
  if(giop_IOP_ServiceContextList_demarshal(buf, &buf->msg.u.request_1_2.service_context))
    return TRUE;
  buf->cur = ALIGN_ADDRESS(buf->cur, 8);

  return FALSE;
}

static gboolean
giop_recv_buffer_demarshal_reply_1_1(GIOPRecvBuffer *buf)
{
  gboolean do_bswap = giop_msg_conversion_needed(buf);

  buf->msg.u.reply_1_1.service_context._buffer = NULL;
  if(giop_IOP_ServiceContextList_demarshal(buf, &buf->msg.u.reply_1_1.service_context))
    return TRUE;
  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 8) > buf->end)
    return TRUE;
  if(do_bswap)
    {
      buf->msg.u.reply_1_1.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
      buf->cur += 4;
      buf->msg.u.reply_1_1.reply_status = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
    }
  else
    {
      buf->msg.u.reply_1_1.request_id = *((guint32 *)buf->cur);
      buf->cur += 4;
      buf->msg.u.reply_1_1.reply_status = *((guint32 *)buf->cur);
    }
  buf->cur += 4;

 return FALSE;
}

static gboolean
giop_recv_buffer_demarshal_reply_1_2(GIOPRecvBuffer *buf)
{
  gboolean do_bswap = giop_msg_conversion_needed(buf);

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 8) > buf->end)
    return TRUE;
  if(do_bswap)
    {
      buf->msg.u.reply_1_2.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
      buf->cur += 4;
      buf->msg.u.reply_1_2.reply_status = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
    }
  else
    {
      buf->msg.u.reply_1_2.request_id = *((guint32 *)buf->cur);
      buf->cur += 4;
      buf->msg.u.reply_1_2.reply_status = *((guint32 *)buf->cur);
    }
  buf->cur += 4;

  buf->msg.u.reply_1_2.service_context._buffer = NULL;
  if(giop_IOP_ServiceContextList_demarshal(buf, &buf->msg.u.reply_1_2.service_context))
    return TRUE;

  buf->cur = ALIGN_ADDRESS(buf->cur, 8);
  return FALSE;
}

static gboolean
giop_recv_buffer_demarshal_cancel(GIOPRecvBuffer *buf)
{
	gboolean do_bswap = giop_msg_conversion_needed (buf);

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);
	if ((buf->cur + 4) > buf->end)
		return TRUE;
	if (do_bswap)
		buf->msg.u.cancel_request.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
	else
		buf->msg.u.cancel_request.request_id = *((guint32 *)buf->cur);
	buf->cur += 4;

	return FALSE;
}

static gboolean
giop_recv_buffer_demarshal_locate_request_1_1(GIOPRecvBuffer *buf)
{
	gboolean do_bswap = giop_msg_conversion_needed (buf);

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);

	if ((buf->cur + 8) > buf->end)
		return TRUE;

	if (do_bswap)
		buf->msg.u.locate_request_1_1.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
	else
		buf->msg.u.locate_request_1_1.request_id = *((guint32 *)buf->cur);
	buf->cur += 4;
	if (do_bswap)
		buf->msg.u.locate_request_1_1.object_key._length = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
	else
		buf->msg.u.locate_request_1_1.object_key._length = *((guint32 *)buf->cur);
	buf->cur += 4;

	if ((buf->cur + buf->msg.u.locate_request_1_1.object_key._length) > buf->end ||
	    (buf->cur + buf->msg.u.locate_request_1_1.object_key._length) < buf->cur)
		return TRUE;

	buf->msg.u.locate_request_1_1.object_key._buffer = buf->cur;
	buf->msg.u.locate_request_1_1.object_key._release = CORBA_FALSE;
	buf->cur += buf->msg.u.locate_request_1_1.object_key._length;

	return FALSE;
}

static gboolean
giop_recv_buffer_demarshal_locate_request_1_2(GIOPRecvBuffer *buf)
{
	gboolean do_bswap = giop_msg_conversion_needed(buf);

	buf->cur = ALIGN_ADDRESS(buf->cur, 4);

	if((buf->cur + 4) > buf->end)
		return TRUE;

	if(do_bswap)
		buf->msg.u.locate_request_1_2.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
	else
		buf->msg.u.locate_request_1_2.request_id = *((guint32 *)buf->cur);
	buf->cur += 4;

	return giop_GIOP_TargetAddress_demarshal (buf, &buf->msg.u.locate_request_1_2.target);
}

static gboolean
giop_recv_buffer_demarshal_locate_reply_1_1(GIOPRecvBuffer *buf)
{
	gboolean do_bswap = giop_msg_conversion_needed(buf);

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);

	if ((buf->cur + 8) > buf->end)
		return TRUE;

	if (do_bswap) {
		buf->msg.u.locate_reply_1_1.request_id = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
		buf->cur += 4;
		buf->msg.u.locate_reply_1_1.locate_status = GUINT32_SWAP_LE_BE(*((guint32 *)buf->cur));
	} else {
		buf->msg.u.locate_reply_1_1.request_id = *((guint32 *)buf->cur);
		buf->cur += 4;
		buf->msg.u.locate_reply_1_1.locate_status = *((guint32 *)buf->cur);
	}
	buf->cur += 4;
  
	return FALSE;
}

static gboolean
giop_recv_buffer_demarshal_locate_reply_1_2 (GIOPRecvBuffer *buf)
{
	return giop_recv_buffer_demarshal_locate_reply_1_1 (buf);
}

typedef gboolean (*GIOPDecodeFunc) (GIOPRecvBuffer *buf);

static gboolean
giop_recv_buffer_demarshal (GIOPRecvBuffer *buf)
{
	GIOPDecodeFunc              decode_func;
	static const GIOPDecodeFunc decode_funcs [GIOP_NUM_MSG_TYPES] [GIOP_NUM_VERSIONS] = {
		/* request */
		{ giop_recv_buffer_demarshal_request_1_1,
		  giop_recv_buffer_demarshal_request_1_1,
		  giop_recv_buffer_demarshal_request_1_2},
		/* reply */
		{ giop_recv_buffer_demarshal_reply_1_1,
		  giop_recv_buffer_demarshal_reply_1_1,
		  giop_recv_buffer_demarshal_reply_1_2},
		/* cancel request */
		{ giop_recv_buffer_demarshal_cancel,
		  giop_recv_buffer_demarshal_cancel,
		  giop_recv_buffer_demarshal_cancel},
		/* locate request */
		{ giop_recv_buffer_demarshal_locate_request_1_1,
		  giop_recv_buffer_demarshal_locate_request_1_1,
		  giop_recv_buffer_demarshal_locate_request_1_2},
		/* locate reply */
		{ giop_recv_buffer_demarshal_locate_reply_1_1,
		  giop_recv_buffer_demarshal_locate_reply_1_1,
		  giop_recv_buffer_demarshal_locate_reply_1_2},
		/* close connection */
		{NULL, NULL, NULL},
		/* message error */
		{NULL, NULL, NULL},
		/* fragment */
		{NULL, NULL, NULL}
	};

	if (buf->msg.header.message_type >= GIOP_NUM_MSG_TYPES)
		return TRUE;

	if (buf->giop_version >= GIOP_NUM_VERSIONS)
		return TRUE;

	decode_func = decode_funcs [buf->msg.header.message_type] [buf->giop_version];

	if (decode_func)
		return decode_func (buf);

	return FALSE;
}

GIOPRecvBuffer *
giop_recv_buffer_use_encaps (guchar *mem, gulong len)
{
	GIOPRecvBuffer *buf = giop_recv_buffer_use_buf (NULL);

	buf->cur = buf->message_body = mem;
	buf->end = buf->cur + len;
	buf->msg.header.message_size = len;
	buf->msg.header.flags = *(buf->cur++);
	buf->giop_version = GIOP_LATEST;
	buf->left_to_read = 0;
	buf->state = GIOP_MSG_READY;
	buf->free_body = FALSE;

	return buf;
}

GIOPRecvBuffer *
giop_recv_buffer_use_encaps_buf (GIOPRecvBuffer *buf)
{
	guchar             *ptr;
	CORBA_unsigned_long len;

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);

	if ((buf->cur + 4) > buf->end)
		return NULL;
	len = *(CORBA_unsigned_long *) buf->cur;

	if (giop_msg_conversion_needed (buf))
		len = GUINT32_SWAP_LE_BE (len);

	buf->cur += 4;
	if ((buf->cur + len) > buf->end ||
	    (buf->cur + len) < buf->cur)
		return NULL;

	ptr = buf->cur;
	buf->cur += len;

	return giop_recv_buffer_use_encaps (ptr, len);
}

void
giop_recv_buffer_unuse (GIOPRecvBuffer *buf)
{
	if (!buf)
		return;

	if (buf->free_body) {
		g_free (buf->message_body);
		buf->message_body = NULL;
	}

	switch (buf->giop_version) {
	case GIOP_1_0:
	case GIOP_1_1:
		switch (buf->msg.header.message_type) {
		case GIOP_REPLY:
			giop_IOP_ServiceContextList_free (
				&buf->msg.u.reply_1_1.service_context);
			break;
		case GIOP_REQUEST:
			giop_IOP_ServiceContextList_free (
				&buf->msg.u.request_1_1.service_context);
			break;
		default:
			break;
		}
		break;
	case GIOP_1_2:
		switch (buf->msg.header.message_type) {
		case GIOP_REPLY:
			giop_IOP_ServiceContextList_free (
				&buf->msg.u.reply_1_2.service_context);
			break;
		case GIOP_REQUEST:
			giop_IOP_ServiceContextList_free (
				&buf->msg.u.request_1_2.service_context);
			break;
		default:
			break;
		}
		break;
	default:
		break;
	}
	if(buf->connection)
		giop_connection_unref (buf->connection);
	g_free (buf);
}

static void
ent_lock (GIOPMessageQueueEntry *ent)
{
	if (ent->src_thread)
		g_mutex_lock (ent->src_thread->lock);
}

static void
ent_unlock (GIOPMessageQueueEntry *ent)
{
	if (ent->src_thread)
		g_mutex_unlock (ent->src_thread->lock);
}

static void
giop_recv_destroy_queue_entry_T (GIOPMessageQueueEntry *ent)
{
	if (ent->cnx) {
		giop_connection_unref (ent->cnx);
		ent->cnx = NULL;
	}
}

void
giop_recv_list_destroy_queue_entry (GIOPMessageQueueEntry *ent)
{
	LINK_MUTEX_LOCK (giop_queued_messages_lock);
#ifdef DEBUG
	g_warning ("Remove XX:%p:(%p) - %d", ent, ent->async_cb,
		   g_list_length (giop_queued_messages));
#endif
	giop_queued_messages = g_list_remove (giop_queued_messages, ent);
	LINK_MUTEX_UNLOCK (giop_queued_messages_lock);

	giop_recv_destroy_queue_entry_T (ent);
}

void
giop_recv_list_setup_queue_entry (GIOPMessageQueueEntry *ent,
				  GIOPConnection        *cnx,
				  CORBA_unsigned_long    msg_type,
				  CORBA_unsigned_long    request_id)
{
	ent->src_thread = giop_thread_self ();
	ent->async_cb = NULL;

	ent->cnx = giop_connection_ref (cnx);
	ent->msg_type = msg_type;
	ent->request_id = request_id;
	ent->buffer = NULL;

	LINK_MUTEX_LOCK   (giop_queued_messages_lock);
#ifdef DEBUG
	g_warning ("Push XX:%p:(%p) - %d", ent, ent->async_cb,
		   g_list_length (giop_queued_messages));
#endif
	giop_queued_messages = g_list_prepend (giop_queued_messages, ent);
	LINK_MUTEX_UNLOCK (giop_queued_messages_lock);
}

void
giop_recv_list_setup_queue_entry_async (GIOPMessageQueueEntry *ent,
					GIOPAsyncCallback      cb)
{
	g_return_if_fail (ent != NULL);

	ent->async_cb = cb;
}

void
giop_recv_list_zap (GIOPConnection *cnx)
{
	GList  *l, *next;
	GSList *sl, *notify = NULL;

	LINK_MUTEX_LOCK (giop_queued_messages_lock);

	for (l = giop_queued_messages; l; l = next) {
		GIOPMessageQueueEntry *ent = l->data;

		next = l->next;

		if (ent->cnx == cnx) {
			ent_lock (ent);

			dprintf (ERRORS, "Zap listener on dead cnx with buffer %p\n",
				 ent->buffer);

			giop_recv_buffer_unuse (ent->buffer);
			ent->buffer = NULL;

			giop_recv_destroy_queue_entry_T (ent);

			if (giop_thread_io () && !ent->async_cb)
				giop_incoming_signal_T (ent->src_thread, GIOP_CLOSECONNECTION);
			ent_unlock (ent);

			if (ent->async_cb)
				notify = g_slist_prepend (notify, ent);
			giop_queued_messages = g_list_delete_link (
				giop_queued_messages, l);
		}
	}

	LINK_MUTEX_UNLOCK (giop_queued_messages_lock);

	for (sl = notify; sl; sl = sl->next) {
		GIOPMessageQueueEntry *ent = sl->data;
		
		if (!ent->async_cb) {
			/* This should never happen */
			g_warning ("Extraordinary recv list re-enterancy");
			continue;
		}

		giop_invoke_async (ent);
	}
	g_slist_free (notify);
}

CORBA_unsigned_long
giop_recv_buffer_get_request_id (GIOPRecvBuffer *buf)
{
	static const glong reqid_offsets [GIOP_NUM_MSG_TYPES] [GIOP_NUM_VERSIONS] = {
		/* GIOP_REQUEST */
		{ G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.request_1_0.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.request_1_1.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.request_1_2.request_id)},
		/* GIOP_REPLY */
		{ G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.reply_1_0.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.reply_1_1.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.reply_1_2.request_id)},
		/* GIOP_CANCELREQUEST */
		{ G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.cancel_request.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.cancel_request.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.cancel_request.request_id)},
		/* GIOP_LOCATEREQUEST */
		{ G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.locate_request_1_0.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.locate_request_1_1.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.locate_request_1_2.request_id)},
		/* GIOP_LOCATEREPLY */
		{ G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.locate_reply_1_0.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.locate_reply_1_1.request_id),
		  G_STRUCT_OFFSET(GIOPRecvBuffer,
				  msg.u.locate_reply_1_2.request_id)},
		{0,0,0}, /* GIOP_CLOSECONNECTION */
		{0,0,0}, /* GIOP_MESSAGEERROR */
		{0,0,0} /* GIOP_FRAGMENT */
	};
	gulong offset;

	offset = reqid_offsets [buf->msg.header.message_type] [buf->giop_version];
	if (!offset)
		return 0;

	return G_STRUCT_MEMBER (CORBA_unsigned_long, buf, offset);
}

static inline gboolean
check_got (GIOPMessageQueueEntry *ent)
{
	return (ent->buffer || 
		!ent->cnx ||
		(ent->cnx->parent.status == LINK_DISCONNECTED) ||
		(ent->cnx->parent.status == LINK_TIMEOUT));
}

GIOPRecvBuffer *
giop_recv_buffer_get (GIOPMessageQueueEntry *ent,
		      gboolean *timeout)
{
	GIOPThread *tdata = NULL;

	*timeout = FALSE;
	tdata = giop_thread_self ();

 thread_switch:
	if (giop_thread_io ()) {
		ent_lock (ent);

		for (; !check_got (ent); ) {
			if (!giop_thread_queue_empty_T (tdata)) {
				ent_unlock (ent);
				giop_thread_queue_process (tdata);
				ent_lock (ent);
			} else
				g_cond_wait (tdata->incoming, tdata->lock);
		}
		
		ent_unlock (ent);

	} else { /* non-threaded */

		while (!ent->buffer && ent->cnx &&
		       (ent->cnx->parent.status != LINK_DISCONNECTED) &&
		       (ent->cnx->parent.status != LINK_TIMEOUT) &&
		       !giop_thread_io())
			link_main_iteration (TRUE);

		if (giop_thread_io())
			goto thread_switch;
	}

	if (giop_thread_io() 
	    && ent 
	    && ent->cnx 
	    && ent->cnx->parent.timeout_mutex) {
		g_mutex_lock (ent->cnx->parent.timeout_mutex);
		if (ent->cnx->parent.timeout_status == LINK_TIMEOUT_UNKNOWN) {
			link_io_thread_remove_timeout (ent->cnx->parent.timeout_source_id);
			ent->cnx->parent.timeout_source_id = 0;
			ent->cnx->parent.timeout_status = LINK_TIMEOUT_NO;
			giop_connection_unref (ent->cnx); // we remove the source so we must unref the connection
		} else if (ent->cnx->parent.timeout_status == LINK_TIMEOUT_YES)
			*timeout = TRUE;
		g_mutex_unlock (ent->cnx->parent.timeout_mutex);
	}

	giop_thread_queue_tail_wakeup (tdata);
	giop_recv_list_destroy_queue_entry (ent);

	return ent->buffer;
}

ORBit_ObjectKey*
giop_recv_buffer_get_objkey (GIOPRecvBuffer *buf)
{
	switch (buf->msg.header.message_type) {
	case GIOP_REQUEST:
		switch (buf->msg.header.version [1]) {
		case 0:
			return &buf->msg.u.request_1_0.object_key;
			break;
		case 1:
			return &buf->msg.u.request_1_1.object_key;
			break;
		case 2:
			g_assert (buf->msg.u.request_1_2.target._d == GIOP_KeyAddr);
			return &buf->msg.u.request_1_2.target._u.object_key;
			break;
		}
		break;

	case GIOP_LOCATEREQUEST:
		switch (buf->msg.header.version [1]) {
		case 0:
			return &buf->msg.u.locate_request_1_0.object_key;
			break;
		case 1:
			return &buf->msg.u.locate_request_1_1.object_key;
			break;
		case 2:
			g_assert (buf->msg.u.locate_request_1_2.target._d == GIOP_KeyAddr);
			return &buf->msg.u.locate_request_1_2.target._u.object_key;
			break;
		}
		break;

	default:
		g_assert_not_reached ();
	}

	return NULL;
}

char *
giop_recv_buffer_get_opname (GIOPRecvBuffer *buf)
{
	switch(buf->msg.header.version [1]) {
	case 0:
		return buf->msg.u.request_1_0.operation;
		break;
	case 1:
		return buf->msg.u.request_1_1.operation;
		break;
	case 2:
		return buf->msg.u.request_1_2.operation;
		break;
	}

	return NULL;
}

void
giop_recv_buffer_init (void)
{
	giop_queued_messages_lock = link_mutex_new ();
}

static void
giop_connection_add_frag (GIOPConnection *cnx,
			  GIOPRecvBuffer *buf)
{
	cnx->incoming_frags = g_list_prepend (cnx->incoming_frags,
					      g_list_prepend (NULL, buf));
}

static GList *
giop_connection_get_frag (GIOPConnection     *cnx,
			  CORBA_unsigned_long request_id,
			  gboolean            return_first_if_none)
{
	GList *l;

	for (l = cnx->incoming_frags; l; l = l->next) {
		GList *frags = l->data;

		if (giop_recv_buffer_get_request_id (frags->data) == request_id)
			return l->data;
	}

	/* This sucks, but it's prolly a GIOP-1.1 spec issue */
	if (return_first_if_none && cnx->incoming_frags) {
		static int warned = 0;
		if (!warned++)
			dprintf (MESSAGES, "GIOP-1.1 1 fragment set per cnx (?)");

		return cnx->incoming_frags->data;
	}

	return NULL;
}

static void
giop_connection_remove_frag (GIOPConnection *cnx, GList *frags)
{
	GList *l;

	g_return_if_fail (frags != NULL);

	for (l = frags->next; l; l = l->next)
		giop_recv_buffer_unuse (l->data);

	cnx->incoming_frags = g_list_remove (cnx->incoming_frags, frags);
	g_list_free (frags);
}

void
giop_connection_destroy_frags (GIOPConnection *cnx)
{
	GList *l;

	for (l = cnx->incoming_frags; l; l = l->next) {
		GList *l2;

		for (l2 = l->data; l2; l2 = l2->next)
			giop_recv_buffer_unuse (l2->data);

		g_list_free (l->data);
	}
	g_list_free (cnx->incoming_frags);
	cnx->incoming_frags = NULL;
}

static gboolean
alloc_buffer (GIOPRecvBuffer *buf, gpointer old_alloc, gulong body_size)
{
	buf->message_body = g_try_realloc (old_alloc, body_size + 12);

	if (!buf->message_body)
		return TRUE;

	/*
	 *   We assume that this is 8 byte aligned, for efficiency -
	 * so we can align to the memory address rather than the offset
	 * into the buffer.
	 */
	g_assert (((gulong)buf->message_body & 0x3) == 0);
	buf->free_body = TRUE;
	buf->cur = buf->message_body + 12;
	buf->end = buf->cur + body_size;
	buf->left_to_read = body_size;

	return FALSE;
}

static gboolean
concat_frags (GList *list)
{
	GList *l;
	guchar *ptr;
	gulong length = 0;
	gulong initial_length;
	gulong initial_offset;
	GIOPRecvBuffer *head;

	head = list->data;

	length = head->msg.header.message_size;
	initial_offset = (head->cur - head->message_body);
	initial_length = (head->end - head->cur);

	length += initial_offset - 12; /* include what we read of the header */

	g_assert (head->free_body);

	if (alloc_buffer (head, head->message_body, length)) {
		dprintf (ERRORS, "failed to allocate fragment collation buffer");
		return TRUE;
	}

	head->left_to_read = 0;
	head->cur = head->message_body + initial_offset;

	ptr = head->cur + initial_length;

	for (l = list->next; l; l = l->next) {
		gulong len;
		GIOPRecvBuffer *buf = l->data;
			
		len = buf->end - buf->cur;
		memcpy (ptr, buf->cur, len);
		ptr+= len;
	}

	head->end = ptr;

	return FALSE;
}

static glong giop_initial_msg_size_limit = GIOP_INITIAL_MSG_SIZE_LIMIT;

void
giop_recv_set_limit (glong limit)
{
	if (limit > 256) /* Something slightly sensible ? */
		giop_initial_msg_size_limit = limit;
}

glong
giop_recv_get_limit (void)
{
	return giop_initial_msg_size_limit;
}

/**
 * giop_recv_buffer_handle_fragmented:
 * @buf: pointer to recv buffer pointer
 * @cnx: current connection.
 * 
 *   This will append @buf to the right list of buffers
 * on the connection, forming a complete message, and
 * re-write *@buf to the first buffer in the chain.
 * 
 * Return value: TRUE on error else FALSE
 **/
static gboolean
giop_recv_buffer_handle_fragmented (GIOPRecvBuffer **ret_buf,
				    GIOPConnection  *cnx)
{
	GList *list;
	gboolean giop_1_1;
	gboolean error = FALSE;
	CORBA_long message_id;
	GIOPRecvBuffer *buf = *ret_buf;

	giop_1_1 = (buf->giop_version == GIOP_1_1);

	switch (buf->msg.header.message_type) {
	case GIOP_REPLY:
	case GIOP_LOCATEREPLY:
	case GIOP_REQUEST:
	case GIOP_LOCATEREQUEST:
		message_id = giop_recv_buffer_get_request_id (buf);
		break;
	case GIOP_FRAGMENT:
		if (!giop_1_1) {
			buf->cur = ALIGN_ADDRESS (buf->cur, 4);

			if ((buf->cur + 4) > buf->end) {
				dprintf (ERRORS, "incoming bogus fragment length");
				return TRUE;
			}
			if (giop_msg_conversion_needed (buf))
				message_id = GUINT32_SWAP_LE_BE (*((guint32 *)buf->cur));
			else
				message_id = *(guint32 *) buf->cur;
			buf->cur += 4;
		} else
			message_id = 0;
		break;
	default:
		dprintf (ERRORS, "Bogus fragment packet type %d",
			 buf->msg.header.message_type);
		return TRUE;
	}

	if (!(list = giop_connection_get_frag (cnx, message_id, giop_1_1))) {
		if (!MORE_FRAGMENTS_FOLLOW (buf))
			return TRUE;

		giop_connection_add_frag (cnx, buf);

	} else {
		GIOPRecvBuffer *head = list->data;

		*ret_buf = head;
		g_assert (head->msg.header.message_type != GIOP_FRAGMENT);

		/* track total length on head node */
		/* (end - cur) to account for fragment (msg id) header */
		head->msg.header.message_size += (buf->end - buf->cur);

		list = g_list_append (list, buf);

		if (!cnx->parent.is_auth &&
		    buf->msg.header.message_size > giop_initial_msg_size_limit) {
			dprintf (ERRORS, "Message exceeded initial size limit\n");
			error = TRUE;
			giop_connection_remove_frag (cnx, list);
		}
			
		if (!MORE_FRAGMENTS_FOLLOW (buf)) {
			g_assert (buf->msg.header.message_type == GIOP_FRAGMENT);

			/* concat all fragments - re-write & continue */
			error = concat_frags (list);

			giop_connection_remove_frag (cnx, list);
		}
	}

	return error;
}

static gboolean
handle_reply (GIOPRecvBuffer *buf)
{
	GList                 *l;
	gboolean               error;
	GIOPMessageQueueEntry *ent;
	CORBA_unsigned_long    request_id;

	request_id = giop_recv_buffer_get_request_id (buf);

	error = FALSE;

	LINK_MUTEX_LOCK (giop_queued_messages_lock);

	for (l = giop_queued_messages; l; l = l->next) {
		ent = l->data;

		if (ent->request_id == request_id &&
		    ent->msg_type == buf->msg.header.message_type)
			break;
	}

	ent = l ? l->data : NULL;

	if (!ent) {
		if (giop_recv_buffer_reply_status (buf) ==
		    CORBA_SYSTEM_EXCEPTION) {
			/*
			 * Unexpected - but sometimes a oneway
			 * method invocation on a de-activated
			 * object results in us getting a bogus
			 * system exception in reply.
			 */
 		} else {
#ifdef G_ENABLE_DEBUG
			if (giop_debug_hook_unexpected_reply)
				giop_debug_hook_unexpected_reply (buf);
			else
				dprintf (ERRORS, "We received an unexpected reply\n");
#endif /* G_ENABLE_DEBUG */
			error = TRUE;
		}

	} else if (ent->cnx != buf->connection) {
#ifdef G_ENABLE_DEBUG
		if (giop_debug_hook_spoofed_reply)
			giop_debug_hook_spoofed_reply (buf, ent);
#endif
		dprintf (ERRORS, "We received a bogus reply\n");

		error = TRUE;

	} else {
#ifdef DEBUG
		g_warning ("Pop XX:%p:%p - %d",
			   ent, ent->async_cb,
			   g_list_length (giop_queued_messages));
#endif
		giop_queued_messages = g_list_delete_link
			(giop_queued_messages, l);
	}

	LINK_MUTEX_UNLOCK (giop_queued_messages_lock);

	if (ent && !error) {
		gboolean async = FALSE;

		ent_lock (ent);
		ent->buffer = buf;

		if (giop_thread_io () && !ent->async_cb)
			giop_incoming_signal_T (ent->src_thread,
						GIOP_REPLY);

		else if (ent->async_cb)
			async = TRUE;

		ent_unlock (ent);

		if (async)
			giop_invoke_async (ent);

		buf = NULL;
	}
	
	giop_recv_buffer_unuse (buf);

	return error;
}

static gboolean
giop_recv_msg_reading_body (GIOPRecvBuffer *buf,
			    gboolean        is_auth)
{
	dprintf (GIOP, "Incoming IIOP header:\n");

	do_giop_dump (stderr, (guint8 *) &buf->msg.header, 12, 0);

	/* Check the header */
	if (memcmp (buf->msg.header.magic, "GIOP", 4))
		return TRUE;
	
	if (buf->msg.header.message_type >= GIOP_NUM_MSG_TYPES)
		return TRUE;
	
	switch (buf->msg.header.version [0]) {
	case 1:
		switch (buf->msg.header.version [1]) {
		case 0:
			buf->giop_version = GIOP_1_0;
			break;
		case 1:
			buf->giop_version = GIOP_1_1;
			break;
		case 2:
			buf->giop_version = GIOP_1_2;
			break;
		default:
			return TRUE;
			break;
		}
		break;
	default:
		return TRUE;
		break;
	}

	if ((buf->msg.header.flags & GIOP_FLAG_LITTLE_ENDIAN) != GIOP_FLAG_ENDIANNESS)
		buf->msg.header.message_size = GUINT32_SWAP_LE_BE (buf->msg.header.message_size);

	/* NB. at least CLOSECONNECTION has 0 length message_size */

	if (!is_auth && buf->msg.header.message_size > giop_initial_msg_size_limit) {
		dprintf (ERRORS, "Message exceeded unauthorized size limit\n");
		return TRUE;
	}

	if (alloc_buffer (buf, NULL, buf->msg.header.message_size))
		return TRUE;

	return FALSE;
}

/*
 * FIXME: we should definately handle things more asynchronously,
 * perhaps even at the expense of having to go to the GSource
 * twice in order to get fresh input (?)
 * or should we poll ourselves on the source to see what's up?
 *
 * The whole locking concept here looks broken to me,
 * especially since 'read' can flag the connection disconnected
 * giving a nice deadlock.
 */
gboolean
giop_connection_handle_input (LinkConnection *lcnx)
{
	GIOPRecvBuffer *buf;
	GIOPConnection *cnx = (GIOPConnection *) lcnx;

	do {
		int n;

		if (!cnx->incoming_msg)
			cnx->incoming_msg = giop_recv_buffer_use_buf (cnx);

		buf = cnx->incoming_msg;

		n = link_connection_read (
			lcnx, buf->cur, buf->left_to_read, FALSE);

		if (n == 0) /* We'll be back */
			return TRUE;

		if (n < 0 || !buf->left_to_read) /* HUP */
			goto msg_error;

/*		fprintf (stderr, "Read %d\n", n);
		giop_dump (stderr, buf->cur, n, 0); */

		buf->left_to_read -= n;
		buf->cur += n;

		if (buf->left_to_read == 0) {

#ifdef G_ENABLE_DEBUG
			if (giop_debug_hook_incoming_mangler)
				giop_debug_hook_incoming_mangler (buf);
#endif

			switch (buf->state) {

			case GIOP_MSG_READING_HEADER:
				if (giop_recv_msg_reading_body (buf, cnx->parent.is_auth)) {
					dprintf (ERRORS, "OOB incoming msg header data\n");
					goto msg_error;
				}
				buf->state = GIOP_MSG_READING_BODY;
				break;

			case GIOP_MSG_READING_BODY: {

				dprintf (GIOP, "Incoming IIOP body:\n");

				buf->cur = buf->message_body + 12;
				if ((buf->cur + buf->msg.header.message_size) > buf->end) {
					dprintf (ERRORS, "broken incoming length data\n");
					goto msg_error;
				}
				do_giop_dump (stderr, buf->cur, buf->msg.header.message_size, 12);

				buf->state = GIOP_MSG_READY;

				if (giop_recv_buffer_demarshal (buf)) {
					dprintf (ERRORS, "broken incoming header data\n");
					goto msg_error;
				}

				if (MORE_FRAGMENTS_FOLLOW (buf)) {
					if (giop_recv_buffer_handle_fragmented (&buf, cnx))
						goto msg_error;

					else {
						cnx->incoming_msg = NULL;
						goto frag_out;
					}

				} else if (buf->msg.header.message_type == GIOP_FRAGMENT) {
					if (giop_recv_buffer_handle_fragmented (&buf, cnx))
						goto msg_error;
					/* else last fragment */
				}
				break;
			}

			case GIOP_MSG_AWAITING_FRAGMENTS:
			case GIOP_MSG_READY:
				g_assert_not_reached ();
				break;
			}
		}

	} while (cnx->incoming_msg &&
		 buf->left_to_read > 0 &&
		 buf->state != GIOP_MSG_READY);

	cnx->incoming_msg = NULL;

	switch (buf->msg.header.message_type) {
	case GIOP_REPLY:
	case GIOP_LOCATEREPLY:
		dprintf (MESSAGES, "handling reply\n");
		if (handle_reply (buf)) /* dodgy inbound data, pull the cnx */
			link_connection_state_changed (lcnx, LINK_DISCONNECTED);
		break;

	case GIOP_REQUEST:
		dprintf (MESSAGES, "handling request\n");
		ORBit_handle_request (cnx->orb_data, buf);
		break;

	case GIOP_LOCATEREQUEST:
		dprintf (MESSAGES, "handling locate request\n");
		ORBit_handle_locate_request (cnx->orb_data, buf);
		break;

	case GIOP_CANCELREQUEST:
	case GIOP_MESSAGEERROR:
		dprintf (ERRORS, "dropping an unusual & unhandled input buffer 0x%x",
			 buf->msg.header.message_type);
		giop_recv_buffer_unuse (buf);
		break;

	case GIOP_CLOSECONNECTION:
		dprintf (MESSAGES, "received close connection\n");
		giop_recv_buffer_unuse (buf);
		link_connection_state_changed (lcnx, LINK_DISCONNECTED);
		break;

	case GIOP_FRAGMENT:
		dprintf (ERRORS, "Fragment got in the wrong channel\n");
	default:
		dprintf (ERRORS, "dropping an out of bound input buffer "
			 "on the floor 0x%x\n", buf->msg.header.message_type);
		goto msg_error;
		break;
	}

 frag_out:	
	return TRUE;

 msg_error:
	cnx->incoming_msg = NULL;

	buf->msg.header.message_type = GIOP_MESSAGEERROR;
	buf->msg.header.message_size = 0;

	giop_recv_buffer_unuse (buf);

	/* Zap it for badness.
	 * XXX We should probably handle oversized
	 * messages more graciously XXX */
	link_connection_state_changed (LINK_CONNECTION (cnx),
				       LINK_DISCONNECTED);

	return TRUE;
}

static gboolean
giop_timeout (gpointer data)
{
	gboolean retv = FALSE;
	GIOPConnection *cnx = (GIOPConnection*)data;
	LinkConnection *lcnx = LINK_CONNECTION (cnx);
	GIOPThread *tdata = (GIOPThread *)lcnx->tdata;

	g_assert (lcnx->timeout_mutex);

	if (lcnx->status == LINK_DISCONNECTED) {
		giop_connection_unref (cnx); // we remove the source so we must unref cnx
		goto out;
	}
		
	g_mutex_lock (lcnx->timeout_mutex);
	if (lcnx->timeout_status == LINK_TIMEOUT_UNKNOWN) {
		lcnx->timeout_source_id = 0;
		lcnx->timeout_status = LINK_TIMEOUT_YES;
	} else {
		g_mutex_unlock (lcnx->timeout_mutex);
		retv = TRUE; // do not remove the source - the one who sets timeout_status will do that
		goto out;
	}
	g_mutex_unlock (lcnx->timeout_mutex);

	link_connection_state_changed (lcnx, LINK_TIMEOUT);

	g_mutex_lock (tdata->lock); /* ent_lock */
	giop_incoming_signal_T (tdata, GIOP_CLOSECONNECTION);
	g_mutex_unlock (tdata->lock); /* ent_lock */
	
	giop_connection_unref (cnx); // we remove the source so we must unref cnx

out:
	return retv;
}

void
giop_timeout_add (GIOPConnection *cnx)
{
	static GStaticMutex static_mutex = G_STATIC_MUTEX_INIT;
	LinkConnection *lcnx = LINK_CONNECTION (cnx);

	if (!giop_thread_io ())
		return;
	if (!lcnx->timeout_msec) 
		return;

	g_static_mutex_lock (&static_mutex);
	if (lcnx->timeout_source_id)
		goto out;

	giop_connection_ref (cnx); // to be unref'ed by the one who removes the timeout source
	
	if (!lcnx->timeout_mutex)
		lcnx->timeout_mutex = g_mutex_new ();

	g_mutex_lock (lcnx->timeout_mutex);
	lcnx->timeout_status = LINK_TIMEOUT_UNKNOWN;
	g_mutex_unlock (lcnx->timeout_mutex);

	lcnx->tdata = giop_thread_self ();

	lcnx->timeout_source_id = link_io_thread_add_timeout (lcnx->timeout_msec, giop_timeout, (gpointer)cnx);

out:
	g_static_mutex_unlock (&static_mutex);
}

GIOPRecvBuffer *
giop_recv_buffer_use_buf (GIOPConnection *cnx)
{
	GIOPRecvBuffer *buf = NULL;
	
	if(cnx)
		giop_connection_ref (cnx);
	
	buf = g_new0 (GIOPRecvBuffer, 1);

	buf->state = GIOP_MSG_READING_HEADER;
	buf->cur = (guchar *)&buf->msg.header;
	buf->left_to_read = 12;
	buf->connection = cnx;

	return buf;
}
