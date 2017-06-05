#include <config.h>
#include <string.h>
#include <sys/types.h>
#include "giop-private.h"
#ifdef HAVE_SYS_UIO_H
#  include <sys/uio.h>
#endif
#include <orbit/GIOP/giop.h>
#include "../util/orbit-purify.h"

#define GIOP_CHUNK_ALIGN 8
#define GIOP_CHUNK_SIZE (GIOP_CHUNK_ALIGN * 256)

static gboolean giop_blank_wire_data = FALSE;
static GSList *send_buffer_list = NULL;
static GMutex *send_buffer_list_lock = NULL;

static const char giop_zero_buf [GIOP_CHUNK_ALIGN * 10] = {0};

void
giop_send_buffer_init (gboolean wipe)
{
#ifdef ORBIT_PURIFY
	giop_blank_wire_data = TRUE;
#else
	giop_blank_wire_data = wipe;
#endif
	send_buffer_list_lock = link_mutex_new ();
}

/* Marshal it at compile time so we don't have to do it over and over. This just stores codeset info to say that
     we only speak UTF-8/UTF-16 */
static const CORBA_unsigned_long iop_service_context_data [] = {
	1 /* num_contexts */,
	1 /* ServiceId for CodeSets */,
	12 /* length of encapsulation: 4 endianness+align, 4 charset_id, 4 wcharset_id */,
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
	0x01010101 /* start of encapsulation */,
#else
	0,
#endif
	0x05010001, /* UTF-8 */
	0x00010109 /* UTF-16 */
};

static const GIOP_AddressingDisposition giop_1_2_target_type = GIOP_KeyAddr;

static gboolean
giop_send_buffer_is_oneway(const GIOPSendBuffer *buf)
{
	g_assert (buf);

	switch (buf->giop_version) {
	case GIOP_1_0:
	case GIOP_1_1:
		return (buf->msg.u.request_1_0.response_expected ? FALSE : TRUE);
	case GIOP_1_2:
		return (buf->msg.u.request_1_2.response_flags ? FALSE : TRUE);
	default:
		break;
	}
	g_assert_not_reached();

	return TRUE;
}

GIOPSendBuffer *
giop_send_buffer_use_request (GIOPVersion giop_version,
			      CORBA_unsigned_long request_id,
			      CORBA_boolean response_expected,
			      const CORBA_sequence_CORBA_octet *objkey,
			      const struct iovec *operation_vec,
			      const struct iovec *principal_vec)
{
	GIOPSendBuffer *buf = giop_send_buffer_use (giop_version);
	struct iovec zerovec;

	if(!principal_vec) {
		zerovec.iov_base = (gpointer) giop_zero_buf;
		zerovec.iov_len = sizeof (CORBA_unsigned_long);
		principal_vec = &zerovec;
	}

	buf->msg.header.message_type = GIOP_REQUEST;
	giop_send_buffer_align (buf, sizeof(CORBA_unsigned_long));

	switch (giop_version) {
	case GIOP_1_0:
	case GIOP_1_1:
		buf->msg.u.request_1_0.request_id = request_id;
		buf->msg.u.request_1_0.response_expected = response_expected;

		giop_send_buffer_append (buf, (const guchar *)iop_service_context_data, sizeof(iop_service_context_data));
		giop_send_buffer_append (buf, &buf->msg.u.request_1_0.request_id, sizeof(CORBA_unsigned_long));
		giop_send_buffer_append (buf, &buf->msg.u.request_1_0.response_expected, sizeof(CORBA_boolean));
		giop_send_buffer_append_aligned (buf, &objkey->_length, sizeof(CORBA_unsigned_long));
		giop_send_buffer_append (buf, objkey->_buffer, objkey->_length);
		giop_send_buffer_align (buf, sizeof(CORBA_unsigned_long));
		giop_send_buffer_append (buf, operation_vec->iov_base, operation_vec->iov_len);
		giop_send_buffer_append (buf, principal_vec->iov_base, principal_vec->iov_len);
		break;

	case GIOP_1_2:
		buf->msg.u.request_1_2.request_id = request_id;
		buf->msg.u.request_1_2.response_flags = response_expected ? 0x3 /* SYNC_WITH_TARGET */ : 0x0 /* SYNC_NONE */;

		giop_send_buffer_align (buf, sizeof(CORBA_unsigned_long));
		giop_send_buffer_append (buf, &buf->msg.u.request_1_2.request_id, sizeof(CORBA_unsigned_long));
		giop_send_buffer_append (buf, &buf->msg.u.request_1_2.response_flags, sizeof(CORBA_octet));
		giop_send_buffer_append (buf, giop_zero_buf, 3);
		giop_send_buffer_append (buf, &giop_1_2_target_type, 2); /* We always use GIOP::KeyAddr addressing - the only sane way */
		giop_send_buffer_append_aligned (buf, &objkey->_length, sizeof(CORBA_unsigned_long));
		giop_send_buffer_append (buf, objkey->_buffer, objkey->_length);
		giop_send_buffer_align (buf, sizeof(CORBA_unsigned_long));
		giop_send_buffer_append (buf, operation_vec->iov_base, operation_vec->iov_len);
		giop_send_buffer_append (buf, (const guchar *)iop_service_context_data, sizeof(iop_service_context_data));
		giop_send_buffer_align (buf, 8); /* alignment for the body */
	default:
		break;
	}

	return buf;
}

GIOPSendBuffer *
giop_send_buffer_use_reply(GIOPVersion giop_version,
			   CORBA_unsigned_long request_id,
			   CORBA_unsigned_long reply_status)
{
  GIOPSendBuffer *buf = giop_send_buffer_use(giop_version);

  buf->msg.header.message_type = GIOP_REPLY;

  switch(giop_version)
    {
    case GIOP_1_0:
    case GIOP_1_1:
      buf->msg.u.reply_1_0.reply_status = reply_status;
      buf->msg.u.reply_1_0.request_id = request_id;
      giop_send_buffer_append(buf, (const guchar *)iop_service_context_data, sizeof(iop_service_context_data));      
      giop_send_buffer_append(buf, &buf->msg.u.reply_1_0.request_id, sizeof(CORBA_unsigned_long));
      giop_send_buffer_append(buf, &buf->msg.u.reply_1_0.reply_status, sizeof(CORBA_unsigned_long));
      break;
    case GIOP_1_2:
      buf->msg.u.reply_1_2.reply_status = reply_status;
      buf->msg.u.reply_1_2.request_id = request_id;
      giop_send_buffer_append(buf, &buf->msg.u.reply_1_2.request_id, sizeof(CORBA_unsigned_long));
      giop_send_buffer_append(buf, &buf->msg.u.reply_1_2.reply_status, sizeof(CORBA_unsigned_long));
      giop_send_buffer_append(buf, (const guchar *)iop_service_context_data, sizeof(iop_service_context_data));
      giop_send_buffer_align(buf, 8); /* alignment for the body */
    default:
      break;
    }

  return buf;
}

GIOPSendBuffer *
giop_send_buffer_use_locate_request (GIOPVersion giop_version,
				     CORBA_unsigned_long request_id,
				     const CORBA_sequence_CORBA_octet *objkey)
{
  GIOPSendBuffer *buf = giop_send_buffer_use(giop_version);

  buf->msg.header.message_type = GIOP_LOCATEREQUEST;

  buf->msg.u.locate_request_1_0.request_id = request_id;
  giop_send_buffer_append(buf, &buf->msg.u.locate_request_1_0.request_id, sizeof(CORBA_unsigned_long));

  switch(giop_version)
    {
    case GIOP_1_0:
    case GIOP_1_1:
      giop_send_buffer_append_aligned(buf, &objkey->_length, sizeof(CORBA_unsigned_long));
      giop_send_buffer_append(buf, objkey->_buffer, objkey->_length);
      break;
    case GIOP_1_2:
      giop_send_buffer_append(buf, &giop_1_2_target_type, sizeof(giop_1_2_target_type));
      giop_send_buffer_append_aligned(buf, &objkey->_length, sizeof(CORBA_unsigned_long));
      giop_send_buffer_append(buf, objkey->_buffer, objkey->_length);
    default:
      break;
    }

  return buf;
}

GIOPSendBuffer *
giop_send_buffer_use_locate_reply(GIOPVersion giop_version,
				  CORBA_unsigned_long request_id,
				  CORBA_unsigned_long locate_status)
{
  GIOPSendBuffer *buf = giop_send_buffer_use(giop_version);

  buf->msg.header.message_type = GIOP_LOCATEREPLY;

  buf->msg.u.locate_reply_1_0.request_id = request_id;
  giop_send_buffer_append(buf, &buf->msg.u.locate_reply_1_0.request_id, sizeof(CORBA_unsigned_long));
  buf->msg.u.locate_reply_1_0.locate_status = locate_status;
  giop_send_buffer_append(buf, &buf->msg.u.locate_reply_1_0.locate_status, sizeof(CORBA_unsigned_long));

  return buf;
}

GIOPSendBuffer *
giop_send_buffer_use_close_connection (GIOPVersion giop_version)
{
	GIOPSendBuffer *buf = giop_send_buffer_use (giop_version);

	buf->msg.header.message_type = GIOP_CLOSECONNECTION;  
  
	return buf;
}

GIOPSendBuffer *
giop_send_buffer_use_message_error (GIOPVersion giop_version)
{
	GIOPSendBuffer *buf = giop_send_buffer_use (giop_version);

	buf->msg.header.message_type = GIOP_MESSAGEERROR;  
  
	return buf;
}

void
giop_send_buffer_unuse (GIOPSendBuffer *buf)
{
	int i;

	for (i = 0; i < buf->num_indirects_used; i++) {
		if (buf->indirects[i].size > GIOP_CHUNK_SIZE) {
			buf->indirects [i].size = GIOP_CHUNK_SIZE;
			buf->indirects [i].ptr = g_realloc (buf->indirects [i].ptr,
							    buf->indirects [i].size);
		}
	}

	LINK_MUTEX_LOCK (send_buffer_list_lock);
	send_buffer_list = g_slist_prepend (send_buffer_list, buf);

	LINK_MUTEX_UNLOCK (send_buffer_list_lock);
}

static void
giop_send_buffer_append_real (GIOPSendBuffer *buf,
			      gconstpointer   mem,
			      gulong          len)
{
	register gulong num_used;
	register const guchar *lastptr;

	g_assert (mem);

	lastptr = buf->lastptr;
	num_used = buf->num_used;
	if(num_used && mem == lastptr)
		buf->iovecs[num_used-1].iov_len += len;

	else {
		if(num_used >= buf->num_alloced) {
			buf->num_alloced = MAX (buf->num_alloced, 4) * 2;
			buf->iovecs = g_realloc (buf->iovecs,
						 buf->num_alloced *
						 sizeof (struct iovec));
		}

		buf->iovecs [num_used].iov_base = (gpointer) mem;
		buf->iovecs [num_used].iov_len = len;
		buf->num_used = num_used + 1;
	}

	buf->msg.header.message_size += len;

	buf->lastptr = ((const guchar *) mem) + len;
}

/*
 * get_next_indirect:
 * @buf: the send buffer with an exhausted indirect.
 * @for_size_hint: for very large buffers specify this
 * so we don't allocate too much. If this is non 0 then
 * buf->indirect will contain at least this much space.
 * 
 * Pulls in the next indirect block into buf, and
 * sets up @buf->indirect_left, and @buf->indirect
 * to be correct.
 */
static void
get_next_indirect (GIOPSendBuffer *buf, gulong for_size_hint)
{
	gulong max = buf->num_indirects_used;

	if (max >= buf->num_indirects_alloced) {
		gulong new_size;

		buf->num_indirects_alloced++;
		buf->indirects = g_realloc (
			buf->indirects, buf->num_indirects_alloced * sizeof (GIOPIndirectChunk));

		if (for_size_hint) {
			new_size = (for_size_hint + 7) & ~7;
			if (new_size < GIOP_CHUNK_SIZE)
				new_size = GIOP_CHUNK_SIZE;
		} else
			new_size = GIOP_CHUNK_SIZE;

		buf->indirects [max].size = new_size;

		if (giop_blank_wire_data)
			buf->indirects [max].ptr = g_malloc0 (new_size);
		else
			buf->indirects [max].ptr = g_malloc (new_size);

		/*
		 *   We assume that this is 8 byte aligned, for efficiency -
		 * so we can align to the memory address rather than the offset
		 * into the buffer.
		 */
		g_assert (((gulong)buf->indirects [max].ptr & 0x3) == 0);
	}

	buf->indirect = buf->indirects [max].ptr;
	buf->indirect_left = buf->indirects [max].size;
	buf->num_indirects_used = max + 1;
}

static void
giop_send_buffer_append_copy (GIOPSendBuffer *buf,
			      gconstpointer   mem,
			      gulong          len)
{
	/* FIXME: should we fill up the full indirects ? */
	if (buf->indirect_left < len)
		get_next_indirect (buf, len);

	memcpy (buf->indirect, mem, len);

	giop_send_buffer_append_real (buf, buf->indirect, len);
	
	buf->indirect      += len;
	buf->indirect_left -= len;
}


void
giop_send_buffer_append (GIOPSendBuffer *buf,
			 gconstpointer   mem,
			 gulong          len)
{
	if (len <= 32)
		giop_send_buffer_append_copy (buf, mem, len);
	else
		giop_send_buffer_append_real (buf, mem, len);
}

/**
 * giop_send_buffer_align:
 * @buf: the buffer
 * @boundary: the boundary.
 * 
 * Appends memory to the SendBuffer to align it to a boundary
 * of size @boundary bytes - if neccessary.
 **/
void
giop_send_buffer_align (GIOPSendBuffer *buf, gulong boundary)
{
	gulong align_amt, ms;

	/* 1. Figure out how much to align by */
	ms = buf->msg.header.message_size + buf->header_size;
	align_amt = ALIGN_VALUE(ms, boundary) - ms;

	/* 2. Do the alignment */
	if (align_amt) {

		if (buf->indirect_left < align_amt)
			get_next_indirect (buf, 0);

		p_memzero (buf->indirect, align_amt);
		giop_send_buffer_append_real (buf, buf->indirect, align_amt);

		buf->indirect      += align_amt;
		buf->indirect_left -= align_amt;
	}
}

/**
 * giop_send_buffer_append_aligned:
 * @buf: the buffer
 * @mem: the memory pointer
 * @align_len: the alignment and length of @mem.
 * 
 * This routine alignes the send buffer to a byte boundary
 * of size @align_len, and writes align_len bytes of memory
 * pointed to by @mem to the buffer, or simply expands the
 * buffer if mem is NULL by that much.
 * 
 * Return value: a pointer to the beggining of the 
 * contiguous space available for @mem
 *
 * Note: do not assume anything about the physical
 *       alignment of the returned pointer.
 **/
guchar *
giop_send_buffer_append_aligned (GIOPSendBuffer *buf,
				 gconstpointer   mem,
				 gulong          align_len)
{
	guchar *indirect;

	/* FIXME: could make this more efficient by in-lining the align
	   more aggressively here */
	giop_send_buffer_align (buf, align_len);
  
	if (buf->indirect_left < align_len)
		get_next_indirect (buf, 0);

	indirect = buf->indirect;

	if (mem)
		memcpy (indirect, mem, align_len);
	else
		p_memzero (indirect, align_len);

	giop_send_buffer_append_real (buf, indirect, align_len);
	
	buf->indirect      += align_len;
	buf->indirect_left -= align_len;
	
	return indirect;
}

/**
 * giop_send_buffer_write:
 * @buf: the buffer to write
 * @cnx: the connection to write it to.
 * 
 * Writes @buf to @cnx as a block.
 * 
 * Return value: 0 on sucess, non 0 on error.
 **/
int
giop_send_buffer_write (GIOPSendBuffer *buf,
			GIOPConnection *cnx,
			gboolean        blocking)
{
	int retval;
	LinkConnection *lcnx = LINK_CONNECTION (cnx);
	static LinkWriteOpts *non_block = NULL;

	if (!non_block)
		non_block = link_write_options_new (FALSE);

	/* FIXME: if a FRAGMENT, assert the 8 byte tail align,
	   &&|| giop_send_buffer_align (buf, 8); */

	if (g_thread_supported () 
	    && lcnx->timeout_msec 
	    && !lcnx->timeout_source_id
	    && !giop_send_buffer_is_oneway (buf)) {
		giop_timeout_add (cnx);
	}

	retval = link_connection_writev (lcnx, 
					 buf->iovecs,
					 buf->num_used, 
					 blocking ? NULL : non_block);

	if (!blocking && retval == LINK_IO_QUEUED_DATA)
		retval = 0;

	/* FIXME: we need to flag the connection disconnected on fatal error */

	return retval;
}


GIOPSendBuffer *
giop_send_buffer_use (GIOPVersion giop_version)
{
	GIOPSendBuffer *buf;

	g_return_val_if_fail (
		((int) giop_version) >= 0 &&
		giop_version < GIOP_NUM_VERSIONS, NULL);

	LINK_MUTEX_LOCK (send_buffer_list_lock);
	if (send_buffer_list) {
		GSList *ltmp;

		ltmp = send_buffer_list;
		send_buffer_list = g_slist_remove_link (
			send_buffer_list, ltmp);

		LINK_MUTEX_UNLOCK (send_buffer_list_lock);

		buf = ltmp->data;
		g_slist_free_1 (ltmp);
		buf->num_used = buf->indirect_left = 0;

		if (giop_blank_wire_data) {
			int i;

			for (i = 0; i < buf->num_indirects_used; i++)
				memset (buf->indirects [i].ptr, 0,
					buf->indirects [i].size);
		}

		buf->num_indirects_used = 0;
	} else {
		LINK_MUTEX_UNLOCK (send_buffer_list_lock);

		buf = g_new0 (GIOPSendBuffer, 1);

		memcpy (buf->msg.header.magic, "GIOP", 4);
		buf->msg.header.flags = GIOP_FLAG_ENDIANNESS;
		buf->num_alloced = 8;
		buf->iovecs = g_new (struct iovec, 8);
	}


	memcpy (buf->msg.header.version,
		giop_version_ids [giop_version], 2);
	buf->giop_version = giop_version;

	g_assert (sizeof (buf->msg.header) == 12);
	giop_send_buffer_append_real (
		buf, (guchar *)&buf->msg.header, 12);

	buf->msg.header.message_size = 0;
	buf->header_size = 12;

	return buf;
}

void
giop_send_buffer_append_string (GIOPSendBuffer *buf,
				const char     *str)
{
	CORBA_unsigned_long len;

	len = strlen (str) + 1;

	/* FIXME: inline me ? */
	giop_send_buffer_align (buf, 4);

	/* be cleverer for short strings */
	if (buf->indirect_left >= 4 + len) {
		guchar *indirect = buf->indirect;

		memcpy (indirect, &len, 4);
		memcpy (indirect + 4, str, len);

		giop_send_buffer_append_real (buf, indirect, 4 + len);
	
		buf->indirect      += 4 + len;
		buf->indirect_left -= 4 + len;
	} else {
		giop_send_buffer_append_copy (buf, &len, 4);
		giop_send_buffer_append (buf, str, len);
	}
}

