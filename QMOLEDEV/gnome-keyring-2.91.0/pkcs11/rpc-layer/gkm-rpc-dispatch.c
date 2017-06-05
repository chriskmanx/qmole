/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-rpc-dispatch.h - receiver of our PKCS#11 protocol.

   Copyright (C) 2008, Stef Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gkm-rpc-layer.h"
#include "gkm-rpc-private.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11g.h"
#include "pkcs11/pkcs11i.h"

#include "egg/egg-error.h"
#include "egg/egg-unix-credentials.h"

#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <pthread.h>

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

#include <glib.h>

/* Where we dispatch the calls to */
static CK_FUNCTION_LIST_PTR pkcs11_module = NULL;

/* The error returned on protocol failures */
#define PARSE_ERROR CKR_DEVICE_ERROR
#define PREP_ERROR  CKR_DEVICE_MEMORY

/* -----------------------------------------------------------------------------
 * LOGGING and DEBUGGING
 */

#if DEBUG_OUTPUT
#define debug(x) gkm_rpc_debug x
#else
#define debug(x)
#endif

#define warning(x) gkm_rpc_warn x

#define return_val_if_fail(x, v) \
	if (!(x)) { rpc_warn ("'%s' not true at %s", #x, __func__); return v; }

void
gkm_rpc_log (const char *line)
{
	g_message ("%s", line);
}

/* -------------------------------------------------------------------------------
 * CALL STRUCTURES
 */

typedef struct _CallState {
	GkmRpcMessage *req;
	GkmRpcMessage *resp;
	void *allocated;
	CK_G_APPLICATION application;
} CallState;

static int
call_init (CallState *cs)
{
	assert (cs);

	cs->req = gkm_rpc_message_new ((EggBufferAllocator)realloc);
	cs->resp = gkm_rpc_message_new ((EggBufferAllocator)realloc);
	if (!cs->req || !cs->resp) {
		gkm_rpc_message_free (cs->req);
		gkm_rpc_message_free (cs->resp);
		return 0;
	}

	memset (&cs->application, 0, sizeof (cs->application));
	cs->application.applicationData = cs;

	cs->allocated = NULL;
	return 1;
}

static void*
call_alloc (CallState *cs, size_t length)
{
	void **data;

	assert (cs);

	if (length > 0x7fffffff)
		return NULL;

	data = malloc (sizeof (void*) + length);
	if (!data)
		return NULL;

	/* Munch up the memory to help catch bugs */
	memset (data, 0xff, sizeof (void*) + length);

	/* Store pointer to next allocated block at beginning */
	*data = cs->allocated;
	cs->allocated = data;

	/* Data starts after first pointer */
	return (void*)(data + 1);
}

static void
call_reset (CallState *cs)
{
	void *allocated;
	void **data;

	assert (cs);

	allocated = cs->allocated;
	while (allocated) {
		data = (void**)allocated;

		/* Pointer to the next allocation */
		allocated = *data;
		free (data);
	}

	cs->allocated = NULL;
	gkm_rpc_message_reset (cs->req);
	gkm_rpc_message_reset (cs->resp);
}

static void
call_uninit (CallState *cs)
{
	assert (cs);

	call_reset (cs);

	gkm_rpc_message_free (cs->req);
	gkm_rpc_message_free (cs->resp);
}

/* -------------------------------------------------------------------
 * PROTOCOL CODE
 */

static CK_RV
proto_read_byte_buffer (CallState *cs, CK_BYTE_PTR* buffer, CK_ULONG* n_buffer)
{
	GkmRpcMessage *msg;
	uint32_t length;

	assert (cs);
	assert (buffer);
	assert (n_buffer);

	msg = cs->req;

	/* Check that we're supposed to be reading this at this point */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "fy"));

	/* The number of ulongs there's room for on the other end */
	if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &length))
		return PARSE_ERROR;

	*n_buffer = length;
	*buffer = NULL;

	/* If set to zero, then they just want the length */
	if (!length)
		return CKR_OK;

	*buffer = call_alloc (cs, length * sizeof (CK_BYTE));
	if (!*buffer)
		return CKR_DEVICE_MEMORY;

	return CKR_OK;
}

static CK_RV
proto_read_byte_array (CallState *cs, CK_BYTE_PTR* array, CK_ULONG* n_array)
{
	GkmRpcMessage *msg;
	const unsigned char *data;
	unsigned char valid;
	size_t n_data;

	assert (cs);

	msg = cs->req;

	/* Check that we're supposed to have this at this point */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "ay"));

	/* Read out the byte which says whether data is present or not */
	if (!egg_buffer_get_byte (&msg->buffer, msg->parsed, &msg->parsed, &valid))
		return PARSE_ERROR;

	if (!valid) {
		*array = NULL;
		*n_array = 0;
		return CKR_OK;
	}

	/* Point our arguments into the buffer */
	if (!egg_buffer_get_byte_array (&msg->buffer, msg->parsed, &msg->parsed,
	                                &data, &n_data))
		return PARSE_ERROR;

	*array = (CK_BYTE_PTR)data;
	*n_array = n_data;
	return CKR_OK;
}

static CK_RV
proto_write_byte_array (CallState *cs, CK_BYTE_PTR array, CK_ULONG len, CK_RV ret)
{
	assert (cs);

	/*
	 * When returning an byte array, in many cases we need to pass
	 * an invalid array along with a length, which signifies CKR_BUFFER_TOO_SMALL.
	 */

	switch (ret) {
	case CKR_BUFFER_TOO_SMALL:
		array = NULL;
		/* fall through */
	case CKR_OK:
		break;

	/* Pass all other errors straight through */
	default:
		return ret;
	};

	if (!gkm_rpc_message_write_byte_array (cs->resp, array, len))
		return PREP_ERROR;

	return CKR_OK;
}

static CK_RV
proto_read_ulong_buffer (CallState *cs, CK_ULONG_PTR* buffer, CK_ULONG* n_buffer)
{
	GkmRpcMessage *msg;
	uint32_t length;

	assert (cs);
	assert (buffer);
	assert (n_buffer);

	msg = cs->req;

	/* Check that we're supposed to be reading this at this point */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "fu"));

	/* The number of ulongs there's room for on the other end */
	if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &length))
		return PARSE_ERROR;

	*n_buffer = length;
	*buffer = NULL;

	/* If set to zero, then they just want the length */
	if (!length)
		return CKR_OK;

	*buffer = call_alloc (cs, length * sizeof (CK_ULONG));
	if (!*buffer)
		return CKR_DEVICE_MEMORY;

	return CKR_OK;
}

static CK_RV
proto_write_ulong_array (CallState *cs, CK_ULONG_PTR array, CK_ULONG len, CK_RV ret)
{
	assert (cs);

	/*
	 * When returning an ulong array, in many cases we need to pass
	 * an invalid array along with a length, which signifies CKR_BUFFER_TOO_SMALL.
	 */

	switch (ret) {
	case CKR_BUFFER_TOO_SMALL:
		array = NULL;
		/* fall through */
	case CKR_OK:
		break;

	/* Pass all other errors straight through */
	default:
		return ret;
	};

	if (!gkm_rpc_message_write_ulong_array (cs->resp, array, len))
		return PREP_ERROR;

	return CKR_OK;
}

static CK_RV
proto_read_attribute_buffer (CallState *cs, CK_ATTRIBUTE_PTR* result, CK_ULONG* n_result)
{
	CK_ATTRIBUTE_PTR attrs;
	GkmRpcMessage *msg;
	uint32_t n_attrs, i;
	uint32_t value;

	assert (cs);
	assert (result);
	assert (n_result);

	msg = cs->req;

	/* Make sure this is in the rigth order */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "fA"));

	/* Read the number of attributes */
	if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &n_attrs))
		return PARSE_ERROR;

	/* Allocate memory for the attribute structures */
	attrs = call_alloc (cs, n_attrs * sizeof (CK_ATTRIBUTE));
	if (!attrs)
		return CKR_DEVICE_MEMORY;

	/* Now go through and fill in each one */
	for (i = 0; i < n_attrs; ++i) {

		/* The attribute type */
		if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &value))
			return PARSE_ERROR;

		attrs[i].type = value;

		/* The number of bytes to allocate */
		if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &value))
			return PARSE_ERROR;

		if (value == 0) {
			attrs[i].pValue = NULL;
			attrs[i].ulValueLen = 0;
		} else {
			attrs[i].pValue = call_alloc (cs, value);
			if (!attrs[i].pValue)
				return CKR_DEVICE_MEMORY;
			attrs[i].ulValueLen = value;
		}
	}

	*result = attrs;
	*n_result = n_attrs;
	return CKR_OK;
}

static CK_RV
proto_read_attribute_array (CallState *cs, CK_ATTRIBUTE_PTR* result, CK_ULONG* n_result)
{
	CK_ATTRIBUTE_PTR attrs;
	const unsigned char *data;
	unsigned char valid;
	GkmRpcMessage *msg;
	uint32_t n_attrs, i;
	uint32_t value;
	size_t n_data;

	assert (cs);
	assert (result);
	assert (n_result);

	msg = cs->req;

	/* Make sure this is in the rigth order */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "aA"));

	/* Read the number of attributes */
	if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &n_attrs))
		return PARSE_ERROR;

	/* Allocate memory for the attribute structures */
	attrs = call_alloc (cs, n_attrs * sizeof (CK_ATTRIBUTE));
	if (!attrs)
		return CKR_DEVICE_MEMORY;

	/* Now go through and fill in each one */
	for (i = 0; i < n_attrs; ++i) {

		/* The attribute type */
		if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &value))
			return PARSE_ERROR;

		attrs[i].type = value;

		/* Whether this one is valid or not */
		if (!egg_buffer_get_byte (&msg->buffer, msg->parsed, &msg->parsed, &valid))
			return PARSE_ERROR;

		if (valid) {
			if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &value))
				return PARSE_ERROR;
			if (!egg_buffer_get_byte_array (&msg->buffer, msg->parsed, &msg->parsed, &data, &n_data))
				return PARSE_ERROR;

			if (data != NULL && n_data != value) {
				g_warning ("attribute length and data do not match");
				return PARSE_ERROR;
			}

			attrs[i].pValue = (CK_VOID_PTR)data;
			attrs[i].ulValueLen = value;
		} else {
			attrs[i].pValue = NULL;
			attrs[i].ulValueLen = -1;
		}
	}

	*result = attrs;
	*n_result = n_attrs;
	return CKR_OK;
}

static CK_RV
proto_write_attribute_array (CallState *cs, CK_ATTRIBUTE_PTR array, CK_ULONG len, CK_RV ret)
{
	assert (cs);

	/*
	 * When returning an attribute array, certain errors aren't
	 * actually real errors, these are passed through to the other
	 * side along with the attribute array.
	 */

	switch (ret) {
	case CKR_ATTRIBUTE_SENSITIVE:
	case CKR_ATTRIBUTE_TYPE_INVALID:
	case CKR_BUFFER_TOO_SMALL:
	case CKR_OK:
		break;

	/* Pass all other errors straight through */
	default:
		return ret;
	};

	if (!gkm_rpc_message_write_attribute_array (cs->resp, array, len) ||
	    !gkm_rpc_message_write_ulong (cs->resp, ret))
		return PREP_ERROR;

	return CKR_OK;
}

static CK_RV
proto_read_null_string (CallState *cs, CK_UTF8CHAR_PTR* val)
{
	GkmRpcMessage *msg;
	const unsigned char *data;
	size_t n_data;

	assert (cs);
	assert (val);

	msg = cs->req;

	/* Check that we're supposed to have this at this point */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "z"));

	if (!egg_buffer_get_byte_array (&msg->buffer, msg->parsed, &msg->parsed, &data, &n_data))
		return PARSE_ERROR;

	/* Allocate a block of memory for it */
	*val = call_alloc (cs, n_data);
	if (!*val)
		return CKR_DEVICE_MEMORY;

	memcpy (*val, data, n_data);
	(*val)[n_data] = 0;

	return CKR_OK;
}

static CK_RV
proto_read_mechanism (CallState *cs, CK_MECHANISM_PTR mech)
{
	GkmRpcMessage *msg;
	const unsigned char *data;
	uint32_t value;
	size_t n_data;

	assert (cs);
	assert (mech);

	msg = cs->req;

	/* Make sure this is in the right order */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "M"));

	/* The mechanism type */
	if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &value))
		return PARSE_ERROR;

	/* The mechanism data */
	if (!egg_buffer_get_byte_array (&msg->buffer, msg->parsed, &msg->parsed, &data, &n_data))
		return PARSE_ERROR;

	mech->mechanism = value;
	mech->pParameter = (CK_VOID_PTR)data;
	mech->ulParameterLen = n_data;
	return CKR_OK;
}

static CK_RV
proto_write_info (CallState *cs, CK_INFO_PTR info)
{
	GkmRpcMessage *msg;

	assert (cs);
	assert (info);

	msg = cs->resp;

	if (!gkm_rpc_message_write_version (msg, &info->cryptokiVersion) ||
	    !gkm_rpc_message_write_space_string (msg, info->manufacturerID, 32) ||
	    !gkm_rpc_message_write_ulong (msg, info->flags) ||
	    !gkm_rpc_message_write_space_string (msg, info->libraryDescription, 32) ||
	    !gkm_rpc_message_write_version (msg, &info->libraryVersion))
		return PREP_ERROR;

	return CKR_OK;
}

static CK_RV
proto_write_slot_info (CallState *cs, CK_SLOT_INFO_PTR info)
{
	GkmRpcMessage *msg;

	assert (cs);
	assert (info);

	msg = cs->resp;

	if (!gkm_rpc_message_write_space_string (msg, info->slotDescription, 64) ||
	    !gkm_rpc_message_write_space_string (msg, info->manufacturerID, 32) ||
	    !gkm_rpc_message_write_ulong (msg, info->flags) ||
	    !gkm_rpc_message_write_version (msg, &info->hardwareVersion) ||
	    !gkm_rpc_message_write_version (msg, &info->firmwareVersion))
		return PREP_ERROR;

	return CKR_OK;
}

static CK_RV
proto_write_token_info (CallState *cs, CK_TOKEN_INFO_PTR info)
{
	GkmRpcMessage *msg;

	assert (cs);
	assert (info);

	msg = cs->resp;

	if (!gkm_rpc_message_write_space_string (msg, info->label, 32) ||
	    !gkm_rpc_message_write_space_string (msg, info->manufacturerID, 32) ||
	    !gkm_rpc_message_write_space_string (msg, info->model, 16) ||
	    !gkm_rpc_message_write_space_string (msg, info->serialNumber, 16) ||
	    !gkm_rpc_message_write_ulong (msg, info->flags) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulMaxSessionCount) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulSessionCount) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulMaxRwSessionCount) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulRwSessionCount) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulMaxPinLen) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulMinPinLen) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulTotalPublicMemory) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulFreePublicMemory) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulTotalPrivateMemory) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulFreePrivateMemory) ||
	    !gkm_rpc_message_write_version (msg, &info->hardwareVersion) ||
	    !gkm_rpc_message_write_version (msg, &info->firmwareVersion) ||
	    !gkm_rpc_message_write_space_string (msg, info->utcTime, 16))
		return PREP_ERROR;

	return CKR_OK;
}

static CK_RV
proto_write_mechanism_info (CallState *cs, CK_MECHANISM_INFO_PTR info)
{
	GkmRpcMessage *msg;

	assert (cs);
	assert (info);

	msg = cs->resp;

	if (!gkm_rpc_message_write_ulong (msg, info->ulMinKeySize) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulMaxKeySize) ||
	    !gkm_rpc_message_write_ulong (msg, info->flags))
		return PREP_ERROR;

	return CKR_OK;
}

static CK_RV
proto_write_session_info (CallState *cs, CK_SESSION_INFO_PTR info)
{
	GkmRpcMessage *msg;

	assert (cs);
	assert (info);

	msg = cs->resp;

	if (!gkm_rpc_message_write_ulong (msg, info->slotID) ||
	    !gkm_rpc_message_write_ulong (msg, info->state) ||
	    !gkm_rpc_message_write_ulong (msg, info->flags) ||
	    !gkm_rpc_message_write_ulong (msg, info->ulDeviceError))
		return PREP_ERROR;

	return CKR_OK;
}

/* -------------------------------------------------------------------
 * CALL MACROS
 */

#define BEGIN_CALL(call_id) \
	debug ((#call_id ": enter")); \
	assert (cs); \
	assert (pkcs11_module); \
	{  \
		CK_ ## call_id _func = pkcs11_module-> call_id; \
		CK_RV _ret = CKR_OK; \
		if (!_func) { _ret = CKR_GENERAL_ERROR; goto _cleanup; }

#define PROCESS_CALL(args)\
	assert (gkm_rpc_message_is_verified (cs->req)); \
	_ret = _func args

#define END_CALL \
	_cleanup: \
		debug (("ret: %d", _ret)); \
		return _ret; \
	}

#define IN_BYTE(val) \
	if (!gkm_rpc_message_read_byte (cs->req, &val)) \
		{ _ret = PARSE_ERROR; goto _cleanup; }

#define IN_ULONG(val) \
	if (!gkm_rpc_message_read_ulong (cs->req, &val)) \
		{ _ret = PARSE_ERROR; goto _cleanup; }

#define IN_STRING(val) \
	_ret = proto_read_null_string (cs, &val); \
	if (_ret != CKR_OK) goto _cleanup;

#define IN_BYTE_BUFFER(buffer, buffer_len) \
	_ret = proto_read_byte_buffer (cs, &buffer, &buffer_len); \
	if (_ret != CKR_OK) goto _cleanup;

#define IN_BYTE_ARRAY(buffer, buffer_len) \
	_ret = proto_read_byte_array (cs, &buffer, &buffer_len); \
	if (_ret != CKR_OK) goto _cleanup;

#define IN_ULONG_BUFFER(buffer, buffer_len) \
	_ret = proto_read_ulong_buffer (cs, &buffer, &buffer_len); \
	if (_ret != CKR_OK) goto _cleanup;

#define IN_ATTRIBUTE_BUFFER(buffer, buffer_len) \
	_ret = proto_read_attribute_buffer (cs, &buffer, &buffer_len); \
	if (_ret != CKR_OK) goto _cleanup;

#define IN_ATTRIBUTE_ARRAY(attrs, n_attrs) \
	_ret = proto_read_attribute_array (cs, &attrs, &n_attrs); \
	if (_ret != CKR_OK) goto _cleanup;

#define IN_MECHANISM(mech) \
	_ret = proto_read_mechanism (cs, &mech); \
	if (_ret != CKR_OK) goto _cleanup;


#define OUT_ULONG(val) \
	if (_ret == CKR_OK && !gkm_rpc_message_write_ulong (cs->resp, val)) \
		_ret = PREP_ERROR;

#define OUT_BYTE_ARRAY(array, len) \
	/* Note how we filter return codes */ \
	_ret = proto_write_byte_array (cs, array, len, _ret);

#define OUT_ULONG_ARRAY(array, len) \
	/* Note how we filter return codes */ \
	_ret = proto_write_ulong_array (cs, array, len, _ret);

#define OUT_ATTRIBUTE_ARRAY(array, len) \
	/* Note how we filter return codes */ \
	_ret = proto_write_attribute_array (cs, array, len, _ret);

#define OUT_INFO(val) \
	if (_ret == CKR_OK) \
		_ret = proto_write_info (cs, &val);

#define OUT_SLOT_INFO(val) \
	if (_ret == CKR_OK) \
		_ret = proto_write_slot_info (cs, &val);

#define OUT_TOKEN_INFO(val) \
	if (_ret == CKR_OK) \
		_ret = proto_write_token_info (cs, &val);

#define OUT_MECHANISM_INFO(val) \
	if (_ret == CKR_OK) \
		_ret = proto_write_mechanism_info (cs, &val);

#define OUT_SESSION_INFO(val) \
	if (_ret == CKR_OK) \
		_ret = proto_write_session_info (cs, &val);

/* ---------------------------------------------------------------------------
 * DISPATCH SPECIFIC CALLS
 */

static CK_RV
rpc_C_Initialize (CallState *cs)
{
	CK_BYTE_PTR handshake;
	CK_ULONG n_handshake;
	CK_RV ret = CKR_OK;

	debug (("C_Initialize: enter"));

	assert (cs);
	assert (pkcs11_module);

	ret = proto_read_byte_array (cs, &handshake, &n_handshake);
	if (ret == CKR_OK) {

		/* Check to make sure the header matches */
		if (n_handshake != GKM_RPC_HANDSHAKE_LEN ||
		    memcmp (handshake, GKM_RPC_HANDSHAKE, n_handshake) != 0) {
			gkm_rpc_warn ("invalid handshake received from connecting module");
			ret = CKR_GENERAL_ERROR;
		}

		assert (gkm_rpc_message_is_verified (cs->req));
	}

	/*
	 * We don't actually C_Initialize lower layers. It's assumed
	 * that they'll already be initialzied by the code that loaded us.
	 */

	debug (("ret: %d", ret));
	return ret;
}

static CK_RV
rpc_C_Finalize (CallState *cs)
{
	CK_SLOT_ID_PTR slots;
	CK_ULONG n_slots, i;
	CK_RV ret;

	debug (("C_Finalize: enter"));

	assert (cs);
	assert (pkcs11_module);

	/*
	 * We don't actually C_Finalize lower layers, since this would finalize
	 * for all appartments, client applications. Anyway this is done by
	 * the code that loaded us.
	 *
	 * But we do need to cleanup resources used by this client, so instead
	 * we call C_CloseAllSessions for each slot for this client application.
	 */

	if (cs->application.applicationId) {
		ret = (pkcs11_module->C_GetSlotList) (TRUE, NULL, &n_slots);
		if (ret == CKR_OK) {
			slots = calloc (n_slots, sizeof (CK_SLOT_ID));
			if (slots == NULL) {
				ret = CKR_DEVICE_MEMORY;
			} else {
				ret = (pkcs11_module->C_GetSlotList) (TRUE, slots, &n_slots);
				for (i = 0; ret == CKR_OK && i < n_slots; ++i)
					ret = (pkcs11_module->C_CloseAllSessions) (slots[i] | cs->application.applicationId);
				free (slots);
			}
		}
	}

	debug (("ret: %d", ret));
	return ret;
}

static CK_RV
rpc_C_GetInfo (CallState *cs)
{
	CK_INFO info;

	BEGIN_CALL (C_GetInfo);
	PROCESS_CALL ((&info));
		OUT_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_GetSlotList (CallState *cs)
{
	CK_BBOOL token_present;
	CK_SLOT_ID_PTR slot_list;
	CK_ULONG count;

	BEGIN_CALL (C_GetSlotList);
		IN_BYTE (token_present);
		IN_ULONG_BUFFER (slot_list, count);
	PROCESS_CALL ((token_present, slot_list, &count));
		OUT_ULONG_ARRAY (slot_list, count);
	END_CALL;
}

static CK_RV
rpc_C_GetSlotInfo (CallState *cs)
{
	CK_SLOT_ID slot_id;
	CK_SLOT_INFO info;

	/* Slot id becomes appartment so lower layers can tell clients apart. */

	BEGIN_CALL (C_GetSlotInfo);
		IN_ULONG (slot_id);
	PROCESS_CALL ((slot_id, &info));
		OUT_SLOT_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_GetTokenInfo (CallState *cs)
{
	CK_SLOT_ID slot_id;
	CK_TOKEN_INFO info;

	/* Slot id becomes appartment so lower layers can tell clients apart. */

	BEGIN_CALL (C_GetTokenInfo);
		IN_ULONG (slot_id);
	PROCESS_CALL ((slot_id, &info));
		OUT_TOKEN_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_GetMechanismList (CallState *cs)
{
	CK_SLOT_ID slot_id;
	CK_MECHANISM_TYPE_PTR mechanism_list;
	CK_ULONG count;

	/* Slot id becomes appartment so lower layers can tell clients apart. */

	BEGIN_CALL (C_GetMechanismList);
		IN_ULONG (slot_id);
		IN_ULONG_BUFFER (mechanism_list, count);
	PROCESS_CALL ((slot_id, mechanism_list, &count));
		OUT_ULONG_ARRAY (mechanism_list, count);
	END_CALL;
}

static CK_RV
rpc_C_GetMechanismInfo (CallState *cs)
{
	CK_SLOT_ID slot_id;
	CK_MECHANISM_TYPE type;
	CK_MECHANISM_INFO info;

	/* Slot id becomes appartment so lower layers can tell clients apart. */

	BEGIN_CALL (C_GetMechanismInfo);
		IN_ULONG (slot_id);
		IN_ULONG (type);
	PROCESS_CALL ((slot_id, type, &info));
		OUT_MECHANISM_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_InitToken (CallState *cs)
{
	CK_SLOT_ID slot_id;
	CK_UTF8CHAR_PTR pin;
	CK_ULONG pin_len;
	CK_UTF8CHAR_PTR label;

	/* Slot id becomes appartment so lower layers can tell clients apart. */

	BEGIN_CALL (C_InitToken);
		IN_ULONG (slot_id);
		IN_BYTE_ARRAY (pin, pin_len);
		IN_STRING (label);
	PROCESS_CALL ((slot_id, pin, pin_len, label));
	END_CALL;
}

static CK_RV
rpc_C_WaitForSlotEvent (CallState *cs)
{
	CK_FLAGS flags;
	CK_SLOT_ID slot_id;

	/* Get slot id from appartment lower layers use. */

	BEGIN_CALL (C_WaitForSlotEvent);
		IN_ULONG (flags);
	PROCESS_CALL ((flags, &slot_id, NULL));
		OUT_ULONG (slot_id);
	END_CALL;
}

static CK_RV
rpc_C_OpenSession (CallState *cs)
{
	CK_SLOT_ID slot_id;
	CK_FLAGS flags;
	CK_SESSION_HANDLE session;

	/* Slot id becomes appartment so lower layers can tell clients apart. */

	BEGIN_CALL (C_OpenSession);
		IN_ULONG (slot_id);
		IN_ULONG (flags);
		flags |= CKF_G_APPLICATION_SESSION;
	PROCESS_CALL ((slot_id, flags, &cs->application, NULL, &session));
		OUT_ULONG (session);
	END_CALL;
}


static CK_RV
rpc_C_CloseSession (CallState *cs)
{
	CK_SESSION_HANDLE session;

	BEGIN_CALL (C_CloseSession);
		IN_ULONG (session);
	PROCESS_CALL ((session));
	END_CALL;
}

static CK_RV
rpc_C_CloseAllSessions (CallState *cs)
{
	CK_SLOT_ID slot_id;

	/* Slot id becomes appartment so lower layers can tell clients apart. */

	BEGIN_CALL (C_CloseAllSessions);
		IN_ULONG (slot_id);
		slot_id |= cs->application.applicationId;
	PROCESS_CALL ((slot_id));
	END_CALL;
}

static CK_RV
rpc_C_GetFunctionStatus (CallState *cs)
{
	CK_SESSION_HANDLE session;

	BEGIN_CALL (C_GetFunctionStatus);
		IN_ULONG (session);
	PROCESS_CALL ((session));
	END_CALL;
}

static CK_RV
rpc_C_CancelFunction (CallState *cs)
{
	CK_SESSION_HANDLE session;

	BEGIN_CALL (C_CancelFunction);
		IN_ULONG (session);
	PROCESS_CALL ((session));
	END_CALL;
}

static CK_RV
rpc_C_GetSessionInfo (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_SESSION_INFO info;

	/* Get slot id from appartment lower layers use. */

	BEGIN_CALL (C_GetSessionInfo);
		IN_ULONG (session);
	PROCESS_CALL ((session, &info));
		OUT_SESSION_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_InitPIN (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_UTF8CHAR_PTR pin;
	CK_ULONG pin_len;

	BEGIN_CALL (C_InitPIN);
		IN_ULONG (session);
		IN_BYTE_ARRAY (pin, pin_len);
	PROCESS_CALL ((session, pin, pin_len));
	END_CALL;
}

static CK_RV
rpc_C_SetPIN (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_UTF8CHAR_PTR old_pin;
	CK_ULONG old_len;
	CK_UTF8CHAR_PTR new_pin;
	CK_ULONG new_len;

	BEGIN_CALL (C_SetPIN);
		IN_ULONG (session);
		IN_BYTE_ARRAY (old_pin, old_len);
		IN_BYTE_ARRAY (new_pin, new_len);
	PROCESS_CALL ((session, old_pin, old_len, new_pin, new_len));
	END_CALL;
}

static CK_RV
rpc_C_GetOperationState (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR operation_state;
	CK_ULONG operation_state_len;

	BEGIN_CALL (C_GetOperationState);
		IN_ULONG (session);
		IN_BYTE_BUFFER (operation_state, operation_state_len);
	PROCESS_CALL ((session, operation_state, &operation_state_len));
		OUT_BYTE_ARRAY (operation_state, operation_state_len);
	END_CALL;
}

static CK_RV
rpc_C_SetOperationState (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR operation_state;
	CK_ULONG operation_state_len;
	CK_OBJECT_HANDLE encryption_key;
	CK_OBJECT_HANDLE authentication_key;

	BEGIN_CALL (C_SetOperationState);
		IN_ULONG (session);
		IN_BYTE_ARRAY (operation_state, operation_state_len);
		IN_ULONG (encryption_key);
		IN_ULONG (authentication_key);
	PROCESS_CALL ((session, operation_state, operation_state_len, encryption_key, authentication_key));
	END_CALL;
}

static CK_RV
rpc_C_Login (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_USER_TYPE user_type;
	CK_UTF8CHAR_PTR pin;
	CK_ULONG pin_len;

	BEGIN_CALL (C_Login);
		IN_ULONG (session);
		IN_ULONG (user_type);
		IN_BYTE_ARRAY (pin, pin_len);
	PROCESS_CALL ((session, user_type, pin, pin_len));
	END_CALL;
}

static CK_RV
rpc_C_Logout (CallState *cs)
{
	CK_SESSION_HANDLE session;

	BEGIN_CALL (C_Logout);
		IN_ULONG (session);
	PROCESS_CALL ((session));
	END_CALL;
}

/* -----------------------------------------------------------------------------
 * OBJECT OPERATIONS
 */

static CK_RV
rpc_C_CreateObject (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG count;
	CK_OBJECT_HANDLE new_object;

	BEGIN_CALL (C_CreateObject);
		IN_ULONG (session);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL ((session, template, count, &new_object));
		OUT_ULONG (new_object);
	END_CALL;
}

static CK_RV
rpc_C_CopyObject (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG count;
	CK_OBJECT_HANDLE new_object;

	BEGIN_CALL (C_CopyObject);
		IN_ULONG (session);
		IN_ULONG (object);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL ((session, object, template, count, &new_object));
		OUT_ULONG (new_object);
	END_CALL;
}

static CK_RV
rpc_C_DestroyObject (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE object;

	BEGIN_CALL (C_DestroyObject);
		IN_ULONG (session);
		IN_ULONG (object);
	PROCESS_CALL ((session, object));
	END_CALL;
}

static CK_RV
rpc_C_GetObjectSize (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE object;
	CK_ULONG size;

	BEGIN_CALL (C_GetObjectSize);
		IN_ULONG (session);
		IN_ULONG (object);
	PROCESS_CALL ((session, object, &size));
		OUT_ULONG (size);
	END_CALL;
}

static CK_RV
rpc_C_GetAttributeValue (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG count;

	BEGIN_CALL (C_GetAttributeValue);
		IN_ULONG (session);
		IN_ULONG (object);
		IN_ATTRIBUTE_BUFFER (template, count);
	PROCESS_CALL ((session, object, template, count));
		OUT_ATTRIBUTE_ARRAY (template, count);
	END_CALL;
}

static CK_RV
rpc_C_SetAttributeValue (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE object;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG count;

	BEGIN_CALL (C_SetAttributeValue);
		IN_ULONG (session);
		IN_ULONG (object);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL ((session, object, template, count));
	END_CALL;
}

static CK_RV
rpc_C_FindObjectsInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG count;

	BEGIN_CALL (C_FindObjectsInit);
		IN_ULONG (session);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL ((session, template, count));
	END_CALL;
}

static CK_RV
rpc_C_FindObjects (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE_PTR objects;
	CK_ULONG max_object_count;
	CK_ULONG object_count;

	BEGIN_CALL (C_FindObjects);
		IN_ULONG (session);
		IN_ULONG_BUFFER (objects, max_object_count);
	PROCESS_CALL ((session, objects, max_object_count, &object_count));
		OUT_ULONG_ARRAY (objects, object_count);
	END_CALL;
}

static CK_RV
rpc_C_FindObjectsFinal (CallState *cs)
{
	CK_SESSION_HANDLE session;

	BEGIN_CALL (C_FindObjectsFinal);
		IN_ULONG (session);
	PROCESS_CALL ((session));
	END_CALL;
}

static CK_RV
rpc_C_EncryptInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_EncryptInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL ((session, &mechanism, key));
	END_CALL;

}

static CK_RV
rpc_C_Encrypt (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR data;
	CK_ULONG data_len;
	CK_BYTE_PTR encrypted_data;
	CK_ULONG encrypted_data_len;

	BEGIN_CALL (C_Encrypt);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_BUFFER (encrypted_data, encrypted_data_len);
	PROCESS_CALL ((session, data, data_len, encrypted_data, &encrypted_data_len));
		OUT_BYTE_ARRAY (encrypted_data, encrypted_data_len);
	END_CALL;
}

static CK_RV
rpc_C_EncryptUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR part;
	CK_ULONG part_len;
	CK_BYTE_PTR encrypted_part;
	CK_ULONG encrypted_part_len;

	BEGIN_CALL (C_EncryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
		IN_BYTE_BUFFER (encrypted_part, encrypted_part_len);
	PROCESS_CALL ((session, part, part_len, encrypted_part, &encrypted_part_len));
		OUT_BYTE_ARRAY (encrypted_part, encrypted_part_len);
	END_CALL;
}

static CK_RV
rpc_C_EncryptFinal (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR last_encrypted_part;
	CK_ULONG last_encrypted_part_len;

	BEGIN_CALL (C_EncryptFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (last_encrypted_part, last_encrypted_part_len);
	PROCESS_CALL ((session, last_encrypted_part, &last_encrypted_part_len));
		OUT_BYTE_ARRAY (last_encrypted_part, last_encrypted_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_DecryptInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL ((session, &mechanism, key));
	END_CALL;
}

static CK_RV
rpc_C_Decrypt (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR encrypted_data;
	CK_ULONG encrypted_data_len;
	CK_BYTE_PTR data;
	CK_ULONG data_len;

	BEGIN_CALL (C_Decrypt);
		IN_ULONG (session);
		IN_BYTE_ARRAY (encrypted_data, encrypted_data_len);
		IN_BYTE_BUFFER (data, data_len);
	PROCESS_CALL ((session, encrypted_data, encrypted_data_len, data, &data_len));
		OUT_BYTE_ARRAY (data, data_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR encrypted_part;
	CK_ULONG encrypted_part_len;
	CK_BYTE_PTR part;
	CK_ULONG part_len;

	BEGIN_CALL (C_DecryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (encrypted_part, encrypted_part_len);
		IN_BYTE_BUFFER (part, part_len);
	PROCESS_CALL ((session, encrypted_part, encrypted_part_len, part, &part_len));
		OUT_BYTE_ARRAY (part, part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptFinal (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR last_part;
	CK_ULONG last_part_len;

	BEGIN_CALL (C_DecryptFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (last_part, last_part_len);
	PROCESS_CALL ((session, last_part, &last_part_len));
		OUT_BYTE_ARRAY (last_part, last_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DigestInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;

	BEGIN_CALL (C_DigestInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
	PROCESS_CALL ((session, &mechanism));
	END_CALL;
}

static CK_RV
rpc_C_Digest (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR data;
	CK_ULONG data_len;
	CK_BYTE_PTR digest;
	CK_ULONG digest_len;

	BEGIN_CALL (C_Digest);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_BUFFER (digest, digest_len);
	PROCESS_CALL ((session, data, data_len, digest, &digest_len));
		OUT_BYTE_ARRAY (digest, digest_len);
	END_CALL;
}

static CK_RV
rpc_C_DigestUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR part;
	CK_ULONG part_len;

	BEGIN_CALL (C_DigestUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
	PROCESS_CALL ((session, part, part_len));
	END_CALL;
}

static CK_RV
rpc_C_DigestKey (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_DigestKey);
		IN_ULONG (session);
		IN_ULONG (key);
	PROCESS_CALL ((session, key));
	END_CALL;
}

static CK_RV
rpc_C_DigestFinal (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR digest;
	CK_ULONG digest_len;

	BEGIN_CALL (C_DigestFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (digest, digest_len);
	PROCESS_CALL ((session, digest, &digest_len));
		OUT_BYTE_ARRAY (digest, digest_len);
	END_CALL;
}

static CK_RV
rpc_C_SignInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_SignInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL ((session, &mechanism, key));
	END_CALL;
}

static CK_RV
rpc_C_Sign (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR part;
	CK_ULONG part_len;
	CK_BYTE_PTR signature;
	CK_ULONG signature_len;

	BEGIN_CALL (C_Sign);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
		IN_BYTE_BUFFER (signature, signature_len);
	PROCESS_CALL ((session, part, part_len, signature, &signature_len));
		OUT_BYTE_ARRAY (signature, signature_len);
	END_CALL;

}

static CK_RV
rpc_C_SignUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR part;
	CK_ULONG part_len;

	BEGIN_CALL (C_SignUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
	PROCESS_CALL ((session, part, part_len));
	END_CALL;
}

static CK_RV
rpc_C_SignFinal (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR signature;
	CK_ULONG signature_len;

	BEGIN_CALL (C_SignFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (signature, signature_len);
	PROCESS_CALL ((session, signature, &signature_len));
		OUT_BYTE_ARRAY (signature, signature_len);
	END_CALL;
}

static CK_RV
rpc_C_SignRecoverInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_SignRecoverInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL ((session, &mechanism, key));
	END_CALL;
}

static CK_RV
rpc_C_SignRecover (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR data;
	CK_ULONG data_len;
	CK_BYTE_PTR signature;
	CK_ULONG signature_len;

	BEGIN_CALL (C_SignRecover);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_BUFFER (signature, signature_len);
	PROCESS_CALL ((session, data, data_len, signature, &signature_len));
		OUT_BYTE_ARRAY (signature, signature_len);
	END_CALL;
}

static CK_RV
rpc_C_VerifyInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_VerifyInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL ((session, &mechanism, key));
	END_CALL;
}

static CK_RV
rpc_C_Verify (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR data;
	CK_ULONG data_len;
	CK_BYTE_PTR signature;
	CK_ULONG signature_len;

	BEGIN_CALL (C_Verify);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_ARRAY (signature, signature_len);
	PROCESS_CALL ((session, data, data_len, signature, signature_len));
	END_CALL;
}

static CK_RV
rpc_C_VerifyUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR part;
	CK_ULONG part_len;

	BEGIN_CALL (C_VerifyUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
	PROCESS_CALL ((session, part, part_len));
	END_CALL;
}

static CK_RV
rpc_C_VerifyFinal (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR signature;
	CK_ULONG signature_len;

	BEGIN_CALL (C_VerifyFinal);
		IN_ULONG (session);
		IN_BYTE_ARRAY (signature, signature_len);
	PROCESS_CALL ((session, signature, signature_len));
	END_CALL;
}

static CK_RV
rpc_C_VerifyRecoverInit (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_VerifyRecoverInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL ((session, &mechanism, key));
	END_CALL;
}

static CK_RV
rpc_C_VerifyRecover (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR signature;
	CK_ULONG signature_len;
	CK_BYTE_PTR data;
	CK_ULONG data_len;

	BEGIN_CALL (C_VerifyRecover);
		IN_ULONG (session);
		IN_BYTE_ARRAY (signature, signature_len);
		IN_BYTE_BUFFER (data, data_len);
	PROCESS_CALL ((session, signature, signature_len, data, &data_len));
		OUT_BYTE_ARRAY (data, data_len);
	END_CALL;
}

static CK_RV
rpc_C_DigestEncryptUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR part;
	CK_ULONG part_len;
	CK_BYTE_PTR encrypted_part;
	CK_ULONG encrypted_part_len;

	BEGIN_CALL (C_DigestEncryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
		IN_BYTE_BUFFER (encrypted_part, encrypted_part_len);
	PROCESS_CALL ((session, part, part_len, encrypted_part, &encrypted_part_len));
		OUT_BYTE_ARRAY (encrypted_part, encrypted_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptDigestUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR encrypted_part;
	CK_ULONG encrypted_part_len;
	CK_BYTE_PTR part;
	CK_ULONG part_len;

	BEGIN_CALL (C_DecryptDigestUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (encrypted_part, encrypted_part_len);
		IN_BYTE_BUFFER (part, part_len);
	PROCESS_CALL ((session, encrypted_part, encrypted_part_len, part, &part_len));
		OUT_BYTE_ARRAY (part, part_len);
	END_CALL;
}

static CK_RV
rpc_C_SignEncryptUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR part;
	CK_ULONG part_len;
	CK_BYTE_PTR encrypted_part;
	CK_ULONG encrypted_part_len;

	BEGIN_CALL (C_SignEncryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
		IN_BYTE_BUFFER (encrypted_part, encrypted_part_len);
	PROCESS_CALL ((session, part, part_len, encrypted_part, &encrypted_part_len));
		OUT_BYTE_ARRAY (encrypted_part, encrypted_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptVerifyUpdate (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR encrypted_part;
	CK_ULONG encrypted_part_len;
	CK_BYTE_PTR part;
	CK_ULONG part_len;

	BEGIN_CALL (C_DecryptVerifyUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (encrypted_part, encrypted_part_len);
		IN_BYTE_BUFFER (part, part_len);
	PROCESS_CALL ((session, encrypted_part, encrypted_part_len, part, &part_len));
		OUT_BYTE_ARRAY (part, part_len);
	END_CALL;
}

/* -----------------------------------------------------------------------------
 * KEY OPERATIONS
 */

static CK_RV
rpc_C_GenerateKey (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG count;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_GenerateKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL ((session, &mechanism, template, count, &key));
		OUT_ULONG (key);
	END_CALL;
}

static CK_RV
rpc_C_GenerateKeyPair (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_ATTRIBUTE_PTR public_key_template;
	CK_ULONG public_key_attribute_count;
	CK_ATTRIBUTE_PTR private_key_template;
	CK_ULONG private_key_attribute_count;
	CK_OBJECT_HANDLE public_key;
	CK_OBJECT_HANDLE private_key;

	BEGIN_CALL (C_GenerateKeyPair);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ATTRIBUTE_ARRAY (public_key_template, public_key_attribute_count);
		IN_ATTRIBUTE_ARRAY (private_key_template, private_key_attribute_count);
	PROCESS_CALL ((session, &mechanism, public_key_template, public_key_attribute_count, private_key_template, private_key_attribute_count, &public_key, &private_key));
		OUT_ULONG (public_key);
		OUT_ULONG (private_key);
	END_CALL;

}

static CK_RV
rpc_C_WrapKey (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE wrapping_key;
	CK_OBJECT_HANDLE key;
	CK_BYTE_PTR wrapped_key;
	CK_ULONG wrapped_key_len;

	BEGIN_CALL (C_WrapKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (wrapping_key);
		IN_ULONG (key);
		IN_BYTE_BUFFER (wrapped_key, wrapped_key_len);
	PROCESS_CALL ((session, &mechanism, wrapping_key, key, wrapped_key, &wrapped_key_len));
		OUT_BYTE_ARRAY (wrapped_key, wrapped_key_len);
	END_CALL;
}

static CK_RV
rpc_C_UnwrapKey (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE unwrapping_key;
	CK_BYTE_PTR wrapped_key;
	CK_ULONG wrapped_key_len;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG attribute_count;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_UnwrapKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (unwrapping_key);
		IN_BYTE_ARRAY (wrapped_key, wrapped_key_len);
		IN_ATTRIBUTE_ARRAY (template, attribute_count);
	PROCESS_CALL ((session, &mechanism, unwrapping_key, wrapped_key, wrapped_key_len, template, attribute_count, &key));
		OUT_ULONG (key);
	END_CALL;
}

static CK_RV
rpc_C_DeriveKey (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_MECHANISM mechanism;
	CK_OBJECT_HANDLE base_key;
	CK_ATTRIBUTE_PTR template;
	CK_ULONG attribute_count;
	CK_OBJECT_HANDLE key;

	BEGIN_CALL (C_DeriveKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (base_key);
		IN_ATTRIBUTE_ARRAY (template, attribute_count);
	PROCESS_CALL ((session, &mechanism, base_key, template, attribute_count, &key));
		OUT_ULONG (key);
	END_CALL;
}

static CK_RV
rpc_C_SeedRandom (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR seed;
	CK_ULONG seed_len;

	BEGIN_CALL (C_SeedRandom);
		IN_ULONG (session);
		IN_BYTE_ARRAY (seed, seed_len);
	PROCESS_CALL ((session, seed, seed_len));
	END_CALL;
}

static CK_RV
rpc_C_GenerateRandom (CallState *cs)
{
	CK_SESSION_HANDLE session;
	CK_BYTE_PTR random_data;
	CK_ULONG random_len;

	BEGIN_CALL (C_GenerateRandom);
		IN_ULONG (session);
		IN_BYTE_BUFFER (random_data, random_len);
	PROCESS_CALL ((session, random_data, random_len));
		OUT_BYTE_ARRAY (random_data, random_len);
	END_CALL;
}

/* ---------------------------------------------------------------------------
 * DISPATCH THREAD HANDLING
 */

static int
dispatch_call (CallState *cs)
{
	GkmRpcMessage *req, *resp;
	CK_RV ret = CKR_OK;

	assert (cs);

	req = cs->req;
	resp = cs->resp;

	/* This should have been checked by the parsing code */
	assert (req->call_id > GKM_RPC_CALL_ERROR);
	assert (req->call_id < GKM_RPC_CALL_MAX);

	/* Prepare a response for the function to fill in */
	if (!gkm_rpc_message_prep (resp, req->call_id, GKM_RPC_RESPONSE)) {
		gkm_rpc_warn ("couldn't prepare message");
		return 0;
	}

	switch(req->call_id) {

	#define CASE_CALL(name) \
		case GKM_RPC_CALL_##name: \
			ret = rpc_##name (cs); \
			break;
	CASE_CALL(C_Initialize)
	CASE_CALL(C_Finalize)
	CASE_CALL(C_GetInfo)
	CASE_CALL(C_GetSlotList)
	CASE_CALL(C_GetSlotInfo)
	CASE_CALL(C_GetTokenInfo)
	CASE_CALL(C_GetMechanismList)
	CASE_CALL(C_GetMechanismInfo)
	CASE_CALL(C_InitToken)
	CASE_CALL(C_WaitForSlotEvent)
	CASE_CALL(C_OpenSession)
	CASE_CALL(C_CloseSession)
	CASE_CALL(C_CloseAllSessions)
	CASE_CALL(C_GetFunctionStatus)
	CASE_CALL(C_CancelFunction)
	CASE_CALL(C_GetSessionInfo)
	CASE_CALL(C_InitPIN)
	CASE_CALL(C_SetPIN)
	CASE_CALL(C_GetOperationState)
	CASE_CALL(C_SetOperationState)
	CASE_CALL(C_Login)
	CASE_CALL(C_Logout)
	CASE_CALL(C_CreateObject)
	CASE_CALL(C_CopyObject)
	CASE_CALL(C_DestroyObject)
	CASE_CALL(C_GetObjectSize)
	CASE_CALL(C_GetAttributeValue)
	CASE_CALL(C_SetAttributeValue)
	CASE_CALL(C_FindObjectsInit)
	CASE_CALL(C_FindObjects)
	CASE_CALL(C_FindObjectsFinal)
	CASE_CALL(C_EncryptInit)
	CASE_CALL(C_Encrypt)
	CASE_CALL(C_EncryptUpdate)
	CASE_CALL(C_EncryptFinal)
	CASE_CALL(C_DecryptInit)
	CASE_CALL(C_Decrypt)
	CASE_CALL(C_DecryptUpdate)
	CASE_CALL(C_DecryptFinal)
	CASE_CALL(C_DigestInit)
	CASE_CALL(C_Digest)
	CASE_CALL(C_DigestUpdate)
	CASE_CALL(C_DigestKey)
	CASE_CALL(C_DigestFinal)
	CASE_CALL(C_SignInit)
	CASE_CALL(C_Sign)
	CASE_CALL(C_SignUpdate)
	CASE_CALL(C_SignFinal)
	CASE_CALL(C_SignRecoverInit)
	CASE_CALL(C_SignRecover)
	CASE_CALL(C_VerifyInit)
	CASE_CALL(C_Verify)
	CASE_CALL(C_VerifyUpdate)
	CASE_CALL(C_VerifyFinal)
	CASE_CALL(C_VerifyRecoverInit)
	CASE_CALL(C_VerifyRecover)
	CASE_CALL(C_DigestEncryptUpdate)
	CASE_CALL(C_DecryptDigestUpdate)
	CASE_CALL(C_SignEncryptUpdate)
	CASE_CALL(C_DecryptVerifyUpdate)
	CASE_CALL(C_GenerateKey)
	CASE_CALL(C_GenerateKeyPair)
	CASE_CALL(C_WrapKey)
	CASE_CALL(C_UnwrapKey)
	CASE_CALL(C_DeriveKey)
	CASE_CALL(C_SeedRandom)
	CASE_CALL(C_GenerateRandom)
	#undef CASE_CALL

	default:
		/* This should have been caught by the parse code */
		assert (0 && "Unchecked call");
		break;
	};

	if (ret == CKR_OK) {

		/* Parsing errors? */
		if (gkm_rpc_message_buffer_error (req)) {
			gkm_rpc_warn ("invalid request from module, probably too short");
			ret = PARSE_ERROR;
		}

		/* Out of memory errors? */
		if (gkm_rpc_message_buffer_error (resp)) {
			gkm_rpc_warn ("out of memory error putting together message");
			ret = PREP_ERROR;
		}
	}

	/* A filled in response */
	if (ret == CKR_OK) {

		/*
		 * Since we're dealing with many many functions above generating
		 * these messages we want to make sure each of them actually
		 * does what it's supposed to.
		 */

		assert (gkm_rpc_message_is_verified (resp));
		assert (resp->call_type == GKM_RPC_RESPONSE);
		assert (resp->call_id == req->call_id);
		assert (gkm_rpc_calls[resp->call_id].response);
		assert (strcmp (gkm_rpc_calls[resp->call_id].response,
		                resp->signature) == 0);

	/* Fill in an error respnose */
	} else {
		if (!gkm_rpc_message_prep (resp, GKM_RPC_CALL_ERROR, GKM_RPC_RESPONSE) ||
		    !gkm_rpc_message_write_ulong (resp, (uint32_t)ret) ||
		    gkm_rpc_message_buffer_error (resp)) {
			gkm_rpc_warn ("out of memory responding with error");
			return 0;
		}
	}

	return 1;
}

static int
read_all (int sock, unsigned char* data, size_t len)
{
	int r;

	assert (sock >= 0);
	assert (data);
	assert (len > 0);

	while (len > 0) {

		r = read (sock, data, len);

		if (r == 0) {
			/* Connection was closed on client */
			return 0;
		} else if (r == -1) {
			if (errno != EAGAIN && errno != EINTR) {
				gkm_rpc_warn ("couldn't receive data: %s", strerror (errno));
				return 0;
			}
		} else {
			data += r;
			len -= r;
		}
	}

	return 1;
}

static int
write_all (int sock, unsigned char* data, size_t len)
{
	int r;

	assert (sock >= 0);
	assert (data);
	assert (len > 0);

	while (len > 0) {

		r = write (sock, data, len);

		if (r == -1) {
			if (errno == EPIPE) {
				/* Connection closed from client */
				return 0;
			} else if (errno != EAGAIN && errno != EINTR) {
				gkm_rpc_warn ("couldn't send data: %s", strerror (errno));
				return 0;
			}
		} else {
			data += r;
			len -= r;
		}
	}

	return 1;
}

static void
run_dispatch_loop (int sock)
{
	CallState cs;
	pid_t pid;
	uid_t uid;
	unsigned char buf[4];
	uint32_t len;

	assert (sock != -1);

	if (!egg_unix_credentials_read (sock, &pid, &uid) < 0) {
		gkm_rpc_warn ("couldn't read socket credentials");
		return;
	}

	/* Setup our buffers */
	if (!call_init (&cs)) {
		gkm_rpc_warn ("out of memory");
		return;
	}

	/* The main thread loop */
	while (TRUE) {

		call_reset (&cs);

		/* Read the number of bytes ... */
		if (!read_all (sock, buf, 4))
			break;

		/* Calculate the number of bytes */
		len = egg_buffer_decode_uint32 (buf);
		if (len >= 0x0FFFFFFF) {
			gkm_rpc_warn ("invalid message size from module: %u bytes", len);
			break;
		}

		/* Allocate memory */
		egg_buffer_reserve (&cs.req->buffer, cs.req->buffer.len + len);
		if (egg_buffer_has_error (&cs.req->buffer)) {
			gkm_rpc_warn ("error allocating buffer for message");
			break;
		}

		/* ... and read/parse in the actual message */
		if (!read_all (sock, cs.req->buffer.buf, len))
			break;

		egg_buffer_add_empty (&cs.req->buffer, len);

		if (!gkm_rpc_message_parse (cs.req, GKM_RPC_REQUEST))
			break;

		/* ... send for processing ... */
		if (!dispatch_call (&cs))
			break;

		/* .. send back response length, and then response data */
		egg_buffer_encode_uint32 (buf, cs.resp->buffer.len);
		if(!write_all (sock, buf, 4) ||
		   !write_all (sock, cs.resp->buffer.buf, cs.resp->buffer.len))
			break;
	}

	/*
	 * Close all sessions for the client application
	 *
	 * EXTENSION: In our extended application PKCS#11 model
	 * C_CloseAllSessions accepts an application identifier as well
	 * as slot ids. Calling with an application identifier closes all
	 * sessions for just that application identifier.
	 */
	if (cs.application.applicationId)
		(pkcs11_module->C_CloseAllSessions) (cs.application.applicationId);

	call_uninit (&cs);
}

static void*
run_dispatch_thread (void *arg)
{
	int *sock = arg;
	assert (*sock != -1);

	run_dispatch_loop (*sock);

	/* The thread closes the socket and marks as done */
	assert (*sock != -1);
	close (*sock);
	*sock = -1;

	return NULL;
}

/* ---------------------------------------------------------------------------
 * MAIN THREAD
 */

typedef struct _DispatchState {
	struct _DispatchState *next;
	GThread *thread;
	int socket;
} DispatchState;

/* The main daemon socket that we're listening on */
static int pkcs11_socket = -1;

/* The unix socket path, that we listen on */
static char *pkcs11_socket_path = NULL;

/* A linked list of dispatcher threads */
static DispatchState *pkcs11_dispatchers = NULL;

void
gkm_rpc_layer_accept (void)
{
	struct sockaddr_un addr;
	DispatchState *ds, **here;
	GError *error = NULL;
	socklen_t addrlen;
	int new_fd;

	assert (pkcs11_socket != -1);

	/* Cleanup any completed dispatch threads */
	for (here = &pkcs11_dispatchers, ds = *here; ds != NULL; ds = *here) {
		if (ds->socket == -1) {
			g_thread_join (ds->thread);
			*here = ds->next;
			free (ds);
		} else {
			here = &ds->next;
		}
	}

	addrlen = sizeof (addr);
	new_fd = accept (pkcs11_socket, (struct sockaddr*) &addr, &addrlen);
	if (new_fd < 0) {
		gkm_rpc_warn ("cannot accept pkcs11 connection: %s", strerror (errno));
		return;
	}

	ds = calloc (1, sizeof (DispatchState));
	if (ds == NULL) {
		gkm_rpc_warn ("out of memory");
		close (new_fd);
		return;
	}

	ds->socket = new_fd;

	ds->thread = g_thread_create (run_dispatch_thread, &(ds->socket), TRUE, &error);
	if (!ds->thread) {
		gkm_rpc_warn ("couldn't start thread: %s", egg_error_message (error));
		close (new_fd);
		free (ds);
		return;
	}

	ds->next = pkcs11_dispatchers;
	pkcs11_dispatchers = ds;
}

int
gkm_rpc_layer_initialize (CK_FUNCTION_LIST_PTR module)
{
	assert (module);

	/* cannot be called more than once */
	assert (!pkcs11_module);
	pkcs11_module = module;
	return 1;
}

void
gkm_rpc_layer_uninitialize (void)
{
	DispatchState *ds, *next;

	if (!pkcs11_module)
		return;

	/* Close our main listening socket */
	if (pkcs11_socket != -1)
		close (pkcs11_socket);
	pkcs11_socket = -1;

	/* Delete our unix socket */
	if(pkcs11_socket_path) {
		unlink (pkcs11_socket_path);
		free (pkcs11_socket_path);
		pkcs11_socket_path = NULL;
	}

	ds = pkcs11_dispatchers;
	pkcs11_dispatchers = NULL;

	/* Stop all of the dispatch threads */
	for (; ds; ds = next) {
		next = ds->next;

		/* Forcibly shutdown the connection */
		if (ds->socket)
			shutdown (ds->socket, SHUT_RDWR);
		g_thread_join (ds->thread);

		/* This is always closed by dispatch thread */
		assert (ds->socket == -1);
		free (ds);
	}

	pkcs11_module = NULL;
}

int
gkm_rpc_layer_startup (const char *prefix)
{
	struct sockaddr_un addr;
	int sock;

#ifdef _DEBUG
	GKM_RPC_CHECK_CALLS ();
#endif

	assert (prefix);

	/* cannot be called more than once */
	assert (pkcs11_socket == -1);
	assert (pkcs11_dispatchers == NULL);

	free (pkcs11_socket_path);
	pkcs11_socket_path = malloc (strlen (prefix) + strlen ("/pkcs11") + 1);
	if (pkcs11_socket_path == NULL) {
		gkm_rpc_warn ("couldn't allocate memory");
		return -1;
	}
	sprintf (pkcs11_socket_path, "%s/pkcs11", prefix);

	sock = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		gkm_rpc_warn ("couldn't create pkcs11 socket: %s", strerror (errno));
		return -1;
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	unlink (pkcs11_socket_path);
	strncpy (addr.sun_path, pkcs11_socket_path, sizeof (addr.sun_path));
	if (bind (sock, (struct sockaddr*)&addr, sizeof (addr)) < 0) {
		gkm_rpc_warn ("couldn't bind to pkcs11 socket: %s: %s",
		                  pkcs11_socket_path, strerror (errno));
		return -1;
	}

	if (listen (sock, 128) < 0) {
		gkm_rpc_warn ("couldn't listen on pkcs11 socket: %s: %s",
		                  pkcs11_socket_path, strerror (errno));
		return -1;
	}

	pkcs11_socket = sock;
	pkcs11_dispatchers = NULL;

	return sock;
}

void
gkm_rpc_layer_shutdown (void)
{
	DispatchState *ds, *next;

	/* Close our main listening socket */
	if (pkcs11_socket != -1)
		close (pkcs11_socket);
	pkcs11_socket = -1;

	/* Delete our unix socket */
	if(pkcs11_socket_path) {
		unlink (pkcs11_socket_path);
		free (pkcs11_socket_path);
		pkcs11_socket_path = NULL;
	}

	ds = pkcs11_dispatchers;
	pkcs11_dispatchers = NULL;

	/* Stop all of the dispatch threads */
	for (; ds; ds = next) {
		next = ds->next;

		/* Forcibly shutdown the connection */
		if (ds->socket)
			shutdown (ds->socket, SHUT_RDWR);
		g_thread_join (ds->thread);

		/* This is always closed by dispatch thread */
		assert (ds->socket == -1);
		free (ds);
	}
}
