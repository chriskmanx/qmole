/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-pkcs11-rpc-module.c - a PKCS#11 module which communicates with another process

   Copyright (C) 2008, Stefan Walter

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

#include "egg/egg-unix-credentials.h"

#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <stdlib.h>
#include <limits.h>
#include <ctype.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

/* -------------------------------------------------------------------
 * GLOBALS / DEFINES
 */

/* Various mutexes */
static pthread_mutex_t init_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Whether we've been initialized, and on what process id it happened */
static int pkcs11_initialized = 0;
static pid_t pkcs11_initialized_pid = 0;

/* The socket to connect to */
static char *pkcs11_socket_path = NULL;

/* The error used by us when parsing of rpc message fails */
#define PARSE_ERROR   CKR_DEVICE_ERROR

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
	if (!(x)) { gkm_rpc_warn ("'%s' not true at %s", #x, __func__); return v; }

void
gkm_rpc_log (const char *line)
{
	fprintf (stderr, "%s\n", line);
}

/* -----------------------------------------------------------------------------
 * MODULE ARGUMENTS
 */

static void
parse_argument (char *arg)
{
	char *value;

	value = arg + strcspn (arg, ":=");
	if (!*value)
		value = NULL;
	else
		*(value++) = 0;

	/* Setup the socket path from the arguments */
	if (strcmp (arg, "socket") == 0) {
		free (pkcs11_socket_path);
		pkcs11_socket_path = strdup (value);
	} else {
		warning (("unrecognized argument: %s", arg));
	}
}

static void
parse_arguments (const char *string)
{
	char quote = '\0';
	char *src, *dup, *at, *arg;

	if (!string)
		return;

	src = dup = strdup (string);
	if (!dup) {
		warning (("couldn't allocate memory for argument string"));
		return;
	}

	arg = at = src;
	for (src = dup; *src; src++) {

		/* Matching quote */
		if (quote == *src) {
			quote = '\0';

		/* Inside of quotes */
		} else if (quote != '\0') {
			if (*src == '\\') {
				*at++ = *src++;
				if (!*src) {
					warning (("couldn't parse argument string: %s", string));
					goto done;
				}
				if (*src != quote)
					*at++ = '\\';
			}
			*at++ = *src;

		/* Space, not inside of quotes */
		} else if (isspace(*src)) {
			*at = 0;
			parse_argument (arg);
			arg = at;

		/* Other character outside of quotes */
		} else {
			switch (*src) {
			case '\'':
			case '"':
				quote = *src;
				break;
			case '\\':
				*at++ = *src++;
				if (!*src) {
					warning (("couldn't parse argument string: %s", string));
					goto done;
				}
				/* fall through */
			default:
				*at++ = *src;
				break;
			}
		}
	}


	if (at != arg)
		parse_argument (arg);

done:
	free (dup);
}

/* -----------------------------------------------------------------------------
 * CALL SESSION
 */

enum CallStatus {
	CALL_INVALID,
	CALL_READY,
	CALL_PREP,
	CALL_TRANSIT,
	CALL_PARSE
};

typedef struct _CallState {
	int socket;                  /* The connection we're sending on */
	GkmRpcMessage *req;          /* The current request */
	GkmRpcMessage *resp;         /* The current response */
	int call_status;
	struct _CallState *next;     /* For pooling of completed sockets */
} CallState;

/* Maximum number of idle calls */
#define MAX_CALL_STATE_POOL 8

/* All call unused call states are in this list */
static CallState *call_state_pool = NULL;
static unsigned int n_call_state_pool = 0;

/* Mutex to protect above call state list */
static pthread_mutex_t call_state_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Allocator for call session buffers */
static void*
call_allocator (void* p, size_t sz)
{
	void* res = realloc (p, (size_t)sz);
	if (!res && sz)
		warning (("memory allocation of %lu bytes failed", sz));
	return res;
}

static CK_RV
call_connect (CallState *cs)
{
	struct sockaddr_un addr;
	int sock;

	assert (cs);
	assert (cs->socket == -1);
	assert (cs->call_status == CALL_INVALID);

	if (!pkcs11_socket_path) {
		warning (("no socket to connect to"));
		return CKR_DEVICE_REMOVED;
	}

	debug (("connecting to: %s", pkcs11_socket_path));

	addr.sun_family = AF_UNIX;
	strncpy (addr.sun_path, pkcs11_socket_path, sizeof (addr.sun_path));

	sock = socket (AF_UNIX, SOCK_STREAM, 0);
	if (sock < 0) {
		warning (("couldn't open socket: %s", strerror (errno)));
		return CKR_DEVICE_ERROR;
	}

	/* close on exec */
	if (fcntl (sock, F_SETFD, 1) == -1) {
		close (sock);
		warning (("couldn't secure socket: %s", strerror (errno)));
		return CKR_DEVICE_ERROR;
	}

	if (connect (sock, (struct sockaddr*) &addr, sizeof (addr)) < 0) {
		close (sock);
		warning (("couldn't connect to: %s: %s", pkcs11_socket_path, strerror (errno)));
		return CKR_DEVICE_ERROR;
	}

	if (egg_unix_credentials_write (sock) < 0) {
		close (sock);
		warning (("couldn't send socket credentials: %s", strerror (errno)));
		return CKR_DEVICE_ERROR;
	}

	cs->socket = sock;
	cs->call_status = CALL_READY;
	debug (("connected socket"));

	return CKR_OK;
}

static void
call_disconnect (CallState *cs)
{
	assert (cs);

	if (cs->socket != -1) {
		debug (("disconnected socket"));
		close (cs->socket);
		cs->socket = -1;
	}
}

static void
call_destroy (void *value)
{
	CallState *cs = value;

	if (value) {
		call_disconnect (cs);
		assert (cs->socket == -1);

		gkm_rpc_message_free (cs->req);
		gkm_rpc_message_free (cs->resp);

		free (cs);

		debug (("destroyed state"));
	}
}

static CK_RV
call_lookup (CallState **ret)
{
	CallState *cs = NULL;
	CK_RV rv;

	assert (ret);

	pthread_mutex_lock (&call_state_mutex);

		/* Pop one from the pool if possible */
		if (call_state_pool != NULL) {
			cs = call_state_pool;
			call_state_pool = cs->next;
			cs->next = NULL;
			assert (n_call_state_pool > 0);
			--n_call_state_pool;
		}

	pthread_mutex_unlock (&call_state_mutex);

	if (cs == NULL) {
		cs = calloc(1, sizeof (CallState));
		if (cs == NULL)
			return CKR_HOST_MEMORY;
		cs->socket = -1;
		cs->call_status = CALL_INVALID;

		/* Try to connect the call */
		rv = call_connect (cs);
		if (rv != CKR_OK) {
			free (cs);
			return rv;
		}
	}

	assert (cs->call_status == CALL_READY);
	assert (cs->socket != -1);
	assert (cs->next == NULL);
	*ret = cs;
	return CKR_OK;
}

/* Perform the initial setup for a new call. */
static CK_RV
call_prepare (CallState *cs, int call_id)
{
	assert (cs);
	assert (cs->call_status == CALL_READY);

	/* Allocate a new request if we've lost the old one */
	if (!cs->req) {
		cs->req = gkm_rpc_message_new (call_allocator);
		if (!cs->req) {
			warning (("cannot allocate request buffer: out of memory"));
			return CKR_HOST_MEMORY;
		}
	}

	/* Put in the Call ID and signature */
	gkm_rpc_message_reset (cs->req);
	if (!gkm_rpc_message_prep (cs->req, call_id, GKM_RPC_REQUEST))
		return CKR_HOST_MEMORY;

	debug (("prepared call: %d", call_id));

	/* Ready to fill in arguments */
	cs->call_status = CALL_PREP;
	return CKR_OK;
}

/* Write all data to session socket.  */
static CK_RV
call_write (CallState *cs, unsigned char* data, size_t len)
{
	int fd, r;

	assert (cs);
	assert (data);
	assert (len > 0);

	while (len > 0) {

		fd = cs->socket;
		if (fd == -1) {
			warning (("couldn't send data: socket has been closed"));
			return CKR_DEVICE_ERROR;
		}

		r = write (fd, data, len);

		if (r == -1) {
			if (errno == EPIPE) {
				warning (("couldn't send data: daemon closed connection"));
				call_disconnect (cs);
				return CKR_DEVICE_ERROR;
			} else if (errno != EAGAIN && errno != EINTR) {
				warning (("couldn't send data: %s", strerror (errno)));
				return CKR_DEVICE_ERROR;
			}
		} else {
			debug (("wrote %d bytes", r));
			data += r;
			len -= r;
		}
	}

	return CKR_OK;
}

/* Read a certain amount of data from session socket. */
static CK_RV
call_read (CallState *cs, unsigned char* data, size_t len)
{
	int fd, r;

	assert (cs);
	assert (data);
	assert (len > 0);

	while (len > 0) {

		fd = cs->socket;
		if (fd == -1) {
			warning (("couldn't receive data: session socket has been closed"));
			return CKR_DEVICE_ERROR;
		}

		r = read (fd, data, len);

		if (r == 0) {
			warning (("couldn't receive data: daemon closed connection"));
			call_disconnect (cs);
			return CKR_DEVICE_ERROR;
		} else if (r == -1) {
			if (errno != EAGAIN && errno != EINTR) {
				warning (("couldn't receive data: %s", strerror (errno)));
				return CKR_DEVICE_ERROR;
			}
		} else {
			debug (("read %d bytes", r));
			data += r;
			len -= r;
		}
	}

	return CKR_OK;
}

/*
 * Used by call_session_do_call() to actually send the message to the daemon.
 * Note how we unlock and relock the session during the call.
 */
static CK_RV
call_send_recv (CallState *cs)
{
	GkmRpcMessage *req, *resp;
	unsigned char buf[4];
	uint32_t len;
	CK_RV ret;

	assert (cs);
	assert (cs->req);
	assert (cs->call_status == CALL_PREP);

	cs->call_status = CALL_TRANSIT;

	/* Setup the response buffer properly */
	if (!cs->resp) {
		/* TODO: Do secrets or passwords ever flow through here? */
		cs->resp = gkm_rpc_message_new (call_allocator);
		if (!cs->resp) {
			warning (("couldn't allocate response buffer: out of memory"));
			return CKR_HOST_MEMORY;
		}
	}
	gkm_rpc_message_reset (cs->resp);

	/*
	 * Now as an additional check to make sure nothing nasty will
	 * happen while we are unlocked, we remove the request and
	 * response from the session during the action.
	 */
	req = cs->req;
	resp = cs->resp;
	cs->req = cs->resp = NULL;

	/* Send the number of bytes, and then the data */
	egg_buffer_encode_uint32 (buf, req->buffer.len);
	ret = call_write (cs, buf, 4);
	if (ret != CKR_OK)
		goto cleanup;
	ret = call_write (cs, req->buffer.buf, req->buffer.len);
	if (ret != CKR_OK)
		goto cleanup;

	/* Now read out the number of bytes, and then the data */
	ret = call_read (cs, buf, 4);
	if (ret != CKR_OK)
		goto cleanup;
	len = egg_buffer_decode_uint32 (buf);
	if (!egg_buffer_reserve (&resp->buffer, len + resp->buffer.len)) {
		warning (("couldn't allocate %u byte response area: out of memory", len));
		ret = CKR_HOST_MEMORY;
		goto cleanup;
	}
	ret = call_read (cs, resp->buffer.buf, len);
	if (ret != CKR_OK)
		goto cleanup;

	egg_buffer_add_empty (&resp->buffer, len);
	if (!gkm_rpc_message_parse (resp, GKM_RPC_RESPONSE))
		goto cleanup;

	debug (("received response from daemon"));

cleanup:
	/* Make sure nobody else used this thread while unlocked */
	assert (cs->call_status == CALL_TRANSIT);
	assert (cs->resp == NULL);
	cs->resp = resp;
	assert (cs->req == NULL);
	cs->req = req;

	return ret;
}

/*
 * At this point the request is ready. So we validate it, and we send it to
 * the daemon for a response.
 */
static CK_RV
call_run (CallState *cs)
{
	CK_RV ret = CKR_OK;
	CK_ULONG ckerr;

	assert (cs);
	assert (cs->req);
	assert (cs->call_status == CALL_PREP);
	assert (cs->socket != -1);

	/* Did building the call fail? */
	if (gkm_rpc_message_buffer_error (cs->req)) {
		warning (("couldn't allocate request area: out of memory"));
		return CKR_HOST_MEMORY;
	}

	/* Make sure that the signature is valid */
	assert (gkm_rpc_message_is_verified (cs->req));

	/* Do the dialog with daemon */
	ret = call_send_recv (cs);

	cs->call_status = CALL_PARSE;

	if (ret != CKR_OK)
		return ret;

	/* If it's an error code then return it */
	if (cs->resp->call_id == GKM_RPC_CALL_ERROR) {

		if (!gkm_rpc_message_read_ulong (cs->resp, &ckerr)) {
			warning (("invalid error response from gnome-keyring-daemon: too short"));
			return CKR_DEVICE_ERROR;
		}

		if (ckerr <= CKR_OK) {
			warning (("invalid error response from gnome-keyring-daemon: bad error code"));
			return CKR_DEVICE_ERROR;
		}

		/* An error code from the daemon */
		return (CK_RV)ckerr;
	}

	/* Make sure daemon answered the right call */
	if (cs->req->call_id != cs->resp->call_id) {
		warning (("invalid response from gnome-keyring-daemon: call mismatch"));
		return CKR_DEVICE_ERROR;
	}

	assert (!gkm_rpc_message_buffer_error (cs->resp));
	debug (("parsing response values"));

	return CKR_OK;
}

static CK_RV
call_done (CallState *cs, CK_RV ret)
{
	assert (cs);
	assert (cs->call_status > CALL_INVALID);

	if (cs->call_status == CALL_PARSE && cs->req && cs->resp) {

		/* Check for parsing errors that were not caught elsewhere */
		if (ret == CKR_OK) {

			if (gkm_rpc_message_buffer_error (cs->resp)) {
				warning (("invalid response from gnome-keyring-daemon: bad argument data"));
				ret = CKR_GENERAL_ERROR;
			} else {
				/* Double check that the signature matched our decoding */
				assert (gkm_rpc_message_is_verified (cs->resp));
			}
		}
	}

	/* Certain error codes cause us to discard the conenction */
	if (ret != CKR_DEVICE_ERROR && ret != CKR_DEVICE_REMOVED && cs->socket != -1) {

		/* Try and stash it away for later use */
		pthread_mutex_lock (&call_state_mutex);

			if (n_call_state_pool < MAX_CALL_STATE_POOL) {
				cs->call_status = CALL_READY;
				assert (cs->next == NULL);
				cs->next = call_state_pool;
				call_state_pool = cs;
				++n_call_state_pool;
				cs = NULL;
			}

		pthread_mutex_unlock (&call_state_mutex);
	}

	if (cs != NULL)
		call_destroy (cs);

	return ret;
}

/* -----------------------------------------------------------------------------
 * MODULE SPECIFIC PROTOCOL CODE
 */

static CK_RV
proto_read_attribute_array (GkmRpcMessage *msg, CK_ATTRIBUTE_PTR arr, CK_ULONG len)
{
	uint32_t i, num, value, type;
	CK_ATTRIBUTE_PTR attr;
	const unsigned char *attrval;
	size_t attrlen;
	unsigned char validity;
	CK_RV ret;

	assert (len);
	assert (msg);

	/* Make sure this is in the right order */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "aA"));

	/* Get the number of items. We need this value to be correct */
	if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &num))
		return PARSE_ERROR;

	if (len != num) {

		/*
		 * This should never happen in normal operation. It denotes a goof up
		 * on the other side of our RPC. We should be indicating the exact number
		 * of attributes to the other side. And it should respond with the same
		 * number.
		 */

		warning (("received an attribute array with wrong number of attributes"));
		return PARSE_ERROR;
	}

	ret = CKR_OK;

	/* We need to go ahead and read everything in all cases */
	for (i = 0; i < num; ++i) {

		/* The attribute type */
		egg_buffer_get_uint32 (&msg->buffer, msg->parsed,
		                       &msg->parsed, &type);

		/* Attribute validity */
		egg_buffer_get_byte (&msg->buffer, msg->parsed,
		                     &msg->parsed, &validity);

		/* And the data itself */
		if (validity) {
			if (egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &value) &&
			    egg_buffer_get_byte_array (&msg->buffer, msg->parsed, &msg->parsed, &attrval, &attrlen)) {
				if (attrval && value != attrlen) {
					warning (("attribute length does not match attribute data"));
					return PARSE_ERROR;
				}
				attrlen = value;
			}
		}

		/* Don't act on this data unless no errors */
		if (egg_buffer_has_error (&msg->buffer))
			break;

		/* Try and stuff it in the output data */
		if (arr) {
			attr = &(arr[i]);
			if (attr->type != type) {
				warning (("returned attributes in invalid order"));
				return PARSE_ERROR;
			}

			if (validity) {
				/* Just requesting the attribute size */
				if (!attr->pValue) {
					attr->ulValueLen = attrlen;

				/* Wants attribute data, but too small */
				} else if (attr->ulValueLen < attrlen) {
					attr->ulValueLen = attrlen;
					ret = CKR_BUFFER_TOO_SMALL;

				/* Wants attribute data, value is null */
				} else if (attrval == NULL) {
					attr->ulValueLen = 0;

				/* Wants attribute data, enough space */
				} else {
					attr->ulValueLen = attrlen;
					memcpy (attr->pValue, attrval, attrlen);
				}

			/* Not a valid attribute */
			} else {
				attr->ulValueLen = ((CK_ULONG)-1);
			}
		}
	}

	if (egg_buffer_has_error (&msg->buffer))
		return PARSE_ERROR;

	/* Read in the code that goes along with these attributes */
	if (!gkm_rpc_message_read_ulong (msg, &ret))
		return PARSE_ERROR;

	return ret;
}

static CK_RV
proto_read_byte_array (GkmRpcMessage *msg, CK_BYTE_PTR arr,
                       CK_ULONG_PTR len, CK_ULONG max)
{
	const unsigned char *val;
	unsigned char valid;
	size_t vlen;

	assert (len);
	assert (msg);

	/* Make sure this is in the right order */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "ay"));

	/* A single byte which determines whether valid or not */
	if (!egg_buffer_get_byte (&msg->buffer, msg->parsed, &msg->parsed, &valid))
		return PARSE_ERROR;

	/* If not valid, then just the length is encoded, this can signify CKR_BUFFER_TOO_SMALL */
	if (!valid) {
		if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, (uint32_t *)&vlen))
			return PARSE_ERROR;

		*len = vlen;

		if (arr)
			return CKR_BUFFER_TOO_SMALL;
		else
			return CKR_OK;
	}

	/* Get the actual bytes */
	if (!egg_buffer_get_byte_array (&msg->buffer, msg->parsed, &msg->parsed, &val, &vlen))
		return PARSE_ERROR;

	*len = vlen;

	/* Just asking us for size */
	if (!arr)
		return CKR_OK;

	if (max < vlen)
		return CKR_BUFFER_TOO_SMALL;

	/* Enough space, yay */
	memcpy (arr, val, vlen);
	return CKR_OK;
}

static CK_RV
proto_read_ulong_array (GkmRpcMessage *msg, CK_ULONG_PTR arr,
                        CK_ULONG_PTR len, CK_ULONG max)
{
	uint32_t i, num;
	uint64_t val;
	unsigned char valid;

	assert (len);
	assert (msg);

	/* Make sure this is in the right order */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "au"));

	/* A single byte which determines whether valid or not */
	if (!egg_buffer_get_byte (&msg->buffer, msg->parsed, &msg->parsed, &valid))
		return PARSE_ERROR;

	/* Get the number of items. */
	if (!egg_buffer_get_uint32 (&msg->buffer, msg->parsed, &msg->parsed, &num))
		return PARSE_ERROR;

	*len = num;

	/* If not valid, then just the length is encoded, this can signify CKR_BUFFER_TOO_SMALL */
	if (!valid) {
		if (arr)
			return CKR_BUFFER_TOO_SMALL;
		else
			return CKR_OK;
	}

	if (max < num)
		return CKR_BUFFER_TOO_SMALL;

	/* We need to go ahead and read everything in all cases */
	for (i = 0; i < num; ++i) {
		egg_buffer_get_uint64 (&msg->buffer, msg->parsed, &msg->parsed, &val);
		if (arr)
			arr[i] = (CK_ULONG)val;
	}

	return egg_buffer_has_error (&msg->buffer) ? PARSE_ERROR : CKR_OK;
}

static CK_RV
proto_write_mechanism (GkmRpcMessage *msg, CK_MECHANISM_PTR mech)
{
	assert (msg);
	assert (mech);

	/* Make sure this is in the right order */
	assert (!msg->signature || gkm_rpc_message_verify_part (msg, "M"));

	/* The mechanism type */
	egg_buffer_add_uint32 (&msg->buffer, mech->mechanism);

	/*
	 * PKCS#11 mechanism parameters are not easy to serialize. They're
	 * completely different for so many mechanisms, they contain
	 * pointers to arbitrary memory, and many callers don't initialize
	 * them completely or properly.
	 *
	 * We only support certain mechanisms.
	 *
	 * Also callers do yucky things like leaving parts of the structure
	 * pointing to garbage if they don't think it's going to be used.
	 */

	if (gkm_rpc_mechanism_has_no_parameters (mech->mechanism))
		egg_buffer_add_byte_array (&msg->buffer, NULL, 0);
	else if (gkm_rpc_mechanism_has_sane_parameters (mech->mechanism))
		egg_buffer_add_byte_array (&msg->buffer, mech->pParameter,
		                           mech->ulParameterLen);
	else
		return CKR_MECHANISM_INVALID;

	return egg_buffer_has_error (&msg->buffer) ? CKR_HOST_MEMORY : CKR_OK;
}

static CK_RV
proto_read_info (GkmRpcMessage *msg, CK_INFO_PTR info)
{
	assert (msg);
	assert (info);

	if (!gkm_rpc_message_read_version (msg, &info->cryptokiVersion) ||
	    !gkm_rpc_message_read_space_string (msg, info->manufacturerID, 32) ||
	    !gkm_rpc_message_read_ulong (msg, &info->flags) ||
	    !gkm_rpc_message_read_space_string (msg, info->libraryDescription, 32) ||
	    !gkm_rpc_message_read_version (msg, &info->libraryVersion))
		return PARSE_ERROR;

	return CKR_OK;
}

static CK_RV
proto_read_slot_info (GkmRpcMessage *msg, CK_SLOT_INFO_PTR info)
{
	assert (msg);
	assert (info);

	if (!gkm_rpc_message_read_space_string (msg, info->slotDescription, 64) ||
	    !gkm_rpc_message_read_space_string (msg, info->manufacturerID, 32) ||
	    !gkm_rpc_message_read_ulong (msg, &info->flags) ||
	    !gkm_rpc_message_read_version (msg, &info->hardwareVersion) ||
	    !gkm_rpc_message_read_version (msg, &info->firmwareVersion))
		return PARSE_ERROR;

	return CKR_OK;
}

static CK_RV
proto_read_token_info (GkmRpcMessage *msg, CK_TOKEN_INFO_PTR info)
{
	assert (msg);
	assert (info);

	if (!gkm_rpc_message_read_space_string (msg, info->label, 32) ||
	    !gkm_rpc_message_read_space_string (msg, info->manufacturerID, 32) ||
	    !gkm_rpc_message_read_space_string (msg, info->model, 16) ||
	    !gkm_rpc_message_read_space_string (msg, info->serialNumber, 16) ||
	    !gkm_rpc_message_read_ulong (msg, &info->flags) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulMaxSessionCount) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulSessionCount) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulMaxRwSessionCount) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulRwSessionCount) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulMaxPinLen) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulMinPinLen) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulTotalPublicMemory) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulFreePublicMemory) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulTotalPrivateMemory) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulFreePrivateMemory) ||
	    !gkm_rpc_message_read_version (msg, &info->hardwareVersion) ||
	    !gkm_rpc_message_read_version (msg, &info->firmwareVersion) ||
	    !gkm_rpc_message_read_space_string (msg, info->utcTime, 16))
		return PARSE_ERROR;

	return CKR_OK;
}

static CK_RV
proto_read_mechanism_info (GkmRpcMessage *msg, CK_MECHANISM_INFO_PTR info)
{
	assert (msg);
	assert (info);

	if (!gkm_rpc_message_read_ulong (msg, &info->ulMinKeySize) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulMaxKeySize) ||
	    !gkm_rpc_message_read_ulong (msg, &info->flags))
		return PARSE_ERROR;

	return CKR_OK;
}

static CK_RV
proto_read_sesssion_info (GkmRpcMessage *msg, CK_SESSION_INFO_PTR info)
{
	assert (msg);
	assert (info);

	if (!gkm_rpc_message_read_ulong (msg, &info->slotID) ||
	    !gkm_rpc_message_read_ulong (msg, &info->state) ||
	    !gkm_rpc_message_read_ulong (msg, &info->flags) ||
	    !gkm_rpc_message_read_ulong (msg, &info->ulDeviceError))
		return PARSE_ERROR;

	return CKR_OK;
}

/* -------------------------------------------------------------------
 * CALL MACROS
 */

#define BEGIN_CALL(call_id) \
	debug ((#call_id ": enter")); \
	return_val_if_fail (pkcs11_initialized, CKR_CRYPTOKI_NOT_INITIALIZED); \
	{  \
		CallState *_cs; \
		CK_RV _ret = CKR_OK; \
		_ret = call_lookup (&_cs); \
		if (_ret != CKR_OK) return _ret; \
		_ret = call_prepare (_cs, GKM_RPC_CALL_##call_id); \
		if (_ret != CKR_OK) goto _cleanup;

#define PROCESS_CALL \
		_ret = call_run (_cs); \
		if (_ret != CKR_OK) goto _cleanup;

#define RETURN(ret) \
		_ret = ret; \
		goto _cleanup;

#define END_CALL \
	_cleanup: \
		_ret = call_done (_cs, _ret); \
		debug (("ret: %d", _ret)); \
		return _ret; \
	}

#define IN_BYTE(val) \
	if (!gkm_rpc_message_write_byte (_cs->req, val)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_ULONG(val) \
	if (!gkm_rpc_message_write_ulong (_cs->req, val)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_STRING(val) \
	if (!gkm_rpc_message_write_zero_string (_cs->req, val)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_BYTE_BUFFER(arr, len) \
	if (len == NULL) \
		{ _ret = CKR_ARGUMENTS_BAD; goto _cleanup; } \
	if (!gkm_rpc_message_write_byte_buffer (_cs->req, arr ? *len : 0)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_BYTE_ARRAY(arr, len) \
	if (len != 0 && arr == NULL) \
		{ _ret = CKR_ARGUMENTS_BAD; goto _cleanup; } \
	if (!gkm_rpc_message_write_byte_array (_cs->req, arr, len)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_ULONG_BUFFER(arr, len) \
	if (len == NULL) \
		{ _ret = CKR_ARGUMENTS_BAD; goto _cleanup; } \
	if (!gkm_rpc_message_write_ulong_buffer (_cs->req, arr ? *len : 0)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_ULONG_ARRAY(arr, len) \
	if (len != 0 && arr == NULL) \
		{ _ret = CKR_ARGUMENTS_BAD; goto _cleanup; }\
	if (!gkm_rpc_message_write_ulong_array (_cs->req, arr, len)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_ATTRIBUTE_BUFFER(arr, num) \
	if (num != 0 && arr == NULL) \
		{ _ret = CKR_ARGUMENTS_BAD; goto _cleanup; } \
	if (!gkm_rpc_message_write_attribute_buffer (_cs->req, (arr), (num))) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_ATTRIBUTE_ARRAY(arr, num) \
	if (num != 0 && arr == NULL) \
		{ _ret = CKR_ARGUMENTS_BAD; goto _cleanup; } \
	if (!gkm_rpc_message_write_attribute_array (_cs->req, (arr), (num))) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_MECHANISM_TYPE(val) \
	if(!gkm_rpc_mechanism_is_supported (val)) \
		{ _ret = CKR_MECHANISM_INVALID; goto _cleanup; } \
	if (!gkm_rpc_message_write_ulong (_cs->req, val)) \
		{ _ret = CKR_HOST_MEMORY; goto _cleanup; }

#define IN_MECHANISM(val) \
	if (val == NULL) \
		{ _ret = CKR_ARGUMENTS_BAD; goto _cleanup; } \
	_ret = proto_write_mechanism (_cs->req, val); \
	if (_ret != CKR_OK) goto _cleanup;



#define OUT_ULONG(val) \
	if (val == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK && !gkm_rpc_message_read_ulong (_cs->resp, val)) \
		_ret = PARSE_ERROR;

#define OUT_BYTE_ARRAY(arr, len)  \
	if (len == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_byte_array (_cs->resp, (arr), (len), *(len));

#define OUT_ULONG_ARRAY(a, len) \
	if (len == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_ulong_array (_cs->resp, (a), (len), *(len));

#define OUT_ATTRIBUTE_ARRAY(arr, num) \
	if (_ret == CKR_OK) \
		_ret = proto_read_attribute_array (_cs->resp, (arr), (num));

#define OUT_INFO(info) \
	if (info == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_info (_cs->resp, info);

#define OUT_SLOT_INFO(info) \
	if (info == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_slot_info (_cs->resp, info);

#define OUT_TOKEN_INFO(info) \
	if (info == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_token_info (_cs->resp, info);

#define OUT_SESSION_INFO(info) \
	if (info == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_sesssion_info (_cs->resp, info);

#define OUT_MECHANISM_TYPE_ARRAY(arr, len) \
	if (len == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_ulong_array (_cs->resp, (arr), (len), *(len)); \
	if (_ret == CKR_OK && arr) \
		gkm_rpc_mechanism_list_purge (arr, len);

#define OUT_MECHANISM_INFO(info) \
	if (info == NULL) \
		_ret = CKR_ARGUMENTS_BAD; \
	if (_ret == CKR_OK) \
		_ret = proto_read_mechanism_info (_cs->resp, info);


/* -------------------------------------------------------------------
 * INITIALIZATION and 'GLOBAL' CALLS
 */

static CK_RV
rpc_C_Initialize (CK_VOID_PTR init_args)
{
	CK_C_INITIALIZE_ARGS_PTR args = NULL;
	CK_RV ret = CKR_OK;
	const char *path;
	CallState *cs;
	pid_t pid;

	debug (("C_Initialize: enter"));

#ifdef _DEBUG
	GKM_RPC_CHECK_CALLS();
#endif

	pthread_mutex_lock (&init_mutex);

		if (init_args != NULL) {
			int supplied_ok;

			/* pReserved must be NULL */
			args = init_args;

			/* ALL supplied function pointers need to have the value either NULL or non-NULL. */
			supplied_ok = (args->CreateMutex == NULL && args->DestroyMutex == NULL &&
			               args->LockMutex == NULL && args->UnlockMutex == NULL) ||
			              (args->CreateMutex != NULL && args->DestroyMutex != NULL &&
			               args->LockMutex != NULL && args->UnlockMutex != NULL);
			if (!supplied_ok) {
				warning (("invalid set of mutex calls supplied"));
				ret = CKR_ARGUMENTS_BAD;
				goto done;
			}

			/*
			 * When the CKF_OS_LOCKING_OK flag isn't set return an error.
			 * We must be able to use our pthread functionality.
			 */
			if (!(args->flags & CKF_OS_LOCKING_OK)) {
				warning (("can't do without os locking"));
				ret = CKR_CANT_LOCK;
				goto done;
			}

			/*
			 * We support setting the socket path and other arguments from from the
			 * pReserved pointer, similar to how NSS PKCS#11 components are initialized.
			 */
			if (args->pReserved)
				parse_arguments ((const char*)args->pReserved);
		}

		pid = getpid ();
		if (pkcs11_initialized) {

			/* This process has called C_Initialize already */
			if (pid == pkcs11_initialized_pid) {
				warning (("C_Initialize called twice for same process"));
				ret = CKR_CRYPTOKI_ALREADY_INITIALIZED;
				goto done;
			}
		}

		/* Lookup the socket path, append '/pkcs11' */
		if (pkcs11_socket_path == NULL) {
			path = getenv ("GNOME_KEYRING_CONTROL");
			if (path && path[0]) {
				pkcs11_socket_path = malloc (strlen (path) + strlen ("/pkcs11") + 1);
				if (pkcs11_socket_path == NULL) {
					warning (("can't malloc memory"));
					ret = CKR_HOST_MEMORY;
					goto done;
				}
				sprintf (pkcs11_socket_path, "%s/pkcs11", path);
			}
		}

		/* Call through and initialize the daemon */
		ret = call_lookup (&cs);
		if (ret == CKR_OK) {
			ret = call_prepare (cs, GKM_RPC_CALL_C_Initialize);
			if (ret == CKR_OK)
				if (!gkm_rpc_message_write_byte_array (cs->req, GKM_RPC_HANDSHAKE, GKM_RPC_HANDSHAKE_LEN))
					ret = CKR_HOST_MEMORY;
			if (ret == CKR_OK)
				ret = call_run (cs);
			call_done (cs, ret);
		}

done:
		/* Mark us as officially initialized */
		if (ret == CKR_OK) {
			pkcs11_initialized = 1;
			pkcs11_initialized_pid = pid;
		} else if (ret != CKR_CRYPTOKI_ALREADY_INITIALIZED) {
			pkcs11_initialized = 0;
			pkcs11_initialized_pid = 0;
			free (pkcs11_socket_path);
			pkcs11_socket_path = NULL;
		}

	pthread_mutex_unlock (&init_mutex);

	debug (("C_Initialize: %d", ret));
	return ret;
}

static CK_RV
rpc_C_Finalize (CK_VOID_PTR reserved)
{
	CallState *cs;
	CK_RV ret;

	debug (("C_Finalize: enter"));
	return_val_if_fail (pkcs11_initialized, CKR_CRYPTOKI_NOT_INITIALIZED);
	return_val_if_fail (!reserved, CKR_ARGUMENTS_BAD);

	pthread_mutex_lock (&init_mutex);

		ret = call_lookup (&cs);
		if (ret == CKR_OK) {
			ret = call_prepare (cs, GKM_RPC_CALL_C_Finalize);
			if (ret == CKR_OK) {
				ret = call_run (cs);
			}
			call_done (cs, ret);
		}

		if (ret != CKR_OK)
			warning (("finalizing the daemon returned an error: %d", ret));

		/* This should stop all other calls in */
		pkcs11_initialized = 0;
		pkcs11_initialized_pid = 0;
		free (pkcs11_socket_path);
		pkcs11_socket_path = NULL;

	pthread_mutex_unlock (&init_mutex);

	debug (("C_Finalize: %d", CKR_OK));
	return CKR_OK;
}

static CK_RV
rpc_C_GetInfo (CK_INFO_PTR info)
{
	return_val_if_fail (info, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetInfo);
	PROCESS_CALL;
		OUT_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	/* This would be a strange call to receive */
	return C_GetFunctionList (list);
}

static CK_RV
rpc_C_GetSlotList (CK_BBOOL token_present, CK_SLOT_ID_PTR slot_list, CK_ULONG_PTR count)
{
	return_val_if_fail (count, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetSlotList);
		IN_BYTE (token_present);
		IN_ULONG_BUFFER (slot_list, count);
	PROCESS_CALL;
		OUT_ULONG_ARRAY (slot_list, count);
	END_CALL;
}

static CK_RV
rpc_C_GetSlotInfo (CK_SLOT_ID id, CK_SLOT_INFO_PTR info)
{
	return_val_if_fail (info, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetSlotInfo);
		IN_ULONG (id);
	PROCESS_CALL;
		OUT_SLOT_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_GetTokenInfo (CK_SLOT_ID id, CK_TOKEN_INFO_PTR info)
{
	return_val_if_fail (info, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetTokenInfo);
		IN_ULONG (id);
	PROCESS_CALL;
		OUT_TOKEN_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_GetMechanismList (CK_SLOT_ID id, CK_MECHANISM_TYPE_PTR mechanism_list,
                        CK_ULONG_PTR count)
{
	return_val_if_fail (count, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetMechanismList);
		IN_ULONG (id);
		IN_ULONG_BUFFER (mechanism_list, count);
	PROCESS_CALL;
		OUT_MECHANISM_TYPE_ARRAY (mechanism_list, count);
	END_CALL;

}

static CK_RV
rpc_C_GetMechanismInfo (CK_SLOT_ID id, CK_MECHANISM_TYPE type,
                        CK_MECHANISM_INFO_PTR info)
{
	return_val_if_fail (info, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetMechanismInfo);
		IN_ULONG (id);
		IN_MECHANISM_TYPE (type);
	PROCESS_CALL;
		OUT_MECHANISM_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_InitToken (CK_SLOT_ID id, CK_UTF8CHAR_PTR pin, CK_ULONG pin_len,
                 CK_UTF8CHAR_PTR label)
{
	BEGIN_CALL (C_InitToken);
		IN_ULONG (id);
		IN_BYTE_ARRAY (pin, pin_len);
		IN_STRING (label);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_WaitForSlotEvent (CK_FLAGS flags, CK_SLOT_ID_PTR slot, CK_VOID_PTR reserved)
{
	return_val_if_fail (slot, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_WaitForSlotEvent);
		IN_ULONG (flags);
	PROCESS_CALL;
		OUT_ULONG (slot);
	END_CALL;
}

static CK_RV
rpc_C_OpenSession (CK_SLOT_ID id, CK_FLAGS flags, CK_VOID_PTR user_data,
                   CK_NOTIFY callback, CK_SESSION_HANDLE_PTR session)
{
	return_val_if_fail (session, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_OpenSession);
		IN_ULONG (id);
		IN_ULONG (flags);
	PROCESS_CALL;
		OUT_ULONG (session);
	END_CALL;
}

static CK_RV
rpc_C_CloseSession (CK_SESSION_HANDLE session)
{
	BEGIN_CALL (C_CloseSession);
		IN_ULONG (session);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_CloseAllSessions (CK_SLOT_ID id)
{
	BEGIN_CALL (C_CloseAllSessions);
		IN_ULONG (id);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_GetFunctionStatus (CK_SESSION_HANDLE session)
{
	BEGIN_CALL (C_GetFunctionStatus);
		IN_ULONG (session);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_CancelFunction (CK_SESSION_HANDLE session)
{
	BEGIN_CALL (C_CancelFunction);
		IN_ULONG (session);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_GetSessionInfo(CK_SESSION_HANDLE session, CK_SESSION_INFO_PTR info)
{
	return_val_if_fail (info, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetSessionInfo);
		IN_ULONG (session);
	PROCESS_CALL;
		OUT_SESSION_INFO (info);
	END_CALL;
}

static CK_RV
rpc_C_InitPIN (CK_SESSION_HANDLE session, CK_UTF8CHAR_PTR pin,
               CK_ULONG pin_len)
{
	BEGIN_CALL (C_InitPIN);
		IN_ULONG (session);
		IN_BYTE_ARRAY (pin, pin_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_SetPIN (CK_SESSION_HANDLE session, CK_UTF8CHAR_PTR old_pin,
              CK_ULONG old_pin_len, CK_UTF8CHAR_PTR new_pin, CK_ULONG new_pin_len)
{
	BEGIN_CALL (C_SetPIN);
		IN_ULONG (session);
		IN_BYTE_ARRAY (old_pin, old_pin_len);
		IN_BYTE_ARRAY (new_pin, old_pin_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_GetOperationState (CK_SESSION_HANDLE session, CK_BYTE_PTR operation_state,
                         CK_ULONG_PTR operation_state_len)
{
	return_val_if_fail (operation_state_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetOperationState);
		IN_ULONG (session);
		IN_BYTE_BUFFER (operation_state, operation_state_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (operation_state, operation_state_len);
	END_CALL;
}

static CK_RV
rpc_C_SetOperationState (CK_SESSION_HANDLE session, CK_BYTE_PTR operation_state,
                         CK_ULONG operation_state_len, CK_OBJECT_HANDLE encryption_key,
                         CK_OBJECT_HANDLE authentication_key)
{
	BEGIN_CALL (C_SetOperationState);
		IN_ULONG (session);
		IN_BYTE_ARRAY (operation_state, operation_state_len);
		IN_ULONG (encryption_key);
		IN_ULONG (authentication_key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_Login (CK_SESSION_HANDLE session, CK_USER_TYPE user_type,
             CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	BEGIN_CALL (C_Login);
		IN_ULONG (session);
		IN_ULONG (user_type);
		IN_BYTE_ARRAY (pin, pin_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_Logout (CK_SESSION_HANDLE session)
{
	BEGIN_CALL (C_Logout);
		IN_ULONG (session);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_CreateObject (CK_SESSION_HANDLE session, CK_ATTRIBUTE_PTR template,
                    CK_ULONG count, CK_OBJECT_HANDLE_PTR new_object)
{
	return_val_if_fail (new_object, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_CreateObject);
		IN_ULONG (session);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL;
		OUT_ULONG (new_object);
	END_CALL;
}

static CK_RV
rpc_C_CopyObject (CK_SESSION_HANDLE session, CK_OBJECT_HANDLE object,
                  CK_ATTRIBUTE_PTR template, CK_ULONG count,
                  CK_OBJECT_HANDLE_PTR new_object)
{
	return_val_if_fail (new_object, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_CopyObject);
		IN_ULONG (session);
		IN_ULONG (object);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL;
		OUT_ULONG (new_object);
	END_CALL;
}


static CK_RV
rpc_C_DestroyObject (CK_SESSION_HANDLE session, CK_OBJECT_HANDLE object)
{
	BEGIN_CALL (C_DestroyObject);
		IN_ULONG (session);
		IN_ULONG (object);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_GetObjectSize (CK_SESSION_HANDLE session, CK_OBJECT_HANDLE object,
                     CK_ULONG_PTR size)
{
	return_val_if_fail (size, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_GetObjectSize);
		IN_ULONG (session);
		IN_ULONG (object);
	PROCESS_CALL;
		OUT_ULONG (size);
	END_CALL;
}

static CK_RV
rpc_C_GetAttributeValue (CK_SESSION_HANDLE session, CK_OBJECT_HANDLE object,
                         CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	BEGIN_CALL (C_GetAttributeValue);
		IN_ULONG (session);
		IN_ULONG (object);
		IN_ATTRIBUTE_BUFFER (template, count);
	PROCESS_CALL;
		OUT_ATTRIBUTE_ARRAY (template, count);
	END_CALL;
}

static CK_RV
rpc_C_SetAttributeValue (CK_SESSION_HANDLE session, CK_OBJECT_HANDLE object,
                         CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	BEGIN_CALL (C_SetAttributeValue);
		IN_ULONG (session);
		IN_ULONG (object);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_FindObjectsInit (CK_SESSION_HANDLE session, CK_ATTRIBUTE_PTR template,
                       CK_ULONG count)
{
	BEGIN_CALL (C_FindObjectsInit);
		IN_ULONG (session);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_FindObjects (CK_SESSION_HANDLE session, CK_OBJECT_HANDLE_PTR objects,
                   CK_ULONG max_count, CK_ULONG_PTR count)
{
	return_val_if_fail (count, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_FindObjects);
		IN_ULONG (session);
		IN_ULONG_BUFFER (objects, &max_count);
	PROCESS_CALL;
		*count = max_count;
		OUT_ULONG_ARRAY (objects, count);
	END_CALL;
}

static CK_RV
rpc_C_FindObjectsFinal (CK_SESSION_HANDLE session)
{
	BEGIN_CALL (C_FindObjectsFinal);
		IN_ULONG (session);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_EncryptInit (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                   CK_OBJECT_HANDLE key)
{
	BEGIN_CALL (C_EncryptInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_Encrypt (CK_SESSION_HANDLE session, CK_BYTE_PTR data, CK_ULONG data_len,
               CK_BYTE_PTR encrypted_data, CK_ULONG_PTR encrypted_data_len)
{
	return_val_if_fail (encrypted_data_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_Encrypt);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_BUFFER (encrypted_data, encrypted_data_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (encrypted_data, encrypted_data_len);
	END_CALL;
}

static CK_RV
rpc_C_EncryptUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR part,
                     CK_ULONG part_len, CK_BYTE_PTR encrypted_part,
                     CK_ULONG_PTR encrypted_part_len)
{
	return_val_if_fail (encrypted_part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_EncryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
		IN_BYTE_BUFFER (encrypted_part, encrypted_part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (encrypted_part, encrypted_part_len);
	END_CALL;
}

static CK_RV
rpc_C_EncryptFinal (CK_SESSION_HANDLE session, CK_BYTE_PTR last_part,
                    CK_ULONG_PTR last_part_len)
{
	return_val_if_fail (last_part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_EncryptFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (last_part, last_part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (last_part, last_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptInit (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                   CK_OBJECT_HANDLE key)
{
	BEGIN_CALL (C_DecryptInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_Decrypt (CK_SESSION_HANDLE session, CK_BYTE_PTR enc_data,
               CK_ULONG enc_data_len, CK_BYTE_PTR data, CK_ULONG_PTR data_len)
{
	return_val_if_fail (data_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_Decrypt);
		IN_ULONG (session);
		IN_BYTE_ARRAY (enc_data, enc_data_len);
		IN_BYTE_BUFFER (data, data_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (data, data_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR enc_part,
                     CK_ULONG enc_part_len, CK_BYTE_PTR part, CK_ULONG_PTR part_len)
{
	return_val_if_fail (part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_DecryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (enc_part, enc_part_len);
		IN_BYTE_BUFFER (part, part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (part, part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptFinal (CK_SESSION_HANDLE session, CK_BYTE_PTR last_part,
                    CK_ULONG_PTR last_part_len)
{
	return_val_if_fail (last_part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_DecryptFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (last_part, last_part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (last_part, last_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DigestInit (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism)
{
	BEGIN_CALL (C_DigestInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_Digest (CK_SESSION_HANDLE session, CK_BYTE_PTR data, CK_ULONG data_len,
              CK_BYTE_PTR digest, CK_ULONG_PTR digest_len)
{
	return_val_if_fail (digest_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_Digest);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_BUFFER (digest, digest_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (digest, digest_len);
	END_CALL;
}

static CK_RV
rpc_C_DigestUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR part, CK_ULONG part_len)
{
	BEGIN_CALL (C_DigestUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_DigestKey (CK_SESSION_HANDLE session, CK_OBJECT_HANDLE key)
{
	BEGIN_CALL (C_DigestKey);
		IN_ULONG (session);
		IN_ULONG (key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_DigestFinal (CK_SESSION_HANDLE session, CK_BYTE_PTR digest,
                   CK_ULONG_PTR digest_len)
{
	return_val_if_fail (digest_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_DigestFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (digest, digest_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (digest, digest_len);
	END_CALL;
}

static CK_RV
rpc_C_SignInit (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                CK_OBJECT_HANDLE key)
{
	BEGIN_CALL (C_SignInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_Sign (CK_SESSION_HANDLE session, CK_BYTE_PTR data, CK_ULONG data_len,
            CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	return_val_if_fail (signature_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_Sign);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_BUFFER (signature, signature_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (signature, signature_len);
	END_CALL;
}

static CK_RV
rpc_C_SignUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR part, CK_ULONG part_len)
{
	return_val_if_fail (part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_SignUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_SignFinal (CK_SESSION_HANDLE session, CK_BYTE_PTR signature,
                 CK_ULONG_PTR signature_len)
{
	return_val_if_fail (signature_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_SignFinal);
		IN_ULONG (session);
		IN_BYTE_BUFFER (signature, signature_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (signature, signature_len);
	END_CALL;
}

static CK_RV
rpc_C_SignRecoverInit (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                       CK_OBJECT_HANDLE key)
{
	BEGIN_CALL (C_SignRecoverInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_SignRecover (CK_SESSION_HANDLE session, CK_BYTE_PTR data, CK_ULONG data_len,
                   CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	return_val_if_fail (signature_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_SignRecover);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_BUFFER (signature, signature_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (signature, signature_len);
	END_CALL;
}

static CK_RV
rpc_C_VerifyInit (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                  CK_OBJECT_HANDLE key)
{
	BEGIN_CALL (C_VerifyInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_Verify (CK_SESSION_HANDLE session, CK_BYTE_PTR data, CK_ULONG data_len,
              CK_BYTE_PTR signature, CK_ULONG signature_len)
{
	BEGIN_CALL (C_Verify);
		IN_ULONG (session);
		IN_BYTE_ARRAY (data, data_len);
		IN_BYTE_ARRAY (signature, signature_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_VerifyUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR part, CK_ULONG part_len)
{
	BEGIN_CALL (C_VerifyUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_VerifyFinal (CK_SESSION_HANDLE session, CK_BYTE_PTR signature,
                   CK_ULONG signature_len)
{
	BEGIN_CALL (C_VerifyFinal);
		IN_ULONG (session);
		IN_BYTE_ARRAY (signature, signature_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_VerifyRecoverInit (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                         CK_OBJECT_HANDLE key)
{
	BEGIN_CALL (C_VerifyRecoverInit);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (key);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_VerifyRecover (CK_SESSION_HANDLE session, CK_BYTE_PTR signature,
                     CK_ULONG signature_len, CK_BYTE_PTR data, CK_ULONG_PTR data_len)
{
	return_val_if_fail (data_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_VerifyRecover);
		IN_ULONG (session);
		IN_BYTE_ARRAY (signature, signature_len);
		IN_BYTE_BUFFER (data, data_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (data, data_len);
	END_CALL;
}

static CK_RV
rpc_C_DigestEncryptUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR part,
                           CK_ULONG part_len, CK_BYTE_PTR enc_part,
                           CK_ULONG_PTR enc_part_len)
{
	return_val_if_fail (enc_part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_DigestEncryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
		IN_BYTE_BUFFER (enc_part, enc_part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (enc_part, enc_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptDigestUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR enc_part,
                           CK_ULONG enc_part_len, CK_BYTE_PTR part,
                           CK_ULONG_PTR part_len)
{
	return_val_if_fail (part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_DecryptDigestUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (enc_part, enc_part_len);
		IN_BYTE_BUFFER (part, part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (part, part_len);
	END_CALL;
}

static CK_RV
rpc_C_SignEncryptUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR part,
                         CK_ULONG part_len, CK_BYTE_PTR enc_part,
                         CK_ULONG_PTR enc_part_len)
{
	return_val_if_fail (enc_part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_SignEncryptUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (part, part_len);
		IN_BYTE_BUFFER (enc_part, enc_part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (enc_part, enc_part_len);
	END_CALL;
}

static CK_RV
rpc_C_DecryptVerifyUpdate (CK_SESSION_HANDLE session, CK_BYTE_PTR enc_part,
                           CK_ULONG enc_part_len, CK_BYTE_PTR part,
                           CK_ULONG_PTR part_len)
{
	return_val_if_fail (part_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_DecryptVerifyUpdate);
		IN_ULONG (session);
		IN_BYTE_ARRAY (enc_part, enc_part_len);
		IN_BYTE_BUFFER (part, part_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (part, part_len);
	END_CALL;
}

static CK_RV
rpc_C_GenerateKey (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                   CK_ATTRIBUTE_PTR template, CK_ULONG count,
                   CK_OBJECT_HANDLE_PTR key)
{
	BEGIN_CALL (C_GenerateKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL;
		OUT_ULONG (key);
	END_CALL;
}

static CK_RV
rpc_C_GenerateKeyPair (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                       CK_ATTRIBUTE_PTR pub_template, CK_ULONG pub_count,
                       CK_ATTRIBUTE_PTR priv_template, CK_ULONG priv_count,
                       CK_OBJECT_HANDLE_PTR pub_key, CK_OBJECT_HANDLE_PTR priv_key)
{
	BEGIN_CALL (C_GenerateKeyPair);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ATTRIBUTE_ARRAY (pub_template, pub_count);
		IN_ATTRIBUTE_ARRAY (priv_template, priv_count);
	PROCESS_CALL;
		OUT_ULONG (pub_key);
		OUT_ULONG (priv_key);
	END_CALL;
}

static CK_RV
rpc_C_WrapKey (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
               CK_OBJECT_HANDLE wrapping_key, CK_OBJECT_HANDLE key,
               CK_BYTE_PTR wrapped_key, CK_ULONG_PTR wrapped_key_len)
{
	return_val_if_fail (wrapped_key_len, CKR_ARGUMENTS_BAD);

	BEGIN_CALL (C_WrapKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (wrapping_key);
		IN_ULONG (key);
		IN_BYTE_BUFFER (wrapped_key, wrapped_key_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (wrapped_key, wrapped_key_len);
	END_CALL;
}

static CK_RV
rpc_C_UnwrapKey (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                 CK_OBJECT_HANDLE unwrapping_key, CK_BYTE_PTR wrapped_key,
                 CK_ULONG wrapped_key_len, CK_ATTRIBUTE_PTR template,
                 CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	BEGIN_CALL (C_UnwrapKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (unwrapping_key);
		IN_BYTE_ARRAY (wrapped_key, wrapped_key_len);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL;
		OUT_ULONG (key);
	END_CALL;
}

static CK_RV
rpc_C_DeriveKey (CK_SESSION_HANDLE session, CK_MECHANISM_PTR mechanism,
                 CK_OBJECT_HANDLE base_key, CK_ATTRIBUTE_PTR template,
                 CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	BEGIN_CALL (C_DeriveKey);
		IN_ULONG (session);
		IN_MECHANISM (mechanism);
		IN_ULONG (base_key);
		IN_ATTRIBUTE_ARRAY (template, count);
	PROCESS_CALL;
		OUT_ULONG (key);
	END_CALL;
}

static CK_RV
rpc_C_SeedRandom (CK_SESSION_HANDLE session, CK_BYTE_PTR seed, CK_ULONG seed_len)
{
	BEGIN_CALL (C_SeedRandom);
		IN_ULONG (session);
		IN_BYTE_ARRAY (seed, seed_len);
	PROCESS_CALL;
	END_CALL;
}

static CK_RV
rpc_C_GenerateRandom (CK_SESSION_HANDLE session, CK_BYTE_PTR random_data,
                      CK_ULONG random_len)
{
	BEGIN_CALL (C_GenerateRandom);
		IN_ULONG (session);
		IN_BYTE_BUFFER (random_data, &random_len);
	PROCESS_CALL;
		OUT_BYTE_ARRAY (random_data, &random_len);
	END_CALL;
}

/* --------------------------------------------------------------------
 * MODULE ENTRY POINT
 */

/*
 * PKCS#11 is broken here. It states that Unix compilers automatically byte
 * pack structures. This is wrong. GCC on Linux aligns to 4 by default.
 *
 * This results in incompatibilities. Where this structure's first version
 * members take up too much or too little space depending on how this module
 * is compiled.
 */

static CK_FUNCTION_LIST functionList = {
	{ CRYPTOKI_VERSION_MAJOR, CRYPTOKI_VERSION_MINOR },  /* version */
	rpc_C_Initialize,
	rpc_C_Finalize,
	rpc_C_GetInfo,
	rpc_C_GetFunctionList,
	rpc_C_GetSlotList,
	rpc_C_GetSlotInfo,
	rpc_C_GetTokenInfo,
	rpc_C_GetMechanismList,
	rpc_C_GetMechanismInfo,
	rpc_C_InitToken,
	rpc_C_InitPIN,
	rpc_C_SetPIN,
	rpc_C_OpenSession,
	rpc_C_CloseSession,
	rpc_C_CloseAllSessions,
	rpc_C_GetSessionInfo,
	rpc_C_GetOperationState,
	rpc_C_SetOperationState,
	rpc_C_Login,
	rpc_C_Logout,
	rpc_C_CreateObject,
	rpc_C_CopyObject,
	rpc_C_DestroyObject,
	rpc_C_GetObjectSize,
	rpc_C_GetAttributeValue,
	rpc_C_SetAttributeValue,
	rpc_C_FindObjectsInit,
	rpc_C_FindObjects,
	rpc_C_FindObjectsFinal,
	rpc_C_EncryptInit,
	rpc_C_Encrypt,
	rpc_C_EncryptUpdate,
	rpc_C_EncryptFinal,
	rpc_C_DecryptInit,
	rpc_C_Decrypt,
	rpc_C_DecryptUpdate,
	rpc_C_DecryptFinal,
	rpc_C_DigestInit,
	rpc_C_Digest,
	rpc_C_DigestUpdate,
	rpc_C_DigestKey,
	rpc_C_DigestFinal,
	rpc_C_SignInit,
	rpc_C_Sign,
	rpc_C_SignUpdate,
	rpc_C_SignFinal,
	rpc_C_SignRecoverInit,
	rpc_C_SignRecover,
	rpc_C_VerifyInit,
	rpc_C_Verify,
	rpc_C_VerifyUpdate,
	rpc_C_VerifyFinal,
	rpc_C_VerifyRecoverInit,
	rpc_C_VerifyRecover,
	rpc_C_DigestEncryptUpdate,
	rpc_C_DecryptDigestUpdate,
	rpc_C_SignEncryptUpdate,
	rpc_C_DecryptVerifyUpdate,
	rpc_C_GenerateKey,
	rpc_C_GenerateKeyPair,
	rpc_C_WrapKey,
	rpc_C_UnwrapKey,
	rpc_C_DeriveKey,
	rpc_C_SeedRandom,
	rpc_C_GenerateRandom,
	rpc_C_GetFunctionStatus,
	rpc_C_CancelFunction,
	rpc_C_WaitForSlotEvent
};

CK_RV
C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	return_val_if_fail (list, CKR_ARGUMENTS_BAD);

	*list = &functionList;
	return CKR_OK;
}
