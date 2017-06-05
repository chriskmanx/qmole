/*
 * Copyright 2004 James Bursa <bursa@users.sourceforge.net>
 * Copyright 2006 Rob Kendrick <rjek@rjek.com>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * \file
 * Localised message support implementation.
 *
 * Native language messages are loaded from a file and stored hashed by key for
 * fast access.
 */

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <zlib.h>
#include <stdarg.h>

#include "utils/log.h"
#include "utils/messages.h"
#include "utils/utils.h"
#include "utils/hashtable.h"

/** Messages are stored in a fixed-size hash table. */
#define HASH_SIZE 101

/** The hash table used to store the standard Messages file for the old API */
static struct hash_table *messages_hash = NULL;

/**
 * Read keys and values from messages file.
 *
 * \param  path  pathname of messages file
 * \param  ctx   reference of hash table to merge with.
 * \return NSERROR_OK on sucess and ctx updated or error code on faliure.
 */
static nserror messages_load_ctx(const char *path, struct hash_table **ctx)
{
	char s[400]; /* line buffer */
	gzFile fp; /* compressed file handle */
	struct hash_table *nctx; /* new context */

	assert(path != NULL);

	fp = gzopen(path, "r");
	if (!fp) {
		LOG(("Unable to open messages file \"%.100s\": %s",
		     path, strerror(errno)));

		return NSERROR_NOT_FOUND;
	}

	if (*ctx == NULL) {
		nctx = hash_create(HASH_SIZE);
	} else {
		/**
		 * \note The passed hash is not copied here so this
		 * updates in place.
		 */
		nctx = *ctx;
	}
	if (nctx == NULL) {
		LOG(("Unable to create hash table for messages file %s", path));
		gzclose(fp);
		return NSERROR_NOMEM;
	}

	while (gzgets(fp, s, sizeof s)) {
		char *colon, *value;

		if (s[0] == 0 || s[0] == '#')
			continue;

		s[strlen(s) - 1] = 0;  /* remove \n at end */
		colon = strchr(s, ':');
		if (!colon)
			continue;
		*colon = 0;  /* terminate key */
		value = colon + 1;

		if (hash_add(nctx, s, value) == false) {
			LOG(("Unable to add %s:%s to hash table of %s",
				s, value, path));
			gzclose(fp);
			if (*ctx == NULL) {
				hash_destroy(nctx);
			}
			return NSERROR_INVALID;
		}
	}

	gzclose(fp);

	*ctx = nctx;

	return NSERROR_OK;
}


/**
 * Fast lookup of a message by key.
 *
 * \param  key  key of message
 * \param  ctx  context of messages file to look up in
 * \return value of message, or key if not found
 */
static const char *
messages_get_ctx(const char *key, struct hash_table *ctx)
{
	const char *r = NULL;

	assert(key != NULL);

	/* allow attempts to retrieve messages before context is set up. */
	if (ctx != NULL) {
		r = hash_get(ctx, key);
	}

	/* If called with no context or unable to retrive a value
	 * return the key.
	 */
	if (r == NULL) {
		r = key;
	}

	return r;
}

/* exported interface documented in messages.h */
nserror messages_load(const char *path)
{
	nserror err;

	if (path == NULL) {
		err = NSERROR_BAD_PARAMETER;
	} else {
		LOG(("Loading Messages from '%s'", path));

		err = messages_load_ctx(path, &messages_hash);
	}

	return err;
}

/* exported interface documented in messages.h */
char *messages_get_buff(const char *key, ...)
{
	const char *msg_fmt;
	char *buff = NULL; /* formatted buffer to return */
	int buff_len = 0;
	va_list ap;

	msg_fmt = messages_get_ctx(key, messages_hash);

	va_start(ap, key);
	buff_len = vsnprintf(buff, buff_len, msg_fmt, ap);
	va_end(ap);

	buff = malloc(buff_len + 1);

	if (buff == NULL) {
		LOG(("malloc failed"));
		warn_user("NoMemory", 0);		
	} else {
		va_start(ap, key);
		vsnprintf(buff, buff_len + 1, msg_fmt, ap);
		va_end(ap);
	}

	return buff;
}


/* exported function documented in utils/messages.h */
const char *messages_get(const char *key)
{
	return messages_get_ctx(key, messages_hash);
}


/* exported function documented in utils/messages.h */
const char *messages_get_errorcode(nserror code)
{
	switch (code) {
	case NSERROR_OK:
		/* No error */
		return messages_get_ctx("OK", messages_hash);

	case NSERROR_NOMEM:
		/* Memory exhaustion */
		return messages_get_ctx("NoMemory", messages_hash);

	case NSERROR_NO_FETCH_HANDLER:
		/* No fetch handler for URL scheme */
		return messages_get_ctx("NoHandler", messages_hash);

	case NSERROR_NOT_FOUND:
		/* Requested item not found */
		return messages_get_ctx("NotFound", messages_hash);

	case NSERROR_SAVE_FAILED:
		/* Failed to save data */
		return messages_get_ctx("SaveFailed", messages_hash);

	case NSERROR_CLONE_FAILED:
		/* Failed to clone handle */
		return messages_get_ctx("CloneFailed", messages_hash);

	case NSERROR_INIT_FAILED:
		/* Initialisation failed */
		return messages_get_ctx("InitFailed", messages_hash);

	case NSERROR_MNG_ERROR:
		/* An MNG error occurred */
		return messages_get_ctx("MNGError", messages_hash);

	case NSERROR_BAD_ENCODING:
		/* The character set is unknown */
		return messages_get_ctx("BadEncoding", messages_hash);

	case NSERROR_NEED_DATA:
		/* More data needed */
		return messages_get_ctx("NeedData", messages_hash);

	case NSERROR_ENCODING_CHANGE:
		/* The character set encoding change was unhandled */
		return messages_get_ctx("EncodingChanged", messages_hash);

	case NSERROR_BAD_PARAMETER:
		/* Bad Parameter */
		return messages_get_ctx("BadParameter", messages_hash);

	case NSERROR_INVALID:
		/* Invalid data */
		return messages_get_ctx("Invalid", messages_hash);

	case NSERROR_BOX_CONVERT:
		/* Box conversion failed */
		return messages_get_ctx("BoxConvert", messages_hash);

	case NSERROR_STOPPED:
		/* Content conversion stopped */
		return messages_get_ctx("Stopped", messages_hash);

	case NSERROR_DOM:
		/* DOM call returned error */
		return messages_get_ctx("ParsingFail", messages_hash);

	case NSERROR_CSS:
                /* CSS call returned error */
		return messages_get_ctx("CSSGeneric", messages_hash);

	case NSERROR_CSS_BASE:
		/* CSS base sheet failed */
		return messages_get_ctx("CSSBase", messages_hash);

	case NSERROR_BAD_URL:
		/* Bad URL */
		return messages_get_ctx("BadURL", messages_hash);

	default:
	case NSERROR_UNKNOWN:
		break;
	}

	/* Unknown error */
	return messages_get_ctx("Unknown", messages_hash);
}
