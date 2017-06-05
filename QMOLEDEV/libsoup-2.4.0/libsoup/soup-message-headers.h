/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2005 Novell, Inc.
 */

#ifndef SOUP_MESSAGE_HEADERS_H
#define SOUP_MESSAGE_HEADERS_H 1

#include <libsoup/soup-types.h>

typedef struct SoupMessageHeaders SoupMessageHeaders;
GType soup_message_headers_get_type (void);
#define SOUP_TYPE_MESSAGE_HEADERS (soup_message_headers_get_type ())

typedef enum {
	SOUP_MESSAGE_HEADERS_REQUEST,
	SOUP_MESSAGE_HEADERS_RESPONSE
} SoupMessageHeadersType;

SoupMessageHeaders *soup_message_headers_new      (SoupMessageHeadersType type);

void                soup_message_headers_free     (SoupMessageHeaders *hdrs);

void                soup_message_headers_append   (SoupMessageHeaders *hdrs,
						   const char         *name,
						   const char         *value);
void                soup_message_headers_replace  (SoupMessageHeaders *hdrs,
						   const char         *name,
						   const char         *value);

void                soup_message_headers_remove   (SoupMessageHeaders *hdrs,
						   const char         *name);
void                soup_message_headers_clear    (SoupMessageHeaders *hdrs);

const char         *soup_message_headers_get      (SoupMessageHeaders *hdrs,
						   const char         *name);

typedef void      (*SoupMessageHeadersForeachFunc)(const char         *name,
						   const char         *value,
						   gpointer            user_data);

void                soup_message_headers_foreach  (SoupMessageHeaders *hdrs,
						   SoupMessageHeadersForeachFunc func,
						   gpointer            user_data);

typedef struct {
	/*< private >*/
	gpointer dummy[3];
} SoupMessageHeadersIter;

void                soup_message_headers_iter_init (SoupMessageHeadersIter  *iter,
						    SoupMessageHeaders      *hdrs);
gboolean            soup_message_headers_iter_next (SoupMessageHeadersIter  *iter,
						    const char             **name,
						    const char             **value);

/* Specific headers */

typedef enum {
	SOUP_ENCODING_UNRECOGNIZED,
	SOUP_ENCODING_NONE,
	SOUP_ENCODING_CONTENT_LENGTH,
	SOUP_ENCODING_EOF,
	SOUP_ENCODING_CHUNKED,
	SOUP_ENCODING_BYTERANGES
} SoupEncoding;

SoupEncoding    soup_message_headers_get_encoding        (SoupMessageHeaders *hdrs);
void            soup_message_headers_set_encoding        (SoupMessageHeaders *hdrs,
							  SoupEncoding        encoding);

goffset         soup_message_headers_get_content_length  (SoupMessageHeaders *hdrs);
void            soup_message_headers_set_content_length  (SoupMessageHeaders *hdrs,
							  goffset             content_length);

typedef enum {
	SOUP_EXPECTATION_UNRECOGNIZED = (1 << 0),
	SOUP_EXPECTATION_CONTINUE     = (1 << 1)
} SoupExpectation;

SoupExpectation soup_message_headers_get_expectations    (SoupMessageHeaders *hdrs);
void            soup_message_headers_set_expectations    (SoupMessageHeaders *hdrs,
							  SoupExpectation     expectations);

#endif /* SOUP_MESSAGE_HEADERS_H */
