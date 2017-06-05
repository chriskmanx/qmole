/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2008 Red Hat, Inc.
 */

#ifndef SOUP_METHOD_H
#define SOUP_METHOD_H 1

G_BEGIN_DECLS

/**
 * SECTION:soup-method
 * @short_description: HTTP method definitions
 *
 * soup-method.h contains a number of defines for standard HTTP and
 * WebDAV headers. You do not need to use these defines; you can pass
 * arbitrary strings to soup_message_new() if you prefer.
 * 
 * The thing that these defines <emphasis>are</emphasis> useful for is
 * performing quick comparisons against #SoupMessage's %method field;
 * because that field always contains an interned string, and these
 * macros return interned strings, you can compare %method directly
 * against these macros rather than needing to use strcmp(). This is
 * most useful in SoupServer handlers. Eg:
 * 
 * <informalexample><programlisting>
 * 	if (msg->method != SOUP_METHOD_GET &amp;&amp; msg->method != SOUP_METHOD_HEAD) {
 * 		soup_message_set_status (msg, SOUP_METHOD_NOT_IMPLEMENTED);
 * 		return;
 * 	}
 * </programlisting></informalexample>
 **/

/* HTTP/1.1 methods */
#define SOUP_METHOD_OPTIONS   (_SOUP_METHOD_OPTIONS ? _SOUP_METHOD_OPTIONS : (_SOUP_METHOD_OPTIONS = g_intern_static_string ("OPTIONS")))
#define SOUP_METHOD_GET       (_SOUP_METHOD_GET ? _SOUP_METHOD_GET : (_SOUP_METHOD_GET = g_intern_static_string ("GET")))
#define SOUP_METHOD_HEAD      (_SOUP_METHOD_HEAD ? _SOUP_METHOD_HEAD : (_SOUP_METHOD_HEAD = g_intern_static_string ("HEAD")))
#define SOUP_METHOD_POST      (_SOUP_METHOD_POST ? _SOUP_METHOD_POST : (_SOUP_METHOD_POST = g_intern_static_string ("POST")))
#define SOUP_METHOD_PUT       (_SOUP_METHOD_PUT ? _SOUP_METHOD_PUT : (_SOUP_METHOD_PUT = g_intern_static_string ("PUT")))
#define SOUP_METHOD_DELETE    (_SOUP_METHOD_DELETE ? _SOUP_METHOD_DELETE : (_SOUP_METHOD_DELETE = g_intern_static_string ("DELETE")))
#define SOUP_METHOD_TRACE     (_SOUP_METHOD_TRACE ? _SOUP_METHOD_TRACE : (_SOUP_METHOD_TRACE = g_intern_static_string ("TRACE")))
#define SOUP_METHOD_CONNECT   (_SOUP_METHOD_CONNECT ? _SOUP_METHOD_CONNECT : (_SOUP_METHOD_CONNECT = g_intern_static_string ("CONNECT")))

/* WebDAV methods */
#define SOUP_METHOD_PROPFIND  (_SOUP_METHOD_PROPFIND ? _SOUP_METHOD_PROPFIND : (_SOUP_METHOD_PROPFIND = g_intern_static_string ("PROPFIND")))
#define SOUP_METHOD_PROPPATCH (_SOUP_METHOD_PROPPATCH ? _SOUP_METHOD_PROPPATCH : (_SOUP_METHOD_PROPPATCH = g_intern_static_string ("PROPPATCH")))
#define SOUP_METHOD_MKCOL     (_SOUP_METHOD_MKCOL ? _SOUP_METHOD_MKCOL : (_SOUP_METHOD_MKCOL = g_intern_static_string ("MKCOL")))
#define SOUP_METHOD_COPY      (_SOUP_METHOD_COPY ? _SOUP_METHOD_COPY : (_SOUP_METHOD_COPY = g_intern_static_string ("COPY")))
#define SOUP_METHOD_MOVE      (_SOUP_METHOD_MOVE ? _SOUP_METHOD_MOVE : (_SOUP_METHOD_MOVE = g_intern_static_string ("MOVE")))
#define SOUP_METHOD_LOCK      (_SOUP_METHOD_LOCK ? _SOUP_METHOD_LOCK : (_SOUP_METHOD_LOCK = g_intern_static_string ("LOCK")))
#define SOUP_METHOD_UNLOCK    (_SOUP_METHOD_UNLOCK ? _SOUP_METHOD_UNLOCK : (_SOUP_METHOD_UNLOCK = g_intern_static_string ("UNLOCK")))

/* Do not use these variables directly; use the macros above, which
 * ensure that they get initialized properly.
 */
extern const char *_SOUP_METHOD_OPTIONS;
extern const char *_SOUP_METHOD_GET;
extern const char *_SOUP_METHOD_HEAD;
extern const char *_SOUP_METHOD_POST;
extern const char *_SOUP_METHOD_PUT;
extern const char *_SOUP_METHOD_DELETE;
extern const char *_SOUP_METHOD_TRACE;
extern const char *_SOUP_METHOD_CONNECT;

extern const char *_SOUP_METHOD_PROPFIND;
extern const char *_SOUP_METHOD_PROPPATCH;
extern const char *_SOUP_METHOD_MKCOL;
extern const char *_SOUP_METHOD_COPY;
extern const char *_SOUP_METHOD_MOVE;
extern const char *_SOUP_METHOD_LOCK;
extern const char *_SOUP_METHOD_UNLOCK;

G_END_DECLS

#endif /* SOUP_METHOD_H */
