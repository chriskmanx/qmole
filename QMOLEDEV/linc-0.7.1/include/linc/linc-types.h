/*
 * linc-types.h: This file is part of the linc library.
 *
 * Authors:
 *    Elliot Lee     (sopwith@redhat.com)
 *    Michael Meeks  (michael@ximian.com)
 *    Mark McLouglin (mark@skynet.ie) & others
 *
 * Copyright 2001, Red Hat, Inc., Ximian, Inc.,
 *                 Sun Microsystems, Inc.
 */
#ifndef _LINC_TYPES_H_
#define _LINC_TYPES_H_

#include <glib/gmacros.h>
#include <glib/gthread.h>
#include <glib-object.h>

G_BEGIN_DECLS

typedef enum {
	LINC_CONNECTION_SSL          = 1 << 0,
	LINC_CONNECTION_NONBLOCKING  = 1 << 1,
	LINC_CONNECTION_BLOCK_SIGNAL = 1 << 2,
	LINC_CONNECTION_LOCAL_ONLY   = 1 << 3
} LINCConnectionOptions;

typedef struct _LincWatch        LincWatch;
typedef struct _LINCProtocolInfo LINCProtocolInfo;

#ifdef G_THREADS_ENABLED

#  define LINC_MUTEX_LOCK(x) G_STMT_START {	\
	if (x)					\
		g_mutex_lock (x);		\
	} G_STMT_END
#  define LINC_MUTEX_UNLOCK(x) G_STMT_START {	\
	if (x)					\
		g_mutex_unlock (x);		\
	} G_STMT_END

#else /* ! G_THREADS_ENABLED */

#  define LINC_MUTEX_LOCK(x)
#  define LINC_MUTEX_UNLOCK(x)

#endif /* G_THREADS_ENABLED */

GMutex *linc_mutex_new (void);

G_END_DECLS

#endif /* _LINC_TYPES_H_ */
