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
#ifndef _LINK_TYPES_H_
#define _LINK_TYPES_H_

#include <glib.h>
#include <glib-object.h>

G_BEGIN_DECLS

typedef enum {
	LINK_CONNECTION_SSL          = 1 << 0,
	LINK_CONNECTION_NONBLOCKING  = 1 << 1,
	LINK_CONNECTION_BLOCK_SIGNAL = 1 << 2,
	LINK_CONNECTION_LOCAL_ONLY   = 1 << 3
} LinkConnectionOptions;

typedef struct _LinkWatch        LinkWatch;
typedef struct _LinkProtocolInfo LinkProtocolInfo;

#ifdef G_THREADS_ENABLED

#  define LINK_MUTEX_LOCK(x) G_STMT_START {	\
	if (x)					\
		g_mutex_lock (x);		\
	} G_STMT_END
#  define LINK_MUTEX_UNLOCK(x) G_STMT_START {	\
	if (x)					\
		g_mutex_unlock (x);		\
	} G_STMT_END

#else /* ! G_THREADS_ENABLED */

#  define LINK_MUTEX_LOCK(x)
#  define LINK_MUTEX_UNLOCK(x)

#endif /* G_THREADS_ENABLED */

GMutex *link_mutex_new (void);

G_END_DECLS

#endif /* _LINK_TYPES_H_ */
