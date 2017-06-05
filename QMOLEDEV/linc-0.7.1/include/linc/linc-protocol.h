/*
 * linc-protocol.h: This file is part of the linc library.
 *
 * Authors:
 *    Elliot Lee     (sopwith@redhat.com)
 *    Michael Meeks  (michael@ximian.com)
 *    Mark McLouglin (mark@skynet.ie) & others
 *
 * Copyright 2001, Red Hat, Inc., Ximian, Inc.,
 *                 Sun Microsystems, Inc.
 */
#ifndef _LINC_PROTOCOL_H_
#define _LINC_PROTOCOL_H_

#include <glib/gmacros.h>

G_BEGIN_DECLS

#include <linc/linc-types.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

/* socklen_t seems rather un-portable */
typedef unsigned int LincSockLen;

typedef enum {
	LINC_PROTOCOL_SECURE     = 1<<0,
	LINC_PROTOCOL_NEEDS_BIND = 1<<1
} LINCProtocolFlags;

typedef void (*LINCProtocolSetupFunc)       (int                     fd,
					     LINCConnectionOptions   cnx_flags);
typedef void (*LINCProtocolDestroyFunc)     (int                     fd,
					     const char             *host_info,
					     const char             *serv_info);
typedef struct sockaddr *(*LINCProtocolGetSockAddrFunc) (const LINCProtocolInfo *proto,
							 const char             *hostname,
							 const char             *service,
							 LincSockLen            *saddr_len);

typedef gboolean (*LINCProtocolGetSockInfoFunc) (const LINCProtocolInfo *proto,
						 const struct sockaddr  *sockaddr,
						 gchar                 **hostname,
						 gchar                 **service);

typedef gboolean (*LINCProtocolIsLocal)         (const LINCProtocolInfo *proto,
						 const struct sockaddr  *sockaddr,
						 LincSockLen             saddr_len);

struct _LINCProtocolInfo {
	const char                 *name;
	int                         family;
	int                         addr_len;
	int                         stream_proto_num;
	LINCProtocolFlags           flags;

	LINCProtocolSetupFunc       setup;
	LINCProtocolDestroyFunc     destroy;
	LINCProtocolGetSockAddrFunc get_sockaddr;
	LINCProtocolGetSockInfoFunc get_sockinfo;
	LINCProtocolIsLocal         is_local;
	/* This structure is private and may be extended in future */
	gpointer                    dummy[8];
};

LINCProtocolInfo * const linc_protocol_find     (const char *name);
LINCProtocolInfo * const linc_protocol_find_num (const int   family);
LINCProtocolInfo * const linc_protocol_all      (void);
char                    *linc_get_tmpdir        (void);
void                     linc_set_tmpdir        (const char *dir);

G_END_DECLS

#endif /* _LINC_PROTOCOL_H_ */
