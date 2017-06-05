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
#ifndef _LINK_PROTOCOL_H_
#define _LINK_PROTOCOL_H_

#include <glib.h>

G_BEGIN_DECLS

#include <linc/linc-types.h>
#include <sys/types.h>

#ifdef G_OS_WIN32
#  include <winsock2.h>
#  undef interface		/* #defined as struct! */
#else
#  include <sys/socket.h>
#  include <netdb.h>
#endif

/* socklen_t seems rather un-portable */
typedef unsigned int LinkSockLen;

typedef enum {
	LINK_PROTOCOL_SECURE     = 1<<0,
	LINK_PROTOCOL_NEEDS_BIND = 1<<1
} LinkProtocolFlags;

typedef void (*LinkProtocolSetupFunc)       (int                     fd,
					     LinkConnectionOptions   cnx_flags);
typedef void (*LinkProtocolDestroyFunc)     (int                     fd,
					     const char             *host_info,
					     const char             *serv_info);
typedef struct sockaddr *(*LinkProtocolGetSockAddrFunc) (const LinkProtocolInfo *proto,
							 const char             *hostname,
							 const char             *service,
							 LinkSockLen            *saddr_len);

typedef gboolean (*LinkProtocolGetSockInfoFunc) (const LinkProtocolInfo *proto,
						 const struct sockaddr  *sockaddr,
						 gchar                 **hostname,
						 gchar                 **service);

typedef void (*LinkProtocolPostCreateFunc) (int fd,
					    struct sockaddr *sockaddr);

typedef gboolean (*LinkProtocolIsLocal)         (const LinkProtocolInfo *proto,
						 const struct sockaddr  *sockaddr,
						 LinkSockLen             saddr_len);

struct _LinkProtocolInfo {
	const char                 *name;
	int                         family;
	int                         addr_len;
	int                         stream_proto_num;
	LinkProtocolFlags           flags;

	LinkProtocolSetupFunc       setup;
	LinkProtocolDestroyFunc     destroy;
	LinkProtocolGetSockAddrFunc get_sockaddr;
	LinkProtocolGetSockInfoFunc get_sockinfo;
	LinkProtocolIsLocal         is_local;
	LinkProtocolPostCreateFunc  post_create;
	/* This structure is private and may be extended in future */
	gpointer                    dummy[7];
};

typedef enum {
	LINK_NET_ID_IS_LOCAL,
	LINK_NET_ID_IS_SHORT_HOSTNAME,
	LINK_NET_ID_IS_FQDN,
	LINK_NET_ID_IS_IPADDR,
	LINK_NET_ID_IS_CUSTOM
} LinkNetIdType;


LinkProtocolInfo * link_protocol_find     (const char *name);
LinkProtocolInfo * link_protocol_find_num (const int   family);
LinkProtocolInfo * link_protocol_all      (void);
char                    *link_get_tmpdir        (void);
void                     link_set_tmpdir        (const char *dir);
void                     link_use_local_hostname (LinkNetIdType use);
void                     link_set_local_hostname (const char *host_id);
const char*              link_get_local_hostname (void);

G_END_DECLS

#endif /* _LINK_PROTOCOL_H_ */
