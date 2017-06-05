/*
 * linc-server.h: This file is part of the linc library.
 *
 * Authors:
 *    Elliot Lee     (sopwith@redhat.com)
 *    Michael Meeks  (michael@ximian.com)
 *    Mark McLouglin (mark@skynet.ie) & others
 *
 * Copyright 2001, Red Hat, Inc., Ximian, Inc.,
 *                 Sun Microsystems, Inc.
 */
#ifndef _LINK_SERVER_H_
#define _LINK_SERVER_H_

#include <glib.h>

G_BEGIN_DECLS

#include <linc/linc-protocol.h>
#include <linc/linc-connection.h>

#define LINK_TYPE_SERVER            (link_server_get_type())
#define LINK_TYPE_IS_SERVER(type)   (G_TYPE_FUNDAMENTAL (type) == LINK_TYPE_SERVER)
#define LINK_SERVER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), LINK_TYPE_SERVER, LinkServer))
#define LINK_SERVER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), LINK_TYPE_CONNETION, LinkServerClass))
#define LINK_IS_SERVER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), LINK_TYPE_SERVER))
#define LINK_IS_SERVER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), LINK_TYPE_SERVER))

typedef struct _LinkServerPrivate LinkServerPrivate;

typedef struct {
	GObject                 parent;

	const LinkProtocolInfo *proto;

	char                   *local_host_info;
	char                   *local_serv_info;

	/* Options that incoming connections are created with */
	LinkConnectionOptions   create_options;

	LinkServerPrivate      *priv;
} LinkServer;

typedef struct {
	GObjectClass       parent_class;

	LinkConnection *(* create_connection) (LinkServer     *srv);

	void            (* new_connection)    (LinkServer     *srv,
					       LinkConnection *cnx);
} LinkServerClass;

GType    link_server_get_type (void) G_GNUC_CONST;

gboolean link_server_setup    (LinkServer *srv,
			       const char *proto_name,
			       const char *local_host_info,
			       const char *local_serv_info,
			       LinkConnectionOptions create_options);

G_END_DECLS

#endif /* _LINK_SERVER_H_ */
