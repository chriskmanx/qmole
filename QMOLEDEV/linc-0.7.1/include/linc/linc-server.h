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
#ifndef _LINC_SERVER_H_
#define _LINC_SERVER_H_

#include <glib/gmacros.h>

G_BEGIN_DECLS

#include <linc/linc-protocol.h>
#include <linc/linc-connection.h>

#define LINC_TYPE_SERVER            (linc_server_get_type())
#define LINC_TYPE_IS_SERVER(type)   (G_TYPE_FUNDAMENTAL (type) == LINC_TYPE_SERVER)
#define LINC_SERVER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), LINC_TYPE_SERVER, LINCServer))
#define LINC_SERVER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), LINC_TYPE_CONNETION, LINCServerClass))
#define LINC_IS_SERVER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), LINC_TYPE_SERVER))
#define LINC_IS_SERVER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), LINC_TYPE_SERVER))

typedef struct _LINCServerPrivate LINCServerPrivate;

typedef struct {
	GObject                 parent;

	const LINCProtocolInfo *proto;

	char                   *local_host_info;
	char                   *local_serv_info;

	/* Options that incoming connections are created with */
	LINCConnectionOptions   create_options;

	LINCServerPrivate      *priv;
} LINCServer;

typedef struct {
	GObjectClass       parent_class;

	LINCConnection *(* create_connection) (LINCServer     *server);

	void            (* new_connection)    (LINCServer     *server,
					       LINCConnection *cnx);
} LINCServerClass;

GType    linc_server_get_type (void) G_GNUC_CONST;

gboolean linc_server_setup    (LINCServer *cnx,
			       const char *proto_name,
			       const char *local_host_info,
			       const char *local_serv_info,
			       LINCConnectionOptions create_options);

G_END_DECLS

#endif /* _LINC_SERVER_H_ */
