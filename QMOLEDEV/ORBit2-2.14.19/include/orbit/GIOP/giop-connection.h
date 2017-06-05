#ifndef GIOP_CONNECTION_H
#define GIOP_CONNECTION_H 1

#include <orbit/GIOP/giop-types.h>
#include <linc/linc.h>
#include <orbit/GIOP/giop-server.h>

#ifndef G_OS_WIN32
#  include <netdb.h>		/* XXX really needed? */
#endif

G_BEGIN_DECLS

#ifdef ORBIT2_INTERNAL_API

#define GIOP_TYPE_CONNECTION            (giop_connection_get_type ())
#define GIOP_TYPE_IS_CONNECTION(type)   (G_TYPE_FUNDAMENTAL (type) == GIOP_TYPE_CONNECTION)
#define GIOP_CONNECTION(object)	        (GIOP_IS_CONNECTION (object) ? ((GIOPConnection*) (object)) : \
				         G_TYPE_CHECK_INSTANCE_CAST ((object), GIOP_TYPE_CONNECTION, GIOPConnection))
#define GIOP_CONNECTION_CLASS(class)    (GIOP_IS_CONNECTION_CLASS (class) ? ((GIOPConnectionClass*) (class)) : \
				         G_TYPE_CHECK_CLASS_CAST ((class), GIOP_TYPE_CONNECTION, GIOPConnectionClass))
#define GIOP_IS_CONNECTION(object)      (((GIOPConnection*) (object)) != NULL && \
				         GIOP_IS_CONNECTION_CLASS (((GTypeInstance*) (object))->g_class))
#define GIOP_IS_CONNECTION_CLASS(class) (((GTypeClass*) (class)) != NULL && \
				         GIOP_TYPE_IS_CONNECTION (((GTypeClass*) (class))->g_type))

struct _GIOPConnection {
	LinkConnection  parent;

	GIOPRecvBuffer *incoming_msg;
	GList          *incoming_frags;

	GIOPVersion     giop_version;

	gpointer        orb_data;
};

typedef struct {
	LinkConnectionClass parent_class;
} GIOPConnectionClass;

GType           giop_connection_get_type      (void) G_GNUC_CONST;
GIOPConnection *giop_connection_initiate      (gpointer              orb_data,
					       const char           *proto_name,
					       const char           *remote_host_info,
					       const char           *remote_serv_info,
					       GIOPConnectionOptions options,
					       GIOPVersion           giop_version);
void            giop_connections_shutdown     (void);
void            giop_connection_close         (GIOPConnection       *cnx);
LinkConnectionStatus giop_connection_try_reconnect (GIOPConnection *cnx);

#define         giop_connection_ref(cnx)      link_connection_ref(&((cnx)->parent))
#define         giop_connection_unref(cnx)    link_connection_unref(&((cnx)->parent))

/* set the link timeout in milliseconds */
extern void giop_set_timeout (guint msec);

#endif /* ORBIT2_INTERNAL_API */

G_END_DECLS

#endif
