#include "config.h"
#include <orbit/GIOP/giop.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include "giop-private.h"

static LinkConnectionClass *parent_class = NULL;

enum {
	PROP_0,
	PROP_ORB,
	PROP_GIOP_VERSION
};

static void
giop_connection_real_state_changed (LinkConnection      *cnx,
				    LinkConnectionStatus status)
{
	GIOPConnection *gcnx = GIOP_CONNECTION (cnx);

	if (parent_class->state_changed)
		parent_class->state_changed (cnx, status);

	switch (status) {
	case LINK_DISCONNECTED:
		if (gcnx->incoming_msg) {
			giop_recv_buffer_unuse (gcnx->incoming_msg);
			gcnx->incoming_msg = NULL;
		}
		giop_recv_list_zap (gcnx);
		break;
	default:
		break;
	}
}

void
giop_connection_close (GIOPConnection *cnx)
{
	if ((cnx->parent.status == LINK_DISCONNECTED)
	    || (cnx->parent.status == LINK_TIMEOUT))
		return;

	if (cnx->parent.status == LINK_CONNECTED &&
	    (!cnx->parent.was_initiated ||
	     cnx->giop_version == GIOP_1_2)) {
		GIOPSendBuffer *buf;

		buf = giop_send_buffer_use_close_connection (
			cnx->giop_version);
		giop_send_buffer_write (buf, cnx, TRUE);
		giop_send_buffer_unuse (buf);
	}

	link_connection_disconnect (LINK_CONNECTION (cnx));
}

static void
giop_connection_dispose (GObject *obj)
{
	GIOPConnection *cnx = (GIOPConnection *) obj;

	giop_thread_key_release (obj);

	giop_connection_close (cnx);

	giop_connection_destroy_frags (cnx);

	g_assert (cnx->incoming_msg == NULL);

	if (((GObjectClass *)parent_class)->dispose)
		((GObjectClass *)parent_class)->dispose (obj);
}

static void
giop_connection_set_property (GObject           *object,
			      guint              prop_id,
			      const GValue      *value,
			      GParamSpec        *pspec)
{
	GIOPConnection *cnx = (GIOPConnection *) object;

	switch (prop_id) {
	case PROP_ORB:
		cnx->orb_data = g_value_get_pointer (value);
		break;
	case PROP_GIOP_VERSION:
		cnx->giop_version = g_value_get_uint (value);
		break;
	}
}
 
static void
giop_connection_get_property (GObject           *object,
			      guint              prop_id,
			      GValue            *value,
			      GParamSpec        *pspec)
{
	GIOPConnection *cnx = (GIOPConnection *) object;

	switch (prop_id) {
	case PROP_ORB:
		g_value_set_pointer (value, cnx->orb_data);
		break;
	case PROP_GIOP_VERSION:
		g_value_set_uint (value, cnx->giop_version);
		break;
	}
}


static void
giop_connection_class_init (GIOPConnectionClass *klass)
{
	GObjectClass *object_class = (GObjectClass *) klass;

	parent_class = g_type_class_peek_parent (klass);

	object_class->dispose = giop_connection_dispose;
	object_class->set_property = giop_connection_set_property;
	object_class->get_property = giop_connection_get_property;

	g_object_class_install_property
		(object_class, PROP_ORB,
		 g_param_spec_pointer ("orb", NULL, NULL,
				       G_PARAM_READWRITE));
	g_object_class_install_property
		(object_class, PROP_GIOP_VERSION,
		 g_param_spec_uint ("version", NULL, NULL,
				    0, G_MAXINT, 0,
				    G_PARAM_READWRITE));

	klass->parent_class.state_changed = giop_connection_real_state_changed;
	klass->parent_class.handle_input  = giop_connection_handle_input;
}

static void
giop_connection_init (GIOPConnection *cnx)
{
}

GType
giop_connection_get_type (void)
{
	static GType object_type = 0;

	if (!object_type) {
		static const GTypeInfo object_info = {
			sizeof (GIOPConnectionClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) giop_connection_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (GIOPConnection),
			0,              /* n_preallocs */
			(GInstanceInitFunc) giop_connection_init
		};
      
		object_type = g_type_register_static (
			link_connection_get_type(),
			"GIOPConnection", &object_info, 0);
	}  

	return object_type;
}

GIOPConnection *
giop_connection_initiate (gpointer orb_data,
			  const char *proto_name,
			  const char *remote_host_info,
			  const char *remote_serv_info,
			  GIOPConnectionOptions options,
			  GIOPVersion giop_version)
{
	g_return_val_if_fail (remote_host_info != NULL, NULL);

	options |= LINK_CONNECTION_NONBLOCKING;

	return (GIOPConnection *)
		link_connection_initiate
			(giop_connection_get_type (),
			 proto_name, remote_host_info,
			 remote_serv_info, (LinkConnectionOptions)options,
			 "orb", orb_data,
			 "version", (guint) giop_version,
			 NULL);
}

LinkConnectionStatus
giop_connection_try_reconnect (GIOPConnection *cnx)
{
	return link_connection_try_reconnect (LINK_CONNECTION (cnx));
}

void
giop_set_timeout (guint msec)
{
	link_set_timeout (msec);
} 

