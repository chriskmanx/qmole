#ifndef _SM_SERVER_H
#define _SM_SERVER_H

#include "statemachine.h"
#include <dbus/dbus-glib.h>

typedef struct SMServer SMServer;
typedef struct SMServerClass SMServerClass;

struct SMServer
{
  GObject parent;

  /* Private */
  DBusGConnection *bus;
  GHashTable *machines;
};

struct SMServerClass
{
  GObjectClass parent;
};

#define SM_TYPE_SERVER              (sm_server_get_type ())
#define SM_SERVER(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), SM_TYPE_SERVER, SMServer))
#define SM_SERVER_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), SM_TYPE_SERVER, SMServerClass))
#define SM_IS_SERVER(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), SM_TYPE_SERVER))
#define SM_IS_SERVER_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), SM_TYPE_SERVER))
#define SM_SERVER_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), SM_TYPE_SERVER, SMServerClass))

GType sm_server_get_type (void);

gboolean sm_server_create_machine (SMServer *server, const char *name, GError **error);

gboolean sm_server_get_machines (SMServer *server, GPtrArray **machines, GError **error);

#endif
