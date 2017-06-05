#ifndef DBUS_GOBJECT_SIGNATURE_H
#define DBUS_GOBJECT_SIGNATURE_H

#include <dbus/dbus.h>
#include <dbus/dbus-signature.h>
#include <glib.h>

GType          _dbus_gtype_from_basic_typecode (int typecode);

GType          _dbus_gtype_from_signature      (const char              *signature,
					       gboolean                 is_client);

GType          _dbus_gtype_from_signature_iter (DBusSignatureIter       *sigiter,
					       gboolean                 is_client);

GArray *       _dbus_gtypes_from_arg_signature (const char              *signature,
						gboolean                 is_client);

#endif
