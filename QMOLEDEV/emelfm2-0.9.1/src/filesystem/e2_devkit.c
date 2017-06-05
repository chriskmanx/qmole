/* $Id: e2_devkit.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2009-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/filesystem/e2_devkit.c
@brief functions related to system drive/disk hardware. Supersedes HAL.
*/

/*
TODO
confirm that mount-point is effectively hardcoded in daemon's mount-opration
check how to work with detachable devices (udisks)
*/

#include "emelfm2.h"

#ifdef E2_DEVKIT

//#ifdef USE_GLIB2_26
//# undef USE_GLIB2_26
//#endif

#ifndef USE_GLIB2_26
# include <dbus/dbus-glib.h>
#endif
#include "e2_fs.h"

#ifdef EXTRA_MESSAGES
# undef EXTRA_MESSAGES
#endif

//define to support devicekit disks as well as udisks, if the latter is N/A
#define DEVKIT_IFACE

#ifdef DEVKIT_IFACE
# define DEVKIT_IFACE_NAME "org.freedesktop.DeviceKit.Disks"
# define DEVKIT_BUS_PATH "/org/freedesktop/DeviceKit/Disks"
//PATH_ROOT supplied to funcs will be "/org/freedesktop/DeviceKit/Disks/devices/"
#endif
#define UDISK_IFACE_NAME "org.freedesktop.UDisks"
#define UDISK_BUS_PATH "/org/freedesktop/UDisks"
//PATH_ROOT supplied to funcs will be "/org/freedesktop/UDisks/devices/"

/*
Property-name strings used here conform to the ones used in gnome-disk-utility 2.28
like "DeviceIsDrive", replacing ones like "device-is-drive".

CHECKME which version of devicekit-disks does this relate to ? = dependency

No use to collect/use luks-related properties. For a removable device, they apply
to volume(s), not the device
*/

//device-properties that are useful here, a subset of all available properties
typedef struct _DeviceProperties
{
	gchar	*device_file; 		//e.g. /dev/sda
#ifdef DEVKIT_IFACE
	gchar	*device_mount_path;	//where the device is mounted, valid only if device-is-mounted is TRUE
#endif
	//for udisks
	gchar	**device_mount_paths;//NULL terminated array of paths where the device is mounted,
								//valid only if device-is-mounted is TRUE

	gchar	*id_usage;	//filesystem; crypto; partitiontable; raid; other; blank=(unknown)
	gchar	*id_type;	//per id-usage: ext3, vfat etc swap blank=(unknown)
	gchar	*drive_media;	//flash* or floppy* or optical*, ? blank or "flash" if unknown
//	guint64 device_media_detection_time;
	gboolean device_is_removable;
	gboolean device_is_detachable; //udisks only
	gboolean device_is_media_available;
	gboolean device_is_media_change_detected;
	gboolean device_is_media_change_detection_inhibited;
	gboolean device_is_drive;
	gboolean device_is_optical_disc;
	gboolean device_is_mounted;
//	gboolean device_is_busy;
	gboolean drive_is_media_ejectable;
//	gboolean drive_requires_eject; not application-controllable ?
	gboolean optical_disc_is_blank;

	gboolean ignored;	//flag for device not relevant, not a daemon property
} DeviceProperties;

typedef struct _DeviceData
{
	gchar *object_path; //=PATH_ROOT"<native-path-cleaned-basename>" i.e. with any '-' >> '_'
#ifdef USE_GLIB2_26
	GDBusProxy *devproxy;
#else
	DBusGProxy *devproxy;	//not the same as the session-proxy, per-device to support async usage of the proxy
#endif
	DeviceProperties props;
	guint timer_id;		//timer to allow DE etc to do [un]mount before we do so
} DeviceData;

typedef struct _DevicesSession
{
#ifdef DEVKIT_IFACE
	gboolean udisks;		//TRUE for udisks session, FALSE for devkit-disks (saves parsing daemon_version)
#endif
	gchar *daemon_version;
	const gchar *iface_name;//DEVKIT_IFACE_NAME or UDISK_IFACE_NAME
#ifdef USE_GLIB2_26
	GDBusConnection *bus;
	GDBusProxy *proxy;
#else
	DBusGConnection *bus;	//connection to system bus
	DBusGProxy *proxy;		//abstracted proxy for interface exported by a connection on bus
#endif
	GHashTable *logged_devices; //known removable devices, with
								// key = (const) device object path,
								// value = the corresponding DeviceData*
} DevicesSession;

static DevicesSession session;

static void _e2_devkit_device_changed_cb (
#ifdef USE_GLIB2_26
GDBusProxy *proxy,
#else
DBusGProxy *proxy,
#endif
	const gchar *object_path, gpointer sessionptr);
static void _e2_devkit_mount_async_cb (
#ifdef USE_GLIB2_26
GObject *proxy, GAsyncResult *res,
#else
DBusGProxy *proxy, DBusGProxyCall *call,
#endif
	gpointer user_data);
static void _e2_devkit_unmount_async_cb (
#ifdef USE_GLIB2_26
GObject *proxy, GAsyncResult *res,
#else
DBusGProxy *proxy, DBusGProxyCall *call,
#endif
	gpointer user_data);


/**
@brief get daemon version
Queries the DaemonVersion property on @a iface of @a object_path owned by the
well-known-name @a iface on the DBus system message bus.
@param bus pointer to dbus connection object
@param iface string like "org.freedesktop.DeviceKit.Disks"
@param object_path string like "/org/freedesktop/DeviceKit/Disks"
@return newly-allocated string containing version descriptor, or NULL upon error
*/
static gchar *_e2_devkit_get_version (
#ifdef USE_GLIB2_26
GDBusConnection *bus,
#else
DBusGConnection *bus,
#endif
	const gchar *iface, const gchar *object_path)
{
	gchar *version;
#ifdef USE_GLIB2_26
	GDBusProxy *prop_proxy;
	GVariant *var;
#else
	DBusGProxy *prop_proxy;
	GValue value = {0};
#endif
	GError *error;

#ifdef USE_GLIB2_26
	prop_proxy = g_dbus_proxy_new_sync
		(bus, G_DBUS_PROXY_FLAGS_DO_NOT_CONNECT_SIGNALS, NULL,
		iface, object_path, "org.freedesktop.DBus.Properties" , NULL, NULL);
#else
	prop_proxy = dbus_g_proxy_new_for_name
		(bus, iface, object_path, "org.freedesktop.DBus.Properties");
#endif
	if (prop_proxy == NULL)
	{
//		_e2_devkit_advise (_("some warning ..."), NULL);
		printd (WARN, "Unable to establish dbus connection for daemon interrogation");
		return NULL;
	}
	error = NULL;

#ifdef USE_GLIB2_26
	var = g_dbus_proxy_call_sync (prop_proxy,
			"Get",
			g_variant_new ("(ss)", iface, "DaemonVersion"),
			G_DBUS_CALL_FLAGS_NONE,
			-1, //default time-limit
			NULL,
			&error);
	if (var != NULL)
	{	//var is a tuple, 1 child
		GVariant *child = NULL;
		g_variant_get_child (var, 0, "v", &child);
		if (G_LIKELY (child != NULL))
		{
			version = g_variant_dup_string (child, NULL);
			g_variant_unref (child);
		}
		else
		{
			version = NULL;
		}
		g_variant_unref (var);
		if (version == NULL)
			version = g_strdup ("protected?");	//hack
		printd (DEBUG,"DaemonVersion property for %s is \"%s\"", object_path, version);
	}
#else
	if (dbus_g_proxy_call (prop_proxy,
			"Get",
			&error,
			G_TYPE_STRING,
			iface,
			G_TYPE_STRING,
			"DaemonVersion",
			G_TYPE_INVALID,
			G_TYPE_VALUE,
			&value,
			G_TYPE_INVALID))
	{
		version = g_strdup (g_value_get_string (&value));
		g_value_unset (&value);
		printd (DEBUG,"DaemonVersion property for %s is \"%s\"", object_path, version);
	}
#endif
	else
	{
		version = NULL;
		printd (DEBUG,"Failed to get DaemonVersion property for %s: %s",
			object_path, error->message);
		g_error_free (error);
	}

	g_object_unref (G_OBJECT(prop_proxy));
	return version;
}
/**
@brief populate relevant properties for a device represented by @a object_path, if it's one of interest
If @a object_path is not for not a removable drive, props->ignored is set TRUE
The property-names conform to the ones used in gnome-disk-utility 2.28
like "DeviceIsDrive", replacing ones like "device-is-drive"
@param sessionptr pointer to connection data struct for this session
@param object_path PATH_ROOT...
@param props pointer to empty (0'd) properties struct to be populated
@return TRUE if process completes successfully, FALSE upon error
*/
static gboolean _e2_devkit_device_properties_get (DevicesSession *sessionptr,
	const gchar *object_path, DeviceProperties *props)
{
	gboolean retval;
	gchar *device_iface;
#ifdef USE_GLIB2_26
	GDBusProxy *prop_proxy;
	GVariant *var, *child;
#else
	DBusGProxy *prop_proxy;
	GValue value = {0};
#endif
	GError *error;
	GHashTable *hash_table;

#ifdef USE_GLIB2_26
	prop_proxy = g_dbus_proxy_new_sync
		(sessionptr->bus, G_DBUS_PROXY_FLAGS_DO_NOT_CONNECT_SIGNALS, NULL,
		sessionptr->iface_name, object_path, "org.freedesktop.DBus.Properties" , NULL, NULL);
#else
	prop_proxy = dbus_g_proxy_new_for_name
		(sessionptr->bus, sessionptr->iface_name, object_path,
		"org.freedesktop.DBus.Properties");
#endif
	if (prop_proxy == NULL)
	{
//		_e2_devkit_advise (_("some warning ..."), NULL);
		printd (WARN, "Unable to establish dbus connection for properties interrogation");
		return FALSE;
	}

	device_iface = g_strconcat (sessionptr->iface_name, ".Device", NULL);
	memset (props, 0, sizeof(DeviceProperties));
	error = NULL;
	//No point in doing async interrogations here
	//Since most devices are not relevant, best to do some separate, minimal,
	//checks before getting full set of device properties
#ifdef USE_GLIB2_26
	var = g_dbus_proxy_call_sync (prop_proxy,
			"Get",
			g_variant_new ("(ss)", device_iface, "DeviceIsDrive"),
			G_DBUS_CALL_FLAGS_NONE,
			-1, //default time-limit
			NULL,
			&error);
	if (var != NULL)
	{	//var is a tuple, 1 child
		child = NULL;
		g_variant_get_child (var, 0, "v", &child);
		if (G_LIKELY (child != NULL))
		{
			props->device_is_drive = g_variant_get_boolean (child);
			g_variant_unref (child);
		}
		g_variant_unref (var);
		if (!props->device_is_drive)
		{
abort:
			g_free (device_iface);
			g_object_unref (G_OBJECT (prop_proxy));
			props->ignored = TRUE;
			return TRUE;
		}
	}
#else
	if (dbus_g_proxy_call (prop_proxy,
			"Get",
			&error,
			G_TYPE_STRING,
			device_iface,
			G_TYPE_STRING,
			"DeviceIsDrive",
			G_TYPE_INVALID,
			G_TYPE_VALUE,
			&value,
			G_TYPE_INVALID))
	{
		props->device_is_drive = g_value_get_boolean (&value);
		g_value_unset (&value);
		if (!props->device_is_drive)
		{
abort:
			g_free (device_iface);
			g_object_unref (G_OBJECT (prop_proxy));
			props->ignored = TRUE;
			return TRUE;
		}
	}
#endif
	else
	{
		printd (DEBUG,"Failed to get DeviceIsDrive property for %s: %s",
			object_path, error->message);
		g_error_free (error);
		g_free (device_iface);
		g_object_unref (G_OBJECT(prop_proxy));
		return FALSE;
	}


#ifdef USE_GLIB2_26
	var = g_dbus_proxy_call_sync (prop_proxy,
			"Get",
			g_variant_new ("(ss)", device_iface, "DeviceIsRemovable"),
			G_DBUS_CALL_FLAGS_NONE,
			-1, //default time-limit
			NULL,
			&error);
	if (var != NULL)
	{	//var is a tuple, 1 child
		child = NULL;
		g_variant_get_child (var, 0, "v", &child);
		if (G_LIKELY (child != NULL))
		{
			props->device_is_removable = g_variant_get_boolean (child);
			g_variant_unref (child);
		}
		g_variant_unref (var);
	}
#else
	if (dbus_g_proxy_call (prop_proxy,
			"Get",
			&error,
			G_TYPE_STRING,
			device_iface,
			G_TYPE_STRING,
			"DeviceIsRemovable",
			G_TYPE_INVALID,
			G_TYPE_VALUE,
			&value,
			G_TYPE_INVALID))
	{
		props->device_is_removable = g_value_get_boolean (&value);
		g_value_unset (&value);
	}
#endif
	else
	{
		printd (DEBUG,"Failed to get DeviceIsRemovable property for %s: %s",
			object_path, error->message);
		g_error_free (error);
		g_free (device_iface);
		g_object_unref (G_OBJECT(prop_proxy));
		return FALSE;
	}

#ifdef DEVKIT_IFACE
	if (sessionptr->udisks)
	{
#endif
#ifdef USE_GLIB2_26
		var = g_dbus_proxy_call_sync (prop_proxy,
				"Get",
				g_variant_new ("(ss)", device_iface, "DriveCanDetach"),
				G_DBUS_CALL_FLAGS_NONE,
				-1, //default time-limit
				NULL,
				&error);
		if (var != NULL)
		{	//var is a tuple, 1 child
			child = NULL;
			g_variant_get_child (var, 0, "v", &child);
			if (G_LIKELY (child != NULL))
			{
				props->device_is_detachable = g_variant_get_boolean (child);
				g_variant_unref (child);
			}
			g_variant_unref (var);
			if (props->device_is_detachable)
				printd (DEBUG,"Device %s is detachable", object_path);
		}
#else
		if (dbus_g_proxy_call (prop_proxy,
				"Get",
				&error,
				G_TYPE_STRING,
				device_iface,
				G_TYPE_STRING,
				"DriveCanDetach",
				G_TYPE_INVALID,
				G_TYPE_VALUE,
				&value,
				G_TYPE_INVALID))
		{
			props->device_is_detachable = g_value_get_boolean (&value);
			g_value_unset (&value);
			if (props->device_is_detachable)
				printd (DEBUG,"Device %s is detachable", object_path);
		}
#endif
		else
		{
			printd (DEBUG,"Failed to get DriveCanDetach property for %s: %s",
				object_path, error->message);
			g_error_free (error);
			g_free (device_iface);
			g_object_unref (G_OBJECT(prop_proxy));
			return FALSE;
		}
#ifdef DEVKIT_IFACE
	}
#endif
	if (!(props->device_is_removable || props->device_is_detachable))
		goto abort;

	//populate other properties the slow and lazy way (get all properties then filter them)
#ifdef USE_GLIB2_26
	var = g_dbus_proxy_call_sync (prop_proxy,
			"GetAll",
			g_variant_new ("(s)", device_iface),
			G_DBUS_CALL_FLAGS_NONE,
			-1, //default time-limit
			NULL,
			&error);
	if (var != NULL)
	{	//var is a tuple
		GVariantIter *iter;
		GVariant *propvar;
		gchar *key;
		//stuff into hash table for faster lookup
		hash_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, (GDestroyNotify) g_variant_unref);
		g_variant_get (var, "(a{sv})", &iter);
		while (g_variant_iter_next (iter, "{sv}", &key, &propvar))
		{
#ifdef EXTRA_MESSAGES
			printd (DEBUG, "got property %s -> %s", key, g_variant_print (propvar, FALSE));
#endif
			g_hash_table_insert (hash_table, key, propvar); //adopts keystring and valuevar
		}
		g_variant_iter_free (iter);
		g_variant_unref (var);
		//interrogate stuff in table
		propvar = g_hash_table_lookup (hash_table, "DeviceFile");
		if (propvar != NULL)
			props->device_file = g_variant_dup_string (propvar, NULL);
#ifdef DEVKIT_IFACE
		if (!sessionptr->udisks)
		{
			propvar = g_hash_table_lookup (hash_table, "DeviceMountPath");
			if (propvar != NULL)
				props->device_mount_path = g_variant_dup_string (propvar, NULL);
		}
		else
		{
#endif
			propvar = g_hash_table_lookup (hash_table, "DeviceMountPaths");
			if (propvar != NULL)
				props->device_mount_paths = g_variant_dup_strv (propvar, NULL);
#ifdef DEVKIT_IFACE
		}
#endif
		propvar = g_hash_table_lookup (hash_table, "IdUsage");
		if (propvar != NULL)
			props->id_usage = g_variant_dup_string (propvar, NULL);
		propvar = g_hash_table_lookup (hash_table, "IdType");
		if (propvar != NULL)
			props->id_type = g_variant_dup_string (propvar, NULL);
		propvar = g_hash_table_lookup (hash_table, "DriveMedia");
		if (propvar != NULL)
			props->drive_media = g_variant_dup_string (propvar, NULL);
//		propvar = g_hash_table_lookup (hash_table, "DeviceMediaDetectionTime");
//		if (propvar != NULL)
//			props->device_media_detection_time = g_variant_get_uint64 (propvar);
		propvar = g_hash_table_lookup (hash_table, "DeviceIsMediaAvailable");
		if (propvar != NULL)
			props->device_is_media_available = g_variant_get_boolean (propvar);
		propvar = g_hash_table_lookup (hash_table, "DeviceIsMediaChangeDetected");
		if (propvar != NULL)
			props->device_is_media_change_detected = g_variant_get_boolean (propvar);

		propvar = g_hash_table_lookup (hash_table, "DeviceIsMediaChangeDetectionInhibited");
		if (propvar != NULL)
			props->device_is_media_change_detection_inhibited = g_variant_get_boolean (propvar);
		propvar = g_hash_table_lookup (hash_table, "DeviceIsOpticalDisc");
		if (propvar != NULL)
			props->device_is_optical_disc = g_variant_get_boolean (propvar);
		propvar = g_hash_table_lookup (hash_table, "DeviceIsMounted");
		if (propvar != NULL)
			props->device_is_mounted = g_variant_get_boolean (propvar);
//		propvar = g_hash_table_lookup (hash_table, "device-is-busy");
//		if (propvar != NULL)
//			props->device_is_busy = g_variant_get_boolean (propvar);
		propvar = g_hash_table_lookup (hash_table, "DriveIsMediaEjectable");
		if (propvar != NULL)
			props->drive_is_media_ejectable = g_variant_get_boolean (propvar);
//		propvar = g_hash_table_lookup (hash_table, "DriveRequiresEject");
//		if (propvar != NULL)
//			props->drive_requires_eject = g_variant_get_boolean (propvar);
		propvar = g_hash_table_lookup (hash_table, "OpticalDiscIsBlank");
		if (propvar != NULL)
			props->optical_disc_is_blank = g_variant_get_boolean (propvar);

		g_hash_table_destroy (hash_table);
		retval = TRUE;
	}
#else
	if (dbus_g_proxy_call (prop_proxy,
			"GetAll",
			&error,
			G_TYPE_STRING,
			device_iface,
			G_TYPE_INVALID,
			dbus_g_type_get_map ("GHashTable", G_TYPE_STRING, G_TYPE_VALUE),
			&hash_table,
			G_TYPE_INVALID))
	{
		GValue *propvalue;

		propvalue = g_hash_table_lookup (hash_table, "DeviceFile");
		if (propvalue != NULL)
			props->device_file = g_strdup (g_value_get_string (propvalue));
#ifdef DEVKIT_IFACE
		if (!sessionptr->udisks)
		{
			propvalue = g_hash_table_lookup (hash_table, "DeviceMountPath");
			if (propvalue != NULL)
				props->device_mount_path = g_strdup (g_value_get_string (propvalue));
		}
		else
		{
#endif
			propvalue = g_hash_table_lookup (hash_table, "DeviceMountPaths");
			if (propvalue != NULL)
				props->device_mount_paths = g_strdupv (g_value_get_boxed (propvalue));
#ifdef DEVKIT_IFACE
		}
#endif
		propvalue = g_hash_table_lookup (hash_table, "IdUsage");
		if (propvalue != NULL)
			props->id_usage = g_strdup (g_value_get_string (propvalue));
		propvalue = g_hash_table_lookup (hash_table, "IdType");
		if (propvalue != NULL)
			props->id_type = g_strdup (g_value_get_string (propvalue));
		propvalue = g_hash_table_lookup (hash_table, "DriveMedia");
		if (propvalue != NULL)
			props->drive_media = g_strdup (g_value_get_string (propvalue));
//		propvalue = g_hash_table_lookup (hash_table, "DeviceMediaDetectionTime");
//		if (propvalue != NULL)
//			props->device_media_detection_time = g_value_get_uint64 (propvalue);
		propvalue = g_hash_table_lookup (hash_table, "DeviceIsMediaAvailable");
		if (propvalue != NULL)
			props->device_is_media_available = g_value_get_boolean (propvalue);
		propvalue = g_hash_table_lookup (hash_table, "DeviceIsMediaChangeDetected");
		if (propvalue != NULL)
			props->device_is_media_change_detected = g_value_get_boolean (propvalue);

		propvalue = g_hash_table_lookup (hash_table, "DeviceIsMediaChangeDetectionInhibited");
		if (propvalue != NULL)
			props->device_is_media_change_detection_inhibited = g_value_get_boolean (propvalue);
		propvalue = g_hash_table_lookup (hash_table, "DeviceIsOpticalDisc");
		if (propvalue != NULL)
			props->device_is_optical_disc = g_value_get_boolean (propvalue);
		propvalue = g_hash_table_lookup (hash_table, "DeviceIsMounted");
		if (propvalue != NULL)
			props->device_is_mounted = g_value_get_boolean (propvalue);
//		propvalue = g_hash_table_lookup (hash_table, "device-is-busy");
//		if (propvalue != NULL)
//			props->device_is_busy = g_value_get_boolean (propvalue);
		propvalue = g_hash_table_lookup (hash_table, "DriveIsMediaEjectable");
		if (propvalue != NULL)
			props->drive_is_media_ejectable = g_value_get_boolean (propvalue);
//		propvalue = g_hash_table_lookup (hash_table, "DriveRequiresEject");
//		if (propvalue != NULL)
//			props->drive_requires_eject = g_value_get_boolean (propvalue);
		propvalue = g_hash_table_lookup (hash_table, "OpticalDiscIsBlank");
		if (propvalue != NULL)
			props->optical_disc_is_blank = g_value_get_boolean (propvalue);

		g_hash_table_destroy (hash_table);
		retval = TRUE;
	}
#endif //ndef USE_GLIB2_26
	else
	{
		printd (DEBUG,"Failed to get all properties for %s: %s",
			object_path, error->message);
		g_error_free (error);
		retval = FALSE;
	}
	//CHECKME props->ignored = ?
	g_free (device_iface);
	g_object_unref (G_OBJECT (prop_proxy));

	return retval;
}
/**
@brief cleanup allocated properties data for a device
@param props the data struct to be processed
@return
*/
static void _e2_devkit_device_properties_clear (DeviceProperties *props)
{
	g_free (props->device_file);
#ifdef DEVKIT_IFACE
	g_free (props->device_mount_path);
#endif
	g_strfreev (props->device_mount_paths);
	g_free (props->id_usage);
	g_free (props->id_type);
	g_free (props->drive_media);
}
/**
@brief cleanup device-data
@param data pointer to data struct for a device
@return
*/
static void _e2_devkit_device_data_clear (DeviceData *data)
{
	if (data->timer_id > 0)	//clobber any timeout now in operation
	{
		g_source_remove (data->timer_id);
//redundant unless some race-risk exists		data->timer_id = 0;
	}
	g_free (data->object_path);
	g_object_unref (G_OBJECT(data->devproxy));
	_e2_devkit_device_properties_clear (&data->props);

	DEALLOCATE (DeviceData, data);
}
/**
@brief zero most device-data
@param data data for a device
@return
*/
static void _e2_devkit_device_data_freshen (DeviceData *data)
{
	//NOTE: ensure this cleanup conforms to the contents of data->props
#ifdef DEVKIT_IFACE
	g_free (data->props.device_mount_path);
	data->props.device_mount_path = NULL;
#endif
	g_strfreev (data->props.device_mount_paths);
	data->props.device_mount_paths = NULL;

	g_free (data->props.id_usage);
 	data->props.id_usage = NULL;

	g_free (data->props.id_type);
 	data->props.id_type = NULL;

	g_free (data->props.drive_media);
 	data->props.drive_media = NULL;

	data->props.device_is_mounted = FALSE;
	data->props.device_is_media_available = FALSE;
}
/**
@brief determine whether @a object_path represents a device we're interested in,
 and if so, set up data for it.
@param sessionptr pointer to session data struct
@param object_path PATH_ROOT...
@return pointer to device data, or NULL if not wanted or error
*/
static DeviceData *_e2_devkit_device_get (DevicesSession *sessionptr,
	const gchar *object_path)
{
	DeviceProperties props;
	if (!_e2_devkit_device_properties_get (sessionptr, object_path, &props))
	{
		printd (DEBUG, "Devices deamon cannot determine properties for device %s", object_path);
		return NULL;
	}

	if (props.ignored)
	{
//		printd (DEBUG, "Devices deamon not interested in device %s", object_path);
		return NULL;
	}
	else
	{
		//this is a device of interest
		gchar *device_iface;
		DeviceData *device;
#ifdef USE_GLIB2_26
		GDBusProxy *dev_proxy;
#else
		DBusGProxy *dev_proxy; //CHECKME needed ?
#endif
		device = ALLOCATE0 (DeviceData);
		CHECKALLOCATEDWARN (device, _e2_devkit_device_properties_clear (&props);return NULL;)

		device_iface = g_strconcat (sessionptr->iface_name, ".Device", NULL);
#ifdef USE_GLIB2_26
		dev_proxy = g_dbus_proxy_new_sync
			(sessionptr->bus, G_DBUS_PROXY_FLAGS_NONE, NULL,
			sessionptr->iface_name, object_path, device_iface , NULL, NULL);
#else
		dev_proxy = dbus_g_proxy_new_for_name
			(sessionptr->bus, sessionptr->iface_name, object_path, device_iface);
#endif
		g_free (device_iface);

		if (dev_proxy == NULL)
		{
//			_e2_devkit_advise (_("message"), NULL);
			printd (WARN, "Unable to connect to devicekit daemon");
			_e2_devkit_device_properties_clear (&props);
			DEALLOCATE (DeviceData, device);
			return NULL;
		}

		device->object_path = g_strdup (object_path);
		memcpy (&device->props, &props, sizeof (DeviceProperties));
		device->devproxy = dev_proxy;

#ifndef USE_GLIB2_26
		dbus_g_proxy_set_default_timeout (dev_proxy, INT_MAX);
#endif
		return device;
	}
}
/**
@brief log current removable devices
@param sessionptr pointer to devkit data struct
@return TRUE if enumeration succeeded
*/
static gboolean _e2_devkit_detect_current_removables (DevicesSession *sessionptr)
{
#ifdef USE_GLIB2_26
	GVariant *var;
#else
	guint i;
	GPtrArray *devices;
#endif
	GError *error;

//	printd (DEBUG, "_e2_devkit_detect_current_removables");
	error = NULL;
	//CHECKME synchronous process ok ?
#ifdef USE_GLIB2_26
	var = g_dbus_proxy_call_sync (sessionptr->proxy,
			"EnumerateDevices",
			NULL,
			G_DBUS_CALL_FLAGS_NONE,
			-1, //default time-limit
			NULL,
			&error);
	if (var != NULL)
	{	//var is a tuple
		GVariantIter *iter;
		gchar *path;
		g_variant_get (var, "(ao)", &iter);
		while (g_variant_iter_next (iter, "o", &path))
		{
#ifdef EXTRA_MESSAGES
			printd (DEBUG, "Found device object-path %s", path);
#endif
			DeviceData *device = _e2_devkit_device_get (sessionptr, path);
			if (device != NULL)
			{
				printd (DEBUG, "Logging removable device %s", device->props.device_file);
				g_hash_table_insert (sessionptr->logged_devices,
					(gpointer) device->object_path,	//not another copy, not truncated
					(gpointer) device);
			}
//			else //it's not removable
//				printd (DEBUG, "Skipping device %s ", path);
			g_free (path);
		}
		g_variant_iter_free (iter);
		g_variant_unref (var);
		return TRUE;
	}
#else
	if (dbus_g_proxy_call (sessionptr->proxy,
		"EnumerateDevices",
		&error,
		G_TYPE_INVALID,
		dbus_g_type_get_collection ("GPtrArray", DBUS_TYPE_G_OBJECT_PATH),
		&devices,
		G_TYPE_INVALID))
	{
//		printd (DEBUG, "Devices daemon reported %u devices", devices->len);
		for (i = 0; i < devices->len; i++)
		{
			DeviceData *device = _e2_devkit_device_get (sessionptr, devices->pdata[i]);
			if (device != NULL)
			{
				printd (DEBUG, "Logging removable device %s", device->props.device_file);
				g_hash_table_insert (sessionptr->logged_devices,
					(gpointer) device->object_path,	//not another copy, not truncated
					(gpointer) device);
			}
	//		else
	//			printd (DEBUG, "Skipping device %s @ %x", device->props.device_file, device);

			g_free (devices->pdata[i]); //CHECKME does the array-data self-destruct (glib >= 2.22)
			devices->pdata[i] = NULL;	//in case of double-free when array destroyed
		}

		g_ptr_array_free (devices, TRUE);
		return TRUE;
	}
#endif
	else
	{
		printd (WARN, "Cannot get devices data: %s", error->message);
		g_error_free (error);
		return FALSE;
	}
}
/**
@brief automount a volume
This is called from within timer callback i.e. BGL is off
@param sessionptr
@param device

@return TRUE if the mount is performed successfully or is not needed
*/
static gboolean _e2_devkit_mount_volume (DevicesSession *sessionptr, DeviceData *device)
{
	gboolean optical;
	gchar *fstype;

	optical = (device->props.drive_media != NULL
				&& g_str_has_prefix (device->props.drive_media, "optical"));

	if (device->props.device_is_mounted
		|| !device->props.device_is_media_available
		|| (optical && device->props.optical_disc_is_blank)
		)
	{
		printd (DEBUG, "Device %s does not need to be mounted", device->props.device_file);
		return TRUE;
	}

	fstype = device->props.id_usage;
	if (fstype == NULL ||
		!( strcmp (fstype, "filesystem") == 0
		|| strcmp (fstype, "raid") == 0 //OK ?
		|| strcmp (fstype, "crypto") == 0)) //OK ? //CHECKME "other"
	{
		printd (DEBUG, "Unmountable device %s", device->props.device_file);
		return TRUE;	//some unmountable sort of device
	}

	fstype = device->props.id_type;
	if (fstype == NULL || *fstype == '\0')
	{
		printd (DEBUG, "Unrecognised filesystem-type for device %s", device->props.device_file);
		return TRUE;	//not a data-medium
	}

#ifdef USE_GLIB2_26
	g_dbus_proxy_call (device->devproxy,
		"FilesystemMount",
		g_variant_new ("(sas)", fstype, NULL), //CHECKME no mount options ?
		G_DBUS_CALL_FLAGS_NONE,
		-1,
		NULL,
		_e2_devkit_mount_async_cb,
		NULL); //cb must destroy dev_proxy
	return TRUE;
#else
	//apparently, a mountpoint can't be specified to the daemon
	//so we don't bother with non-default mount-options, either
	return
		(dbus_g_proxy_begin_call (device->devproxy, "FilesystemMount",
			_e2_devkit_mount_async_cb,
			NULL, (GDestroyNotify) NULL, //no callback data or cleanup
			G_TYPE_STRING, fstype,
			G_TYPE_STRV, NULL,  //no mount options
			G_TYPE_INVALID)
		!= NULL);
#endif
}

/**
@brief if possible, change CWD out of @mountpoint so we don't ourself block the unmount
@param mountpoint a mountpoint path supplied by the daemon, converted to UTF-8

@return
*/
static void _e2_devkit_skipaway (const gchar *mountpoint)
{
	gchar *elsewhere, *s;

	if (mountpoint == NULL)
		return;
	elsewhere = g_strdup (mountpoint);
	g_strchug (elsewhere); //just in case ...
	if (*elsewhere != G_DIR_SEPARATOR)
	{
		g_free (elsewhere);
		return;
	}
	if (e2_utils_pass_whitespace ((gchar *)(elsewhere + 1)) == NULL)
	{
		g_free (elsewhere);
		return; //nothing to do
	}
	while ((s = strrchr (elsewhere, G_DIR_SEPARATOR)) != NULL)
	{
		if (s > elsewhere) //keep the root dir
			*s = '\0';
		if (!g_str_has_prefix (elsewhere, mountpoint))
		{
			e2_fs_get_valid_path (&elsewhere, TRUE E2_ERR_NONE());
			e2_pane_change_dir (NULL, elsewhere);
			break;
		}
		else if (s == elsewhere)
			break; //nowhere to go
	}
	g_free (elsewhere);
}
/**
@brief change displayed directory if necessary, after an unmount
BGL may be open or closed
@param rt pointer to pane data, curr_pane or other_pane
@return
*/
static void _e2_devkit_checkdir (E2_PaneRuntime *rt)
{
	gchar *elsewhere = g_strdup (rt->path);
	g_strchug (elsewhere); //just in case ...
	if (!e2_fs_get_valid_path (&elsewhere, TRUE E2_ERR_NONE()))
		e2_pane_change_dir (rt, elsewhere);
	g_free (elsewhere);
}
/**
@brief auto-unmount a volume
This is called from within timer callback i.e. BGL is off
@param sessionptr pointer to devkit data struct
@param device pointer to data for the device

@return TRUE if the unmount is performed successfully or is not needed
*/
static gboolean _e2_devkit_unmount_volume (DevicesSession *sessionptr, DeviceData *device)
{
	//CHECKME upstream check for still-need-unmount is too racy?
	if (!device->props.device_is_mounted)
	{
		printd (DEBUG, "_e2_devkit_unmount_volume() - no need to do anything");
		return TRUE;
	}

	gchar *mountpoint;
#ifdef DEVKIT_IFACE
	mountpoint = device->props.device_mount_path;
	gboolean moved;
	if (!sessionptr->udisks && mountpoint != NULL) //this is a devkit-disks session
	{
		mountpoint = F_FILENAME_FROM_LOCALE (mountpoint);
# ifdef E2_VFSTMP
		//FIXME dir when not mounted local
# endif
		if (g_str_has_prefix (curr_view->dir, mountpoint))
		{
			_e2_devkit_skipaway (mountpoint);
			moved = TRUE;
		}
		else
			moved = FALSE;
		F_FREE (mountpoint, device->props.device_mount_path);
	}
	else
		moved = FALSE;

	if (sessionptr->udisks && !moved)
	{
#endif
		gchar **allmounts;
		for (allmounts = device->props.device_mount_paths;
			(mountpoint = *allmounts) != NULL;
			allmounts++)
		{
			mountpoint = F_FILENAME_FROM_LOCALE (mountpoint);
#ifdef E2_VFSTMP
			//FIXME dir when not mounted local
#endif
			if (g_str_has_prefix (curr_view->dir, mountpoint))
			{
				_e2_devkit_skipaway (mountpoint);
				F_FREE (mountpoint, device->props.device_mount_path);
				break;
			}
			F_FREE (mountpoint, device->props.device_mount_path);
		}
#ifdef DEVKIT_IFACE
	}
#endif
	//do it
#if 0 //direct
	gint res = run unmount command
	retval = (res == 0);
	//do stuff if successful
	if (retval)
		_e2_devkit_device_data_freshen (device);
	return retval;
#else //via the daemon
//	gpointer cb_data = device;
//	gchar *options[2];
//	options[0] = NULL;
//# ifdef DEBUG_MESSAGES
//	gboolean CHECKME;
//# endif

# ifdef USE_GLIB2_26
	g_dbus_proxy_call (device->devproxy,
		"FilesystemUnmount",
		NULL, //g_variant_new ("(as)", ....), //CHECKME no unmount options
		G_DBUS_CALL_FLAGS_NONE,
		2000,
		NULL,
		_e2_devkit_unmount_async_cb,
		device);
	return TRUE;
# else
	return
		(dbus_g_proxy_begin_call (device->devproxy, "FilesystemUnmount",
			_e2_devkit_unmount_async_cb,
			device, (GDestroyNotify) NULL,
			G_TYPE_STRV, /*options*/NULL, G_TYPE_INVALID)
		!= NULL);
# endif
#endif
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief callback for completion of async mount operation
Unlike gtk callbacks, assumes BGL open/off when using libdbus-glib
@param proxy proxy for the remote interface
@param call the pending call ID from dbus_g_proxy_begin_call()
@param user_data UNUSED pointer specified when cb was connected
@return
*/
static void _e2_devkit_mount_async_cb (
#ifdef USE_GLIB2_26
GObject *proxy, GAsyncResult *res,
#else
DBusGProxy *proxy, DBusGProxyCall *call,
#endif
	gpointer user_data)
{
#ifdef USE_GLIB2_26
	GVariant *var;
#else
	gchar *mount_path;
#endif
	GError *error = NULL;

#ifdef USE_GLIB2_26
	var = g_dbus_proxy_call_finish ((GDBusProxy*)proxy, res, &error);
	if (error == NULL)
	{
		printd (DEBUG, "Device mounted at %s", g_variant_get_string (var, NULL));
		g_variant_unref (var);
	}
#else
	dbus_g_proxy_end_call (proxy, call, &error, G_TYPE_STRING, &mount_path,
		G_TYPE_INVALID);
	if (error == NULL)
	{
		printd (DEBUG, "Device mounted at %s", mount_path);
		//do stuff ?
/*		//"refresh" the filelist(s) if needed
		gchar *utf = F_FILENAME_FROM_LOCALE (mount_path);
		if (g_str_has_prefix (curr_view->dir, utf))
			do something
		if (g_str_has_prefix (other_view->dir, utf))
			do something
		F_FREE (utf, mount_path);
*/
		g_free (mount_path);
	}
#endif
	else
	{
		gchar *msg = g_strdup_printf (_("Error mounting device: %s"), error->message);
#ifdef USE_GLIB2_26
		NEEDCLOSEBGL
#else
		CLOSEBGL
#endif
		e2_output_print_error (msg, TRUE);
#ifdef USE_GLIB2_26
		NEEDOPENBGL
#else
		OPENBGL
#endif
		g_error_free (error);
	}
#ifdef USE_GLIB2_26
	g_object_unref (proxy);
#endif
}
/**
@brief callback for completion of async unmount operation
@param proxy proxy for the remote interface (not unreffed here)
@param call the pending call ID from dbus_g_proxy_begin_call()
@param user_data pointer to device data specified when cb was connected (do not free)
@return
*/
static void _e2_devkit_unmount_async_cb	(
#ifdef USE_GLIB2_26
GObject *proxy, GAsyncResult *res,
#else
DBusGProxy *proxy, DBusGProxyCall *call,
#endif
gpointer user_data)
{
	GError *error = NULL;
#ifdef USE_GLIB2_26
	GVariant *var = g_dbus_proxy_call_finish ((GDBusProxy*)proxy, res, &error);
#else
	dbus_g_proxy_end_call (proxy, call, &error, G_TYPE_INVALID);
#endif
	if (error == NULL)
	{
		//do stuff ?
		DeviceData *device = (DeviceData *)user_data;
		printd (DEBUG, "Device %s unmounted", device->props.device_file);
		_e2_devkit_device_data_freshen (device);
#ifdef USE_GLIB2_26
		g_variant_unref (var);
#endif
	}
	else
	{
		gchar *msg = g_strdup_printf (_("Error unmounting device: %s"), error->message);
#ifdef USE_GLIB2_26
		NEEDCLOSEBGL
#else
		CLOSEBGL
#endif
		e2_output_print_error (msg, TRUE);
#ifdef USE_GLIB2_26
		NEEDOPENBGL
#else
		OPENBGL
#endif
		g_error_free (error);
	}
#ifdef USE_GLIB2_26
	g_object_unref (proxy);
#endif
}
/**
@brief timer callback after a device-add event
If the device's volume(s) are not yet mounted (by the OS etc), mount them now.
Any unmount that happens before this timeout expired will have aborted this callback.
@param device pointer to data for the device

@return FALSE (to cancel the timer) unless mount-operation is needed but it failed
*/
static gboolean _e2_devkit_check_mount (DeviceData *device)
{
	device->timer_id = 0;	//ASAP indicate to any other process
							//but we don't remove this source in case mount fails
	printd (DEBUG, "Device-mount timer callback, device %s", device->props.device_file);

	if (!device->props.device_is_mounted
		//&& non-blank media is present
		)
	{
		printd (DEBUG, "device still not mounted");
#ifdef DEBUG_MESSAGES
		gboolean retval = !_e2_devkit_mount_volume (&session, device);
		if (retval)
			printd (DEBUG, "mount operation failed");
		return retval;	//try again in case of failure
#else
		return (!_e2_devkit_mount_volume (&session, device));
#endif
	}

	return FALSE;
}
/**
@brief callback for a device-add event
@param proxy connection to devkit daemon
@param object_path PATH_ROOT...
@param sessionptr pointer to device monitor data struct specified when callback was connected
@return
*/
static void _e2_devkit_device_added_cb (
#ifdef USE_GLIB2_26
GDBusProxy *proxy,
#else
DBusGProxy *proxy,
#endif
	const gchar *object_path, gpointer sessionptr)
{
	DevicesSession *sp;
	DeviceData *device;

	sp = (DevicesSession *)sessionptr;
	device = g_hash_table_lookup (sp->logged_devices, object_path);

	if (G_LIKELY(device == NULL))
	{
		device = _e2_devkit_device_get (sp, object_path);
		if (device != NULL)
		{
			//new device is one we want
			printd (DEBUG, "Logging device %s @ %x", device->props.device_file, device);
			g_hash_table_insert (sp->logged_devices,
				(gpointer) device->object_path,	//not another copy, not truncated
				(gpointer) device);
			//CHECKME is 5-sec delay long enough ?
			device->timer_id =
#ifdef USE_GLIB2_14
				g_timeout_add_seconds (5,
#else
				g_timeout_add (5000,
#endif
				(GSourceFunc) _e2_devkit_check_mount, device);
		}
		else
			printd (DEBUG,"Not monitoring device %s", object_path);
	}
	else
	{
		//clobber any unmount callback
		if (device->timer_id > 0)
		{
			g_source_remove (device->timer_id);
			device->timer_id = 0;
		}
		printd (WARN, "Devices daemon add-event for previously added device %s, treat as change",
			object_path);
#ifdef USE_GLIB2_26
		NEEDOPENBGL
#endif
		_e2_devkit_device_changed_cb (proxy, object_path, sessionptr);
#ifdef USE_GLIB2_26
		NEEDCLOSEBGL
#endif
	}
}
/**
@brief timer callback after a device-remove event
If the device is not unmounted yet (by the OS etc), unmount it now
Also, always cleanup the mount data
@param device pointer to data struct for the affected device

@return FALSE (to cancel the timer) if unmount succeeded or not needed
*/
static gboolean _e2_devkit_check_unmount (DeviceData *device)
{
	gboolean retval;

	if (device->timer_id > 0)
	{
		g_source_remove (device->timer_id);	//the source may now be unrelated to this callback
		device->timer_id = 0;	//ASAP indicate to any other process
	}

	printd (DEBUG, "Device unmount timer-callback");

	if (device->props.device_is_mounted)
	{
		printd (DEBUG, "device is still mounted");
		retval = !_e2_devkit_unmount_volume (&session, device);
#ifdef DEBUG_MESSAGES
		if (retval)
			printd (DEBUG, "but unmount operation failed");
#endif
	}
	else
	{
		printd (DEBUG, "device is already unmounted");
		retval = FALSE;
		_e2_devkit_device_data_freshen (device);
	}
	//ensure neither filelist shows the unmounted device
	_e2_devkit_checkdir (curr_pane);
	_e2_devkit_checkdir (other_pane);

	return retval;
}
/**
@brief callback for a device-remove event
@param proxy
@param object_path PATH_ROOT...
@param sessionptr pointer to device monitor data struct specified when callback was connected
@return
*/
static void _e2_devkit_device_removed_cb (
#ifdef USE_GLIB2_26
GDBusProxy *proxy,
#else
DBusGProxy *proxy,
#endif
	const gchar *object_path, gpointer sessionptr)
{
	DevicesSession *sp;
	DeviceData *device;

	sp = (DevicesSession *)sessionptr;
	if (sp->logged_devices == NULL)
		return;
	device = g_hash_table_lookup (sp->logged_devices, object_path);

	if (G_LIKELY(device != NULL))
	{
		printd (DEBUG,"Remove logged device %s", object_path);
//		gboolean CHECKME;	//do other stuff ?
		//wait awhile (longer than the mount-timer interval) to check for unmount and then cleanup
		device->timer_id =
#ifdef USE_GLIB2_14
			g_timeout_add_seconds (5,
#else
			g_timeout_add (5000,
#endif
				(GSourceFunc) _e2_devkit_check_unmount, device);
	}
	else
		printd (WARN,"No logged device to remove for %s", object_path);
}
/**
@brief callback for a property-change for a logged (removable) device
This will be called twice when a device is added/removed. The 2nd call does nothing.
@param proxy connection to devkit daemon
@param object_path PATH_ROOT/devicename (e.g. sr0, but with any '-' converted to '_')
@param sessionptr pointer to session-devices data struct specified when callback was connected
@return
*/
static void _e2_devkit_device_changed_cb (
#ifdef USE_GLIB2_26
GDBusProxy *proxy,
#else
DBusGProxy *proxy,
#endif
	const gchar *object_path, gpointer sessionptr)
{
	DevicesSession *sp;
	DeviceData *device;

	printd (DEBUG,"_e2_devkit_device_changed_cb: device %s", object_path);
	sp = (DevicesSession *)sessionptr;
	if (sp->logged_devices == NULL)
	{
#ifdef DEBUG_MESSAGES
unknown:
#endif
		printd (DEBUG,"Ignoring change event on un-logged device");
		return;
	}
	device = g_hash_table_lookup (sp->logged_devices, object_path);

	if (G_LIKELY(device != NULL))
	{
		DeviceProperties new_properties;

		if (device->timer_id > 0)
		{
			g_source_remove (device->timer_id);	//don't need any current timeout
			device->timer_id = 0;
		}

		if (_e2_devkit_device_properties_get (sp, object_path, &new_properties))
		{
			if (G_LIKELY(!new_properties.ignored))
			{
				gboolean differ1;	//, differ2;

				differ1 = (new_properties.device_is_media_available != device->props.device_is_media_available);
//				printd (DEBUG, "Media availability is %s", (differ1)? "different now":"unchanged");
//				differ2 = (new_properties.device_is_media_change_detected != device->props.device_is_media_change_detected);

				//replace all, in case other property(ies) also changed
				_e2_devkit_device_properties_clear (&device->props);
				memcpy (&device->props, &new_properties, sizeof (DeviceProperties));
				if (differ1)
				{
//					gboolean CHECKME2;	//do other stuff ?
					if (new_properties.device_is_media_available)
					{
						device->timer_id =
#ifdef USE_GLIB2_14
							g_timeout_add_seconds (2,
#else
							g_timeout_add (2000,
#endif
							(GSourceFunc) _e2_devkit_check_mount, device);
					}
					else
					{
						device->timer_id =
#ifdef USE_GLIB2_14
							g_timeout_add_seconds (5,
#else
							g_timeout_add (5000,
#endif
							(GSourceFunc) _e2_devkit_check_unmount, device);
					}
				}
			}
			else
			{
				printd (WARN,"device %s is no longer suitable for monitoring", object_path);
				//FIXME unmount? cleanup
			}
		}
	}
#ifdef DEBUG_MESSAGES
	else
		goto unknown;
#endif
}
#ifdef USE_GLIB2_26
/**
@brief "g-signal" signal callback for session.proxy
For removals, this processes volume(s) before the device itself CHECKME handle luks
@param proxy connection to devkit daemon
@param sender_name sender of the signal or NULL
@param signal_name name of the signal
@param parameters a GVariant tuple with parameter(s) for the signal
@param sessionptr pointer to device monitor data struct specified when callback was connected
@return
*/
static void _e2_devkit_device_signal_cb (GDBusProxy *proxy,
										gchar      *sender_name,
										gchar      *signal_name,
										GVariant   *parameters,
										gpointer    sessionptr)
{
	guint mode;
	const gchar *object_path;

	if (strcmp (signal_name, "DeviceAdded") == 0)
		mode = 1;
	else if (strcmp (signal_name, "DeviceRemoved") == 0)
		mode = 2;
	else if (strcmp (signal_name, "DeviceChanged") == 0)
		mode = 3;
	else
		return;

	g_variant_get_child (parameters, 0, "&o", &object_path);
	if (G_LIKELY (object_path != NULL))
	{
		NEEDOPENBGL
		switch (mode)
		{
		case 1:
			_e2_devkit_device_added_cb (proxy, object_path, sessionptr);
			break;
		case 2:
			_e2_devkit_device_removed_cb (proxy, object_path, sessionptr);
			break;
		default:
			_e2_devkit_device_changed_cb (proxy, object_path, sessionptr);
			break;
		}
		NEEDCLOSEBGL
	}
}
#endif
/* *
@brief callback for any changed property(ies) for a removable device
This callback happens only at idle-time, and before any device-changed callback.
There is no info on what properties have changed, so we must poll.
@param object UNUSED object
@param user_data pointer to data struct for device that's changed
@return
*/
/*static void _e2_devkit_device_property_changed_cb (GObject *obj,
	gpointer user_data)
{
	DeviceData *device;

	printd (DEBUG,"_e2_devkit_device__property_changed_cb: device %x", user_data);
	device = (DeviceData*)user_data;
	gboolean TODO;
}
*/

  /********************/
 /****** public ******/
/********************/

/**
@brief helper func for finding a logged removable device with a specific device-path
@param key UNUSED key for a hashtable entry
@param value value for a hashtable entry
@param devpath the device we're looking for
@return TRUE when a match is found
*/
static gboolean _e2_devkit_check_device_path (gpointer *key, gpointer value,
	const gchar *devpath)
{
	if (((DeviceData*)value)->props.device_file == NULL)
	{	//some setup race/problem happened
		printd (DEBUG, "Device %s has a NULL device-file", devpath);
		return FALSE;
	}
	return (!strcmp(((DeviceData*)value)->props.device_file, devpath));
}
/**
@brief check whether native @a devpath resides on a removable device
@param devpath /dev/<whatever>
@return TRUE if it's removable
*/
gboolean e2_devkit_device_is_removable (const gchar *devpath)
{
	if (session.logged_devices != NULL)
	{
		return (g_hash_table_find (session.logged_devices,
				(GHRFunc) _e2_devkit_check_device_path,
				(gpointer)devpath) != NULL);
	}
	return FALSE;
}
/**
@brief check whether native @a devpath resides on an ejectable device
@param devpath /dev/<whatever>
@return TRUE if it's ejectable
*/
gboolean e2_devkit_device_is_ejectable (const gchar *devpath)
{
	if (session.logged_devices != NULL)
	{
		DeviceData *dev;
		dev = (DeviceData *)g_hash_table_find (session.logged_devices,
				(GHRFunc) _e2_devkit_check_device_path,
				(gpointer)devpath);
		if (dev != NULL)
			return (dev->props.drive_is_media_ejectable
					//? && dev->props.device_is_media_available
					);
	}
	return FALSE;
}
/**
@brief initialise connection to devicekit/udisks daemon, and log current removables
Called with BGL open/off
@return
*/
void e2_devkit_init (void)
{
	DevicesSession *sessionptr;
	GError *error;
	const gchar *iface_name, *object_path;
	gchar *version;

	error = NULL;
	object_path = NULL; //warning prevention

#ifdef USE_GLIB2_26
	session.bus = g_bus_get_sync (G_BUS_TYPE_SYSTEM, NULL, &error);
#else
	session.bus = dbus_g_bus_get (DBUS_BUS_SYSTEM, &error);
#endif
	if (session.bus == NULL)
	{
//		_e2_devkit_advise (_("some warning ..."), NULL);
		printd (WARN, "Unable to connect to system bus: %s", error->message);
		g_error_free (error);
		return;
	}

	//check for daemon presence
	iface_name = UDISK_IFACE_NAME;
	version = _e2_devkit_get_version (session.bus, iface_name, UDISK_BUS_PATH);
	if (version != NULL)
	{
#ifdef DEVKIT_IFACE
		session.udisks = TRUE;
#endif
		session.daemon_version = version;
		session.iface_name = iface_name;
		object_path = UDISK_BUS_PATH;
	}
	else
	{
#ifdef DEVKIT_IFACE
		iface_name = DEVKIT_IFACE_NAME;
		version = _e2_devkit_get_version (session.bus, iface_name, DEVKIT_BUS_PATH);
		if (version != NULL)
		{
			session.daemon_version = version;
			session.iface_name = iface_name;
			object_path = DEVKIT_BUS_PATH;
		}
		else
		{
#endif
//			_e2_devkit_advise (_("some warning ..."), NULL);
			printd (WARN, "Unable to connect to any devices daemon");
#ifdef USE_GLIB2_26
			g_object_unref (G_OBJECT (session.bus));
#else
			dbus_g_connection_unref (session.bus);
#endif
			session.bus = NULL;
			return;
#ifdef DEVKIT_IFACE
		}
#endif
	}

#ifdef USE_GLIB2_26
	session.proxy = g_dbus_proxy_new_sync
		(session.bus, G_DBUS_PROXY_FLAGS_NONE, NULL,
		iface_name, object_path, iface_name , NULL, NULL);
#else
	session.proxy = dbus_g_proxy_new_for_name
		(session.bus, iface_name, object_path, iface_name);
#endif
	if (session.proxy == NULL)
	{
//		_e2_devkit_advise (_("some warning ..."), NULL);
		printd (WARN, "Unable to create proxy for dbus connection");
		g_free (session.daemon_version);
#ifdef USE_GLIB2_26
		g_object_unref (G_OBJECT (session.bus));
#else
		dbus_g_connection_unref (session.bus);
#endif
		session.bus = NULL;
		return;
	}

	//table with: key = D-Bus object path, value = the corresponding device data struct
	session.logged_devices = g_hash_table_new_full (g_str_hash, g_str_equal,
		NULL, (GDestroyNotify)_e2_devkit_device_data_clear);

	sessionptr = &session;
	if (_e2_devkit_detect_current_removables (sessionptr))
	{
#ifdef USE_GLIB2_26
		g_signal_connect (G_OBJECT(session.proxy), "g-signal",
			G_CALLBACK (_e2_devkit_device_signal_cb), sessionptr);
#else
		const gchar *signame;
		signame = "DeviceAdded";
		dbus_g_proxy_add_signal (session.proxy, signame,
			DBUS_TYPE_G_OBJECT_PATH, G_TYPE_INVALID);
		dbus_g_proxy_connect_signal (session.proxy, signame,
			G_CALLBACK (_e2_devkit_device_added_cb), sessionptr, NULL);
		signame = "DeviceRemoved";
		dbus_g_proxy_add_signal (session.proxy, signame,
			DBUS_TYPE_G_OBJECT_PATH, G_TYPE_INVALID);
		dbus_g_proxy_connect_signal (session.proxy, signame,
			G_CALLBACK (_e2_devkit_device_removed_cb), sessionptr, NULL);
		signame = "DeviceChanged";
		dbus_g_proxy_add_signal (session.proxy, signame,
			DBUS_TYPE_G_OBJECT_PATH, G_TYPE_INVALID);
		dbus_g_proxy_connect_signal (session.proxy, signame,
			G_CALLBACK (_e2_devkit_device_changed_cb), sessionptr, NULL);
#endif
	}
	else
	{
//		_e2_devkit_advise (_("some warning ..."), NULL);
		g_free (session.daemon_version);
#ifdef USE_GLIB2_26
		g_object_unref (G_OBJECT (session.bus));
#else
		dbus_g_connection_unref (session.bus);
#endif
		g_object_unref (G_OBJECT(session.proxy));
		g_hash_table_destroy (session.logged_devices);
		memset (sessionptr, 0, sizeof (DevicesSession));
	}
}
/**
@brief end devicekit connection and related cleanup
@return
*/
void e2_devkit_disconnect (void)
{
	if (session.daemon_version != NULL)
		g_free (session.daemon_version);
	if (session.bus != NULL)
#ifdef USE_GLIB2_26
		g_object_unref (G_OBJECT (session.bus));
#else
		dbus_g_connection_unref (session.bus);
#endif
	if (session.proxy != NULL)
		g_object_unref (G_OBJECT(session.proxy));
	if (session.logged_devices != NULL)
		g_hash_table_destroy (session.logged_devices);
}

#ifdef EXTRA_MESSAGES
# undef EXTRA_MESSAGES
#endif

#endif //def E2_DEVKIT
