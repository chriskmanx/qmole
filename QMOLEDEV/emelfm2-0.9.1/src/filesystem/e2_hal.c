/* $Id: e2_hal.c 2742 2013-09-11 00:37:42Z tpgww $

Copyright (C) 2008-2013 tooar <tooar@emelfm2.net>

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
@file src/filesystem/e2_hal.c
@brief functions related to system hardware-abstraction-layer
*/

#include "emelfm2.h"

#ifdef E2_HAL

#include "e2_fs.h"
#include <string.h>
#include <dbus/dbus-glib-lowlevel.h>
#include <hal/libhal.h>
#include <hal/libhal-storage.h>

#define COMPUTER_UDI "/org/freedesktop/Hal/devices/computer"
#define COMPUTER_TYPE "system.formfactor"
#define VOLUME "volume"
#define IS_VOLUME "block.is_volume"
#define REMOVABLE "storage.removable"
//mount error strings
#define HAL_VOLPREFIX                 "org.freedesktop.Hal.Device.Volume"
//#define HALERR_VOLPREFIXLEN               34 //sizeof (HAL_VOLPREFIX), includes trailing '.'
//various volume-error strings
#define HALERR_ALREADY_MOUNTED             HAL_VOLPREFIX".AlreadyMounted"
//#define HALERR_BUSY                      HAL_VOLPREFIX".Busy"
//#define HALERR_INVALID_MOUNT_OPT         HAL_VOLPREFIX".InvalidMountOption"
//#define HALERR_INVALID_MOUNT_POINT       HAL_VOLPREFIX".InvalidMountpoint"
//#define HALERR_NOT_AVAILABLE_MOUNT_POINT HAL_VOLPREFIX".MountPointNotAvailable"
//for linux at least, there are also errors like
//"... (-> ...) found in /etc/fstab. Not mounting." and
//"... (-> ...) found in /proc/mounts. Not mounting."
//#define HALERR_NOT_MOUNTED               HALERR_VOLPREFIX".NotMounted"
#define HALERR_NO_AUTHORITY                HAL_VOLPREFIX".PermissionDenied"
//#define HALERR_UNKNOWN_FAILURE           HAL_VOLPREFIX".UnknownFailure"
//#define HALERR_UNKNOWN_SYSTEM            HAL_VOLPREFIX".UnknownFilesystemType"

//#define HALERR_UNKNOWN_DEVICE         "org.freedesktop.Hal.Device.UnknownError"
#define HALERR_UNKNOWN_METHOD         "org.freedesktop.DBus.Error.UnknownMethod"

static DBusConnection *connection = NULL;
static LibHalContext *ctx = NULL;
static GList *removable_devices = NULL; //list of HalStorageData's
typedef struct _HalStorageData
{
	gchar *udi;	//for checking which devices are removed
	gchar *device_path;
//	gchar *mount_point;
	guint timer_id;
	gboolean ejectable;
} HalStorageData;
/*
//user data for HAL context
struct _HalContextData
{
} HalContextData;
*/
static void _e2_hal_add_device_cb (LibHalContext *ctx, const gchar *udi);
static void _e2_hal_remove_device_cb (LibHalContext *ctx, const gchar *udi);

/**
@brief cleanup mount-data item
@param data pointer to hal data for a device

@return
*/
static void _e2_hal_destroy_data (HalStorageData *data)
{
	if (data->timer_id > 0)
		g_source_remove (data->timer_id);
	if (data->udi != NULL)
		g_free (data->udi);
//	if (data->mount_point != NULL)
//		g_free (data->mount_point);
	if (data->device_path != NULL)
		libhal_free_string (data->device_path);
	DEALLOCATE (HalStorageData, data);
}
/* *
@brief display hal/dbus error message
Assumes BGL is open/off upon arrival
@param message specific message
@error dbus error data, or NULL

@return
*/
/*
static void _e2_hal_advise (const gchar *message, const DBusError *error)
{
	CLOSEBGL
	if (error != NULL && dbus_error_is_set (error))
	{
		gchar *msg = g_strdup_printf ("%s (%s)", message, error->message);
		e2_output_print_error (msg, TRUE);
	}
	else
		e2_output_print_error ((gchar *)message, FALSE);
	OPENBGL
}
*/
/**
@brief check for currently-known removable devices
Assumes BGL open/off
@param ctx pointer to HAL data struct

@return TRUE if any device found
*/
static gboolean _e2_hal_detect_current_removables (LibHalContext *ctx)
{
	gint i, count;
	gchar **devices;
	DBusError error;

	dbus_error_init (&error);
	devices = libhal_manager_find_device_string_match (ctx,
		"info.category", "storage", &count, &error);
	if (dbus_error_is_set (&error))
	{
//		_e2_hal_advise (_("HAL: cannot find any storage device"), &error);
		printd (WARN, "HAL: cannot find any storage device - %s", error.message);
		dbus_error_free (&error);
		return FALSE;
	}

	if (devices != NULL)
	{
		for (i = 0; i < count; i++)
		{
			if (devices[i] != NULL)
				_e2_hal_add_device_cb (ctx, devices[i]);
		}
		dbus_free_string_array (devices);
	}

	return TRUE;
}
/**
@brief automount a volume
This is called from within timer callback i.e. BGL is off
@param vol hal data for volume to be mounted
@param realdevice device path-string for the volume, "/dev/....", CHECKME localised

@return TRUE if the mount is performed successfully or is not needed
*/
static gboolean _e2_hal_mount_volume (LibHalVolume *vol, const gchar *realdevice)
{
	gint res;
	gchar *str, *mountpoint;
	const gchar *fstype;

	fstype = libhal_volume_get_fstype (vol); //e.g. "vfat"
	if (fstype == NULL)	//not a data-medium
		return TRUE;
	//if mountpoint exists in fstab etc, HAL will not mount it
	//(or maybe will only mount it if same options are specified ?)
	mountpoint = e2_fs_mount_get_mountpoint_for_device (realdevice); //check fstab etc
	if (mountpoint != NULL)
	{
		str = g_strdup_printf ("|!"E2_MOUNTCOMMAND" '%s' &", mountpoint);
		res = e2_command_run (str, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
		, T/F ?
#endif
		);
		g_free (str);
		if (res != 0)
		{
			if (geteuid () != 0)  //not superuser
			{
				//try again with more authority
				str = g_strdup_printf ("|!sudo "E2_MOUNTCOMMAND" '%s'", mountpoint);
				res = e2_command_run (str, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
				, T/F ?
#endif
				);
				g_free (str);
			}
			if (res != 0)
			{
				printd (WARN, "HAL: cannot mount %s in accord with fstab", realdevice);
				str = g_strdup_printf (_("Cannot auto-mount device '%s' at '%s'"),
					realdevice, mountpoint);
				CLOSEBGL
				e2_output_print_error (str, TRUE);
				OPENBGL
				g_free (mountpoint);
				return FALSE;
			}
		}
		g_free (mountpoint);
		return TRUE;
	}
#if 1
	/* the reported file-system-type may need conversion to suit the OS in use.
	   HAL handles such translation if needed */

	//determine a suitable mountpoint ...
/*LibHalVolumeDiscType enum, not in API doc
	LIBHAL_DRIVE_TYPE_REMOVABLE_DISK,
	LIBHAL_DRIVE_TYPE_DISK,
	LIBHAL_DRIVE_TYPE_CDROM,
	LIBHAL_DRIVE_TYPE_FLOPPY,
	LIBHAL_DRIVE_TYPE_TAPE,
	LIBHAL_DRIVE_TYPE_COMPACT_FLASH,
	LIBHAL_DRIVE_TYPE_MEMORY_STICK,
	LIBHAL_DRIVE_TYPE_SMART_MEDIA,
	LIBHAL_DRIVE_TYPE_SD_MMC,
	LIBHAL_DRIVE_TYPE_CAMERA,
	LIBHAL_DRIVE_TYPE_PORTABLE_AUDIO_PLAYER,
	LIBHAL_DRIVE_TYPE_ZIP,
	LIBHAL_DRIVE_TYPE_JAZ,
	LIBHAL_DRIVE_TYPE_FLASHKEY,
	LIBHAL_DRIVE_TYPE_MO
*/
	const gchar *disktypes[15] =
	{
		"disk",
		"disk",
		"cdrom", //maybe use "cdrecorder" instead
		"floppy",
		"tape",
		"flash",
		"stick",
		"smart",
		"sd_mmc",
		"camera",
		"audio",
		"zip",
		"jaz",
		"key",
		"mo"	//? some sort of cd ?
	};

	const gchar *udi = libhal_volume_get_udi (vol);
	const gchar *devudi = (libhal_volume_is_partition (vol)) ?
		libhal_volume_get_storage_device_udi (vol): udi;
	const gchar *last;
	gboolean freelast = FALSE;
	gboolean readonly;

	if (libhal_volume_is_disc (vol))
	{	//this volume is, or is part of, an optical disk
		last = NULL;
		readonly = TRUE;
//		if (dtype == LIBHAL_DRIVE_TYPE_CDROM)
//		{
/*			LibHalVolumeDiscType vtype = libhal_volume_get_disc_type (vol);
			switch (vtype)
			{
				case LIBHAL_VOLUME_DISC_TYPE_CDRW:
				case LIBHAL_VOLUME_DISC_TYPE_DVDRW:
				case LIBHAL_VOLUME_DISC_TYPE_DVDPLUSRW:
				case LIBHAL_VOLUME_DISC_TYPE_HDDVDRW:
					last = "cdrecorder";
					break;
				default:
					last = disktypes [2];
					break;
			}
*/
			if (libhal_device_property_exists (ctx, devudi, "storage.drive_type", NULL))
			{
				str = libhal_device_get_property_string (ctx, devudi, "storage.drive_type", NULL);
				if (str != NULL && strcmp (str, "cdrom") == 0)
				{
					gboolean writable;
					writable =
						libhal_device_get_property_bool (ctx, devudi, "storage.cdrom.cdrw", NULL)
					 ||	libhal_device_get_property_bool (ctx, devudi, "storage.cdrom.dvdrw", NULL)
					 ||	libhal_device_get_property_bool (ctx, devudi, "storage.cdrom.dvdplusrw", NULL)
					 ||	libhal_device_get_property_bool (ctx, devudi, "storage.cdrom.hddvdrw", NULL);
					if (writable)
					{
						readonly = FALSE;
						last = "cdrecorder";
					}
				}
				libhal_free_string (str);
			}
//		}
		if (last == NULL)
		{
			last = disktypes [LIBHAL_DRIVE_TYPE_CDROM];	//default
		}
	}
	else //not an optical disk
	{
		readonly = FALSE;	//assume this, for now
		LibHalDrive *drv = libhal_drive_from_udi (ctx, devudi);
		if (drv != NULL)
		{
			LibHalDriveType dtype = libhal_drive_get_type (drv);
			if (dtype < sizeof (disktypes) / sizeof (gchar *))
				last = disktypes [dtype];
			else
			{
				last = NULL;
				if (libhal_device_property_exists (ctx, devudi, "storage.drive_type", NULL))
				{
					str = libhal_device_get_property_string (ctx, devudi, "storage.drive_type", NULL);
					if (str != NULL)
					{
						last = g_strdup (str);
						freelast = TRUE;	//leak management
						libhal_free_string (str);
					}
				}
			}
			if (last == NULL)
				last = disktypes [LIBHAL_DRIVE_TYPE_DISK];	//default
			libhal_drive_free (drv);
		}
		else
			last = disktypes [LIBHAL_DRIVE_TYPE_DISK];
	}

/*	heck for an available mountpoint (HAL is hardcoded for "/media/...")
	mountpoint from lsb spec:
		/media
			/floppy	Floppy drive (optional)
			/cdrom	CD-ROM drive (optional)
			/cdrecorder	CD writer (optional)
			/zip	Zip drive (optional)
	Where more than one device exists for mounting a certain type of media, mount
	directories can be created by appending a digit to the name, starting with
	'0', but the unqualified name must also exist

	for device names:
	Partitions or volumes will appear as device-appended numbers, like
	   sda1, sda2 ....
	Some devices e.g. cdrom are themselves treated as a volume, and will then
	have no partition number
	Devices to be mounted may already exist in /dev
*/
	mountpoint = g_build_filename (E2_MOUNTPLACE, last, NULL);
	str = g_strdup (mountpoint);
	guint count = 0;
#ifdef E2_VFS
	VPATH ddata = { str, NULL };	//local mounting only
	while (e2_fs_access3 (&ddata, F_OK E2_ERR_NONE()) == 0)
#else
	while (e2_fs_access3 (str, F_OK E2_ERR_NONE()) == 0)
#endif
	{
		g_free (str);
		str = g_strdup_printf ("%s%d", mountpoint, count++);
#ifdef E2_VFS
		ddata.path = str;
#endif
	}
	g_free (mountpoint);
	mountpoint = str;
	if (freelast)
		g_free ((gchar *)last);
/*	HAL is hardcoded like this: ..."/media/%s", mount_point);
	so we have to peel off at least the first path-segment. In practice, there's
	a dummy-spit for _any_ separator in the mountpoint string, so we just use
	the last segment of it (which restricts the mountpoints, of course)
*/
	str = strrchr (mountpoint, G_DIR_SEPARATOR)	//must succeed
			+ sizeof (gchar);

	//determine mount options from the available ones
	gboolean readonly_valid = FALSE;
	gint freeindex = -1;
	DBusError error;

	dbus_error_init (&error);

	GPtrArray *extra_options = g_ptr_array_sized_new (6);
	gchar **mountopts = libhal_device_get_property_strlist (
							ctx, udi, "volume.mount.valid_options", &error);
	if (dbus_error_is_set (&error))
	{
//		_e2_hal_advise (_("HAL: cannot get mount options"), &error);
#ifdef DEBUG_MESSAGES
		if (!g_str_has_suffix (error.name, "NoSuchProperty")) //e.g. for audio cd
			printd (WARN, "HAL: cannot get mount options - %s", error.message);
#endif
		dbus_error_free (&error);
		if (mountopts != NULL)
			libhal_free_string_array (mountopts);
		g_ptr_array_add (extra_options, NULL);	//NULL-terminate
	}
	else if (mountopts == NULL)
	{
		g_ptr_array_add (extra_options, NULL);	//NULL-terminate
	}
	else
	{
		gchar **thisopt;
		for (thisopt = mountopts; *thisopt != NULL; thisopt++)
		{
		//CHECKME which mount options are most suitable ?
		//e.g. ro,sync,dirsync,noatime,nodiratime,noexec,quiet,remount,exec,utf8
		//shortname=,codepage=,iocharset=,umask=,dmask=,fmask=,uid=,flush
			if (strcmp (*thisopt, "exec") == 0)
			{
				g_ptr_array_add (extra_options, *thisopt);
			}
			else if (strcmp (*thisopt, "umask=") == 0)
			{
				g_ptr_array_add (extra_options, g_strconcat (*thisopt, "0", NULL));
				freeindex = extra_options->len - 1; //for leak fix
			}
			else if (strcmp (*thisopt, "sync") == 0)
			{
				g_ptr_array_add (extra_options, *thisopt);
			}
			else if (strcmp (*thisopt, "ro") == 0)
			{
				readonly_valid = TRUE;
				if (readonly)
					g_ptr_array_add (extra_options, *thisopt);
			}
		}
		g_ptr_array_add (extra_options, NULL);	//NULL-terminate
	}

	while (TRUE)
	{
		//prepare dbus message to initiate "Mount"
		DBusMessage *instruction = dbus_message_new_method_call (
			"org.freedesktop.Hal", udi, HAL_VOLPREFIX, "Mount");
		if (instruction == NULL)
		{
badexit:
//			_e2_hal_advise (_("HAL: cannot initiate mount instruction"), &error);
			printd (WARN, "HAL: cannot initiate mount instruction - %s", error.message);
			dbus_error_free (&error);
			g_free (mountpoint);
			if (freeindex != -1)
				g_free (g_ptr_array_index (extra_options, freeindex));
			g_ptr_array_free (extra_options, TRUE);
			libhal_free_string_array (mountopts);

			return FALSE;
		}

		if (extra_options->len > 1)
		{
			//append message parameters
			if (!dbus_message_append_args (instruction,
					DBUS_TYPE_STRING, &str,
					DBUS_TYPE_STRING, &fstype,
					DBUS_TYPE_ARRAY, DBUS_TYPE_STRING, &extra_options->pdata,
						extra_options->len - 1,
					DBUS_TYPE_INVALID))
			{
				dbus_message_unref (instruction);
				goto badexit;
			}
		}

		DBusMessage *result = dbus_connection_send_with_reply_and_block (
				connection, instruction, -1, &error);
		if (result != NULL)
		{
			if (dbus_message_get_type (result) == DBUS_MESSAGE_TYPE_ERROR)
				dbus_set_error_from_message (&error, result);

			dbus_message_unref (result);
		}

		dbus_message_unref (instruction);

		if (!dbus_error_is_set (&error))
			break;	//success

		//OK if device was already mounted
		if (strcmp (error.name, HALERR_ALREADY_MOUNTED) == 0
			|| strcmp (error.name, HALERR_UNKNOWN_METHOD) == 0)
		{
			dbus_error_free (&error);
			break;
		}

		//might help to add "ro" to the mount options if that's valid
		//CHECKME what error(s) would indicate this is ok to try ?
		if (!readonly_valid || readonly)
			break; //already tried that, too bad ...
		//append "ro" to the options and try again
		g_ptr_array_remove_index (extra_options, extra_options->len - 1);
		g_ptr_array_add (extra_options, "ro");
		g_ptr_array_add (extra_options, NULL);	//NULL-terminate
		readonly = TRUE;
		dbus_error_free (&error);

		//CHECKME try other tweaks ?
	}

	gboolean retval;
	if (dbus_error_is_set (&error))
	{	//we failed
		if (strcmp (error.name, HALERR_NO_AUTHORITY) == 0)
		{
			if (geteuid () != 0)  //not superuser
			{
				//try one more time with extra authority
				gchar *opts = g_strjoinv (",", (gchar **)extra_options->pdata);
				str = g_strdup_printf ("|!sudo "E2_MOUNTCOMMAND" -t %s -o %s '%s' '%s'",
					fstype, opts, realdevice, mountpoint);
				res = e2_command_run (str, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
				, T/F ?
#endif
				);
				g_free (str);
				g_free (opts);
				if (res == 0)
				{
					dbus_error_free (&error);
				}
			}
		}
		if (dbus_error_is_set (&error))
		{
//			_e2_hal_advise (_("HAL: cannot mount"), &error);
			printd (WARN, "HAL: cannot mount %s - %s", realdevice, error.message);
			str = g_strdup_printf (_("Cannot auto-mount device '%s' at '%s'"),
					realdevice, mountpoint);
			CLOSEBGL
			e2_output_print_error (str, TRUE);
			OPENBGL

			dbus_error_free (&error);
			retval = FALSE;
		}
		else
			retval = TRUE;
	}
	else
		retval = TRUE;

/*	if (retval)
	{
		//"refresh" the filelist(s) if needed
		gchar *utf = F_FILENAME_FROM_LOCALE (mountpoint);
		if (g_str_has_prefix (curr_view->dir, utf))
			do something
		if (g_str_has_prefix (other_view->dir, utf))
			do something
		F_FREE (utf, mountpoint);
	}
*/
	//cleanup
	g_free (mountpoint);
	if (freeindex != -1)
		g_free (g_ptr_array_index (extra_options, freeindex));
	g_ptr_array_free (extra_options, TRUE);
	libhal_free_string_array (mountopts);

	return retval;
#else	//not 0
	return FALSE;
#endif //0
}
/**
@brief autounmount a volume
This is called from within timer callback i.e. BGL is off
@param vol hal data for volume to be unmounted
@param realdevice device pathstring, "/dev/...."

@return TRUE if the unmount is performed successfully or is not needed
*/
static gboolean _e2_hal_unmount_volume (LibHalVolume *vol, const gchar *realdevice)
{
	DBusError error;
	const gchar *fstype, *udi;

	fstype = libhal_volume_get_fstype (vol);
	if (fstype == NULL)	//not a data-medium
		return TRUE;

	dbus_error_init (&error);
	udi = libhal_volume_get_udi (vol);
/*	GPtrArray *extra_options = g_ptr_array_sized_new (6);
	gchar **unmountopts = libhal_device_get_property_strlist (
							ctx, udi, "volume.unmount.valid_options", &error);
	if (dbus_error_is_set (&error))
	{
//		_e2_hal_advise (_("HAL: cannot get unmount options"), &error);
		printd (WARN, "HAL: cannot get unmount options - %s", error.message);
		dbus_error_free (&error);
		if (unmountopts != NULL)
			libhal_free_string_array (unmountopts);
		g_ptr_array_add (extra_options, NULL);	//NULL-terminate
	}
	else if (unmountopts == NULL)
	{
		g_ptr_array_add (extra_options, NULL);	//NULL-terminate
	}
	else
	{
		gchar **thisopt;
		for (thisopt = unmountopts; *thisopt != NULL; thisopt++)
		{
		//CHECKME which unmount options are most suitable ?
		//e.g. lazy
			if (strcmp (*thisopt, "?") == 0)
			{
				g_ptr_array_add (extra_options, *thisopt);
			}
		}
		g_ptr_array_add (extra_options, NULL);	//NULL-terminate
	}
*/
	//prepare dbus message to initiate "Unmount"
	DBusMessage *instruction = dbus_message_new_method_call (
		"org.freedesktop.Hal", udi, HAL_VOLPREFIX, "Unmount");
	if (instruction == NULL)
	{
badexit:
//		_e2_hal_advise (_("HAL: cannot initiate unmount instruction"), &error);
		printd (WARN, "HAL: cannot initiate unmount instruction - %s", error.message);
		dbus_error_free (&error);
//		g_ptr_array_free (extra_options, TRUE);
//		libhal_free_string_array (mountopts);

		return FALSE;
	}

	//append message parameters
	if (!dbus_message_append_args (instruction,
//			DBUS_TYPE_ARRAY, DBUS_TYPE_STRING, &extra_options->pdata, extra_options->len - 1,
			DBUS_TYPE_ARRAY, DBUS_TYPE_STRING, NULL, 0,
			DBUS_TYPE_INVALID))
	{
		dbus_message_unref (instruction);
		goto badexit;
	}

	const gchar *mountpoint = libhal_volume_get_mount_point (vol);
	if (mountpoint != NULL	//should never happen
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
		&& g_str_has_prefix (curr_view->dir, mountpoint))
#endif
	{	//if possible, change CWD out of mountpoint so we don't ourself block the unmount
		gchar *elsewhere;
		const gchar *s = strrchr (mountpoint, G_DIR_SEPARATOR);	//assumes no trailer on mountpoint string
		if (s > mountpoint + 1) //not doing root dir
			elsewhere = g_strndup (mountpoint, s - mountpoint);
		else //can't cd
			elsewhere = g_strdup (G_DIR_SEPARATOR_S);

		e2_fs_get_valid_path (&elsewhere, TRUE E2_ERR_NONE());
		e2_pane_change_dir (NULL, elsewhere);
		g_free (elsewhere);
	}

	DBusMessage *result = dbus_connection_send_with_reply_and_block (
			connection, instruction, -1, &error);
	if (result != NULL)
	{
		if (dbus_message_get_type (result) == DBUS_MESSAGE_TYPE_ERROR)
			dbus_set_error_from_message (&error, result);

		dbus_message_unref (result);
	}

	dbus_message_unref (instruction);

	if (dbus_error_is_set (&error))
		goto badexit;

//	g_ptr_array_free (extra_options, TRUE);
//	libhal_free_string_array (mountopts);
	return TRUE;
}
/**
@brief timer callback after a device-add event
If the device's volume(s) are not yet mounted (by the OS etc), mount them now
@param data pointer to hal data for the device

@return FALSE always, to cancel the timer
*/
static gboolean _e2_hal_check_mount (HalStorageData *data)
{
	//FIXME handle umount before this callback, data = invalid MUTEX ?
	data->timer_id = 0;	//quick signal to any unmount process
	printd (DEBUG, "HAL: mount timer callback");
	printd (DEBUG, "HAL: device %s, udi %s", data->device_path, data->udi);

	LibHalDrive *drv = libhal_drive_from_device_file (ctx, data->device_path);
	if (drv != NULL)
	{
		LibHalVolume *vol = libhal_volume_from_device_file (ctx, data->device_path);
		if (vol != NULL)
		{	//this device is itself a volume
			if (libhal_drive_is_media_detected (drv))
			{
				//mountpoint path needed for unmount processing
//				data->mount_point = g_strdup (libhal_volume_get_mount_point (vol));
				if(!libhal_volume_is_mounted (vol))
				{
					printd (DEBUG, "HAL: %s not mounted yet", data->device_path);
					_e2_hal_mount_volume (vol, data->device_path);
				}
			}
			libhal_volume_free (vol);
		}
		else
		{
			gint volcount;
			gchar **voludis = libhal_drive_find_all_volumes (ctx, drv, &volcount);
			if (volcount > 0)
			{
				gint i;
				for (i = 0; i < volcount; i++)
				{
					vol = libhal_volume_from_udi (ctx, voludis[i]);
					if (vol != NULL)
					{
						if (libhal_drive_is_media_detected (drv))
						{
							if (libhal_volume_disc_is_blank (vol))
							{
//								_e2_hal_advise (_("HAL: cannot mount blank medium"), NULL);
								printd (WARN, "HAL: cannot mount blank medium");
							}
							else
							{
//								data->mount_point = g_strdup (libhal_volume_get_mount_point (vol));
								if (!libhal_volume_is_mounted (vol))
								{
									const gchar *realdevice = libhal_volume_get_device_file (vol);
									printd (DEBUG, "HAL: %s not mounted yet", realdevice);
									_e2_hal_mount_volume (vol, realdevice);
								}
							}
						}
						libhal_volume_free (vol);
					}
				}
				g_strfreev (voludis);
			}
		}
		libhal_drive_free (drv);
	}
	return FALSE;
}
/**
@brief callback for a device-add event
@param ctx the hal connection-context
@param udi string identifying the device added

@return
*/
static void _e2_hal_add_device_cb (LibHalContext *ctx, const gchar *udi)
{
	DBusError error;
	dbus_error_init (&error);

	if (libhal_device_property_exists (ctx, udi, "storage.removable", &error))
	{
		if (libhal_device_get_property_bool (ctx, udi, "storage.removable", &error))
		{
			HalStorageData *devicedata = ALLOCATE0 (HalStorageData);
			CHECKALLOCATEDWARN (devicedata, return;);
			//CHECKME is 2-sec delay long enough ?
			devicedata->timer_id =
#ifdef USE_GLIB2_14
				g_timeout_add_seconds (2,
#else
				g_timeout_add (2000,
#endif
						(GSourceFunc) _e2_hal_check_mount, devicedata);

			devicedata->udi = g_strdup (udi);	//need a copy, original doesn't persist during removal
			devicedata->device_path = libhal_device_get_property_string (ctx, udi,
						"block.device", &error);
			printd (DEBUG, "HAL: storage %s is removable", devicedata->device_path);

			if (libhal_device_property_exists (ctx, udi, "storage.requires_eject", &error))
			{
				devicedata->ejectable = (gboolean) libhal_device_get_property_bool
					(ctx, udi, "storage.requires_eject", &error);
				if (devicedata->ejectable)
				{
					if (!libhal_device_add_property_watch (ctx, udi, &error))
					{
//						_e2_hal_advise (_("HAL: cannot set watch on device"), &error);
						printd (WARN, "HAL: cannot set watch on device - %s", error.message);
						dbus_error_free (&error);
					}
					else
					{
						printd (DEBUG, "HAL: Watch added to device udi %s", udi);
						printd (DEBUG, "storage will be ejected");
					}
				}
			}

			removable_devices = g_list_append (removable_devices, devicedata);
		}
		else
			dbus_error_free (&error);
	}
	else
		dbus_error_free (&error);
}
/**
@brief change displayed directory if necessary, after an unmount
BGL may be open or closed
@param rt pointer to pane data, curr_pane or other_pane
@return
*/
static void _e2_hal_checkdir (E2_PaneRuntime *rt)
{
	gchar *elsewhere = g_strdup (rt->path);
	g_strchug (elsewhere); //just in case ...
	if (!e2_fs_get_valid_path (&elsewhere, TRUE E2_ERR_NONE()))
		e2_pane_change_dir (rt, elsewhere);
	g_free (elsewhere);
}
/**
@brief timer callback after a device-remove event
If the device is not unmounted yet (by the OS etc), unmount it now
Also, always cleanup the mount data
@param devicedata pointer to data struct for the affected device

@return FALSE always, to cancel the timer
*/
static gboolean _e2_hal_check_unmount (HalStorageData *devicedata)
{
	devicedata->timer_id = 0;	//signal to others

	printd (DEBUG, "HAL: unmount timer callback");
//	printd (DEBUG, "HAL: device %s, udi %s", devicedata->device_path, devicedata->udi);

	LibHalDrive *drv = libhal_drive_from_device_file (ctx, devicedata->device_path);
	if (drv != NULL)
	{	//device is still present
		if (!libhal_drive_is_media_detected (drv))
		{	//something to unmount
			LibHalVolume *vol = libhal_volume_from_device_file (ctx, devicedata->device_path);
			if (vol != NULL)
			{	//this device is itself a volume
				if (libhal_volume_is_mounted (vol))
				{
					printd (DEBUG, "HAL: %s still mounted", devicedata->device_path);
					_e2_hal_unmount_volume (vol, devicedata->device_path);
				}
				libhal_volume_free (vol);
			}
			else
			{	//maybe it's a device that includes volume(s)
				gint volcount;
				gchar **voludis = libhal_drive_find_all_volumes (ctx, drv, &volcount);
				if (volcount > 0)
				{
					gint i;
					for (i = 0; i < volcount; i++)
					{
						vol = libhal_volume_from_udi (ctx, voludis[i]);
						if (vol != NULL)
						{
							if (libhal_volume_is_mounted (vol))
							{
								const gchar *realdevice = libhal_volume_get_device_file (vol);
								printd (DEBUG, "HAL: %s still mounted", realdevice);
								_e2_hal_unmount_volume (vol, realdevice);
							}
							libhal_volume_free (vol);
						}
					}
					g_strfreev (voludis);
				}
			}
		}
		libhal_drive_free (drv);
	}
	//ensure neither filelist shows the unmounted device
	_e2_hal_checkdir (curr_pane);
	_e2_hal_checkdir (other_pane);
	//CHECME mutex for processing this list ?
	removable_devices = g_list_remove (removable_devices, devicedata);
	_e2_hal_destroy_data (devicedata);

	return FALSE;
}
/**
@brief callback for a device-remove event
@param ctx the hal connection-context
@param udi string identifying the device removed

@return
*/
static void _e2_hal_remove_device_cb (LibHalContext *ctx, const gchar *udi)
{
	if (removable_devices != NULL)
	{
		GList *node;
		for (node = removable_devices; node != NULL; node = node->next)
		{
			HalStorageData *store;
			store = (HalStorageData *)node->data;
			if (strcmp (store->udi, udi) == 0)
			{
/*THIS PROCESS IS SUPPOSED TO BE AUTOMATIC, WHEN A DISPLAYED DIR IS FOUND TO BE INACCESSIBLE
				if (store->mount_point != NULL)
				{
					//make sure the unmounted dir is not shown, or blocking the unmount
					gboolean found = FALSE;
					gchar *elsewhere, *mountpoint, *s;
					mountpoint = store->mount_point;
					s = strrchr (mountpoint, G_DIR_SEPARATOR);	//assumes no trailer on mountpoint string
					if (s > mountpoint + 1) //not doing root dir
						elsewhere = g_strndup (mountpoint, s - mountpoint);
					else //can't cd
						elsewhere = g_strdup (G_DIR_SEPARATOR_S);
#ifdef E2_VFSTMP
		//FIXME dir when not mounted local
#else
					if (g_str_has_prefix (curr_view->dir, mountpoint))
#endif
					{
						found = TRUE;
						e2_fs_get_valid_path (&elsewhere, TRUE E2_ERR_NONE());
						gchar *msg = g_strdup_printf (_("Cannot access %s, going to %s instead"),
							curr_view->dir, elsewhere);
#ifdef E2_VFSTMP
						//FIXME cater for any space-change message
#endif
						CLOSEBGL
						e2_output_print_error (msg, TRUE);
						OPENBGL
						e2_pane_change_dir (curr_pane, elsewhere);
					}
#ifdef E2_VFSTMP
		//FIXME dir when not mounted local
#else
					if (g_str_has_prefix (other_view->dir, mountpoint))
#endif
					{
						if (!found)
							e2_fs_get_valid_path (&elsewhere, TRUE E2_ERR_NONE());
						gchar *msg = g_strdup_printf (_("Cannot access %s, going to %s instead"),
							other_view->dir, elsewhere);
#ifdef E2_VFSTMP
						//FIXME cater for any space-change message
#endif
						CLOSEBGL
						e2_output_print_error (msg, TRUE);
						OPENBGL
						e2_pane_change_dir (other_pane, elsewhere);
					}
					g_free (elsewhere);
				}
*/
				if (store->ejectable)
				{
					DBusError error;
					dbus_error_init (&error);
					if (!libhal_device_remove_property_watch (ctx, udi, &error))
					{
//						_e2_hal_advise (_("HAL: cannot remove watch from device"), &error);
						printd (WARN, "HAL: cannot remove watch from device - %s", error.message);
						dbus_error_free (&error);
					}
					else
						printd (DEBUG, "HAL: removed watch from device udi %s", udi);
				}
				//wait awhile (longer than the mount-timer interval) to check for unmount and then cleanup
				store->timer_id =
#ifdef USE_GLIB2_14
					g_timeout_add_seconds (10,
#else
					g_timeout_add (10000,
#endif
						(GSourceFunc) _e2_hal_check_unmount, store);

				break;
			}
		}
	}
}
/**
@brief callback for a property-change for a removable device that requires ejection
@param ctx the hal connection-context
@param udi string identifying the device whose property changed
@param key string identifying the property which changed
@param is_removed UNUSED TRUE when a property-change is a removal
@param is_added UNUSED TRUE when a property-change is an addition

@return
*/
static void _e2_hal_property_change_cb (LibHalContext *ctx, const gchar *udi,
	const gchar *key, dbus_bool_t is_removed, dbus_bool_t is_added)
{
	//interested in ejectable media property changes
	if (strcmp (key, "storage.removable.media_available") == 0)
	{
		printd (DEBUG, "HAL: property changed for udi %s and key %s", udi, key);
		DBusError error;

		dbus_error_init (&error);
		gboolean loaded = (gboolean) libhal_device_get_property_bool (ctx, udi,
				"storage.removable.media_available", &error);
		if (dbus_error_is_set (&error))
		{
//			_e2_hal_advise (_("HAL: cannot find media property"), &error);
			printd (WARN, "HAL: cannot find media property - %s", error.message);
			dbus_error_free (&error);
			return;
		}

		if (loaded)
		{
			printd (DEBUG, "HAL: removable media present now");
//			CLOSEBGL
			//do stuff
			//mount if device mountpoint is not mounted (os should handle that)
//			OPENBGL
		}
		else
		{
			printd (DEBUG, "HAL: removable media gone");
//			CLOSEBGL
			//eject if not just done to trigger this callback
			//unmount if device mountpoint is mounted (os should handle that)
//			OPENBGL
		}
	}
}
/**
@brief check whether native @a utfpath resides on a removable device
@param devpath /dev/<whatever>

@return TRUE if it's removable
*/
gboolean e2_hal_device_is_removable (const gchar *devpath)
{
	if (removable_devices != NULL)
	{
		GList *node;
		for (node = removable_devices; node != NULL; node = node->next)
		{
			if (strcmp (((HalStorageData *)node->data)->device_path, devpath) == 0)
				return TRUE;
		}
	}
	return FALSE;
}
/**
@brief check whether native @a utfpath resides on an ejectable device
@param devpath /dev/<whatever>

@return TRUE if it's ejectable
*/
gboolean e2_hal_device_is_ejectable (const gchar *devpath)
{
	if (removable_devices != NULL)
	{
		GList *node;
		for (node = removable_devices; node != NULL; node = node->next)
		{
			if (strcmp (((HalStorageData *)node->data)->device_path, devpath) == 0)
				return ((HalStorageData *)node->data)->ejectable;
		}
	}
	return FALSE;
}
/**
@brief initialise HAL connection
Called with BGL open/off
@return
*/
void e2_hal_init (void)
{
	DBusError error;

	ctx = libhal_ctx_new ();
	if (ctx == NULL)
	{
		printd (DEBUG, "Cannot create HAL context");
		return;
	}

	dbus_error_init (&error);
	printd (DEBUG, "connecting to DBUS");
	connection = dbus_bus_get (DBUS_BUS_SYSTEM, &error);
	if (connection == NULL)
	{
//		_e2_hal_advise (_("Cannot connect to system data bus"), &error);
		printd (WARN, "Cannot connect to system data bus - %s", error.message);
		goto cleanup;
	}

	dbus_connection_ref (connection);	//local reference needed
	libhal_ctx_set_dbus_connection (ctx, connection);

//	libhal_ctx_set_user_data (ctx, hal_user_data);
	libhal_ctx_set_device_added (ctx, _e2_hal_add_device_cb);
	libhal_ctx_set_device_removed (ctx, _e2_hal_remove_device_cb);
	libhal_ctx_set_device_property_modified (ctx, _e2_hal_property_change_cb);

	if (!libhal_ctx_init (ctx, &error))
	{
//		_e2_hal_advise (_("Cannot connect to system HAL"), &error);
		printd (WARN, "Cannot connect to system HAL - %s", error.message);
		goto cleanup;
	}
	printd (DEBUG, "Determine mounted volumes at session start");
	if (!_e2_hal_detect_current_removables (ctx))
	{
//		_e2_hal_advise (_(""), NULL);
		printd (WARN, "Cannot determine mounted volumes at session start");
		goto cleanup;
	}

    if (!libhal_device_property_watch_all (ctx, &error))
	{
//		_e2_hal_advise (_("HAL: Cannot watch properties"), &error);
		printd (WARN, "HAL: Cannot watch properties - %s", error.message);
		goto cleanup;
	}
	dbus_connection_setup_with_g_main (connection, NULL);

	return;

cleanup:
	if (ctx != NULL)
	{
		libhal_ctx_free (ctx);
		ctx = NULL;
	}
	if (connection != NULL)
		dbus_connection_unref (connection);
	if (dbus_error_is_set (&error))
		dbus_error_free (&error);
}
/**
@brief end HAL connection and related cleanup

@return
*/
void e2_hal_disconnect (void)
{
	if (ctx != NULL)
	{
		DBusError error;

		dbus_error_init (&error);
		if (!libhal_ctx_shutdown (ctx, &error))
		{
			printd (WARN, "error disconnecting from HAL - %s", error.message);
			dbus_error_free (&error);
		}
		libhal_ctx_free (ctx);
	}
	if (connection != NULL)
		dbus_connection_unref (connection);

	if (removable_devices != NULL)
	{
		GList *node;
		for (node = removable_devices; node != NULL; node = node->next)
		{
			_e2_hal_destroy_data ((HalStorageData *)node->data);
		}
		g_list_free (removable_devices);
	}
}

#endif //def E2_HAL
