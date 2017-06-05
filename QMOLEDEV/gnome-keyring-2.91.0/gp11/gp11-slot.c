/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gp11-slot.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2008, Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <nielsen@memberwebs.com>
*/

#include "config.h"

#include "gp11.h"
#include "gp11-private.h"

#include <string.h>

/**
 * SECTION:gp11-slot
 * @title: GP11Slot
 * @short_description: Represents a PKCS11 slot that can contain a token.
 * 
 * A PKCS11 slot can contain a token. As an example, a slot might be a card reader, and the token
 * the card. If the PKCS11 module is not a hardware driver, often the slot and token are equivalent.  
 */

/**
 * GP11Slot:
 * 
 * Represents a PKCS11 slot.
 */

enum {
	PROP_0,
	PROP_MODULE,
	PROP_HANDLE
};

typedef struct _GP11SlotData {
	GP11Module *module;
	CK_SLOT_ID handle;
} GP11SlotData;

typedef struct _GP11SlotPrivate {
	GP11SlotData data;
} GP11SlotPrivate;

G_DEFINE_TYPE (GP11Slot, gp11_slot, G_TYPE_OBJECT);

#define GP11_SLOT_GET_DATA(o) \
      (G_TYPE_INSTANCE_GET_PRIVATE((o), GP11_TYPE_SLOT, GP11SlotData))

#ifndef HAVE_TIMEGM

time_t 
timegm(struct tm *t)
{
	time_t tl, tb;
	struct tm *tg;

	tl = mktime (t);
	if (tl == -1)
	{
		t->tm_hour--;
		tl = mktime (t);
		if (tl == -1)
			return -1; 
		tl += 3600;
	    }
	tg = gmtime (&tl);
	tg->tm_isdst = 0;
	tb = mktime (tg);
	if (tb == -1)
	{
		tg->tm_hour--;
		tb = mktime (tg);
		if (tb == -1)
			return -1; 
		tb += 3600;
	}
	return (tl - (tb - tl));
}

#endif

/* ----------------------------------------------------------------------------
 * HELPERS
 */

static GP11Session*
make_session_object (GP11Slot *self, gulong flags, CK_SESSION_HANDLE handle)
{
	GP11Session *session;
	GP11Module *module;

	g_return_val_if_fail (handle != 0, NULL);

	module = gp11_slot_get_module (self);

	session = gp11_session_from_handle (self, handle);
	g_return_val_if_fail (session != NULL, NULL);
	
	/* Session keeps a reference to module so this is safe */
	g_signal_connect (session, "discard-handle", 
	                  G_CALLBACK (_gp11_module_pool_session_handle), module); 

	g_object_unref (module);
	
	return session;
}

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static void
gp11_slot_init (GP11Slot *self)
{

}

static void
gp11_slot_get_property (GObject *obj, guint prop_id, GValue *value, 
                        GParamSpec *pspec)
{
	GP11Slot *self = GP11_SLOT (obj);
	
	switch (prop_id) {
	case PROP_MODULE:
		g_value_take_object (value, gp11_slot_get_module (self));
		break;
	case PROP_HANDLE:
		g_value_set_ulong (value, gp11_slot_get_handle (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gp11_slot_set_property (GObject *obj, guint prop_id, const GValue *value, 
                        GParamSpec *pspec)
{
	GP11SlotData *data = GP11_SLOT_GET_DATA (obj);

	/* All writes to data members below, happen only during construct phase */

	switch (prop_id) {
	case PROP_MODULE:
		g_assert (!data->module);
		data->module = g_value_get_object (value);
		g_assert (data->module);
		g_object_ref (data->module);
		break;
	case PROP_HANDLE:
		g_assert (!data->handle);
		data->handle = g_value_get_ulong (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gp11_slot_dispose (GObject *obj)
{
	G_OBJECT_CLASS (gp11_slot_parent_class)->dispose (obj);
}

static void
gp11_slot_finalize (GObject *obj)
{
	GP11SlotData *data = GP11_SLOT_GET_DATA (obj);
	
	data->handle = 0;
	
	if (data->module)
		g_object_unref (data->module);
	data->module = NULL;

	G_OBJECT_CLASS (gp11_slot_parent_class)->finalize (obj);
}


static void
gp11_slot_class_init (GP11SlotClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;
	gp11_slot_parent_class = g_type_class_peek_parent (klass);
	
	gobject_class->get_property = gp11_slot_get_property;
	gobject_class->set_property = gp11_slot_set_property;
	gobject_class->dispose = gp11_slot_dispose;
	gobject_class->finalize = gp11_slot_finalize;
	
	/**
	 * GP11Slot:module:
	 * 
	 * The PKCS11 object that this slot is a part of.
	 */
	g_object_class_install_property (gobject_class, PROP_MODULE,
		g_param_spec_object ("module", "Module", "PKCS11 Module",
		                     GP11_TYPE_MODULE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GP11Slot:handle:
	 * 
	 * The raw CK_SLOT_ID handle of this slot.
	 */
	g_object_class_install_property (gobject_class, PROP_HANDLE,
		g_param_spec_ulong ("handle", "Handle", "PKCS11 Slot ID",
		                   0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (gobject_class, sizeof (GP11SlotPrivate));
}

/* ----------------------------------------------------------------------------
 * PUBLIC 
 */

/**
 * GP11SlotInfo:
 * @slot_description: Description of the slot.
 * @manufacturer_id: The manufacturer of this slot.
 * @flags: Various PKCS11 flags that apply to this slot. 
 * @hardware_version_major: The major version of the hardware.
 * @hardware_version_minor: The minor version of the hardware.
 * @firmware_version_major: The major version of the firmware.
 * @firmware_version_minor: The minor version of the firmware.
 * 
 * Represents information about a PKCS11 slot.
 * 
 * This is analogous to a CK_SLOT_INFO structure, but the 
 * strings are far more usable.
 * 
 * When you're done with this structure it should be released with 
 * gp11_slot_info_free().
 */

/**
 * gp11_slot_info_free:
 * @slot_info: The slot info to free, or NULL.
 * 
 * Free the GP11SlotInfo and associated resources. 
 **/
void
gp11_slot_info_free (GP11SlotInfo *slot_info)
{
	if (!slot_info)
		return;
	g_free (slot_info->slot_description);
	g_free (slot_info->manufacturer_id);
	g_free (slot_info);
}

/**
 * GP11TokenInfo:
 * @label: The displayable token label.
 * @manufacturer_id: The manufacturer of this slot.
 * @model: The token model number as a string.
 * @serial_number: The token serial number as a string.
 * @flags: Various PKCS11 flags that apply to this token.
 * @max_session_count: The maximum number of sessions allowed on this token.
 * @session_count: The number of sessions open on this token.
 * @max_rw_session_count: The maximum number of read/write sessions allowed on this token.
 * @rw_session_count: The number of sessions open on this token.
 * @max_pin_len: The maximum length of a PIN for locking this token.
 * @min_pin_len: The minimum length of a PIN for locking this token.
 * @total_public_memory: The total amount of memory on this token for storing public objects.
 * @free_public_memory: The available amount of memory on this token for storing public objects.  
 * @total_private_memory: The total amount of memory on this token for storing private objects.
 * @free_private_memory: The available amount of memory on this token for storing private objects.  
 * @hardware_version_major: The major version of the hardware.
 * @hardware_version_minor: The minor version of the hardware.
 * @firmware_version_major: The major version of the firmware.
 * @firmware_version_minor: The minor version of the firmware.
 * @utc_time: If the token has a hardware clock, this is set to the number of seconds since the epoch.
 * 
 * Represents information about a PKCS11 token.
 * 
 * This is analogous to a CK_TOKEN_INFO structure, but the 
 * strings are far more usable.
 * 
 * When you're done with this structure it should be released with 
 * gp11_token_info_free().
 */

/**
 * gp11_token_info_free:
 * @token_info: The token info to free, or NULL.
 * 
 * Free the GP11TokenInfo and associated resources.
 **/
void
gp11_token_info_free (GP11TokenInfo *token_info)
{
	if (!token_info)
		return;
	g_free (token_info->label);
	g_free (token_info->manufacturer_id);
	g_free (token_info->model);
	g_free (token_info->serial_number);
	g_free (token_info);
}

/**
 * GP11MechanismInfo:
 * @min_key_size: The minimum key size that can be used with this mechanism.
 * @max_key_size: The maximum key size that can be used with this mechanism.
 * @flags: Various PKCS11 flags that apply to this mechanism.
 * 
 * Represents information about a PKCS11 mechanism.
 * 
 * This is analogous to a CK_MECHANISM_INFO structure.
 * 
 * When you're done with this structure it should be released with 
 * gp11_mechanism_info_free().
 */

/**
 * gp11_mechanism_info_free:
 * @mech_info: The mechanism info to free, or NULL.
 * 
 * Free the GP11MechanismInfo and associated resources.
 **/
void
gp11_mechanism_info_free (GP11MechanismInfo *mech_info)
{
	if (!mech_info)
		return;
	g_free (mech_info);
}

/**
 * GP11Mechanisms:
 * 
 * A set of GP11MechanismInfo structures.
 */

/**
 * gp11_mechanisms_length:
 * @a: A GP11Mechanisms set.
 * 
 * Get the number of GP11MechanismInfo in the set.
 * 
 * Returns: The number in the set.
 */

/**
 * gp11_mechanisms_at:
 * @a: A GP11Mechanisms set.
 * @i: The index of a GP11MechanismInfo.
 * 
 * Get a specific GP11MechanismInfo in a the set.
 * 
 * Returns: The GP11MechanismInfo.
 */

/**
 * gp11_mechanisms_free:
 * @a: A GP11Mechanisms set.
 * 
 * Free a GP11Mechanisms set.
 */

/**
 * gp11_mechanisms_check:
 * @mechanisms: A list of mechanisms, perhaps retrieved from gp11_slot_get_mechanisms().
 * @...: A list of mechanism types followed by GP11_INVALID.
 * 
 * Check whether all the mechanism types are in the list.
 * 
 * The arguments should be a list of CKM_XXX mechanism types. The last argument
 * should be GP11_INVALID. 
 * 
 * Return value: Whether the mechanism is in the list or not.
 **/
gboolean
gp11_mechanisms_check (GP11Mechanisms *mechanisms, ...)
{
	gboolean found = TRUE;
	va_list va;
	gulong mech;
	gsize i;
	
	g_return_val_if_fail (mechanisms, FALSE);
	
	va_start (va, mechanisms);
	for (;;) {
		mech = va_arg (va, gulong);
		if (mech == GP11_INVALID)
			break;
		
		found = FALSE;
		for (i = 0; i < gp11_mechanisms_length (mechanisms); ++i) {
			if (gp11_mechanisms_at (mechanisms, i) == mech) {
				found = TRUE;
				break;
			}
		}
		
		if (found == FALSE)
			break;
		
	}
	va_end (va);
	
	return found;
}

/**
 * gp11_slot_equal:
 * @slot1: A pointer to the first GP11Slot
 * @slot2: A pointer to the second GP11Slot
 * 
 * Checks equality of two slots. Two GP11Slot objects can point to the same 
 * underlying PKCS#11 slot.
 * 
 * Return value: TRUE if slot1 and slot2 are equal. FALSE if either is not a GP11Slot.
 **/
gboolean
gp11_slot_equal (gconstpointer slot1, gconstpointer slot2)
{
	GP11SlotData *data1, *data2;

	if (slot1 == slot2)
		return TRUE;
	if (!GP11_IS_SLOT (slot1) || !GP11_IS_SLOT (slot2))
		return FALSE;
	
	data1 = GP11_SLOT_GET_DATA (slot1);
	data2 = GP11_SLOT_GET_DATA (slot2);
	
	return data1->handle == data2->handle && 
	       gp11_module_equal (data1->module, data2->module);
}

/**
 * gp11_slot_hash:
 * @slot: A pointer to a GP11Slot
 * 
 * Create a hash value for the GP11Slot. 
 * 
 * This function is intended for easily hashing a GP11Slot to add to 
 * a GHashTable or similar data structure.
 * 
 * Return value: An integer that can be used as a hash value, or 0 if invalid.
 **/
guint
gp11_slot_hash (gconstpointer slot)
{
	GP11SlotData *data;
	
	g_return_val_if_fail (GP11_IS_SLOT (slot), 0);

	data = GP11_SLOT_GET_DATA (slot);
	
	return _gp11_ulong_hash (&data->handle) ^
	       gp11_module_hash (data->module);
}

/**
 * gp11_slot_get_handle:
 * @self: The slot to get the handle of.
 * 
 * Get the raw PKCS#11 handle of a slot.
 * 
 * Return value: The raw handle.
 **/
CK_SLOT_ID
gp11_slot_get_handle (GP11Slot *self)
{
	GP11SlotData *data = GP11_SLOT_GET_DATA (self);
	g_return_val_if_fail (GP11_IS_SLOT (self), (CK_SLOT_ID)-1);
	return data->handle;
}

/**
 * gp11_slot_get_module:
 * @self: The slot to get the module for.
 * 
 * Get the module that this slot is on.
 * 
 * Return value: The module, you must unreference this after you're done with it.
 */
GP11Module*
gp11_slot_get_module (GP11Slot *self)
{
	GP11SlotData *data = GP11_SLOT_GET_DATA (self);
	g_return_val_if_fail (GP11_IS_SLOT (self), NULL);
	g_return_val_if_fail (GP11_IS_MODULE (data->module), NULL);
	return g_object_ref (data->module);
}

/**
 * gp11_slot_get_info:
 * @self: The slot to get info for.
 * 
 * Get the information for this slot.
 * 
 * Return value: The slot information. When done, use gp11_slot_info_free()
 * to release it.
 **/
GP11SlotInfo*
gp11_slot_get_info (GP11Slot *self)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	GP11Module *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;
	GP11SlotInfo *slotinfo;
	CK_SLOT_INFO info;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_SLOT (self), NULL);
	
	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GP11_IS_MODULE (module), NULL);
	
	funcs = gp11_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);
	
	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetSlotInfo) (handle, &info);
	
	g_object_unref (module);
	
	if (rv != CKR_OK) {
		g_warning ("couldn't get slot info: %s", gp11_message_from_rv (rv));
		return NULL;
	}
	
	slotinfo = g_new0 (GP11SlotInfo, 1);
	slotinfo->slot_description = gp11_string_from_chars (info.slotDescription, 
	                                                     sizeof (info.slotDescription));
	slotinfo->manufacturer_id = gp11_string_from_chars (info.manufacturerID, 
	                                                    sizeof (info.manufacturerID));
	slotinfo->flags = info.flags;
	slotinfo->hardware_version_major = info.hardwareVersion.major;
	slotinfo->hardware_version_minor = info.hardwareVersion.minor;
	slotinfo->firmware_version_major = info.firmwareVersion.major;
	slotinfo->firmware_version_minor = info.firmwareVersion.minor;

	return slotinfo;
}

/**
 * gp11_slot_get_token_info:
 * @self: The slot to get info for.
 * 
 * Get the token information for this slot.
 * 
 * Return value: The token information. When done, use gp11_token_info_free()
 * to release it.
 **/
GP11TokenInfo*
gp11_slot_get_token_info (GP11Slot *self)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	CK_FUNCTION_LIST_PTR funcs;
	GP11Module *module = NULL;
	GP11TokenInfo *tokeninfo;
	CK_TOKEN_INFO info;
	gchar *string;
	struct tm tm;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_SLOT (self), NULL);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GP11_IS_MODULE (module), NULL);
	
	funcs = gp11_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);
	
	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetTokenInfo) (handle, &info);
	
	g_object_unref (module);
	
	if (rv != CKR_OK) {
		g_warning ("couldn't get slot info: %s", gp11_message_from_rv (rv));
		return NULL;
	}
	
	tokeninfo = g_new0 (GP11TokenInfo, 1);
	tokeninfo->label = gp11_string_from_chars (info.label, sizeof (info.label));
	tokeninfo->model = gp11_string_from_chars (info.model, sizeof (info.model));
	tokeninfo->manufacturer_id = gp11_string_from_chars (info.manufacturerID, 
	                                                     sizeof (info.manufacturerID));
	tokeninfo->serial_number = gp11_string_from_chars (info.serialNumber, 
	                                                   sizeof (info.serialNumber));
	tokeninfo->flags = info.flags;
	tokeninfo->max_session_count = info.ulMaxSessionCount;
	tokeninfo->session_count = info.ulSessionCount;
	tokeninfo->max_rw_session_count = info.ulMaxRwSessionCount;
	tokeninfo->rw_session_count = info.ulRwSessionCount;
	tokeninfo->max_pin_len = info.ulMaxPinLen;
	tokeninfo->min_pin_len = info.ulMinPinLen;
	tokeninfo->total_public_memory = info.ulTotalPublicMemory;
	tokeninfo->total_private_memory = info.ulTotalPrivateMemory;
	tokeninfo->free_private_memory = info.ulFreePrivateMemory;
	tokeninfo->free_public_memory = info.ulFreePublicMemory;
	tokeninfo->hardware_version_major = info.hardwareVersion.major;
	tokeninfo->hardware_version_minor = info.hardwareVersion.minor;
	tokeninfo->firmware_version_major = info.firmwareVersion.major;
	tokeninfo->firmware_version_minor = info.firmwareVersion.minor;
	
	/* Parse the time into seconds since epoch */
	if (info.flags & CKF_CLOCK_ON_TOKEN) {
		string = g_strndup ((gchar*)info.utcTime, MIN (14, sizeof (info.utcTime)));
		if (!strptime (string, "%Y%m%d%H%M%S", &tm))
			tokeninfo->utc_time = -1;
		else
			tokeninfo->utc_time = timegm (&tm);
		g_free (string);
	} else {
		tokeninfo->utc_time = -1;
	}
	
	return tokeninfo;
}

/**
 * gp11_slot_get_mechanisms:
 * @self: The slot to get mechanisms for.
 * 
 * Get the available mechanisms for this slot.
 * 
 * Return value: A list of the mechanisms for this slot. Use 
 * gp11_mechanisms_free() when done with this.
 **/
GP11Mechanisms*
gp11_slot_get_mechanisms (GP11Slot *self)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	CK_FUNCTION_LIST_PTR funcs;
	GP11Module *module = NULL;
	CK_MECHANISM_TYPE_PTR mech_list = NULL;
	CK_ULONG count, i;
	GP11Mechanisms *result;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_SLOT (self), NULL);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GP11_IS_MODULE (module), NULL);

	funcs = gp11_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);
	
	rv = (funcs->C_GetMechanismList) (handle, NULL, &count);
	if (rv != CKR_OK) {
		g_warning ("couldn't get mechanism count: %s", gp11_message_from_rv (rv));
		count = 0;
	} else {
		mech_list = g_new (CK_MECHANISM_TYPE, count);
		rv = (funcs->C_GetMechanismList) (handle, mech_list, &count);
		if (rv != CKR_OK) {
			g_warning ("couldn't get mechanism list: %s", gp11_message_from_rv (rv));
			g_free (mech_list);
			count = 0;
		}
	}
	
	g_object_unref (module);
	
	if (!count)
		return NULL;
	
	result = g_array_new (FALSE, TRUE, sizeof (CK_MECHANISM_TYPE));
	for (i = 0; i < count; ++i)
		g_array_append_val (result, mech_list[i]);
	
	g_free (mech_list);
	return result;

}

/**
 * gp11_slot_get_mechanism_info:
 * @self: The slot to get mechanism info from.
 * @mech_type: The mechanisms type to get info for.
 * 
 * Get information for the specified mechanism.
 * 
 * Return value: The mechanism information, or NULL if failed. Use 
 * gp11_mechanism_info_free() when done with it.
 **/
GP11MechanismInfo*
gp11_slot_get_mechanism_info (GP11Slot *self, gulong mech_type)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	CK_FUNCTION_LIST_PTR funcs;
	GP11MechanismInfo *mechinfo;
	GP11Module *module = NULL;
	CK_MECHANISM_INFO info;
	struct tm;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_SLOT (self), NULL);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GP11_IS_MODULE (module), NULL);
	
	funcs = gp11_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);
		
	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetMechanismInfo) (handle, mech_type, &info);
	
	g_object_unref (module);
	
	if (rv != CKR_OK) {
		g_warning ("couldn't get mechanism info: %s", gp11_message_from_rv (rv));
		return NULL;
	}
	
	mechinfo = g_new0 (GP11MechanismInfo, 1);
	mechinfo->flags = info.flags;
	mechinfo->max_key_size = info.ulMaxKeySize;
	mechinfo->min_key_size = info.ulMinKeySize;
	
	return mechinfo;
}

/**
 * gp11_slot_has_flags:
 * @self: The GP11Slot object.
 * @flags: The flags to check.
 * 
 * Check if the PKCS11 slot has the given flags.
 * 
 * Returns: Whether one or more flags exist.
 */
gboolean
gp11_slot_has_flags (GP11Slot *self, gulong flags)
{
	CK_FUNCTION_LIST_PTR funcs;
	GP11Module *module = NULL;
	CK_TOKEN_INFO info;
	CK_SLOT_ID handle;
	CK_RV rv;
	
	g_return_val_if_fail (GP11_IS_SLOT (self), FALSE);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GP11_IS_MODULE (module), FALSE);
	
	funcs = gp11_module_get_functions (module);
	g_return_val_if_fail (funcs, FALSE);
	
	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetTokenInfo) (handle, &info);
	
	g_object_unref (module);
	
	if (rv != CKR_OK) {
		g_warning ("couldn't get slot info: %s", gp11_message_from_rv (rv));
		return FALSE;
	}
	
	return (info.flags & flags) != 0;
}

#ifdef UNIMPLEMENTED

typedef struct InitToken {
	GP11Arguments base;
	const guchar *pin;
	gsize length;
	const gchar *label;
} InitToken;

static CK_RV
perform_init_token (InitToken *args)
{
	return (args->base.pkcs11->C_InitToken) (args->base.handle, 
	                                         args->pin, args->length, 
	                                         args->label);
}

gboolean
gp11_slot_init_token (GP11Slot *self, const guchar *pin, gsize length, 
                      const gchar *label, GCancellable *cancellable,
                      GError **err)
{
	InitToken args = { GP11_ARGUMENTS_INIT, pin, length, label };
	return _gp11_call_sync (self, perform_init_token, NULL, &args, err);
}

void
gp11_slot_init_token_async (GP11Slot *self, const guchar *pin, gsize length, 
                            const gchar *label, GCancellable *cancellable,
                            GAsyncReadyCallback callback, gpointer user_data)
{
	InitToken* args = _gp11_call_async_prep (self, self, perform_init_token, 
	                                         NULL, sizeof (*args));
	
	args->pin = pin;
	args->length = length;
	args->label = label;
	
	_gp11_call_async_go (args, cancellable, callback, user_data);
}
	
gboolean
gp11_slot_init_token_finish (GP11Slot *self, GAsyncResult *result, GError **err)
{
	return _gp11_call_basic_finish (self, result, err);
}

#endif /* UNIMPLEMENTED */

typedef struct OpenSession {
	GP11Arguments base;
	GP11Slot *slot;
	gulong flags;
	gpointer app_data;
	CK_NOTIFY notify;
	gchar *password;
	gboolean auto_login;
	CK_SESSION_HANDLE session;
} OpenSession;

static CK_RV
perform_open_session (OpenSession *args)
{
	CK_SESSION_INFO info;
	CK_RV rv = CKR_OK;
	CK_ULONG pin_len;
	
	/* Can be called multiple times */
	
	/* First step, open session */
	if (!args->session) {
		rv = (args->base.pkcs11->C_OpenSession) (args->base.handle, args->flags, 
		                                         args->app_data, args->notify, &args->session);
	}

	if (rv != CKR_OK || !args->auto_login)
		return rv;

	/* Step two, check if session is logged in */
	rv = (args->base.pkcs11->C_GetSessionInfo) (args->session, &info);
	if (rv != CKR_OK)
		return rv;
	
	/* Already logged in? */
	if (info.state != CKS_RO_PUBLIC_SESSION && info.state != CKS_RW_PUBLIC_SESSION)
		return CKR_OK;
	
	/* Try to login */
	pin_len = args->password ? strlen (args->password) : 0;
	return (args->base.pkcs11->C_Login) (args->session, CKU_USER, 
	                                     (CK_UTF8CHAR_PTR)args->password, pin_len);
}

static gboolean
complete_open_session (OpenSession *args, CK_RV result)
{
	GP11Module *module;
	gboolean ret = TRUE;
	
	g_free (args->password);
	args->password = NULL;
	
	/* Ask the token for a password */
	module = gp11_slot_get_module (args->slot);

	if (args->auto_login && result == CKR_PIN_INCORRECT) {
		
		ret = _gp11_module_fire_authenticate_slot (module, args->slot, NULL, &args->password);
		
		/* If authenticate returns TRUE then call is not complete */
		ret = !ret;
	}

	g_object_unref (module);

	return ret;
}

static void
free_open_session (OpenSession *args)
{
	g_assert (!args->password);
	if (args->slot)
		g_object_unref (args->slot);
	g_free (args);
}

/**
 * gp11_slot_open_session:
 * @self: The slot ot open a session on.
 * @flags: The flags to open a session with.
 * @err: A location to return an error, or NULL.
 * 
 * Open a session on the slot. If the 'auto reuse' setting is set,
 * then this may be a recycled session with the same flags.
 * 
 * This call may block for an indefinite period.
 * 
 * Return value: A new session or NULL if an error occurs.
 **/
GP11Session*
gp11_slot_open_session (GP11Slot *self, gulong flags, GError **err)
{
	return gp11_slot_open_session_full (self, flags, NULL, NULL, NULL, err);
}

/**
 * gp11_slot_open_session_full:
 * @self: The slot to open a session on.
 * @flags: The flags to open a session with.
 * @app_data: Application data for notification callback.
 * @notify: PKCS#11 notification callback.
 * @cancellable: Optional cancellation object, or NULL.
 * @err: A location to return an error, or NULL.
 * 
 * Open a session on the slot. If the 'auto reuse' setting is set,
 * then this may be a recycled session with the same flags.
 * 
 * This call may block for an indefinite period.
 * 
 * Return value: A new session or NULL if an error occurs.
 **/
GP11Session*
gp11_slot_open_session_full (GP11Slot *self, gulong flags, gpointer app_data,
                             CK_NOTIFY notify, GCancellable *cancellable, GError **err)
{
	GP11Session *session = NULL;
	GP11Module *module = NULL;
	CK_SESSION_HANDLE handle;
	CK_SLOT_ID slot_id;

	flags |= CKF_SERIAL_SESSION;
	
	g_object_ref (self);
	
	/* Try to use a cached session */
	module = gp11_slot_get_module (self);
	slot_id = gp11_slot_get_handle (self);
	handle = _gp11_module_pooled_session_handle (module, slot_id, flags);
	if (handle != 0) 
		session = make_session_object (self, flags, handle);

	/* Open a new session */
	if (session == NULL) {
		OpenSession args = { GP11_ARGUMENTS_INIT, 0,  };
		
		args.slot = self;
		args.flags = flags;
		args.app_data = app_data;
		args.notify = notify;
		args.password = NULL;
		args.auto_login = (gp11_module_get_auto_authenticate (module) & GP11_AUTHENTICATE_TOKENS) ? TRUE : FALSE;
		args.session = 0;
		
		if (_gp11_call_sync (self, perform_open_session, complete_open_session, &args, cancellable, err))
			session = make_session_object (self, flags, args.session);
	}

	g_object_unref (module);
	g_object_unref (self);
	
	return session;
}

/**
 * gp11_slot_open_session_async:
 * @self: The slot to open a session on.
 * @flags: The flags to open a session with.
 * @app_data: Application data for notification callback.
 * @notify: PKCS#11 notification callback.
 * @cancellable: Optional cancellation object, or NULL.
 * @callback: Called when the operation completes.
 * @user_data: Data to pass to the callback.
 * 
 * Open a session on the slot. If the 'auto reuse' setting is set,
 * then this may be a recycled session with the same flags.
 * 
 * This call will return immediately and complete asynchronously.
 **/
void
gp11_slot_open_session_async (GP11Slot *self, gulong flags, gpointer app_data,
                              CK_NOTIFY notify, GCancellable *cancellable,
                              GAsyncReadyCallback callback, gpointer user_data)
{
	GP11Module *module = NULL;
	GP11Call *call;
	OpenSession *args;
	CK_SLOT_ID slot_id;

	flags |= CKF_SERIAL_SESSION;
	
	g_object_ref (self);
	
	args =  _gp11_call_async_prep (self, self, perform_open_session, complete_open_session, 
	                               sizeof (*args), free_open_session);

	args->flags = flags;
	args->app_data = app_data;
	args->notify = notify;
	args->slot = g_object_ref (self);

	/* Try to use a cached session */
	module = gp11_slot_get_module (self);
	slot_id = gp11_slot_get_handle (self);
	args->session = _gp11_module_pooled_session_handle (module, slot_id, flags);
	args->auto_login = (gp11_module_get_auto_authenticate (module) & GP11_AUTHENTICATE_TOKENS) ? TRUE : FALSE;
	g_object_unref (module);
	
	call = _gp11_call_async_ready (args, cancellable, callback, user_data);
	if (args->session)
		_gp11_call_async_short (call, CKR_OK);
	else
		_gp11_call_async_go (call);
	
	g_object_unref (self);
}

/**
 * gp11_slot_open_session_finish:
 * @self: The slot to open a session on.
 * @result: The result passed to the callback.
 * @err: A location to return an error or NULL.
 * 
 * Get the result of an open session operation. If the 'auto reuse' setting is set,
 * then this may be a recycled session with the same flags.
 * 
 * Return value: The new session or NULL if an error occurs.
 */
GP11Session*
gp11_slot_open_session_finish (GP11Slot *self, GAsyncResult *result, GError **err)
{
	GP11Session *session = NULL;

	g_object_ref (self);
	
	{
		OpenSession *args;

		if (_gp11_call_basic_finish (result, err)) {
			args = _gp11_call_arguments (result, OpenSession);
			session = make_session_object (self, args->flags, args->session);
		}
	}
	
	g_object_unref (self);
	
	return session;
}
