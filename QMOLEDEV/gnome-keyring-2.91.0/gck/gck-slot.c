/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-slot.c - the GObject PKCS#11 wrapper library

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

#include "gck.h"
#include "gck-private.h"

#include <string.h>

/**
 * SECTION:gck-slot
 * @title: GckSlot
 * @short_description: Represents a PKCS11 slot that can contain a token.
 *
 * A PKCS11 slot can contain a token. As an example, a slot might be a card reader, and the token
 * the card. If the PKCS11 module is not a hardware driver, often the slot and token are equivalent.
 */

/**
 * GckSlot:
 *
 * Represents a PKCS11 slot.
 */

enum {
	PROP_0,
	PROP_MODULE,
	PROP_HANDLE
};

struct _GckSlotPrivate {
	GckModule *module;
	CK_SLOT_ID handle;
};

G_DEFINE_TYPE (GckSlot, gck_slot, G_TYPE_OBJECT);

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

static GckSession*
make_session_object (GckSlot *self, guint options, CK_SESSION_HANDLE handle)
{
	GckSession *session;
	GckModule *module;

	g_return_val_if_fail (handle != 0, NULL);

	module = gck_slot_get_module (self);

	session = gck_session_from_handle (self, handle, options);
	g_return_val_if_fail (session != NULL, NULL);

	g_object_unref (module);

	return session;
}

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static void
gck_slot_init (GckSlot *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCK_TYPE_SLOT, GckSlotPrivate);
}

static void
gck_slot_get_property (GObject *obj, guint prop_id, GValue *value,
                       GParamSpec *pspec)
{
	GckSlot *self = GCK_SLOT (obj);

	switch (prop_id) {
	case PROP_MODULE:
		g_value_take_object (value, gck_slot_get_module (self));
		break;
	case PROP_HANDLE:
		g_value_set_ulong (value, gck_slot_get_handle (self));
		break;
	}
}

static void
gck_slot_set_property (GObject *obj, guint prop_id, const GValue *value,
                        GParamSpec *pspec)
{
	GckSlot *self = GCK_SLOT (obj);

	/* All writes to data members below, happen only during construct phase */

	switch (prop_id) {
	case PROP_MODULE:
		g_assert (!self->pv->module);
		self->pv->module = g_value_get_object (value);
		g_assert (self->pv->module);
		g_object_ref (self->pv->module);
		break;
	case PROP_HANDLE:
		g_assert (!self->pv->handle);
		self->pv->handle = g_value_get_ulong (value);
		break;
	}
}

static void
gck_slot_dispose (GObject *obj)
{
	G_OBJECT_CLASS (gck_slot_parent_class)->dispose (obj);
}

static void
gck_slot_finalize (GObject *obj)
{
	GckSlot *self = GCK_SLOT (obj);
	self->pv->handle = 0;

	if (self->pv->module)
		g_object_unref (self->pv->module);
	self->pv->module = NULL;

	G_OBJECT_CLASS (gck_slot_parent_class)->finalize (obj);
}


static void
gck_slot_class_init (GckSlotClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;
	gck_slot_parent_class = g_type_class_peek_parent (klass);

	gobject_class->get_property = gck_slot_get_property;
	gobject_class->set_property = gck_slot_set_property;
	gobject_class->dispose = gck_slot_dispose;
	gobject_class->finalize = gck_slot_finalize;

	/**
	 * GckSlot:module:
	 *
	 * The PKCS11 object that this slot is a part of.
	 */
	g_object_class_install_property (gobject_class, PROP_MODULE,
		g_param_spec_object ("module", "Module", "PKCS11 Module",
		                     GCK_TYPE_MODULE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * GckSlot:handle:
	 *
	 * The raw CK_SLOT_ID handle of this slot.
	 */
	g_object_class_install_property (gobject_class, PROP_HANDLE,
		g_param_spec_ulong ("handle", "Handle", "PKCS11 Slot ID",
		                   0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (gobject_class, sizeof (GckSlotPrivate));
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * GckSlotInfo:
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
 * gck_slot_info_free().
 */

/**
 * gck_slot_info_free:
 * @slot_info: The slot info to free, or NULL.
 *
 * Free the GckSlotInfo and associated resources.
 **/
void
gck_slot_info_free (GckSlotInfo *slot_info)
{
	if (!slot_info)
		return;
	g_free (slot_info->slot_description);
	g_free (slot_info->manufacturer_id);
	g_free (slot_info);
}

/**
 * GckTokenInfo:
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
 * gck_token_info_free().
 */

/**
 * gck_token_info_free:
 * @token_info: The token info to free, or NULL.
 *
 * Free the GckTokenInfo and associated resources.
 **/
void
gck_token_info_free (GckTokenInfo *token_info)
{
	if (!token_info)
		return;
	g_free (token_info->label);
	g_free (token_info->manufacturer_id);
	g_free (token_info->model);
	g_free (token_info->serial_number);
	g_free (token_info);
}

static gboolean
match_token_string (const gchar *match, const gchar *string)
{
	/* NULL matches anything */
	if (match == NULL)
		return TRUE;

	if (string == NULL)
		return FALSE;

	return g_str_equal (match, string);
}

gboolean
_gck_token_info_match (GckTokenInfo *match, GckTokenInfo *info)
{
	/* Matches two GckTokenInfo for use in PKCS#11 URI's */

	g_return_val_if_fail (match, FALSE);
	g_return_val_if_fail (info, FALSE);

	return (match_token_string (match->label, info->label) &&
	        match_token_string (match->manufacturer_id, info->manufacturer_id) &&
	        match_token_string (match->model, info->model) &&
	        match_token_string (match->serial_number, info->serial_number));
}

/**
 * GckMechanismInfo:
 * @min_key_size: The minimum key size that can be used with this mechanism.
 * @max_key_size: The maximum key size that can be used with this mechanism.
 * @flags: Various PKCS11 flags that apply to this mechanism.
 *
 * Represents information about a PKCS11 mechanism.
 *
 * This is analogous to a CK_MECHANISM_INFO structure.
 *
 * When you're done with this structure it should be released with
 * gck_mechanism_info_free().
 */

/**
 * gck_mechanism_info_free:
 * @mech_info: The mechanism info to free, or NULL.
 *
 * Free the GckMechanismInfo and associated resources.
 **/
void
gck_mechanism_info_free (GckMechanismInfo *mech_info)
{
	if (!mech_info)
		return;
	g_free (mech_info);
}

/**
 * GckMechanisms:
 *
 * A set of GckMechanismInfo structures.
 */

/**
 * gck_mechanisms_length:
 * @a: A GckMechanisms set.
 *
 * Get the number of GckMechanismInfo in the set.
 *
 * Returns: The number in the set.
 */

/**
 * gck_mechanisms_at:
 * @a: A GckMechanisms set.
 * @i: The index of a GckMechanismInfo.
 *
 * Get a specific GckMechanismInfo in a the set.
 *
 * Returns: The GckMechanismInfo.
 */

/**
 * gck_mechanisms_free:
 * @a: A GckMechanism set.
 *
 * Free a GckMechanisms set.
 */

/**
 * gck_mechanisms_check:
 * @mechanisms: A list of mechanisms, perhaps retrieved from gck_slot_get_mechanisms().
 * @...: A list of mechanism types followed by GCK_INVALID.
 *
 * Check whether all the mechanism types are in the list.
 *
 * The arguments should be a list of CKM_XXX mechanism types. The last argument
 * should be GCK_INVALID.
 *
 * Return value: Whether the mechanism is in the list or not.
 **/
gboolean
gck_mechanisms_check (GckMechanisms *mechanisms, ...)
{
	gboolean found = TRUE;
	va_list va;
	gulong mech;
	gsize i;

	g_return_val_if_fail (mechanisms, FALSE);

	va_start (va, mechanisms);
	for (;;) {
		mech = va_arg (va, gulong);
		if (mech == GCK_INVALID)
			break;

		found = FALSE;
		for (i = 0; i < gck_mechanisms_length (mechanisms); ++i) {
			if (gck_mechanisms_at (mechanisms, i) == mech) {
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
 * gck_slot_equal:
 * @slot1: A pointer to the first GckSlot
 * @slot2: A pointer to the second GckSlot
 *
 * Checks equality of two slots. Two GckSlot objects can point to the same
 * underlying PKCS#11 slot.
 *
 * Return value: TRUE if slot1 and slot2 are equal. FALSE if either is not a GckSlot.
 **/
gboolean
gck_slot_equal (gconstpointer slot1, gconstpointer slot2)
{
	GckSlot *s1, *s2;

	if (slot1 == slot2)
		return TRUE;
	if (!GCK_IS_SLOT (slot1) || !GCK_IS_SLOT (slot2))
		return FALSE;

	s1 = GCK_SLOT (slot1);
	s2 = GCK_SLOT (slot2);

	return s1->pv->handle == s2->pv->handle &&
	       gck_module_equal (s1->pv->module, s2->pv->module);
}

/**
 * gck_slot_hash:
 * @slot: A pointer to a GckSlot
 *
 * Create a hash value for the GckSlot.
 *
 * This function is intended for easily hashing a GckSlot to add to
 * a GHashTable or similar data structure.
 *
 * Return value: An integer that can be used as a hash value, or 0 if invalid.
 **/
guint
gck_slot_hash (gconstpointer slot)
{
	GckSlot *self;

	g_return_val_if_fail (GCK_IS_SLOT (slot), 0);

	self = GCK_SLOT (slot);

	return _gck_ulong_hash (&self->pv->handle) ^
	       gck_module_hash (self->pv->module);
}

/**
 * gck_slot_from_handle:
 * @module: The module that this slot is on.
 * @slot_id: The raw PKCS#11 handle or slot id of this slot.
 *
 * Create a new GckSlot object for a raw PKCS#11 handle.
 *
 * Return value: The new GckSlot object.
 **/
GckSlot*
gck_slot_from_handle (GckModule *module, CK_SLOT_ID slot_id)
{
	return g_object_new (GCK_TYPE_SLOT, "module", module, "handle", slot_id, NULL);
}

/**
 * gck_slot_get_handle:
 * @self: The slot to get the handle of.
 *
 * Get the raw PKCS#11 handle of a slot.
 *
 * Return value: The raw handle.
 **/
CK_SLOT_ID
gck_slot_get_handle (GckSlot *self)
{
	g_return_val_if_fail (GCK_IS_SLOT (self), (CK_SLOT_ID)-1);
	return self->pv->handle;
}

/**
 * gck_slot_get_module:
 * @self: The slot to get the module for.
 *
 * Get the module that this slot is on.
 *
 * Return value: The module, you must unreference this after you're done with it.
 */
GckModule*
gck_slot_get_module (GckSlot *self)
{
	g_return_val_if_fail (GCK_IS_SLOT (self), NULL);
	g_return_val_if_fail (GCK_IS_MODULE (self->pv->module), NULL);
	return g_object_ref (self->pv->module);
}

/**
 * gck_slot_get_info:
 * @self: The slot to get info for.
 *
 * Get the information for this slot.
 *
 * Return value: The slot information. When done, use gck_slot_info_free()
 * to release it.
 **/
GckSlotInfo*
gck_slot_get_info (GckSlot *self)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	GckModule *module = NULL;
	CK_FUNCTION_LIST_PTR funcs;
	GckSlotInfo *slotinfo;
	CK_SLOT_INFO info;
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_SLOT (self), NULL);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GCK_IS_MODULE (module), NULL);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);

	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetSlotInfo) (handle, &info);

	g_object_unref (module);

	if (rv != CKR_OK) {
		g_warning ("couldn't get slot info: %s", gck_message_from_rv (rv));
		return NULL;
	}

	slotinfo = g_new0 (GckSlotInfo, 1);
	slotinfo->slot_description = gck_string_from_chars (info.slotDescription,
	                                                     sizeof (info.slotDescription));
	slotinfo->manufacturer_id = gck_string_from_chars (info.manufacturerID,
	                                                    sizeof (info.manufacturerID));
	slotinfo->flags = info.flags;
	slotinfo->hardware_version_major = info.hardwareVersion.major;
	slotinfo->hardware_version_minor = info.hardwareVersion.minor;
	slotinfo->firmware_version_major = info.firmwareVersion.major;
	slotinfo->firmware_version_minor = info.firmwareVersion.minor;

	return slotinfo;
}

/**
 * gck_slot_get_token_info:
 * @self: The slot to get info for.
 *
 * Get the token information for this slot.
 *
 * Return value: The token information. When done, use gck_token_info_free()
 * to release it.
 **/
GckTokenInfo*
gck_slot_get_token_info (GckSlot *self)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	CK_FUNCTION_LIST_PTR funcs;
	GckModule *module = NULL;
	GckTokenInfo *tokeninfo;
	CK_TOKEN_INFO info;
	gchar *string;
	struct tm tm;
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_SLOT (self), NULL);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GCK_IS_MODULE (module), NULL);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);

	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetTokenInfo) (handle, &info);

	g_object_unref (module);

	if (rv != CKR_OK) {
		g_warning ("couldn't get slot info: %s", gck_message_from_rv (rv));
		return NULL;
	}

	tokeninfo = g_new0 (GckTokenInfo, 1);
	tokeninfo->label = gck_string_from_chars (info.label, sizeof (info.label));
	tokeninfo->model = gck_string_from_chars (info.model, sizeof (info.model));
	tokeninfo->manufacturer_id = gck_string_from_chars (info.manufacturerID,
	                                                     sizeof (info.manufacturerID));
	tokeninfo->serial_number = gck_string_from_chars (info.serialNumber,
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
 * gck_slot_get_mechanisms:
 * @self: The slot to get mechanisms for.
 *
 * Get the available mechanisms for this slot.
 *
 * Return value: A list of the mechanisms for this slot. Use
 * gck_mechanisms_free() when done with this.
 **/
GckMechanisms*
gck_slot_get_mechanisms (GckSlot *self)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	CK_FUNCTION_LIST_PTR funcs;
	GckModule *module = NULL;
	CK_MECHANISM_TYPE_PTR mech_list = NULL;
	CK_ULONG count, i;
	GckMechanisms *result;
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_SLOT (self), NULL);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GCK_IS_MODULE (module), NULL);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);

	rv = (funcs->C_GetMechanismList) (handle, NULL, &count);
	if (rv != CKR_OK) {
		g_warning ("couldn't get mechanism count: %s", gck_message_from_rv (rv));
		count = 0;
	} else {
		mech_list = g_new (CK_MECHANISM_TYPE, count);
		rv = (funcs->C_GetMechanismList) (handle, mech_list, &count);
		if (rv != CKR_OK) {
			g_warning ("couldn't get mechanism list: %s", gck_message_from_rv (rv));
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
 * gck_slot_get_mechanism_info:
 * @self: The slot to get mechanism info from.
 * @mech_type: The mechanisms type to get info for.
 *
 * Get information for the specified mechanism.
 *
 * Return value: The mechanism information, or NULL if failed. Use
 * gck_mechanism_info_free() when done with it.
 **/
GckMechanismInfo*
gck_slot_get_mechanism_info (GckSlot *self, gulong mech_type)
{
	CK_SLOT_ID handle = (CK_SLOT_ID)-1;
	CK_FUNCTION_LIST_PTR funcs;
	GckMechanismInfo *mechinfo;
	GckModule *module = NULL;
	CK_MECHANISM_INFO info;
	struct tm;
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_SLOT (self), NULL);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GCK_IS_MODULE (module), NULL);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (funcs, NULL);

	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetMechanismInfo) (handle, mech_type, &info);

	g_object_unref (module);

	if (rv != CKR_OK) {
		g_warning ("couldn't get mechanism info: %s", gck_message_from_rv (rv));
		return NULL;
	}

	mechinfo = g_new0 (GckMechanismInfo, 1);
	mechinfo->flags = info.flags;
	mechinfo->max_key_size = info.ulMaxKeySize;
	mechinfo->min_key_size = info.ulMinKeySize;

	return mechinfo;
}

/**
 * gck_slot_has_flags:
 * @self: The GckSlot object.
 * @flags: The flags to check.
 *
 * Check if the PKCS11 slot has the given flags.
 *
 * Returns: Whether one or more flags exist.
 */
gboolean
gck_slot_has_flags (GckSlot *self, gulong flags)
{
	CK_FUNCTION_LIST_PTR funcs;
	GckModule *module = NULL;
	CK_TOKEN_INFO info;
	CK_SLOT_ID handle;
	CK_RV rv;

	g_return_val_if_fail (GCK_IS_SLOT (self), FALSE);

	g_object_get (self, "module", &module, "handle", &handle, NULL);
	g_return_val_if_fail (GCK_IS_MODULE (module), FALSE);

	funcs = gck_module_get_functions (module);
	g_return_val_if_fail (funcs, FALSE);

	memset (&info, 0, sizeof (info));
	rv = (funcs->C_GetTokenInfo) (handle, &info);

	g_object_unref (module);

	if (rv != CKR_OK) {
		g_warning ("couldn't get slot info: %s", gck_message_from_rv (rv));
		return FALSE;
	}

	return (info.flags & flags) != 0;
}

#if UNIMPLEMENTED

typedef struct InitToken {
	GckArguments base;
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
gck_slot_init_token (GckSlot *self, const guchar *pin, gsize length,
                      const gchar *label, GCancellable *cancellable,
                      GError **err)
{
	InitToken args = { GCK_ARGUMENTS_INIT, pin, length, label };
	return _gck_call_sync (self, perform_init_token, NULL, &args, err);
}

void
gck_slot_init_token_async (GckSlot *self, const guchar *pin, gsize length,
                            const gchar *label, GCancellable *cancellable,
                            GAsyncReadyCallback callback, gpointer user_data)
{
	InitToken* args = _gck_call_async_prep (self, self, perform_init_token,
	                                         NULL, sizeof (*args));

	args->pin = pin;
	args->length = length;
	args->label = label;

	_gck_call_async_go (args, cancellable, callback, user_data);
}

gboolean
gck_slot_init_token_finish (GckSlot *self, GAsyncResult *result, GError **err)
{
	return _gck_call_basic_finish (self, result, err);
}

#endif /* UNIMPLEMENTED */

typedef struct OpenSession {
	GckArguments base;
	GckSlot *slot;
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
	GckModule *module;
	gboolean ret = TRUE;

	g_free (args->password);
	args->password = NULL;

	/* Ask the token for a password */
	module = gck_slot_get_module (args->slot);

	if (args->auto_login && result == CKR_PIN_INCORRECT) {

		ret = _gck_module_fire_authenticate_slot (module, args->slot, NULL, &args->password);

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
 * gck_slot_open_session:
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
GckSession*
gck_slot_open_session (GckSlot *self, guint options, GCancellable *cancellable, GError **err)
{
	return gck_slot_open_session_full (self, options, 0, NULL, NULL, cancellable, err);
}

/**
 * gck_slot_open_session_full:
 * @self: The slot to open a session on.
 * @options: The options to open the new session with.
 * @pkcs11_flags: Additional raw PKCS#11 flags.
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
GckSession*
gck_slot_open_session_full (GckSlot *self, guint options, gulong pkcs11_flags, gpointer app_data,
                            CK_NOTIFY notify, GCancellable *cancellable, GError **err)
{
	OpenSession args = { GCK_ARGUMENTS_INIT, 0,  };
	GckSession *session = NULL;
	GckModule *module = NULL;
	CK_SLOT_ID slot_id;

	g_object_ref (self);

	/* Try to use a cached session */
	module = gck_slot_get_module (self);
	slot_id = gck_slot_get_handle (self);

	/* Open a new session */
	args.slot = self;
	args.app_data = app_data;
	args.notify = notify;
	args.password = NULL;
	args.session = 0;

	args.auto_login = ((options & GCK_SESSION_LOGIN_USER) == GCK_SESSION_LOGIN_USER);

	args.flags = pkcs11_flags | CKF_SERIAL_SESSION;
	if ((options & GCK_SESSION_READ_WRITE) == GCK_SESSION_READ_WRITE)
		args.flags |= CKF_RW_SESSION;

	if (_gck_call_sync (self, perform_open_session, complete_open_session, &args, cancellable, err))
		session = make_session_object (self, options, args.session);

	g_object_unref (module);
	g_object_unref (self);

	return session;
}

void
gck_slot_open_session_async (GckSlot *self, guint options, GCancellable *cancellable,
                             GAsyncReadyCallback callback, gpointer user_data)
{
	gck_slot_open_session_full_async (self, options, 0UL, NULL, NULL, cancellable, callback, user_data);
}

/**
 * gck_slot_open_session_async:
 * @self: The slot to open a session on.
 * @options: Options to open the new session with.
 * @pkcs11_flags: Additional raw PKCS#11 flags.
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
gck_slot_open_session_full_async (GckSlot *self, guint options, gulong pkcs11_flags, gpointer app_data,
                                  CK_NOTIFY notify, GCancellable *cancellable,
                                  GAsyncReadyCallback callback, gpointer user_data)
{
	OpenSession *args;

	g_object_ref (self);

	args =  _gck_call_async_prep (self, self, perform_open_session, complete_open_session,
	                              sizeof (*args), free_open_session);

	args->app_data = app_data;
	args->notify = notify;
	args->slot = g_object_ref (self);

	args->auto_login = ((options & GCK_SESSION_LOGIN_USER) == GCK_SESSION_LOGIN_USER);

	args->flags = pkcs11_flags | CKF_SERIAL_SESSION;
	if ((options & GCK_SESSION_READ_WRITE) == GCK_SESSION_READ_WRITE)
		args->flags |= CKF_RW_SESSION;

	_gck_call_async_ready_go (args, cancellable, callback, user_data);
	g_object_unref (self);
}

/**
 * gck_slot_open_session_finish:
 * @self: The slot to open a session on.
 * @result: The result passed to the callback.
 * @err: A location to return an error or NULL.
 *
 * Get the result of an open session operation. If the 'auto reuse' setting is set,
 * then this may be a recycled session with the same flags.
 *
 * Return value: The new session or NULL if an error occurs.
 */
GckSession*
gck_slot_open_session_finish (GckSlot *self, GAsyncResult *result, GError **err)
{
	GckSession *session = NULL;

	g_object_ref (self);

	{
		OpenSession *args;

		if (_gck_call_basic_finish (result, err)) {
			args = _gck_call_arguments (result, OpenSession);
			session = make_session_object (self, args->flags, args->session);
		}
	}

	g_object_unref (self);

	return session;
}
