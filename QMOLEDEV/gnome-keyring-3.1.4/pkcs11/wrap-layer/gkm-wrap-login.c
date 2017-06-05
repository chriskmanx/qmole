/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gkm-wrap-layer.h"
#include "gkm-wrap-login.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-util.h"

#include "egg/egg-secure-memory.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11i.h"

#include <glib/gi18n.h>

#include <string.h>

/* Holds failed unlock password, accessed atomically */
static gpointer unlock_failure = NULL;

void
gkm_wrap_layer_mark_login_unlock_success (void)
{
	gpointer oldval = g_atomic_pointer_get (&unlock_failure);
	if (g_atomic_pointer_compare_and_exchange (&unlock_failure, oldval, NULL))
		egg_secure_strfree (oldval);
}

void
gkm_wrap_layer_mark_login_unlock_failure (const gchar *failed_password)
{
	gpointer oldval;
	gpointer newval;

	g_return_if_fail (failed_password);

	oldval = g_atomic_pointer_get (&unlock_failure);
	newval = egg_secure_strdup (failed_password);

	if (g_atomic_pointer_compare_and_exchange (&unlock_failure, oldval, newval))
		egg_secure_strfree (oldval);
	else
		egg_secure_strfree (newval);
}

gboolean
gkm_wrap_login_did_unlock_fail (void)
{
	return g_atomic_pointer_get (&unlock_failure) ? TRUE : FALSE;
}

gchar*
gkm_wrap_login_steal_failed_password (void)
{
	gpointer oldval;

	oldval = g_atomic_pointer_get (&unlock_failure);
	if (!g_atomic_pointer_compare_and_exchange (&unlock_failure, oldval, NULL))
		oldval = NULL;

	return oldval;
}

static gboolean
prepare_template_for_storage (CK_FUNCTION_LIST_PTR module,
                              CK_SESSION_HANDLE session,
                              CK_OBJECT_HANDLE collection,
                              GArray *template)
{
	CK_ATTRIBUTE attr;
	CK_RV rv;

	g_assert (module);
	g_assert (session);
	g_assert (template);

	/* Lookup the ID attribute */
	attr.type = CKA_ID;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	rv = (module->C_GetAttributeValue) (session, collection, &attr, 1);
	if (rv != CKR_OK || attr.ulValueLen == (CK_ULONG)-1)
		return FALSE;

	attr.pValue = g_malloc0 (attr.ulValueLen);

	rv = (module->C_GetAttributeValue) (session, collection, &attr, 1);
	g_return_val_if_fail (rv == CKR_OK, FALSE);

	/* Use the ID as the collection attribute */
	attr.type = CKA_G_COLLECTION;
	gkm_template_set (template, &attr);
	g_free (attr.pValue);

	gkm_template_set_ulong (template, CKA_CLASS, CKO_SECRET_KEY);
	gkm_template_set_boolean (template, CKA_TOKEN, TRUE);
	return TRUE;
}

static gboolean
prepare_module_session_and_collection (CK_FUNCTION_LIST_PTR_PTR module,
                                       CK_SESSION_HANDLE_PTR session,
                                       CK_OBJECT_HANDLE_PTR object)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	CK_BBOOL trueval = CK_TRUE;
	CK_BBOOL falseval = CK_FALSE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_G_LOGIN_COLLECTION, &trueval, sizeof (trueval) },
		{ CKA_G_LOCKED, &falseval, sizeof (falseval) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &trueval, sizeof (trueval) },
		{ CKA_TRUSTED, &trueval, sizeof (trueval) },
	};

	CK_SESSION_INFO sinfo;
	CK_SLOT_ID_PTR slots = NULL;
	CK_FUNCTION_LIST_PTR funcs;
	CK_ULONG n_slots = 0, i;
	gboolean ret = FALSE;
	CK_ULONG n_objects = 0;
	CK_RV rv;

	g_assert (module);
	g_assert (session);

	funcs = gkm_wrap_layer_get_functions_no_prompts ();
	g_return_val_if_fail (funcs, FALSE);

	rv = (funcs->C_GetSlotList) (CK_TRUE, NULL, &n_slots);
	g_return_val_if_fail (rv == CKR_OK, FALSE);
	slots = g_new0 (CK_SLOT_ID, n_slots);
	rv = (funcs->C_GetSlotList) (CK_TRUE, slots, &n_slots);
	g_return_val_if_fail (rv == CKR_OK, FALSE);

	for (i = 0; !ret && i < n_slots; ++i) {
		/* Open a session with this module */
		rv = (funcs->C_OpenSession) (slots[i], CKF_RW_SESSION | CKF_SERIAL_SESSION, NULL, NULL, session);
		if (rv != CKR_OK)
			continue;

		rv = (funcs->C_GetSessionInfo) (*session, &sinfo);
		if (rv != CKR_OK)
			continue;

		/* Log into the session with no password, in case its needed */
		if (sinfo.state == CKS_RO_PUBLIC_SESSION || sinfo.state == CKS_RW_PUBLIC_SESSION)
			(funcs->C_Login) (*session, CKU_USER, (guchar*)"", 0);

		rv = (funcs->C_FindObjectsInit) (*session, attrs, G_N_ELEMENTS (attrs));
		if (rv == CKR_OK) {
			rv = (funcs->C_FindObjects) (*session, object, 1, &n_objects);
			(funcs->C_FindObjectsFinal) (*session);
			if (rv == CKR_OK && n_objects == 1)
				ret = TRUE;
		}

		if (!ret)
			(funcs->C_CloseSession) (*session);
	}

	g_free (slots);

	*module = funcs;
	return ret;
}

static void
string_fields_to_template_va (va_list args, const gchar *name,
                              GArray *template)
{
	GString *fields = g_string_sized_new (128);
	const gchar *last = NULL;
	gint cmp;

	g_assert (name);
	g_assert (template);

	while (name != NULL) {
		g_string_append (fields, name);
		g_string_append_c (fields, '\0');
		g_string_append (fields, va_arg (args, const gchar*));
		g_string_append_c (fields, '\0');

		/* Names must be in alphabetical order */
		if (name && last) {
			cmp = strcmp (last, name);
			if (cmp == 0)
				g_warning ("duplicate names in attributes not allowed: %s %s", last, name);
			else if (cmp > 0)
				g_warning ("names in attributes must in alphabetical order: %s %s", last, name);
		}

		last = name;
		name = va_arg (args, const gchar*);
	}

	gkm_template_set_value (template, CKA_G_FIELDS, fields->str, fields->len);
	g_string_free (fields, TRUE);
}

static CK_OBJECT_HANDLE
find_login_keyring_item (CK_FUNCTION_LIST_PTR module, CK_SESSION_HANDLE session,
                         GArray *template)
{
	CK_OBJECT_HANDLE item = 0;
	CK_ATTRIBUTE_PTR attr;
	CK_ATTRIBUTE matched;
	CK_OBJECT_HANDLE search;
	GArray *attrs;
	CK_RV rv;

	g_assert (module);
	g_assert (template);

	/* Template for search object */
	attrs = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (attrs, CKA_CLASS, CKO_G_SEARCH);
	gkm_template_set_boolean (attrs, CKA_TOKEN, CK_FALSE);

	attr = gkm_template_find (template, CKA_G_COLLECTION);
	if (attr != NULL)
		gkm_template_set (attrs, attr);

	attr = gkm_template_find (template, CKA_G_FIELDS);
	g_return_val_if_fail (attr, 0);
	gkm_template_set (attrs, attr);

	/* Create new search object */
	rv = (module->C_CreateObject) (session, (CK_ATTRIBUTE_PTR)attrs->data, attrs->len, &search);
	gkm_template_free (attrs);

	if (rv != CKR_OK) {
		g_warning ("couldn't create search for login keyring: %s", gkm_util_rv_stringize (rv));
		return 0;
	}

	matched.type = CKA_G_MATCHED;
	matched.pValue = NULL;
	matched.ulValueLen = 0;

	rv = (module->C_GetAttributeValue) (session, search, &matched, 1);
	g_return_val_if_fail (rv == CKR_OK, 0);
	g_return_val_if_fail (matched.ulValueLen != (CK_ULONG)-1, 0);

	if (matched.ulValueLen >= sizeof (CK_OBJECT_HANDLE)) {
		matched.pValue = g_malloc (matched.ulValueLen);
		rv = (module->C_GetAttributeValue) (session, search, &matched, 1);
		g_return_val_if_fail (rv == CKR_OK, 0);

		item = *((CK_OBJECT_HANDLE_PTR)matched.pValue);
		g_return_val_if_fail (item != 0, 0);
		g_free (matched.pValue);
	}

	/* Destroy the search object */
	(module->C_DestroyObject) (session, search);

	return item;
}

void
gkm_wrap_login_attach_secret (const gchar *label, const gchar *secret,
                              const gchar *first, ...)
{
	CK_FUNCTION_LIST_PTR module;
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE collection;
	CK_OBJECT_HANDLE item;
	CK_ATTRIBUTE attr;
	gchar *display_name;
	GArray *template;
	gsize original_len;
	va_list va;
	CK_RV rv;

	if (first == NULL)
		return;

	if (secret == NULL)
		secret = "";

	/* We only support storing utf-8 strings */
	g_return_if_fail (g_utf8_validate (secret, -1, NULL));

	if (!prepare_module_session_and_collection (&module, &session, &collection))
		return;

	template = gkm_template_new (NULL, 0);
	if (!prepare_template_for_storage (module, session, collection, template)) {
		gkm_template_free (template);
		return;
	}

	va_start(va, first);
	string_fields_to_template_va (va, first, template);
	va_end(va);

	/*
	 * If there already is such an item, then include its identifier.
	 * What this does is overwrite that item, rather than creating new.
	 */
	item = find_login_keyring_item (module, session, template);
	if (item != 0) {

		attr.type = CKA_ID;
		attr.ulValueLen = 0;
		attr.pValue = NULL;

		rv = (module->C_GetAttributeValue) (session, item, &attr, 1);
		if (rv == CKR_OK && attr.ulValueLen != (CK_ULONG)-1) {
			attr.pValue = g_malloc (attr.ulValueLen);
			rv = (module->C_GetAttributeValue) (session, item, &attr, 1);
			if (rv == CKR_OK)
				gkm_template_set (template, &attr);
			g_free (attr.pValue);
		}
	}

	/* Get the label ready */
	display_name = g_strdup_printf (_("Unlock password for: %s"), label ? label : _("Unnamed"));
	gkm_template_set_string (template, CKA_LABEL, display_name);
	g_free (display_name);

	/* Instead of duplicating the password, we tack it into the template */
	original_len = template->len;
	attr.type = CKA_VALUE;
	attr.pValue = (CK_VOID_PTR)secret;
	attr.ulValueLen = strlen (secret);
	g_array_append_val (template, attr);

	/* Actually make the object */
	rv = (module->C_CreateObject) (session, ((CK_ATTRIBUTE_PTR)template->data),
	                               template->len, &item);
	if (rv != CKR_OK)
		g_warning ("couldn't store secret in login keyring: %s", gkm_util_rv_stringize (rv));

	/* Before freeing, truncate our password attribute we tacked on the end */
	g_array_set_size (template, original_len);
	gkm_template_free (template);

	(module->C_CloseSession) (session);
}

gchar*
gkm_wrap_login_lookup_secret (const gchar *first, ...)
{
	CK_FUNCTION_LIST_PTR module;
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE collection;
	CK_OBJECT_HANDLE item;
	CK_ATTRIBUTE attr;
	GArray *template;
	gchar *password = NULL;
	va_list va;
	CK_RV rv;

	if (first == NULL)
		return NULL;

	if (!prepare_module_session_and_collection (&module, &session, &collection))
		return NULL;

	template = gkm_template_new (NULL, 0);
	gkm_template_set_ulong (template, CKA_CLASS, CKO_SECRET_KEY);
	gkm_template_set_boolean (template, CKA_G_LOCKED, FALSE);

	va_start(va, first);
	string_fields_to_template_va (va, first, template);
	va_end(va);

	item = find_login_keyring_item (module, session, template);
	gkm_template_free (template);

	if (item != 0) {

		attr.type = CKA_VALUE;
		attr.pValue = NULL;
		attr.ulValueLen = 0;

		rv = (module->C_GetAttributeValue) (session, item, &attr, 1);
		if (rv == CKR_OK && attr.ulValueLen != (CK_ULONG)-1) {

			/* Allocate memory for password. Note we're null terminating */
			password = attr.pValue = egg_secure_alloc (attr.ulValueLen + 1);
			rv = (module->C_GetAttributeValue) (session, item, &attr, 1);

			/* Double check that this is a password */
			if (rv == CKR_OK) {
				if (!g_utf8_validate (password, -1, NULL)) {
					g_message ("expected string, but found binary secret in login keyring");
					egg_secure_strfree (password);
					password = NULL;
				}

			/* Couldn't read the value. Remember object can go away due to race */
			} else {
				if (rv != CKR_OBJECT_HANDLE_INVALID)
					g_warning ("couldn't read stored secret from login keyring: %s",
					           gkm_util_rv_stringize (rv));
				egg_secure_free (password);
				password = NULL;
			}

		/* Failure. Remember object can go away due to race */
		} else if (rv != CKR_OK && rv != CKR_OBJECT_HANDLE_INVALID) {
			g_warning ("couldn't get stored secret from login keyring: %s",
			           gkm_util_rv_stringize (rv));
		}
	}

	(module->C_CloseSession) (session);

	return password;
}

void
gkm_wrap_login_remove_secret (const gchar *first, ...)
{
	CK_FUNCTION_LIST_PTR module;
	CK_SESSION_HANDLE session;
	CK_OBJECT_HANDLE collection;
	CK_OBJECT_HANDLE item;
	GArray *template;
	va_list va;
	CK_RV rv;

	if (first == NULL)
		return;

	if (!prepare_module_session_and_collection (&module, &session, &collection))
		return;

	template = gkm_template_new (NULL, 0);
	if (!prepare_template_for_storage (module, session, collection, template)) {
		gkm_template_free (template);
		return;
	}

	va_start(va, first);
	string_fields_to_template_va (va, first, template);
	va_end(va);

	item = find_login_keyring_item (module, session, template);
	gkm_template_free (template);

	if (item != 0) {
		rv = (module->C_DestroyObject) (session, item);
		if (rv != CKR_OK && rv != CKR_OBJECT_HANDLE_INVALID)
			g_warning ("couldn't remove stored secret from login keyring: %s",
			           gkm_util_rv_stringize (rv));
	}

	(module->C_CloseSession) (session);
}

gboolean
gkm_wrap_login_is_usable (void)
{
	CK_FUNCTION_LIST_PTR module;
	CK_OBJECT_HANDLE collection;
	CK_SESSION_HANDLE session;

	if (!prepare_module_session_and_collection (&module, &session, &collection))
		return FALSE;

	(module->C_CloseSession) (session);
	return TRUE;
}
