/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#include <glib.h>
#include <glib/gi18n.h>

#include "privacy.h"
#include "procmime.h"
#include "procmsg.h"

static GSList *systems = NULL;
static gchar *privacy_last_error = NULL;

void privacy_set_error(const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFSIZ];

	va_start(args, format);
	g_vsnprintf(buf, BUFSIZ, format, args);
	va_end(args);
	g_free(privacy_last_error);
	privacy_last_error = g_strdup(buf);
}

static gchar tmp_privacy_error[BUFSIZ];

void privacy_reset_error(void)
{
	g_free(privacy_last_error);
	privacy_last_error = NULL;
}

gboolean privacy_peek_error(void)
{
	return (privacy_last_error != NULL);
}

const gchar *privacy_get_error (void)
{
	if (privacy_last_error) {
		strncpy2(tmp_privacy_error, privacy_last_error, BUFSIZ-1);
		privacy_reset_error();
		return tmp_privacy_error;
	} else {
		return _("Unknown error");
	}
}

static PrivacySystem *privacy_data_get_system(PrivacyData *data)
{
	/* Make sure the cached system is still registered */
	if (data->system && g_slist_find(systems, data->system))
		return data->system;
	else
		return NULL;
}
/**
 * Register a new Privacy System
 *
 * \param system The Privacy System that should be registered
 */
void privacy_register_system(PrivacySystem *system)
{
	systems = g_slist_append(systems, system);
}

/**
 * Unregister a new Privacy System. The system must not be in
 * use anymore when it is unregistered.
 *
 * \param system The Privacy System that should be unregistered
 */
void privacy_unregister_system(PrivacySystem *system)
{
	systems = g_slist_remove(systems, system);
}

/**
 * Free a PrivacyData of a PrivacySystem
 *
 * \param privacydata The data to free
 */
void privacy_free_privacydata(PrivacyData *privacydata)
{
	PrivacySystem *system = NULL;
	
	cm_return_if_fail(privacydata != NULL);

	system = privacy_data_get_system(privacydata);
	if (!system)
		return;
	system->free_privacydata(privacydata);
}

/**
 * Check if a MimeInfo is signed with one of the available
 * privacy system. If a privacydata is set in the MimeInfo
 * it will directory return the return value by the system
 * set in the privacy data or check all available privacy
 * systems otherwise.
 *
 * \return True if the MimeInfo has a signature
 */
gboolean privacy_mimeinfo_is_signed(MimeInfo *mimeinfo)
{
	GSList *cur;
	cm_return_val_if_fail(mimeinfo != NULL, FALSE);

	if (mimeinfo->privacy != NULL) {
		PrivacySystem *system = 
			privacy_data_get_system(mimeinfo->privacy);

		if (system == NULL) {
			mimeinfo->privacy = NULL;
			goto try_others;
		}

		if (system->is_signed != NULL)
			return system->is_signed(mimeinfo);
		else
			return FALSE;
	}
try_others:
	for(cur = systems; cur != NULL; cur = g_slist_next(cur)) {
		PrivacySystem *system = (PrivacySystem *) cur->data;

		if(system->is_signed != NULL && system->is_signed(mimeinfo))
			return TRUE;
	}

	return FALSE;
}

struct SignedState {
	MsgInfo *msginfo;
	gchar **system;
};

static void msginfo_set_signed_flag(GNode *node, gpointer data)
{
	struct SignedState *sstate = (struct SignedState *)data;
	MsgInfo *msginfo = sstate->msginfo;
	MimeInfo *mimeinfo = node->data;
	
	if (privacy_mimeinfo_is_signed(mimeinfo)) {
		procmsg_msginfo_set_flags(msginfo, 0, MSG_SIGNED);
		if (sstate->system && !*(sstate->system) && mimeinfo->privacy)
			*(sstate->system) = g_strdup(mimeinfo->privacy->system->id);
	}
	if (privacy_mimeinfo_is_encrypted(mimeinfo)) {
		procmsg_msginfo_set_flags(msginfo, 0, MSG_ENCRYPTED);
		if (sstate->system && !*(sstate->system) && mimeinfo->privacy)
			*(sstate->system) = g_strdup(mimeinfo->privacy->system->id);
	} else {
		/* searching inside encrypted parts doesn't really make sense */
		g_node_children_foreach(mimeinfo->node, G_TRAVERSE_ALL, msginfo_set_signed_flag, sstate);
	}
}

void privacy_msginfo_get_signed_state(MsgInfo *msginfo, gchar **system)
{
	struct SignedState sstate;
	MimeInfo *mimeinfo = procmime_scan_message(msginfo);
	if (!mimeinfo)
		return;
	sstate.msginfo = msginfo;
	sstate.system = system;
	g_node_children_foreach(mimeinfo->node, G_TRAVERSE_ALL, msginfo_set_signed_flag, &sstate);
}

/**
 * Check the signature of a MimeInfo. privacy_mimeinfo_is_signed
 * should be called before otherwise it is done by this function.
 * If the MimeInfo is not signed an error code will be returned.
 *
 * \return Error code indicating the result of the check,
 *         < 0 if an error occured
 */
gint privacy_mimeinfo_check_signature(MimeInfo *mimeinfo)
{
	PrivacySystem *system;

	cm_return_val_if_fail(mimeinfo != NULL, -1);

	if (mimeinfo->privacy == NULL)
		privacy_mimeinfo_is_signed(mimeinfo);
	
	if (mimeinfo->privacy == NULL)
		return -1;
	
	system = privacy_data_get_system(mimeinfo->privacy);
	if (system == NULL)
		return -1;

	if (system->check_signature == NULL)
		return -1;
	
	return system->check_signature(mimeinfo);
}

SignatureStatus privacy_mimeinfo_get_sig_status(MimeInfo *mimeinfo)
{
	PrivacySystem *system;

	cm_return_val_if_fail(mimeinfo != NULL, -1);

	if (mimeinfo->privacy == NULL)
		privacy_mimeinfo_is_signed(mimeinfo);
	
	if (mimeinfo->privacy == NULL)
		return SIGNATURE_UNCHECKED;
	
	system = privacy_data_get_system(mimeinfo->privacy);
	if (system == NULL)
		return SIGNATURE_UNCHECKED;
	if (system->get_sig_status == NULL)
		return SIGNATURE_UNCHECKED;
	
	return system->get_sig_status(mimeinfo);
}

gchar *privacy_mimeinfo_sig_info_short(MimeInfo *mimeinfo)
{
	PrivacySystem *system;

	cm_return_val_if_fail(mimeinfo != NULL, NULL);

	if (mimeinfo->privacy == NULL)
		privacy_mimeinfo_is_signed(mimeinfo);
	
	if (mimeinfo->privacy == NULL)
		return g_strdup(_("No signature found"));
	
	system = privacy_data_get_system(mimeinfo->privacy);
	if (system == NULL)
		return g_strdup(_("No signature found"));
	if (system->get_sig_info_short == NULL)
		return g_strdup(_("No information available"));
	
	return system->get_sig_info_short(mimeinfo);
}

gchar *privacy_mimeinfo_sig_info_full(MimeInfo *mimeinfo)
{
	PrivacySystem *system;

	cm_return_val_if_fail(mimeinfo != NULL, NULL);

	if (mimeinfo->privacy == NULL)
		privacy_mimeinfo_is_signed(mimeinfo);
	
	if (mimeinfo->privacy == NULL)
		return g_strdup(_("No signature found"));
	
	system = privacy_data_get_system(mimeinfo->privacy);
	if (system == NULL)
		return g_strdup(_("No signature found"));
	if (system->get_sig_info_full == NULL)
		return g_strdup(_("No information available"));
	
	return system->get_sig_info_full(mimeinfo);
}

gboolean privacy_mimeinfo_is_encrypted(MimeInfo *mimeinfo)
{
	GSList *cur;
	cm_return_val_if_fail(mimeinfo != NULL, FALSE);

	for(cur = systems; cur != NULL; cur = g_slist_next(cur)) {
		PrivacySystem *system = (PrivacySystem *) cur->data;

		if(system->is_encrypted != NULL && system->is_encrypted(mimeinfo))
			return TRUE;
	}

	return FALSE;
}

static gint decrypt(MimeInfo *mimeinfo, PrivacySystem *system)
{
	MimeInfo *decryptedinfo, *parentinfo;
	gint childnumber;
	
	cm_return_val_if_fail(system->decrypt != NULL, -1);
	
	decryptedinfo = system->decrypt(mimeinfo);
	if (decryptedinfo == NULL)
		return -1;

	parentinfo = procmime_mimeinfo_parent(mimeinfo);
	childnumber = g_node_child_index(parentinfo->node, mimeinfo);
	
	procmime_mimeinfo_free_all(mimeinfo);

	g_node_insert(parentinfo->node, childnumber, decryptedinfo->node);

	return 0;
}

gint privacy_mimeinfo_decrypt(MimeInfo *mimeinfo)
{
	GSList *cur;
	cm_return_val_if_fail(mimeinfo != NULL, FALSE);

	procmime_decode_content(mimeinfo);

	for(cur = systems; cur != NULL; cur = g_slist_next(cur)) {
		PrivacySystem *system = (PrivacySystem *) cur->data;

		if(system->is_encrypted != NULL && system->is_encrypted(mimeinfo))
			return decrypt(mimeinfo, system);
	}

	return -1;
}

GSList *privacy_get_system_ids()
{
	GSList *cur;
	GSList *ret = NULL;

	for(cur = systems; cur != NULL; cur = g_slist_next(cur)) {
		PrivacySystem *system = (PrivacySystem *) cur->data;

		ret = g_slist_append(ret, g_strdup(system->id));
	}

	return ret;
}

static PrivacySystem *privacy_get_system(const gchar *id)
{
	GSList *cur;

	cm_return_val_if_fail(id != NULL, NULL);

	for(cur = systems; cur != NULL; cur = g_slist_next(cur)) {
		PrivacySystem *system = (PrivacySystem *) cur->data;

		if(strcmp(id, system->id) == 0)
			return system;
	}

	return NULL;
}

const gchar *privacy_system_get_name(const gchar *id)
{
	PrivacySystem *system;

	cm_return_val_if_fail(id != NULL, NULL);

	system = privacy_get_system(id);
	if (system == NULL)
		return NULL;

	return system->name;
}

gboolean privacy_system_can_sign(const gchar *id)
{
	PrivacySystem *system;

	cm_return_val_if_fail(id != NULL, FALSE);

	system = privacy_get_system(id);
	if (system == NULL)
		return FALSE;

	return system->can_sign;
}

gboolean privacy_system_can_encrypt(const gchar *id)
{
	PrivacySystem *system;

	cm_return_val_if_fail(id != NULL, FALSE);

	system = privacy_get_system(id);
	if (system == NULL)
		return FALSE;

	return system->can_encrypt;
}

gboolean privacy_sign(const gchar *id, MimeInfo *target, PrefsAccount *account, const gchar *from_addr)
{
	PrivacySystem *system;

	cm_return_val_if_fail(id != NULL, FALSE);
	cm_return_val_if_fail(target != NULL, FALSE);

	system = privacy_get_system(id);
	if (system == NULL)
		return FALSE;
	if (!system->can_sign)
		return FALSE;
	if (system->sign == NULL)
		return FALSE;

	return system->sign(target, account, from_addr);
}

gchar *privacy_get_encrypt_data(const gchar *id, GSList *recp_names)
{
	PrivacySystem *system;
	gchar *ret = NULL;
	GSList *uniq_names = NULL, *cur;

	cm_return_val_if_fail(id != NULL, NULL);
	cm_return_val_if_fail(recp_names != NULL, NULL);

	system = privacy_get_system(id);
	if (system == NULL)
		return NULL;
	if (!system->can_encrypt)
		return NULL;
	if (system->get_encrypt_data == NULL)
		return NULL;

	for (cur = recp_names; cur; cur = cur->next) {
		if (!g_slist_find_custom(uniq_names, cur->data, (GCompareFunc)strcmp)) {
			uniq_names = g_slist_prepend(uniq_names, cur->data);
		}
	}
	ret = system->get_encrypt_data(uniq_names);
	
	g_slist_free(uniq_names);
	return ret;
}

const gchar *privacy_get_encrypt_warning(const gchar *id)
{
	PrivacySystem *system;

	cm_return_val_if_fail(id != NULL, NULL);

	system = privacy_get_system(id);
	if (system == NULL)
		return NULL;
	if (!system->can_encrypt)
		return NULL;
	if (system->get_encrypt_warning == NULL)
		return NULL;

	return system->get_encrypt_warning();
}

void privacy_inhibit_encrypt_warning(const gchar *id, gboolean inhibit)
{
	PrivacySystem *system;

	cm_return_if_fail(id != NULL);

	system = privacy_get_system(id);
	if (system == NULL)
		return;
	if (!system->can_encrypt)
		return;
	if (system->inhibit_encrypt_warning == NULL)
		return;

	system->inhibit_encrypt_warning(inhibit);
}

gboolean privacy_encrypt(const gchar *id, MimeInfo *mimeinfo, const gchar *encdata)
{
	PrivacySystem *system;

	cm_return_val_if_fail(id != NULL, FALSE);
	cm_return_val_if_fail(mimeinfo != NULL, FALSE);
	if (encdata == NULL) {
		privacy_set_error(_("No recipient keys defined."));
		return FALSE;
	}

	system = privacy_get_system(id);
	if (system == NULL)
		return FALSE;
	if (!system->can_encrypt)
		return FALSE;
	if (system->encrypt == NULL)
		return FALSE;

	return system->encrypt(mimeinfo, encdata);
}
