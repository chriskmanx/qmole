/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2004-2012 the Claws Mail team
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#include "claws-features.h"
#endif

#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gi18n.h>

#include "defs.h"
#include "gtk/gtkutils.h"
#include "utils.h" 
#include "prefs.h"
#include "prefs_gtk.h"
#include "prefs_gpg.h"
#include "sgpgme.h"

struct GPGConfig prefs_gpg;

static PrefParam param[] = {
	/* Privacy */
	{"auto_check_signatures", "FALSE",
	 &prefs_gpg.auto_check_signatures, P_BOOL,
	 NULL, NULL, NULL},
	{"use_gpg_agent", "TRUE", &prefs_gpg.use_gpg_agent, P_BOOL,
	 NULL, NULL, NULL},
	{"store_passphrase", "FALSE", &prefs_gpg.store_passphrase, P_BOOL,
	 NULL, NULL, NULL},
	{"store_passphrase_timeout", "0",
	 &prefs_gpg.store_passphrase_timeout, P_INT,
	 NULL, NULL, NULL},
	{"passphrase_grab", "FALSE", &prefs_gpg.passphrase_grab, P_BOOL,
	 NULL, NULL, NULL},
	{"gpg_warning", "TRUE", &prefs_gpg.gpg_warning, P_BOOL,
	 NULL, NULL, NULL},
	{"gpg_ask_create_key", "TRUE", &prefs_gpg.gpg_ask_create_key, P_BOOL,
	 NULL, NULL, NULL},
	{"skip_encryption_warning", "", &prefs_gpg.skip_encryption_warning, P_STRING,
	 NULL, NULL, NULL},

	{NULL, NULL, NULL, P_OTHER, NULL, NULL, NULL}
};

static gchar *saved_gpg_agent_info = NULL;

struct GPGPage
{
	PrefsPage page;

	GtkWidget *checkbtn_auto_check_signatures;
	GtkWidget *checkbtn_use_gpg_agent;
        GtkWidget *checkbtn_store_passphrase;  
        GtkWidget *spinbtn_store_passphrase;  
        GtkWidget *checkbtn_passphrase_grab;  
        GtkWidget *checkbtn_gpg_warning;
};

static void prefs_gpg_create_widget_func(PrefsPage *_page,
					 GtkWindow *window,
					 gpointer data)
{
	struct GPGPage *page = (struct GPGPage *) _page;
	struct GPGConfig *config;

	GtkWidget *checkbtn_use_gpg_agent;
	GtkWidget *checkbtn_passphrase_grab;
	GtkWidget *checkbtn_store_passphrase;
	GtkWidget *checkbtn_auto_check_signatures;
	GtkWidget *checkbtn_gpg_warning;
	GtkWidget *hbox1;
	GtkWidget *vbox1, *vbox2;
	GtkWidget *label_expire1;
	GtkAdjustment *spinbtn_store_passphrase_adj;
	GtkWidget *spinbtn_store_passphrase;
	GtkWidget *label_expire2;
	GtkWidget *frame_passphrase;

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (vbox1), vbox2, FALSE, FALSE, 0);

	PACK_CHECK_BUTTON (vbox2, checkbtn_auto_check_signatures,
			_("Automatically check signatures"));

	vbox2 = gtkut_get_options_frame(vbox1, &frame_passphrase, _("Passphrase"));

	PACK_CHECK_BUTTON (vbox2, checkbtn_use_gpg_agent,
			_("Use gpg-agent to manage passwords"));
	if (saved_gpg_agent_info == NULL)
		gtk_widget_set_sensitive(checkbtn_use_gpg_agent, FALSE);

	PACK_CHECK_BUTTON (vbox2, checkbtn_store_passphrase,
			_("Store passphrase in memory"));

	SET_TOGGLE_SENSITIVITY_REVERSE(checkbtn_use_gpg_agent, checkbtn_store_passphrase);

	hbox1 = gtk_hbox_new (FALSE, 8);
	gtk_widget_show (hbox1);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox1, FALSE, FALSE, 0);

	SET_TOGGLE_SENSITIVITY_REVERSE(checkbtn_use_gpg_agent, hbox1);

	label_expire1 = gtk_label_new(_("Expire after"));
	gtk_widget_show (label_expire1);
	gtk_box_pack_start (GTK_BOX (hbox1), label_expire1, FALSE, FALSE, 0);

	spinbtn_store_passphrase_adj =
	    GTK_ADJUSTMENT(gtk_adjustment_new(1, 0, 1440, 1, 10, 0));
	spinbtn_store_passphrase =
	    gtk_spin_button_new(GTK_ADJUSTMENT
				(spinbtn_store_passphrase_adj), 1, 0);
	gtk_widget_show(spinbtn_store_passphrase);
	gtk_box_pack_start(GTK_BOX(hbox1), spinbtn_store_passphrase, FALSE,
			   FALSE, 0);
	gtk_widget_set_size_request(spinbtn_store_passphrase, 64, -1);
	CLAWS_SET_TIP(spinbtn_store_passphrase,
			     _
			     ("Setting to '0' will store the passphrase for the whole session"));
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON
				    (spinbtn_store_passphrase), TRUE);

	label_expire2 = gtk_label_new(_("minute(s)"));
	gtk_widget_show(label_expire2);
	gtk_box_pack_start(GTK_BOX(hbox1), label_expire2, FALSE, FALSE, 0);
	gtk_misc_set_alignment(GTK_MISC(label_expire2), 0.0, 0.5);

	SET_TOGGLE_SENSITIVITY (checkbtn_store_passphrase, label_expire1);
	SET_TOGGLE_SENSITIVITY (checkbtn_store_passphrase, spinbtn_store_passphrase);
	SET_TOGGLE_SENSITIVITY (checkbtn_store_passphrase, label_expire2);

	PACK_CHECK_BUTTON (vbox2, checkbtn_passphrase_grab,
			_("Grab input while entering a passphrase"));

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (vbox1), vbox2, FALSE, FALSE, 0);

	PACK_CHECK_BUTTON (vbox2, checkbtn_gpg_warning,
			_("Display warning on start-up if GnuPG doesn't work"));

	config = prefs_gpg_get_config();

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_auto_check_signatures), config->auto_check_signatures);
	if (!getenv("GPG_AGENT_INFO"))
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_use_gpg_agent), FALSE);
	else
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_use_gpg_agent), config->use_gpg_agent);
	if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbtn_use_gpg_agent)))
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_store_passphrase), config->store_passphrase);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spinbtn_store_passphrase), (float) config->store_passphrase_timeout);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_passphrase_grab), config->passphrase_grab);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_gpg_warning), config->gpg_warning);

	page->checkbtn_auto_check_signatures = checkbtn_auto_check_signatures;
	page->checkbtn_store_passphrase = checkbtn_store_passphrase;
	page->spinbtn_store_passphrase = spinbtn_store_passphrase;
	page->checkbtn_passphrase_grab = checkbtn_passphrase_grab;
	page->checkbtn_gpg_warning = checkbtn_gpg_warning;
	page->checkbtn_use_gpg_agent = checkbtn_use_gpg_agent;
	page->page.widget = vbox1;
}

static void prefs_gpg_destroy_widget_func(PrefsPage *_page)
{
}

static void prefs_gpg_save_func(PrefsPage *_page)
{
	struct GPGPage *page = (struct GPGPage *) _page;
	GPGConfig *config = prefs_gpg_get_config();

	config->auto_check_signatures =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_auto_check_signatures));
	config->use_gpg_agent = 
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_use_gpg_agent));
	config->store_passphrase = 
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_store_passphrase));
	config->store_passphrase_timeout = 
		gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(page->spinbtn_store_passphrase));
	config->passphrase_grab = 
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_passphrase_grab));
	config->gpg_warning = 
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->checkbtn_gpg_warning));

	prefs_gpg_enable_agent(config->use_gpg_agent);

	prefs_gpg_save_config();
}

struct GPGAccountPage
{
	PrefsPage page;

	GtkWidget *key_default;
	GtkWidget *key_by_from;
	GtkWidget *key_custom;
	GtkWidget *keyid;
	GtkWidget *keyid_label;
	GtkWidget *new_key_label;
	GtkWidget *new_key_btn;
	GtkWidget *new_key_box;

	PrefsAccount *account;
};

void key_custom_toggled(GtkToggleButton *togglebutton, gpointer user_data)
{
	struct GPGAccountPage *page = (struct GPGAccountPage *) user_data;
	gboolean active;

	active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->key_custom));
	gtk_widget_set_sensitive(GTK_WIDGET(page->keyid_label), active);
	gtk_widget_set_sensitive(GTK_WIDGET(page->keyid), active);
	if (!active)
		gtk_editable_delete_text(GTK_EDITABLE(page->keyid), 0, -1);
}

static void prefs_gpg_update_sens(struct GPGAccountPage *page)
{
	gboolean active;
	active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->key_custom));
	if (sgpgme_has_secret_key()) {
		gtk_widget_hide(page->new_key_box);
		gtk_widget_set_sensitive(page->key_default, TRUE);
		gtk_widget_set_sensitive(page->key_by_from, TRUE);
		gtk_widget_set_sensitive(page->key_custom, TRUE);
		gtk_widget_set_sensitive(page->keyid, active);
		gtk_widget_set_sensitive(page->keyid_label, active);
	} else {
		gtk_widget_show(page->new_key_box);
		gtk_widget_set_sensitive(page->key_default, FALSE);
		gtk_widget_set_sensitive(page->key_by_from, FALSE);
		gtk_widget_set_sensitive(page->key_custom, FALSE);
		gtk_widget_set_sensitive(page->keyid, FALSE);
		gtk_widget_set_sensitive(page->keyid_label, FALSE);
	}
}

static void new_key_clicked(GtkWidget *widget, gpointer user_data)
{
	struct GPGAccountPage *page = (struct GPGAccountPage *) user_data;
	sgpgme_create_secret_key(page->account, FALSE);
	prefs_gpg_update_sens(page);
}

static void prefs_gpg_account_create_widget_func(PrefsPage *_page,
						 GtkWindow *window,
						 gpointer data)
{
	struct GPGAccountPage *page = (struct GPGAccountPage *) _page;
	PrefsAccount *account = (PrefsAccount *) data;
	GPGAccountConfig *config;

	GtkWidget *vbox;
	GtkWidget *frame1;
	GtkWidget *vbox2;
	GtkWidget *hbox;
	GSList *key_group = NULL;
	GtkWidget *key_default;
	GtkWidget *key_by_from;
	GtkWidget *key_custom;
	GtkWidget *keyid_label;
	GtkWidget *keyid;
	GtkWidget *image;
	GtkWidget *new_key_label;
	GtkWidget *new_key_btn;
	GtkWidget *new_key_box;

	vbox = gtk_vbox_new(FALSE, VSPACING);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), VBOX_BORDER);
	gtk_widget_show(vbox);

	vbox2 = gtkut_get_options_frame(vbox, &frame1, _("Sign key"));

	hbox = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox), 0);

	key_default = gtk_radio_button_new_with_label(key_group,
			_("Use default GnuPG key"));
	key_group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(key_default));
	gtk_widget_show(key_default);
	gtk_box_pack_start(GTK_BOX(hbox), key_default, FALSE, FALSE, 0);

	hbox = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox), 0);

	key_by_from = gtk_radio_button_new_with_label(key_group,
		_("Select key by your email address"));
	key_group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(key_by_from));
	gtk_widget_show(key_by_from);
	gtk_box_pack_start(GTK_BOX(hbox), key_by_from, FALSE, FALSE, 0);

	hbox = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox), 0);

	key_custom = gtk_radio_button_new_with_label(key_group,
		_("Specify key manually"));
	key_group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(key_custom));
	gtk_widget_show(key_custom);
	gtk_box_pack_start(GTK_BOX(hbox), key_custom, FALSE, FALSE, 0);

	hbox = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (hbox), 0);

	keyid_label = gtk_label_new(_("User or key ID:"));
	gtk_widget_show(keyid_label);
	gtk_label_set_justify(GTK_LABEL(keyid_label), GTK_JUSTIFY_LEFT);
	gtk_box_pack_start(GTK_BOX(hbox), keyid_label, FALSE, FALSE, 0);

	keyid = gtk_entry_new();
	gtk_widget_show(keyid);
	gtk_box_pack_start(GTK_BOX(hbox), keyid, FALSE, FALSE, 0);

	config = prefs_gpg_account_get_config(account);
	switch (config->sign_key) {
	case SIGN_KEY_DEFAULT:
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(key_default), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(keyid_label), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(keyid), FALSE);
		break;
	case SIGN_KEY_BY_FROM:
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(key_by_from), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(keyid_label), FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(keyid), FALSE);
		break;
	case SIGN_KEY_CUSTOM:
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(key_custom), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(keyid_label), TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(keyid), TRUE);
		break;
	}

	hbox = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

	new_key_box = gtk_hbox_new(FALSE, 6);
	gtk_widget_show(new_key_box);
	gtk_box_pack_start(GTK_BOX(hbox), new_key_box, FALSE, FALSE, 0);

	image = gtk_image_new_from_stock(GTK_STOCK_DIALOG_WARNING,
			GTK_ICON_SIZE_SMALL_TOOLBAR);

	gtk_box_pack_start(GTK_BOX(new_key_box), image, FALSE, FALSE, 0);
	new_key_label = gtk_label_new(
			_("No secret key found."));
	gtk_box_pack_start(GTK_BOX(new_key_box), new_key_label, FALSE, FALSE, 0);

	new_key_btn = gtk_button_new_with_label(_("Generate a new key pair"));
	gtk_widget_show(new_key_btn);
	gtk_box_pack_start(GTK_BOX(hbox), new_key_btn, FALSE, FALSE, 0);

	if (config->sign_key_id != NULL)
		gtk_entry_set_text(GTK_ENTRY(keyid), config->sign_key_id);

	g_signal_connect(G_OBJECT(key_custom), "toggled", G_CALLBACK(key_custom_toggled), page);
	g_signal_connect(G_OBJECT(new_key_btn), "clicked", G_CALLBACK(new_key_clicked), page);

	page->key_default = key_default;
	page->key_by_from = key_by_from;
	page->key_custom = key_custom;
	page->keyid = keyid;
	page->keyid_label = keyid_label;
	page->new_key_box = new_key_box;

	page->page.widget = vbox;
	page->account = account;
	prefs_gpg_update_sens(page);
}

static void prefs_gpg_account_destroy_widget_func(PrefsPage *_page)
{
	/* nothing to do here */
}

static void prefs_gpg_account_save_func(PrefsPage *_page)
{
	struct GPGAccountPage *page = (struct GPGAccountPage *) _page;
	GPGAccountConfig *config;

	config = prefs_gpg_account_get_config(page->account);

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->key_default)))
		config->sign_key = SIGN_KEY_DEFAULT;
	else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->key_by_from)))
		config->sign_key = SIGN_KEY_BY_FROM;
	else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(page->key_custom))) {
		config->sign_key = SIGN_KEY_CUSTOM;
		g_free(config->sign_key_id);
		config->sign_key_id = gtk_editable_get_chars(GTK_EDITABLE(page->keyid), 0, -1);
	}

	prefs_gpg_account_set_config(page->account, config);
	prefs_gpg_account_free_config(config);
}

GPGConfig *prefs_gpg_get_config(void)
{
	return &prefs_gpg;
}

void prefs_gpg_save_config(void)
{
	PrefFile *pfile;
	gchar *rcpath;

	debug_print("Saving GPG config\n");

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, COMMON_RC, NULL);
	pfile = prefs_write_open(rcpath);
	g_free(rcpath);
	if (!pfile || (prefs_set_block_label(pfile, "GPG") < 0))
		return;

	if (prefs_write_param(param, pfile->fp) < 0) {
		g_warning("failed to write GPG configuration to file\n");
		prefs_file_close_revert(pfile);
		return;
	}
        if (fprintf(pfile->fp, "\n") < 0) {
		FILE_OP_ERROR(rcpath, "fprintf");
		prefs_file_close_revert(pfile);
	} else
	        prefs_file_close(pfile);
}

struct GPGAccountConfig *prefs_gpg_account_get_config(PrefsAccount *account)
{
	GPGAccountConfig *config;
	const gchar *confstr;
	gchar **strv;

	config = g_new0(GPGAccountConfig, 1);
	config->sign_key = SIGN_KEY_DEFAULT;
	config->sign_key_id = NULL;

	confstr = prefs_account_get_privacy_prefs(account, "gpg");
	if (confstr == NULL)
		return config;

	strv = g_strsplit(confstr, ";", 0);
	if (strv[0] != NULL) {
		if (!strcmp(strv[0], "DEFAULT"))
			config->sign_key = SIGN_KEY_DEFAULT;
		if (!strcmp(strv[0], "BY_FROM"))
			config->sign_key = SIGN_KEY_BY_FROM;
		if (!strcmp(strv[0], "CUSTOM")) {
			if (strv[1] != NULL) {
				config->sign_key = SIGN_KEY_CUSTOM;
				config->sign_key_id = g_strdup(strv[1]);
			} else
				config->sign_key = SIGN_KEY_DEFAULT;
		}
	}
	g_strfreev(strv);

	return config;
}

void prefs_gpg_account_set_config(PrefsAccount *account, GPGAccountConfig *config)
{
	gchar *confstr = NULL;

	switch (config->sign_key) {
	case SIGN_KEY_DEFAULT:
		confstr = g_strdup("DEFAULT");
		break;
	case SIGN_KEY_BY_FROM:
		confstr = g_strdup("BY_FROM");
		break;
	case SIGN_KEY_CUSTOM:
		confstr = g_strdup_printf("CUSTOM;%s", config->sign_key_id);
		break;
	default:
		confstr = g_strdup("");
		g_warning("prefs_gpg_account_set_config: bad sign_key val\n");
	}

	prefs_account_set_privacy_prefs(account, "gpg", confstr);

	g_free(confstr);
}

void prefs_gpg_account_free_config(GPGAccountConfig *config)
{
	g_free(config->sign_key_id);
	g_free(config);
}

static struct GPGPage gpg_page;
static struct GPGAccountPage gpg_account_page;

void prefs_gpg_enable_agent(gboolean enable)
{
	if (enable) {
		if (saved_gpg_agent_info) {
			g_setenv("GPG_AGENT_INFO",
				 saved_gpg_agent_info, TRUE);
			debug_print("set GPG_AGENT_INFO=%s\n", 
				saved_gpg_agent_info);
		} else { 
			debug_print("Can't enable gpg agent (no GPG_AGENT_INFO)\n");
		}
	} else {
		if (saved_gpg_agent_info) {
			g_unsetenv("GPG_AGENT_INFO");
			debug_print("unset GPG_AGENT_INFO=%s\n", 
				saved_gpg_agent_info);
		} else {
			debug_print("Can't disable gpg agent (no GPG_AGENT_INFO)\n");
		}
	}
}

void prefs_gpg_init()
{
	static gchar *path[3];
	gchar *rcpath;
	const gchar *tmp = NULL;

	prefs_set_default(param);
	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, COMMON_RC, NULL);
	prefs_read_config(param, "GPG", rcpath, NULL);
	g_free(rcpath);

        path[0] = _("Plugins");
        path[1] = _("GPG");
        path[2] = NULL;

        gpg_page.page.path = path;
        gpg_page.page.create_widget = prefs_gpg_create_widget_func;
        gpg_page.page.destroy_widget = prefs_gpg_destroy_widget_func;
        gpg_page.page.save_page = prefs_gpg_save_func;
        gpg_page.page.weight = 30.0;

        prefs_gtk_register_page((PrefsPage *) &gpg_page);

        gpg_account_page.page.path = path;
        gpg_account_page.page.create_widget = prefs_gpg_account_create_widget_func;
        gpg_account_page.page.destroy_widget = prefs_gpg_account_destroy_widget_func;
        gpg_account_page.page.save_page = prefs_gpg_account_save_func;
        gpg_account_page.page.weight = 30.0;

        prefs_account_register_page((PrefsPage *) &gpg_account_page);
	
	tmp = g_getenv("GPG_AGENT_INFO");
	if (tmp)
		saved_gpg_agent_info = g_strdup(tmp);

	prefs_gpg_enable_agent(prefs_gpg_get_config()->use_gpg_agent);
}

void prefs_gpg_done()
{
	prefs_gtk_unregister_page((PrefsPage *) &gpg_page);
	prefs_account_unregister_page((PrefsPage *) &gpg_account_page);
	prefs_gpg_enable_agent(TRUE);
}

gboolean prefs_gpg_should_skip_encryption_warning(const gchar *systemid)
{
	gchar **systems = NULL;
	int i = 0;
	if (prefs_gpg_get_config()->skip_encryption_warning == NULL)
		return FALSE;
	systems = g_strsplit(prefs_gpg_get_config()->skip_encryption_warning,
				",", -1);
	while (systems && systems[i]) {
		debug_print(" cmp %s %s\n", systems[i], systemid);
		if (!strcmp(systems[i],systemid)) {
			g_strfreev(systems);
			return TRUE;
		}
		i++;
	}
	g_strfreev(systems);
	return FALSE;
}

void prefs_gpg_add_skip_encryption_warning(const gchar *systemid)
{
	gchar *tmp = NULL;
	if (prefs_gpg_get_config()->skip_encryption_warning == NULL)
		prefs_gpg_get_config()->skip_encryption_warning =
			g_strdup_printf("%s,", systemid);
	else if (!prefs_gpg_should_skip_encryption_warning(systemid)) {
		tmp = g_strdup_printf("%s%s,",
			prefs_gpg_get_config()->skip_encryption_warning,
			systemid);
		g_free(prefs_gpg_get_config()->skip_encryption_warning);
		prefs_gpg_get_config()->skip_encryption_warning = tmp;
	}
	prefs_gpg_save_config();
}

void prefs_gpg_remove_skip_encryption_warning(const gchar *systemid)
{
	gchar **systems = NULL;
	int i = 0;
	if (prefs_gpg_get_config()->skip_encryption_warning == NULL)
		return;

	if (prefs_gpg_should_skip_encryption_warning(systemid)) {
		systems = g_strsplit(prefs_gpg_get_config()->skip_encryption_warning,
				",", -1);
		g_free(prefs_gpg_get_config()->skip_encryption_warning);
		prefs_gpg_get_config()->skip_encryption_warning = NULL;

		while (systems && systems[i]) {
			if (!strcmp(systems[i],systemid)) {
				i++;
				continue;
			}
			prefs_gpg_add_skip_encryption_warning(systems[i]);
			i++;
		}
		
		g_strfreev(systems);
	}
	prefs_gpg_save_config();
}
