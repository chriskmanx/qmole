/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2005-2012 Colin Leroy <colin@colino.net> & The Claws Mail Team
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
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "prefs_common.h"
#include "prefs_gtk.h"
#include "gtk/menu.h"

#include "gtk/gtkutils.h"
#include "gtk/prefswindow.h"
#include "gtk/combobox.h"

#include "manage_window.h"

typedef struct _SendPage
{
	PrefsPage page;

	GtkWidget *window;

	GtkWidget *checkbtn_savemsg;
	GtkWidget *checkbtn_confirm_send_queued_messages;
	GtkWidget *checkbtn_never_send_retrcpt;
	GtkWidget *checkbtn_senddialog;
	GtkWidget *combobox_charset;
	GtkWidget *combobox_encoding_method;
} SendPage;

static gchar * prefs_common_charset_set_data_from_optmenu(GtkWidget *widget)
{
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *data = NULL;

	cm_return_val_if_fail(widget != NULL, NULL);

	cm_return_val_if_fail(gtk_combo_box_get_active_iter(
				GTK_COMBO_BOX(widget), &iter), NULL);

	model = gtk_combo_box_get_model(GTK_COMBO_BOX(widget));

	gtk_tree_model_get(model, &iter, COMBOBOX_DATA, &data, -1);

	return data;
}

typedef struct _combobox_sel_by_data_ctx {
	GtkComboBox *combobox;
	gchar *data;
} ComboboxSelCtx;

static gboolean _select_by_data_func(GtkTreeModel *model, GtkTreePath *path,
		GtkTreeIter *iter, ComboboxSelCtx *ctx)
{
	GtkComboBox *combobox = ctx->combobox;
	gchar *data = ctx->data;
	gchar *curdata; 

	gtk_tree_model_get(GTK_TREE_MODEL(model), iter, COMBOBOX_DATA, &curdata, -1);
	if ( data != NULL && curdata != NULL && !strcmp(data, curdata) ) {
		gtk_combo_box_set_active_iter(combobox, iter);
		return TRUE;
	}

	return FALSE;
}

static void prefs_common_charset_set_optmenu(GtkWidget *widget, gchar *data)
{
	GtkComboBox *combobox = GTK_COMBO_BOX(widget);
	GtkTreeModel *model;
	ComboboxSelCtx *ctx = NULL;
	cm_return_if_fail(combobox != NULL);

	model = gtk_combo_box_get_model(combobox);

	ctx = g_new(ComboboxSelCtx,
			sizeof(ComboboxSelCtx));
	ctx->combobox = combobox;
	ctx->data = data;

	gtk_tree_model_foreach(model, (GtkTreeModelForeachFunc)_select_by_data_func, ctx);
	g_free(ctx);
}

static gboolean _combobox_separator_func(GtkTreeModel *model,
		GtkTreeIter *iter, gpointer data)
{
	gchar *txt = NULL;

	cm_return_val_if_fail(model != NULL, FALSE);

	gtk_tree_model_get(model, iter, COMBOBOX_TEXT, &txt, -1);

	if( txt == NULL )
		return TRUE;
	return FALSE;
}

static void prefs_send_create_widget(PrefsPage *_page, GtkWindow *window, 
			       	  gpointer data)
{
	SendPage *prefs_send = (SendPage *) _page;
	
	GtkWidget *vbox1;
	GtkWidget *vbox2;
	GtkWidget *checkbtn_savemsg;
	GtkWidget *label_outcharset;
	GtkWidget *combobox_charset;
	GtkListStore *optmenu;
	GtkTreeIter iter;
	GtkCellRenderer *rend;
	GtkWidget *combobox_encoding;
	GtkWidget *label_encoding;
	GtkWidget *checkbtn_senddialog;
	GtkWidget *checkbtn_confirm_send_queued_messages;
	GtkWidget *checkbtn_never_send_retrcpt;
	GtkWidget *table;

	vbox1 = gtk_vbox_new (FALSE, VSPACING);
	gtk_widget_show (vbox1);
	gtk_container_set_border_width (GTK_CONTAINER (vbox1), VBOX_BORDER);

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (vbox1), vbox2, FALSE, FALSE, 0);

	PACK_CHECK_BUTTON(vbox2, checkbtn_savemsg,
			_("Save sent messages to Sent folder"));

	PACK_CHECK_BUTTON(vbox2, checkbtn_confirm_send_queued_messages,
			_("Confirm before sending queued messages"));

	PACK_CHECK_BUTTON(vbox2, checkbtn_never_send_retrcpt,
			_("Never send Return Receipts"));

	PACK_CHECK_BUTTON(vbox2, checkbtn_senddialog,
			_("Show send dialog"));

	table = gtk_table_new(2, 2, FALSE);
	gtk_widget_show(table);
	gtk_container_add (GTK_CONTAINER (vbox1), table);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	label_outcharset = gtk_label_new (_("Outgoing encoding"));
	gtk_widget_show (label_outcharset);
	gtk_table_attach(GTK_TABLE(table), label_outcharset, 0, 1, 1, 2,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);
	gtk_label_set_justify(GTK_LABEL(label_outcharset), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(label_outcharset), 1, 0.5);

	optmenu = gtk_list_store_new(2,
			G_TYPE_STRING,		/* Menu label */
			G_TYPE_STRING);		/* Actual charset data string */

	combobox_charset = gtk_combo_box_new_with_model(
			GTK_TREE_MODEL(optmenu));
	rend = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_charset), rend, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_charset), rend,
			"text", COMBOBOX_TEXT,
			NULL);

	gtk_combo_box_set_row_separator_func(GTK_COMBO_BOX(combobox_charset),
			(GtkTreeViewRowSeparatorFunc)_combobox_separator_func, NULL, NULL);

	gtk_widget_show (combobox_charset);
	CLAWS_SET_TIP(combobox_charset,
			     _("If 'Automatic' is selected, the optimal encoding"
		   	       " for the current locale will be used"));
	gtk_table_attach(GTK_TABLE(table), combobox_charset, 1, 2, 1, 2,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);

#define SET_MENUITEM(str, data) \
{ \
	gtk_list_store_append(optmenu, &iter); \
	gtk_list_store_set(optmenu, &iter, \
			COMBOBOX_TEXT, str, \
			COMBOBOX_DATA, data, \
			-1); \
}

	SET_MENUITEM(_("Automatic (Recommended)"),	 CS_AUTO);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("7bit ASCII (US-ASCII)"),	 CS_US_ASCII);
	SET_MENUITEM(_("Unicode (UTF-8)"),		 CS_UTF_8);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Western European (ISO-8859-1)"),  CS_ISO_8859_1);
	SET_MENUITEM(_("Western European (ISO-8859-15)"), CS_ISO_8859_15);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Central European (ISO-8859-2)"), CS_ISO_8859_2);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Baltic (ISO-8859-13)"),		 CS_ISO_8859_13);
	SET_MENUITEM(_("Baltic (ISO-8859-4)"),		 CS_ISO_8859_4);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Greek (ISO-8859-7)"),		 CS_ISO_8859_7);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Hebrew (ISO-8859-8)"),		 CS_ISO_8859_8);
	SET_MENUITEM(_("Hebrew (Windows-1255)"),	 CS_WINDOWS_1255);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Arabic (ISO-8859-6)"),		 CS_ISO_8859_6);
	SET_MENUITEM(_("Arabic (Windows-1256)"),	 CS_WINDOWS_1256);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Turkish (ISO-8859-9)"),		 CS_ISO_8859_9);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Cyrillic (ISO-8859-5)"),	 CS_ISO_8859_5);
	SET_MENUITEM(_("Cyrillic (KOI8-R)"),		 CS_KOI8_R);
	SET_MENUITEM(_("Cyrillic (KOI8-U)"),		 CS_KOI8_U);
	SET_MENUITEM(_("Cyrillic (Windows-1251)"),	 CS_WINDOWS_1251);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Japanese (ISO-2022-JP)"),	 CS_ISO_2022_JP);
#if 0
	SET_MENUITEM(_("Japanese (EUC-JP)"),		 CS_EUC_JP);
	SET_MENUITEM(_("Japanese (Shift_JIS)"),		 CS_SHIFT_JIS);
#endif /* 0 */
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Simplified Chinese (GB18030)"),	 CS_GB18030);
	SET_MENUITEM(_("Simplified Chinese (GB2312)"),	 CS_GB2312);
	SET_MENUITEM(_("Simplified Chinese (GBK)"),	 CS_GBK);
	SET_MENUITEM(_("Traditional Chinese (Big5)"),	 CS_BIG5);
#if 0
	SET_MENUITEM(_("Traditional Chinese (EUC-TW)"),  CS_EUC_TW);
	SET_MENUITEM(_("Chinese (ISO-2022-CN)"),	 CS_ISO_2022_CN);
#endif /* 0 */
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Korean (EUC-KR)"),		 CS_EUC_KR);
	SET_MENUITEM(NULL, NULL);
	SET_MENUITEM(_("Thai (TIS-620)"),		 CS_TIS_620);
	SET_MENUITEM(_("Thai (Windows-874)"),		 CS_WINDOWS_874);

#undef SET_MENUITEM

	label_encoding = gtk_label_new (_("Transfer encoding"));
	gtk_widget_show (label_encoding);
	gtk_table_attach(GTK_TABLE(table), label_encoding, 0, 1, 2, 3,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);
	gtk_label_set_justify(GTK_LABEL(label_encoding), GTK_JUSTIFY_RIGHT);
	gtk_misc_set_alignment(GTK_MISC(label_encoding), 1, 0.5);

	combobox_encoding = gtkut_sc_combobox_create(NULL, FALSE);
	gtk_widget_show (combobox_encoding);
	CLAWS_SET_TIP(combobox_encoding,
			     _("Specify Content-Transfer-Encoding used when"
		   	       " message body contains non-ASCII characters"));
	gtk_table_attach(GTK_TABLE(table), combobox_encoding, 1, 2, 2, 3,
			(GtkAttachOptions) (GTK_FILL),
			(GtkAttachOptions) (0), 0, 0);

	optmenu = GTK_LIST_STORE(gtk_combo_box_get_model(
				GTK_COMBO_BOX(combobox_encoding)));

	COMBOBOX_ADD(optmenu, _("Automatic"),	 CTE_AUTO);
	COMBOBOX_ADD(optmenu, NULL, 0);
	COMBOBOX_ADD(optmenu, "base64",		 CTE_BASE64);
	COMBOBOX_ADD(optmenu, "quoted-printable", CTE_QUOTED_PRINTABLE);
	COMBOBOX_ADD(optmenu, "8bit",		 CTE_8BIT);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_savemsg),
		prefs_common.savemsg);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_confirm_send_queued_messages),
		prefs_common.confirm_send_queued_messages);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_never_send_retrcpt),
		prefs_common.never_send_retrcpt);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbtn_senddialog),
		!prefs_common.send_dialog_invisible);
	prefs_common_charset_set_optmenu(combobox_charset, 
		prefs_common.outgoing_charset);
	combobox_select_by_data(GTK_COMBO_BOX(combobox_encoding),
		prefs_common.encoding_method);
	
	prefs_send->window			= GTK_WIDGET(window);
	
	prefs_send->checkbtn_savemsg = checkbtn_savemsg;
	prefs_send->checkbtn_confirm_send_queued_messages = checkbtn_confirm_send_queued_messages;
 	prefs_send->checkbtn_never_send_retrcpt = checkbtn_never_send_retrcpt;
	prefs_send->checkbtn_senddialog = checkbtn_senddialog;
	prefs_send->combobox_charset = combobox_charset;
	prefs_send->combobox_encoding_method = combobox_encoding;

	prefs_send->page.widget = vbox1;
}

static void prefs_send_save(PrefsPage *_page)
{
	SendPage *page = (SendPage *) _page;

	prefs_common.savemsg = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_savemsg));
	prefs_common.confirm_send_queued_messages = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_confirm_send_queued_messages));
	prefs_common.never_send_retrcpt = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_never_send_retrcpt));
	prefs_common.send_dialog_invisible = !gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(page->checkbtn_senddialog));

	g_free(prefs_common.outgoing_charset);
	prefs_common.outgoing_charset = prefs_common_charset_set_data_from_optmenu(
		page->combobox_charset);
	prefs_common.encoding_method =
		combobox_get_active_data(GTK_COMBO_BOX(page->combobox_encoding_method));
}

static void prefs_send_destroy_widget(PrefsPage *_page)
{
}

SendPage *prefs_send;

void prefs_send_init(void)
{
	SendPage *page;
	static gchar *path[3];

	path[0] = _("Mail Handling");
	path[1] = _("Sending");
	path[2] = NULL;

	page = g_new0(SendPage, 1);
	page->page.path = path;
	page->page.create_widget = prefs_send_create_widget;
	page->page.destroy_widget = prefs_send_destroy_widget;
	page->page.save_page = prefs_send_save;
	page->page.weight = 195.0;
	prefs_gtk_register_page((PrefsPage *) page);
	prefs_send = page;
}

void prefs_send_done(void)
{
	prefs_gtk_unregister_page((PrefsPage *) prefs_send);
	g_free(prefs_send);
}
