/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Michael Rasmussen and the Claws Mail team
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

#ifdef USE_LDAP

#include "defs.h"

#include "mgutils.h"
#include "addressbook.h"
#include "addressitem.h"
#include "addritem.h"
#include "addrbook.h"
#include "manage_window.h"
#include "gtkutils.h"
#include "codeconv.h"
#include "editaddress.h"
#include "editaddress_other_attributes_ldap.h"
#include "prefs_common.h"

#define	ATTRIB_COL_NAME			0
#define	ATTRIB_COL_VALUE		1
#define ATTRIB_N_COLS			2
#define EMAIL_N_COLS			3
#define ATTRIB_COL_WIDTH_NAME	120
#define ATTRIB_COL_WIDTH_VALUE	180

PersonEditDlg *personEditDlg;
gboolean attrib_adding = FALSE, attrib_saving = FALSE;

int get_attribute_index(const gchar *string_literal) {
	int i = 0;
	/*int count = sizeof(ATTRIBUTE) / sizeof(*ATTRIBUTE);*/
	const gchar **attribute = ATTRIBUTE;

	cm_return_val_if_fail(string_literal != NULL, -1);
	while (*attribute) {
		debug_print("Comparing %s to %s\n", *attribute, string_literal);
		if (strcmp(*attribute++, string_literal) == 0)
			return i;
		i++;
	}
	return -1;
}

static void edit_person_status_show(gchar *msg) {
	if (personEditDlg->statusbar != NULL) {
		gtk_statusbar_pop(GTK_STATUSBAR(personEditDlg->statusbar), personEditDlg->status_cid);
		if(msg) {
			gtk_statusbar_push(GTK_STATUSBAR(personEditDlg->statusbar), personEditDlg->status_cid, msg);
		}
	}
}

static void edit_person_attrib_clear(gpointer data) {
	gtk_combo_box_set_active(GTK_COMBO_BOX(personEditDlg->entry_atname), 0);
	gtk_entry_set_text(GTK_ENTRY(personEditDlg->entry_atvalue), "");
}

static gboolean list_find_attribute(const gchar *attr)
{
	GtkCMCList *clist = GTK_CMCLIST(personEditDlg->clist_attrib);
	UserAttribute *attrib;
	gint row = 0;
	while((attrib = gtk_cmclist_get_row_data(clist, row))) {
		if (!g_ascii_strcasecmp(attrib->name, attr)) {
			gtk_cmclist_select_row(clist, row, 0);
			return TRUE;
		}
		row++;
	}
	return FALSE;
}

/*
* Comparison using cell contents (text in first column). Used for sort
* address index widget.
*/
static gint edit_person_attrib_compare_func(GtkCMCList *clist, gconstpointer ptr1, gconstpointer ptr2) {
	GtkCMCell *cell1 = ((GtkCMCListRow *)ptr1)->cell;
	GtkCMCell *cell2 = ((GtkCMCListRow *)ptr2)->cell;
	gchar *name1 = NULL, *name2 = NULL;

	if (cell1) name1 = cell1->u.text;
	if (cell2) name2 = cell2->u.text;
	if (!name1) return (name2 != NULL);
	if (!name2) return -1;
	return g_utf8_collate(name1, name2);
}

static void edit_person_combo_box_changed(GtkComboBox *opt_menu, gpointer data)
{
	GtkCMCList *clist = GTK_CMCLIST(data);
	gint row = personEditDlg->rowIndAttrib;
	UserAttribute *attrib = gtk_cmclist_get_row_data(clist, row);
	gint option = gtk_combo_box_get_active(opt_menu);
	const gchar *str = attrib ? attrib->name:"";

	cm_return_if_fail (option < ATTRIBUTE_SIZE);
	/* A corresponding attribute in contact does not match selected option */ 
	if (strcmp(ATTRIBUTE[option], str) != 0) {
		gtk_widget_set_sensitive(personEditDlg->attrib_add, TRUE);
		gtk_widget_set_sensitive(personEditDlg->attrib_mod, TRUE);
		gtk_widget_set_sensitive(personEditDlg->attrib_del, FALSE);
		gtk_entry_set_text(GTK_ENTRY(personEditDlg->entry_atvalue), "");
		gtk_widget_grab_focus(personEditDlg->entry_atvalue);
		edit_person_status_show(NULL);
	}
}

static void edit_person_attrib_list_selected(GtkCMCList *clist, gint row, gint column, GdkEvent *event, gpointer data) {
	UserAttribute *attrib = gtk_cmclist_get_row_data(clist, row);
	if (attrib && !personEditDlg->read_only) {
		int index = get_attribute_index(attrib->name);
		if (index == -1)
			index = 0;

		gtk_combo_box_set_active(GTK_COMBO_BOX(personEditDlg->entry_atname), index);
		gtk_entry_set_text( GTK_ENTRY(personEditDlg->entry_atvalue), attrib->value );
		gtk_widget_set_sensitive(personEditDlg->attrib_del, TRUE);
	}
	else {
		/*g_printerr("Row: %d -> empty attribute\n", row);*/
		gtk_entry_set_text( GTK_ENTRY(personEditDlg->entry_atvalue), "");	
		gtk_widget_set_sensitive(personEditDlg->attrib_del, FALSE);
	}
	personEditDlg->rowIndAttrib = row;
	edit_person_status_show(NULL);
}

static void edit_person_attrib_delete(gpointer data) {
	GtkCMCList *clist = GTK_CMCLIST(personEditDlg->clist_attrib);
	gint row = personEditDlg->rowIndAttrib;
	UserAttribute *attrib = gtk_cmclist_get_row_data(clist, row);
	edit_person_attrib_clear(NULL);
	if (attrib) {
		/* Remove list entry */
		gtk_cmclist_remove(clist, row);
		addritem_free_attribute(attrib);
		attrib = NULL;
	}

	/* Position hilite bar */
	attrib = gtk_cmclist_get_row_data(clist, row);
	if (!attrib) {
		personEditDlg->rowIndAttrib = -1 + row;
	} 
	
	if (!personEditDlg->read_only)
		gtk_widget_set_sensitive(personEditDlg->attrib_del, gtk_cmclist_get_row_data(clist, 0) != NULL);
	
	edit_person_status_show(NULL);
}

static UserAttribute *edit_person_attrib_edit(gboolean *error, UserAttribute *attrib) {
	UserAttribute *retVal = NULL;
	gchar *sName, *sValue, *sName_, *sValue_;
	gint index;

	*error = TRUE;
	index = gtk_combo_box_get_active(GTK_COMBO_BOX(personEditDlg->entry_atname));
	sName_ = (gchar *) ATTRIBUTE[index];
	sValue_ = gtk_editable_get_chars(GTK_EDITABLE(personEditDlg->entry_atvalue), 0, -1);
	sName = mgu_email_check_empty(sName_);
	sValue = mgu_email_check_empty(sValue_);
	g_free(sValue_);

	if (sName && sValue) {
		if (attrib == NULL) {
			attrib = addritem_create_attribute();
		}
		addritem_attrib_set_name(attrib, sName);
		addritem_attrib_set_value(attrib, sValue);
		retVal = attrib;
		*error = FALSE;
	}
	else {
		edit_person_status_show(N_( "A Name and Value must be supplied." ));
		gtk_widget_grab_focus(personEditDlg->entry_atvalue);		
	}

	g_free(sName);
	g_free(sValue);

	return retVal;
}

static void edit_person_attrib_modify(gpointer data) {
	gboolean errFlg = FALSE;
	GtkCMCList *clist = GTK_CMCLIST(personEditDlg->clist_attrib);
	gint row = personEditDlg->rowIndAttrib;
	UserAttribute *attrib = gtk_cmclist_get_row_data(clist, row);
	if (attrib) {
		edit_person_attrib_edit(&errFlg, attrib);
		if (!errFlg) {
			gtk_cmclist_set_text(clist, row, ATTRIB_COL_NAME, attrib->name);
			gtk_cmclist_set_text(clist, row, ATTRIB_COL_VALUE, attrib->value);
			edit_person_attrib_clear(NULL);
		}
	}
}

static void edit_person_attrib_add(gpointer data) {
	GtkCMCList *clist = GTK_CMCLIST(personEditDlg->clist_attrib);
	gboolean errFlg = FALSE;
	UserAttribute *attrib = NULL;
	gint row = personEditDlg->rowIndAttrib;
	if (gtk_cmclist_get_row_data(clist, row) == NULL) row = 0;

	attrib = edit_person_attrib_edit(&errFlg, NULL);
	if (!errFlg) {
		gchar *text[EMAIL_N_COLS];
		text[ATTRIB_COL_NAME] = attrib->name;
		text[ATTRIB_COL_VALUE] = attrib->value;

		row = gtk_cmclist_insert(clist, 1 + row, text);
		gtk_cmclist_set_row_data(clist, row, attrib);
		gtk_cmclist_select_row(clist, row, 0);
		edit_person_attrib_clear(NULL);
	}
}

static void edit_person_entry_att_changed (GtkWidget *entry, gpointer data)
{
	gboolean non_empty = gtk_cmclist_get_row_data(GTK_CMCLIST(personEditDlg->clist_attrib), 0) != NULL;
	const gchar *sName;
	int index;

	if (personEditDlg->read_only)
		return;

	index = gtk_combo_box_get_active(GTK_COMBO_BOX(personEditDlg->entry_atname));
	sName = ATTRIBUTE[index];
	if (list_find_attribute(sName)) {
		gtk_widget_set_sensitive(personEditDlg->attrib_add,FALSE);
		gtk_widget_set_sensitive(personEditDlg->attrib_mod,non_empty);
		attrib_adding = FALSE;
		attrib_saving = non_empty;
	} 
	else {
		gtk_widget_set_sensitive(personEditDlg->attrib_add,TRUE);
		gtk_widget_set_sensitive(personEditDlg->attrib_mod,non_empty);
		attrib_adding = TRUE;
		attrib_saving = non_empty;
	}
}

static gboolean edit_person_entry_att_pressed(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if (event && (event->keyval == GDK_KEY_Return || event->keyval == GDK_KEY_KP_Enter)) {
		if (attrib_saving)
			edit_person_attrib_modify(NULL);
		else if (attrib_adding)
			edit_person_attrib_add(NULL);
	}
	return FALSE;
}

void addressbook_edit_person_page_attrib_ldap(PersonEditDlg *dialog, gint pageNum, gchar *pageLbl) {
	GtkWidget *combo_box;
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *vboxl;
	GtkWidget *vboxb;
	GtkWidget *vbuttonbox;
	GtkWidget *buttonDel;
	GtkWidget *buttonMod;
	GtkWidget *buttonAdd;

	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *clist_swin;
	GtkWidget *clist;
	GtkWidget *entry_value;
	gint top;

	personEditDlg = dialog;

	gchar *titles[ATTRIB_N_COLS];
	gint i;

	titles[ATTRIB_COL_NAME] = N_("Name");
	titles[ATTRIB_COL_VALUE] = N_("Value");

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER(personEditDlg->notebook), vbox);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), BORDER_WIDTH);

	label = gtk_label_new_with_mnemonic(pageLbl);
	gtk_widget_show(label);
	gtk_notebook_set_tab_label(
		GTK_NOTEBOOK(personEditDlg->notebook),
		gtk_notebook_get_nth_page(GTK_NOTEBOOK(personEditDlg->notebook), pageNum), label);

	/* Split into two areas */
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(vbox), hbox);

	/* Attribute list */
	vboxl = gtk_vbox_new(FALSE, 4);
	gtk_container_add(GTK_CONTAINER(hbox), vboxl);
	gtk_container_set_border_width(GTK_CONTAINER(vboxl), 4);

	clist_swin = gtk_scrolled_window_new(NULL, NULL);
	gtk_container_add(GTK_CONTAINER(vboxl), clist_swin);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(clist_swin),
				       GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

	clist = gtk_cmclist_new_with_titles(ATTRIB_N_COLS, titles);
	gtk_container_add(GTK_CONTAINER(clist_swin), clist);
	gtk_cmclist_set_selection_mode(GTK_CMCLIST(clist), GTK_SELECTION_BROWSE);
	gtk_cmclist_set_column_width(GTK_CMCLIST(clist), ATTRIB_COL_NAME, ATTRIB_COL_WIDTH_NAME);
	gtk_cmclist_set_column_width(GTK_CMCLIST(clist), ATTRIB_COL_VALUE, ATTRIB_COL_WIDTH_VALUE);
	gtk_cmclist_set_compare_func(GTK_CMCLIST(clist), edit_person_attrib_compare_func);
	gtk_cmclist_set_auto_sort(GTK_CMCLIST(clist), TRUE);

	for (i = 0; i < ATTRIB_N_COLS; i++)
		gtkut_widget_set_can_focus(GTK_CMCLIST(clist)->column[i].button, FALSE);

	/* Data entry area */
	table = gtk_table_new(4, 2, FALSE);
	gtk_box_pack_start(GTK_BOX(vboxl), table, FALSE, FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(table), 4);
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 4);

	/* First row */
	top = 0;
	label = gtk_label_new(N_("Name"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	gchar **attribute = (gchar **) ATTRIBUTE;

	combo_box = gtk_combo_box_new_text();

	while (*attribute) {
		if (!strcmp(*attribute, "jpegPhoto")) {
			attribute++;
			continue;
		}
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo_box), *attribute++);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(combo_box), 0);

	gtk_table_attach(GTK_TABLE(table), combo_box, 1, 2, top, (top + 1), GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	/* Next row */
	++top;
	label = gtk_label_new(N_("Value"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	entry_value = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(table), entry_value, 1, 2, top, (top + 1), GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	/* Button box */
	vboxb = gtk_vbox_new(FALSE, 4);
	gtk_box_pack_start(GTK_BOX(hbox), vboxb, FALSE, FALSE, 2);

	vbuttonbox = gtk_vbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(vbuttonbox), GTK_BUTTONBOX_START);
	gtk_box_set_spacing(GTK_BOX(vbuttonbox), 8);
	gtk_container_set_border_width(GTK_CONTAINER(vbuttonbox), 4);
	gtk_container_add(GTK_CONTAINER(vboxb), vbuttonbox);

	/* Buttons */
	buttonDel = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_container_add(GTK_CONTAINER(vbuttonbox), buttonDel);

	buttonMod = gtk_button_new_from_stock(GTK_STOCK_SAVE);
	gtk_container_add(GTK_CONTAINER(vbuttonbox), buttonMod);

	buttonAdd = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_container_add(GTK_CONTAINER(vbuttonbox), buttonAdd);
	
	gtk_widget_set_sensitive(buttonDel,FALSE);
	gtk_widget_set_sensitive(buttonMod,FALSE);
	gtk_widget_set_sensitive(buttonAdd,FALSE);

	gtk_widget_show_all(vbox);
	
	/* Event handlers */
	g_signal_connect(G_OBJECT(clist), "select_row",
			  G_CALLBACK( edit_person_attrib_list_selected), NULL);
	g_signal_connect(G_OBJECT(buttonDel), "clicked",
			  G_CALLBACK(edit_person_attrib_delete), NULL);
	g_signal_connect(G_OBJECT(buttonMod), "clicked",
			  G_CALLBACK(edit_person_attrib_modify), NULL);
	g_signal_connect(G_OBJECT(buttonAdd), "clicked",
			  G_CALLBACK(edit_person_attrib_add), NULL);
	g_signal_connect(G_OBJECT(combo_box), "changed",
			 G_CALLBACK(edit_person_entry_att_changed), NULL);
	g_signal_connect(G_OBJECT(entry_value), "key_press_event",
			 G_CALLBACK(edit_person_entry_att_pressed), NULL);
	g_signal_connect(G_OBJECT(combo_box), "changed",
			 G_CALLBACK(edit_person_combo_box_changed), clist);

	personEditDlg->clist_attrib  = clist;
	personEditDlg->entry_atname  = combo_box;
	personEditDlg->entry_atvalue = entry_value;
	personEditDlg->attrib_add = buttonAdd;
	personEditDlg->attrib_del = buttonDel;
	personEditDlg->attrib_mod = buttonMod;
}

#endif	/* USE_LDAP */


