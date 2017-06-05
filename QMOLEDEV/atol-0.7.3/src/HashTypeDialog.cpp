////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to select type of hash algorithm (md5,crc32 or sha1)
//////////////////////////////////////////////////////////////////////////// 

#include "HashTypeDialog.h"
#include "support.h"

extern GtkWidget* atol_main;
static GtkWidget* create_hash_type_dialog ();
static void on_crc32_activated	(GtkToggleButton *togglebutton, gpointer user_data);
static void on_md5_activated	(GtkToggleButton *togglebutton, gpointer user_data);
static void on_sha1_activated	(GtkToggleButton *togglebutton, gpointer user_data);

HashTypeDialog::HashTypeDialog()
{
	m_nHashType = 1;
	Create();
}

HashTypeDialog::~HashTypeDialog()
{
	Destroy();
}

void HashTypeDialog::Create()
{
	m_pDialog = create_hash_type_dialog ();

	GtkWidget *radiobutton1 = lookup_widget(m_pDialog, "radiobutton1");
	GtkWidget *radiobutton2 = lookup_widget(m_pDialog, "radiobutton2");
	GtkWidget *radiobutton3 = lookup_widget(m_pDialog, "radiobutton3");

	g_signal_connect(radiobutton1, "toggled",	G_CALLBACK (on_crc32_activated), this);
	g_signal_connect(radiobutton2, "toggled",	G_CALLBACK (on_md5_activated), this);
	g_signal_connect(radiobutton3, "toggled",	G_CALLBACK (on_sha1_activated), this);
}

GtkWidget* create_hash_type_dialog ()
{
	GtkWidget *hash_type_dialog;
	GtkWidget *dialog_vbox7;
	GtkWidget *vbox11;
	GtkWidget *radiobutton1;
	GSList *radiobutton1_group = NULL;
	GtkWidget *radiobutton2;
	GtkWidget *radiobutton3;
	GtkWidget *dialog_action_area7;
	GtkWidget *cancelbutton6;
	GtkWidget *okbutton6;
	
	hash_type_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (hash_type_dialog), _("Select hash algorithm"));
	gtk_window_set_type_hint (GTK_WINDOW (hash_type_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (hash_type_dialog), GTK_WINDOW(atol_main));   //set parent

	dialog_vbox7 = GTK_DIALOG (hash_type_dialog)->vbox;
	gtk_widget_show (dialog_vbox7);
	
	vbox11 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox11);
	gtk_box_pack_start (GTK_BOX (dialog_vbox7), vbox11, TRUE, TRUE, 0);
	
	radiobutton1 = gtk_radio_button_new_with_mnemonic (NULL, _("_CRC32"));
	gtk_widget_show (radiobutton1);
	gtk_box_pack_start (GTK_BOX (vbox11), radiobutton1, FALSE, FALSE, 0);
	gtk_radio_button_set_group (GTK_RADIO_BUTTON (radiobutton1), radiobutton1_group);
	radiobutton1_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton1));
	
	radiobutton2 = gtk_radio_button_new_with_mnemonic (NULL, _("_MD5"));
	gtk_widget_show (radiobutton2);
	gtk_box_pack_start (GTK_BOX (vbox11), radiobutton2, FALSE, FALSE, 0);
	gtk_radio_button_set_group (GTK_RADIO_BUTTON (radiobutton2), radiobutton1_group);
	radiobutton1_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton2));
	
	radiobutton3 = gtk_radio_button_new_with_mnemonic (NULL, _("_SHA1"));
	gtk_widget_show (radiobutton3);
	gtk_box_pack_start (GTK_BOX (vbox11), radiobutton3, FALSE, FALSE, 0);
	gtk_radio_button_set_group (GTK_RADIO_BUTTON (radiobutton3), radiobutton1_group);
	radiobutton1_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton3));
	
	dialog_action_area7 = GTK_DIALOG (hash_type_dialog)->action_area;
	gtk_widget_show (dialog_action_area7);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area7), GTK_BUTTONBOX_END);
	
	cancelbutton6 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton6);
	gtk_dialog_add_action_widget (GTK_DIALOG (hash_type_dialog), cancelbutton6, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton6, GTK_CAN_DEFAULT);
	
	okbutton6 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton6);
	gtk_dialog_add_action_widget (GTK_DIALOG (hash_type_dialog), okbutton6, GTK_RESPONSE_OK);
	GTK_WIDGET_SET_FLAGS (okbutton6, GTK_CAN_DEFAULT);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (hash_type_dialog, hash_type_dialog, "hash_type_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (hash_type_dialog, dialog_vbox7, "dialog_vbox7");
	GLADE_HOOKUP_OBJECT (hash_type_dialog, vbox11, "vbox11");
	GLADE_HOOKUP_OBJECT (hash_type_dialog, radiobutton1, "radiobutton1");
	GLADE_HOOKUP_OBJECT (hash_type_dialog, radiobutton2, "radiobutton2");
	GLADE_HOOKUP_OBJECT (hash_type_dialog, radiobutton3, "radiobutton3");
	GLADE_HOOKUP_OBJECT_NO_REF (hash_type_dialog, dialog_action_area7, "dialog_action_area7");
	GLADE_HOOKUP_OBJECT (hash_type_dialog, cancelbutton6, "cancelbutton6");
	GLADE_HOOKUP_OBJECT (hash_type_dialog, okbutton6, "okbutton6");
	
	gtk_dialog_set_default_response (GTK_DIALOG (hash_type_dialog), GTK_RESPONSE_OK);

	return hash_type_dialog;
}

void on_crc32_activated	(GtkToggleButton *togglebutton, gpointer user_data)
{
	HashTypeDialog *pDlg = (HashTypeDialog *)user_data;
	pDlg->m_nHashType = 1;
}

void on_md5_activated	(GtkToggleButton *togglebutton, gpointer user_data)
{
	HashTypeDialog *pDlg = (HashTypeDialog *)user_data;
	pDlg->m_nHashType = 2;
}

void on_sha1_activated	(GtkToggleButton *togglebutton, gpointer user_data)
{
	HashTypeDialog *pDlg = (HashTypeDialog *)user_data;
	pDlg->m_nHashType = 3;
}
