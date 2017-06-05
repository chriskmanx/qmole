////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define file encryption/decryption operation
//////////////////////////////////////////////////////////////////////////// 

#include "support.h"
#include "EncryptionDialog.h"
#include "GuiInputDlg.h"

extern GtkWidget *atol_main;
static void on_encrypt_clicked(GtkButton *button, gpointer user_data);
static void on_decrypt_clicked(GtkButton *button, gpointer user_data);

EncryptionDialog::EncryptionDialog()
{
	m_bSelected = false;
	m_bEncrypt = false;
	m_bDeleteOriginal = false;

	Create();
}

EncryptionDialog::~EncryptionDialog()
{
	Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void EncryptionDialog::Create()
{
	m_pDialog = create_encryption_dialog ();
}

GtkWidget* EncryptionDialog::create_encryption_dialog ()
{
	GtkWidget *encryption_dialog;
	GtkWidget *dialog_vbox11;
	GtkWidget *vbox13;
	GtkWidget *label24;
	GtkWidget *checkbutton14;
	GtkWidget *dialog_action_area11;
	GtkWidget *btn_encrypt;
	GtkWidget *btn_decrypt;
	GtkWidget *btn_cancel;
	
	encryption_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (encryption_dialog), _("Encrypt/decrypt"));
	gtk_window_set_type_hint (GTK_WINDOW (encryption_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	
	dialog_vbox11 = GTK_DIALOG (encryption_dialog)->vbox;
	gtk_widget_show (dialog_vbox11);
	
	vbox13 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox13);
	gtk_box_pack_start (GTK_BOX (dialog_vbox11), vbox13, TRUE, TRUE, 0);
	
	label24 = gtk_label_new (_("Encrypt/decrypt the files using strong algorithm"));
	gtk_widget_show (label24);
	gtk_box_pack_start (GTK_BOX (vbox13), label24, FALSE, FALSE, 0);
	
	checkbutton14 = gtk_check_button_new_with_mnemonic (_("Delete _original files"));
	gtk_widget_show (checkbutton14);
	gtk_box_pack_start (GTK_BOX (vbox13), checkbutton14, FALSE, FALSE, 0);
	
	dialog_action_area11 = GTK_DIALOG (encryption_dialog)->action_area;
	gtk_widget_show (dialog_action_area11);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area11), GTK_BUTTONBOX_END);

	btn_encrypt =  gtk_button_new_with_mnemonic(_("_Encrypt"));
	gtk_widget_show (btn_encrypt);
	gtk_container_add (GTK_CONTAINER(dialog_action_area11), btn_encrypt);
	GTK_WIDGET_SET_FLAGS (btn_encrypt, GTK_CAN_DEFAULT);

	btn_decrypt = gtk_button_new_with_mnemonic(_("_Decrypt"));
	gtk_widget_show (btn_decrypt);
	gtk_container_add (GTK_CONTAINER(dialog_action_area11), btn_decrypt);
	GTK_WIDGET_SET_FLAGS (btn_decrypt, GTK_CAN_DEFAULT);
	
	btn_cancel = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (btn_cancel);
	gtk_dialog_add_action_widget (GTK_DIALOG (encryption_dialog), btn_cancel, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (btn_cancel, GTK_CAN_DEFAULT);
	
	g_signal_connect(btn_encrypt, "clicked",	G_CALLBACK (on_encrypt_clicked), this);
	g_signal_connect(btn_decrypt, "clicked",	G_CALLBACK (on_decrypt_clicked), this);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (encryption_dialog, encryption_dialog, "encryption_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (encryption_dialog, dialog_vbox11, "dialog_vbox11");
	GLADE_HOOKUP_OBJECT (encryption_dialog, vbox13, "vbox13");
	GLADE_HOOKUP_OBJECT (encryption_dialog, label24, "label24");
	GLADE_HOOKUP_OBJECT (encryption_dialog, checkbutton14, "checkbutton14");
	GLADE_HOOKUP_OBJECT_NO_REF (encryption_dialog, dialog_action_area11, "dialog_action_area11");
	GLADE_HOOKUP_OBJECT (encryption_dialog, btn_cancel, "btn_cancel");
	GLADE_HOOKUP_OBJECT (encryption_dialog, btn_encrypt, "btn_encrypt");
	GLADE_HOOKUP_OBJECT (encryption_dialog, btn_decrypt, "btn_decrypt");
	
	return encryption_dialog;
}

void on_encrypt_clicked(GtkButton *button, gpointer user_data)
{
	EncryptionDialog *pDlg = (EncryptionDialog *)user_data;

	pDlg->m_bEncrypt = true;
	
	//read check box state
	GtkWidget *checkbutton14 = lookup_widget(pDlg->m_pDialog, "checkbutton14");
	pDlg->m_bDeleteOriginal = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton14))>0;

	//hide the dialog
	pDlg->Destroy();

	//get password
	GuiInputDlg dlg(true);
	dlg.SetLabel(_("Please type the password"));
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	pDlg->m_strPassword = dlg.GetInput();
	pDlg->m_bSelected = true;
}

void on_decrypt_clicked(GtkButton *button, gpointer user_data)
{
	EncryptionDialog *pDlg = (EncryptionDialog *)user_data;

	pDlg->m_bEncrypt = false;
	
	//read check box state
	GtkWidget *checkbutton14 = lookup_widget(pDlg->m_pDialog, "checkbutton14");
	pDlg->m_bDeleteOriginal = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton14))>0;

	//hide the dialog
	pDlg->Destroy();

	//get password
	GuiInputDlg dlg(true);
	dlg.SetLabel(_("Please type the password"));
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	pDlg->m_strPassword = dlg.GetInput();
	pDlg->m_bSelected = true;
}

