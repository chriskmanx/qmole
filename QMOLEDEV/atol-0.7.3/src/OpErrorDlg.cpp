////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to handle copy failure (inside file operation)
//////////////////////////////////////////////////////////////////////////// 

#include "OpErrorDlg.h"
#include "support.h"
#include "core/opcodes.h"

extern GtkWidget *atol_main;
 
static void on_skip_button (GtkWidget *button, gpointer user_data);
static void on_skip_all_button (GtkWidget *button, gpointer user_data);
//static void on_retry_button (GtkWidget *button, gpointer user_data);
static void on_abort_button (GtkWidget *button, gpointer user_data);

OpErrorDlg::OpErrorDlg()
{
}

OpErrorDlg::~OpErrorDlg()
{
	Destroy();
}

void OpErrorDlg::SetInfo(const char *szTxt)
{
	GtkWidget *label8 = lookup_widget(m_pDialog, "label8");
	gtk_label_set_text(GTK_LABEL(label8), szTxt);
}

void OpErrorDlg::Create()
{
	m_pDialog = create_dialog ();

	SetInfo(m_strInfo);
}

GtkWidget* OpErrorDlg::create_dialog ()
{
	GtkWidget *err_dialog;
	GtkWidget *dialog_vbox4;
	GtkWidget *table5;
	GtkWidget *button24;
	GtkWidget *button25;
	//GtkWidget *button27;
	GtkWidget *button28;
	GtkWidget *label8;
	GtkWidget *dialog_action_area4;

	err_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (err_dialog), _("Operation failed"));
	gtk_window_set_type_hint (GTK_WINDOW (err_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (err_dialog), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_resizable (GTK_WINDOW (err_dialog), FALSE);
	gtk_widget_realize(err_dialog);
	gdk_window_set_decorations(err_dialog->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE));
	gtk_window_set_destroy_with_parent (GTK_WINDOW (err_dialog), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (err_dialog), TRUE);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (err_dialog), TRUE);
#endif

	dialog_vbox4 = GTK_DIALOG (err_dialog)->vbox;
	gtk_widget_show (dialog_vbox4);

	label8 = gtk_label_new ("");
	gtk_misc_set_alignment(GTK_MISC(label8), 0.5f, 0.5f);
	gtk_widget_set_size_request (label8, -1, 40);
	gtk_widget_show (label8);
	gtk_box_pack_start (GTK_BOX (dialog_vbox4), label8, TRUE, TRUE, 0);
	
	dialog_action_area4 = GTK_DIALOG (err_dialog)->action_area;
	gtk_widget_show (dialog_action_area4);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area4), GTK_BUTTONBOX_END);
	
	table5 = gtk_table_new (3, 1, FALSE);
	gtk_widget_show (table5);
	gtk_box_pack_start (GTK_BOX (dialog_action_area4), table5, TRUE, TRUE, 0);
	gtk_table_set_row_spacings (GTK_TABLE (table5), 4);
	gtk_table_set_col_spacings (GTK_TABLE (table5), 3);
	
	button24 = gtk_button_new_with_mnemonic (_("Skip"));
	gtk_widget_show (button24);
	gtk_widget_set_size_request (button24, 70, -1);
	gtk_table_attach (GTK_TABLE (table5), button24, 0, 1, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button24, "clicked", G_CALLBACK (on_skip_button), err_dialog);
	
	button25 = gtk_button_new_with_mnemonic (_("Skip all"));
	gtk_widget_show (button25);
	gtk_widget_set_size_request (button25, 70, -1);
	gtk_table_attach (GTK_TABLE (table5), button25, 1, 2, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button25, "clicked", G_CALLBACK (on_skip_all_button), err_dialog);
	
	//button27 = gtk_button_new_with_mnemonic (_("Retry"));
	//gtk_widget_show (button27);
	//gtk_table_attach (GTK_TABLE (table5), button27, 0, 1, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	//g_signal_connect (button27, "clicked", G_CALLBACK (on_retry_button), err_dialog);
	
	button28 = gtk_button_new_with_mnemonic (_("Abort"));
	gtk_widget_show (button28);
	gtk_widget_set_size_request (button28, 70, -1);
	gtk_table_attach (GTK_TABLE (table5), button28, 2, 3, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button28, "clicked", G_CALLBACK (on_abort_button), err_dialog);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (err_dialog, err_dialog, "err_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (err_dialog, dialog_vbox4, "dialog_vbox4");
	GLADE_HOOKUP_OBJECT (err_dialog, table5, "table5");
	GLADE_HOOKUP_OBJECT (err_dialog, button24, "button24");
	GLADE_HOOKUP_OBJECT (err_dialog, button25, "button25");
	//GLADE_HOOKUP_OBJECT (err_dialog, button27, "button27");
	GLADE_HOOKUP_OBJECT (err_dialog, button28, "button28");
	GLADE_HOOKUP_OBJECT (err_dialog, label8, "label8");
	GLADE_HOOKUP_OBJECT_NO_REF (err_dialog, dialog_action_area4, "dialog_action_area4");

	gtk_widget_grab_focus   (button24);
	gtk_widget_grab_default (button24);
	
	return err_dialog;
}

void on_skip_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_SKIP);
}

void on_skip_all_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_CPY_SKIP_ALL);
}

void on_abort_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_ABORT);
}

//void on_retry_button (GtkWidget *button, gpointer user_data)
//{
//	gtk_dialog_response(GTK_DIALOG(user_data), OPF_CPY_RESUME);
//}
