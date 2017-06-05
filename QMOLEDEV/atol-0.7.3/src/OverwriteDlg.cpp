////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to ask user a permission to overwrite files (inside file operation)
//////////////////////////////////////////////////////////////////////////// 

#include "OverwriteDlg.h"
#include "support.h"
#include "core/opcodes.h"
#include <gdk/gdkkeysyms.h>

extern GtkWidget *atol_main;
 
static void on_overwrite_button (GtkWidget *button, gpointer user_data);
static void on_overwrite_all_button (GtkWidget *button, gpointer user_data);
static void on_skip_button (GtkWidget *button, gpointer user_data);
static void on_skip_all_button (GtkWidget *button, gpointer user_data);
static void on_resume_button (GtkWidget *button, gpointer user_data);
static void on_rename_button (GtkWidget *button, gpointer user_data);
static void on_cancel_button (GtkWidget *button, gpointer user_data);
static gint dlg_keyboard_handler (GtkWidget *widget, GdkEventKey *event, gpointer data);

OverwriteDlg::OverwriteDlg()
{
}

OverwriteDlg::~OverwriteDlg()
{
	Destroy();
}

void OverwriteDlg::SetInfo(const char *szTxt)
{
	GtkWidget *textview7 = lookup_widget(m_pDialog, "textview7");
	GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(textview7));
	gtk_text_buffer_insert_at_cursor(buffer, szTxt, -1);
}

void OverwriteDlg::Create()
{
	m_pDialog = create_overwrite_dialog ();

	SetInfo(m_strInfo);
}

GtkWidget* OverwriteDlg::create_overwrite_dialog ()
{
	GtkWidget *overwrite_dialog;
	GtkWidget *dialog_vbox4;
	GtkWidget *table5;
	GtkWidget *button23;
	GtkWidget *button24;
	GtkWidget *button25;
	GtkWidget *button26;
	GtkWidget *button27;
	GtkWidget *button28;
	GtkWidget *button29;
	GtkWidget *scrolledwindow12;
	GtkWidget *textview7;
	GtkWidget *dialog_action_area4;

	overwrite_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (overwrite_dialog), _("Overwrite"));
	gtk_window_set_type_hint (GTK_WINDOW (overwrite_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (overwrite_dialog), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_resizable (GTK_WINDOW (overwrite_dialog), FALSE);
	gtk_widget_realize(overwrite_dialog);
	gdk_window_set_decorations(overwrite_dialog->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE));
	gtk_window_set_destroy_with_parent (GTK_WINDOW (overwrite_dialog), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (overwrite_dialog), TRUE);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (overwrite_dialog), TRUE);
#endif

	dialog_vbox4 = GTK_DIALOG (overwrite_dialog)->vbox;
	gtk_widget_show (dialog_vbox4);

	scrolledwindow12 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow12);
	gtk_box_pack_start(GTK_BOX(dialog_vbox4), scrolledwindow12, TRUE, TRUE, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow12), GTK_SHADOW_IN);
	
	textview7 = gtk_text_view_new ();
	gtk_widget_show (textview7);
	gtk_container_add (GTK_CONTAINER (scrolledwindow12), textview7);
	gtk_widget_set_size_request (textview7, -1, 70);
	gtk_widget_set_sensitive (textview7, FALSE);
	gtk_text_view_set_editable (GTK_TEXT_VIEW (textview7), FALSE);
	gtk_text_view_set_accepts_tab (GTK_TEXT_VIEW (textview7), FALSE);
	gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (textview7), FALSE);

	dialog_action_area4 = GTK_DIALOG (overwrite_dialog)->action_area;
	gtk_widget_show (dialog_action_area4);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area4), GTK_BUTTONBOX_END);
	
	table5 = gtk_table_new (4, 3, FALSE);
	gtk_widget_show (table5);
	gtk_box_pack_start (GTK_BOX (dialog_action_area4), table5, TRUE, TRUE, 0);
	gtk_table_set_row_spacings (GTK_TABLE (table5), 4);
	gtk_table_set_col_spacings (GTK_TABLE (table5), 3);
	
	button23 = gtk_button_new_with_mnemonic (_("_Overwrite"));
	gtk_widget_set_size_request (button23, 90, -1);
	gtk_widget_show (button23);
	gtk_table_attach (GTK_TABLE (table5), button23, 0, 1, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button23, "clicked", G_CALLBACK (on_overwrite_button), overwrite_dialog);

	button24 = gtk_button_new_with_mnemonic (_("_Skip"));
	gtk_widget_set_size_request (button24, 90, -1);
	gtk_widget_show (button24);
	gtk_table_attach (GTK_TABLE (table5), button24, 2, 3, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button24, "clicked", G_CALLBACK (on_skip_button), overwrite_dialog);
	
	button25 = gtk_button_new_with_mnemonic (_("S_kip all"));
	gtk_widget_set_size_request (button25, 90, -1);
	gtk_widget_show (button25);
	gtk_table_attach (GTK_TABLE (table5), button25, 0, 1, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button25, "clicked", G_CALLBACK (on_skip_all_button), overwrite_dialog);
	
	button26 = gtk_button_new_with_mnemonic (_("_Resume"));
	gtk_widget_set_size_request (button26, 90, -1);
	gtk_widget_show (button26);
	gtk_table_attach (GTK_TABLE (table5), button26, 1, 2, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button26, "clicked", G_CALLBACK (on_resume_button), overwrite_dialog);

	button27 = gtk_button_new_with_mnemonic (_("Re_name"));
	gtk_widget_set_size_request (button27, 90, -1);
	gtk_widget_show (button27);
	gtk_table_attach (GTK_TABLE (table5), button27, 2, 3, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button27, "clicked", G_CALLBACK (on_rename_button), overwrite_dialog);
	
	button28 = gtk_button_new_with_mnemonic (_("Cancel"));
	gtk_widget_set_size_request (button28, 90, -1);
	gtk_widget_show (button28);
	gtk_table_attach (GTK_TABLE (table5), button28, 2, 3, 3, 4, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button28, "clicked", G_CALLBACK (on_cancel_button), overwrite_dialog);
	
	button29 = gtk_button_new_with_mnemonic (_("Overwrite _all"));
	gtk_widget_set_size_request (button29, 90, -1);
	gtk_widget_show (button29);
	gtk_table_attach (GTK_TABLE (table5), button29, 1, 2, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button29, "clicked", G_CALLBACK (on_overwrite_all_button), overwrite_dialog);

	g_signal_connect (G_OBJECT (overwrite_dialog), "key_press_event", G_CALLBACK (dlg_keyboard_handler), this);
	
	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (overwrite_dialog, overwrite_dialog, "overwrite_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (overwrite_dialog, dialog_vbox4, "dialog_vbox4");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, table5, "table5");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, button23, "button23");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, button24, "button24");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, button25, "button25");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, button26, "button26");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, button27, "button27");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, button28, "button28");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, button29, "button29");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, scrolledwindow12, "scrolledwindow12");
	GLADE_HOOKUP_OBJECT (overwrite_dialog, textview7, "textview7");
	GLADE_HOOKUP_OBJECT_NO_REF (overwrite_dialog, dialog_action_area4, "dialog_action_area4");

	gtk_widget_grab_focus   (button29);
	gtk_widget_grab_default (button29);
	
	return overwrite_dialog;
}

void on_overwrite_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_OVERWRITE);
}

void on_overwrite_all_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_CPY_OVERWRITE_ALL);
}

void on_skip_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_SKIP);
}

void on_skip_all_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_CPY_SKIP_ALL);
}

void on_resume_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_CPY_RESUME);
}

void on_rename_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_CPY_RENAME);
}

void on_cancel_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_ABORT);
}

gint dlg_keyboard_handler(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	//Esc closes the dialog with skip response
	if( event->keyval == GDK_Escape )  
	{
		gtk_dialog_response(GTK_DIALOG(widget), OPF_SKIP);
		return TRUE;    //eat event (handled here)
	}
	return FALSE;
}
