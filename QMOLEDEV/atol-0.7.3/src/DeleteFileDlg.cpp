////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to ask permission to delete (read-only) files
//////////////////////////////////////////////////////////////////////////// 

#include "DeleteFileDlg.h"
#include "support.h"
#include "core/opcodes.h"
#include "core/IniFile.h"

bool g_bShowReadOnlyWarningDialog = true;

extern GtkWidget *atol_main;
static void on_delete_button (GtkWidget *button, gpointer user_data);
static void on_delete_all_button (GtkWidget *button, gpointer user_data);
static void on_skip_button (GtkWidget *button, gpointer user_data);
static void on_skip_all_button (GtkWidget *button, gpointer user_data);
static void on_abort_button (GtkWidget *button, gpointer user_data);
static void on_hide_dialog_check (GtkButton *menuitem, gpointer user_data);
const char *GetIniFile(); 

DeleteFileDlg::DeleteFileDlg()
{
	m_bHideDialog = false;
}

DeleteFileDlg::~DeleteFileDlg()
{
	Destroy();

	//save option to INI
	if(m_bHideDialog){
		IniFile ini;
		ini.Load(GetIniFile());
		ini.SetValue("Operation", "ReadOnlyWarning", m_bHideDialog);
		ini.Save();
	}
	g_bShowReadOnlyWarningDialog = !m_bHideDialog; 
}

void DeleteFileDlg::SetInfo(const char *szTxt)
{
	GtkWidget *textview7 = lookup_widget(m_pDialog, "textview7");
	GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(textview7));
	gtk_text_buffer_insert_at_cursor(buffer, szTxt, -1);
}

void DeleteFileDlg::Create()
{
	m_pDialog = create_dialog ();

	SetInfo(m_strInfo);
}

GtkWidget* DeleteFileDlg::create_dialog ()
{
	GtkWidget *delete_dialog;
	GtkWidget *dialog_vbox4;
	GtkWidget *table5;
	GtkWidget *button23;
	GtkWidget *button24;
	GtkWidget *button25;
	GtkWidget *button26;
	GtkWidget *button27;
	GtkWidget *checkbutton15;
	GtkWidget *scrolledwindow12;
	GtkWidget *textview7;
	GtkWidget *dialog_action_area4;

	delete_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (delete_dialog), _("Delete"));
	gtk_window_set_type_hint (GTK_WINDOW (delete_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (delete_dialog), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_resizable (GTK_WINDOW (delete_dialog), FALSE);
	gtk_widget_realize(delete_dialog);
	gdk_window_set_decorations(delete_dialog->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE)); 
	gtk_window_set_destroy_with_parent (GTK_WINDOW (delete_dialog), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (delete_dialog), TRUE);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (delete_dialog), TRUE);
#endif

	dialog_vbox4 = GTK_DIALOG (delete_dialog)->vbox;
	gtk_widget_show (dialog_vbox4);

	scrolledwindow12 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow12);
	gtk_box_pack_start(GTK_BOX(dialog_vbox4), scrolledwindow12, TRUE, TRUE, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow12), GTK_SHADOW_IN);
	
	textview7 = gtk_text_view_new ();
	gtk_widget_show (textview7);
	gtk_container_add (GTK_CONTAINER (scrolledwindow12), textview7);
	gtk_widget_set_size_request (textview7, -1, 40);
	gtk_widget_set_sensitive (textview7, FALSE);
	gtk_text_view_set_editable (GTK_TEXT_VIEW (textview7), FALSE);
	gtk_text_view_set_accepts_tab (GTK_TEXT_VIEW (textview7), FALSE);
	gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (textview7), FALSE);

	checkbutton15 = gtk_check_button_new_with_mnemonic (_("Do not ask this anymore"));
	gtk_widget_show (checkbutton15);
	gtk_box_pack_start (GTK_BOX (dialog_vbox4), checkbutton15, TRUE, TRUE, 0);
	g_signal_connect (checkbutton15, "clicked", G_CALLBACK (on_hide_dialog_check), this);

	dialog_action_area4 = GTK_DIALOG (delete_dialog)->action_area;
	gtk_widget_show (dialog_action_area4);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area4), GTK_BUTTONBOX_END);
	
	table5 = gtk_table_new (4, 2, FALSE);
	gtk_widget_show (table5);
	gtk_box_pack_start (GTK_BOX (dialog_action_area4), table5, TRUE, TRUE, 0);
	gtk_table_set_row_spacings (GTK_TABLE (table5), 4);
	gtk_table_set_col_spacings (GTK_TABLE (table5), 3);
	
	button23 = gtk_button_new_with_mnemonic (_("Delete"));
	gtk_widget_set_size_request (button23, 70, -1);
	gtk_widget_show (button23);
	gtk_table_attach (GTK_TABLE (table5), button23, 0, 1, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button23, "clicked", G_CALLBACK (on_delete_button), delete_dialog);

	button24 = gtk_button_new_with_mnemonic (_("Delete All"));
	gtk_widget_set_size_request (button24, 70, -1);
	gtk_widget_show (button24);
	gtk_table_attach (GTK_TABLE (table5), button24, 1, 2, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button24, "clicked", G_CALLBACK (on_delete_all_button), delete_dialog);
	
	button25 = gtk_button_new_with_mnemonic (_("Skip"));
	gtk_widget_set_size_request (button25, 70, -1);
	gtk_widget_show (button25);
	gtk_table_attach (GTK_TABLE (table5), button25, 2, 3, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button25, "clicked", G_CALLBACK (on_skip_button), delete_dialog);
	GTK_WIDGET_SET_FLAGS (button25, GTK_CAN_DEFAULT);

	button26 = gtk_button_new_with_mnemonic (_("Skip All"));
	gtk_widget_set_size_request (button26, 70, -1);
	gtk_widget_show (button26);
	gtk_table_attach (GTK_TABLE (table5), button26, 1, 2, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button26, "clicked", G_CALLBACK (on_skip_all_button), delete_dialog);

	button27 = gtk_button_new_with_mnemonic (_("Abort"));
	gtk_widget_set_size_request (button27, 70, -1);
	gtk_widget_show (button27);
	gtk_table_attach (GTK_TABLE (table5), button27, 2, 3, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	g_signal_connect (button27, "clicked", G_CALLBACK (on_abort_button), delete_dialog);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (delete_dialog, delete_dialog, "delete_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (delete_dialog, dialog_vbox4, "dialog_vbox4");
	GLADE_HOOKUP_OBJECT (delete_dialog, table5, "table5");
	GLADE_HOOKUP_OBJECT (delete_dialog, button23, "button23");
	GLADE_HOOKUP_OBJECT (delete_dialog, button24, "button24");
	GLADE_HOOKUP_OBJECT (delete_dialog, button25, "button25");
	GLADE_HOOKUP_OBJECT (delete_dialog, button26, "button26");
	GLADE_HOOKUP_OBJECT (delete_dialog, button27, "button27");
	GLADE_HOOKUP_OBJECT (delete_dialog, scrolledwindow12, "scrolledwindow12");
	GLADE_HOOKUP_OBJECT (delete_dialog, textview7, "textview7");
	GLADE_HOOKUP_OBJECT (delete_dialog, checkbutton15, "checkbutton15"); 
	GLADE_HOOKUP_OBJECT_NO_REF (delete_dialog, dialog_action_area4, "dialog_action_area4");
	
	gtk_widget_grab_focus   (button24);
	gtk_widget_grab_default (button24);

	return delete_dialog;
}

void on_delete_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_DELETE);
}

void on_delete_all_button (GtkWidget *button, gpointer user_data)
{
	gtk_dialog_response(GTK_DIALOG(user_data), OPF_DEL_ALL_RO_FILES);
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

void on_hide_dialog_check (GtkButton *menuitem, gpointer user_data)
{
	DeleteFileDlg *pDlg = (DeleteFileDlg *)user_data;
	GtkWidget *hide_check = lookup_widget(pDlg->m_pDialog, "checkbutton15");
	pDlg->m_bHideDialog = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(hide_check)) ? true : false;
}
