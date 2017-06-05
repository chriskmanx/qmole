////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: General dialog for input of a single string (plain or as a password)
//////////////////////////////////////////////////////////////////////////// 

#include "GuiInputDlg.h"
#include "support.h"

extern GtkWidget *atol_main;
static void on_ok_button (GtkWidget *button, gpointer user_data);

GuiInputDlg::GuiInputDlg(bool bPassword, bool bCreate)
{
	m_bPasswordMode = bPassword;
	if(bCreate) Create();
}

GuiInputDlg::~GuiInputDlg()
{
	if(m_pDialog){
		gtk_widget_destroy(m_pDialog);
	}
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void GuiInputDlg::Create()
{
	m_pDialog = create_input_dialog();

	if(!m_strLabel.IsEmpty()){
		GtkWidget *label8 = lookup_widget(m_pDialog, "label8");
		gtk_label_set_text(GTK_LABEL(label8), m_strLabel.c_str());
	}
}

void GuiInputDlg::SetLabel(const char *szText)
{
	if(m_pDialog){
		GtkWidget *label8 = lookup_widget(m_pDialog, "label8");
		gtk_label_set_text(GTK_LABEL(label8), szText);
	}
	else
		m_strLabel = szText;
}

void GuiInputDlg::SetInput(const char *szText, bool bSelectAll)
{
	GtkWidget *entry1 = lookup_widget(m_pDialog, "entry1");
	gtk_entry_set_text(GTK_ENTRY(entry1), szText);
	if(bSelectAll)
		gtk_editable_select_region(GTK_EDITABLE(entry1), 0, -1);
}

const char *GuiInputDlg::GetInput()
{
	return m_strData.c_str();
}

GtkWidget* GuiInputDlg::create_input_dialog()
{
	GtkWidget *dialog1;
	GtkWidget *dialog_vbox2;
	GtkWidget *vbox8;
	GtkWidget *label8;
	GtkWidget *entry1;
	GtkWidget *dialog_action_area2;
	GtkWidget *cancelbutton2;
	GtkWidget *okbutton2;

	dialog1 = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (dialog1), "Atol");
	gtk_window_set_type_hint (GTK_WINDOW (dialog1), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (dialog1), TRUE);
	#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
		gtk_window_set_skip_taskbar_hint (GTK_WINDOW (dialog1), TRUE);
	#endif
	gtk_window_set_transient_for(GTK_WINDOW (dialog1), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_resizable (GTK_WINDOW (dialog1), FALSE);
	gtk_widget_set_size_request (dialog1, 300, -1);
	gtk_widget_realize(dialog1);
	gdk_window_set_decorations(dialog1->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE));

	dialog_vbox2 = GTK_DIALOG (dialog1)->vbox;
	gtk_widget_show (dialog_vbox2);

	vbox8 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox8);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox8, TRUE, TRUE, 0);

	label8 = gtk_label_new ("");
	gtk_misc_set_alignment(GTK_MISC(label8), 0.0f, 0.5f);
	gtk_widget_show (label8);
	gtk_box_pack_start (GTK_BOX (vbox8), label8, FALSE, FALSE, 0);

	entry1 = gtk_entry_new ();
	gtk_widget_show (entry1);
	gtk_entry_set_activates_default (GTK_ENTRY (entry1), TRUE);
	if(m_bPasswordMode)
		gtk_entry_set_visibility (GTK_ENTRY (entry1), FALSE);
	gtk_box_pack_start (GTK_BOX (vbox8), entry1, FALSE, FALSE, 0);

	dialog_action_area2 = GTK_DIALOG (dialog1)->action_area;
	gtk_widget_show (dialog_action_area2);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area2), GTK_BUTTONBOX_END);

	cancelbutton2 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton2);
	gtk_dialog_add_action_widget (GTK_DIALOG (dialog1), cancelbutton2, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton2, GTK_CAN_DEFAULT);

	okbutton2 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton2);
	gtk_container_add (GTK_CONTAINER(dialog_action_area2), okbutton2);
	GTK_WIDGET_SET_FLAGS (okbutton2, GTK_CAN_DEFAULT);

	g_signal_connect (okbutton2, "clicked", G_CALLBACK (on_ok_button), this);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog1, "dialog1");
	GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog_vbox2, "dialog_vbox2");
	GLADE_HOOKUP_OBJECT (dialog1, vbox8, "vbox8");
	GLADE_HOOKUP_OBJECT (dialog1, label8, "label8");
	GLADE_HOOKUP_OBJECT (dialog1, entry1, "entry1");
	GLADE_HOOKUP_OBJECT_NO_REF (dialog1, dialog_action_area2, "dialog_action_area2");
	GLADE_HOOKUP_OBJECT (dialog1, cancelbutton2, "cancelbutton2");
	GLADE_HOOKUP_OBJECT (dialog1, okbutton2, "okbutton2");

	gtk_widget_grab_focus (entry1);
	gtk_dialog_set_default_response (GTK_DIALOG (dialog1), GTK_RESPONSE_OK);
	gtk_widget_grab_default (okbutton2);

	return dialog1;
}

void on_ok_button (GtkWidget *button, gpointer user_data)
{
	GuiInputDlg *pDlg = (GuiInputDlg *)user_data;

	//remember data input
	GtkWidget *entry1 = lookup_widget(pDlg->m_pDialog, "entry1");
	pDlg->m_strData = gtk_entry_get_text(GTK_ENTRY(entry1));

	//exit
	gtk_dialog_response(GTK_DIALOG(pDlg->m_pDialog), GTK_RESPONSE_OK);
}
