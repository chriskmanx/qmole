////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Starting dialog for file delete operation
//////////////////////////////////////////////////////////////////////////// 

#include "DeleteStartDlg.h"
#include "support.h"
#include "../res/trash.xpm"
#include "../res/shred.xpm"
 
extern GtkWidget *atol_main;

DeleteStartDlg::DeleteStartDlg(bool bRecycle) :
 m_bRecycle (bRecycle)
{
	Create();
}

DeleteStartDlg::~DeleteStartDlg()
{
	Destroy();
}

void DeleteStartDlg::Create()
{
	m_pDialog = create_delete_start_dialog();
}

void DeleteStartDlg::SetList(const char *szList)
{
	GtkWidget *textview3 = lookup_widget(m_pDialog, "textview3");
	GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(textview3));
	gtk_text_buffer_insert_at_cursor(buffer, szList, -1);
}

GtkWidget* DeleteStartDlg::create_delete_start_dialog ()
{
	GtkWidget *delete_start_dialog;
	GtkWidget *dialog_vbox3;
	GtkWidget *vbox9;
	GtkWidget *label9;
	GtkWidget *scrolledwindow8;
	GtkWidget *textview3;
	GtkWidget *dialog_action_area3;
	GtkWidget *cancelbutton3;
	GtkWidget *okbutton3;
	GtkWidget *hbox;
	GtkWidget *image;
	GdkPixbuf *pixbuf;
	
	delete_start_dialog = gtk_dialog_new ();
	if(m_bRecycle)
		gtk_window_set_title (GTK_WINDOW (delete_start_dialog), _("Move to trash"));
	else
		gtk_window_set_title (GTK_WINDOW (delete_start_dialog), _("Delete"));

	gtk_window_set_modal (GTK_WINDOW (delete_start_dialog), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (delete_start_dialog), TRUE);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (delete_start_dialog), TRUE);
#endif
	gtk_window_set_type_hint (GTK_WINDOW (delete_start_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (delete_start_dialog), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_position(GTK_WINDOW (delete_start_dialog), GTK_WIN_POS_CENTER_ON_PARENT);
	gtk_window_set_resizable (GTK_WINDOW (delete_start_dialog), FALSE);
	gtk_widget_realize(delete_start_dialog);
	gdk_window_set_decorations(delete_start_dialog->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE)); 

	dialog_vbox3 = GTK_DIALOG (delete_start_dialog)->vbox;
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox3), 5);
	gtk_widget_show (dialog_vbox3);
	
	hbox = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (dialog_vbox3), hbox, FALSE, FALSE, 10);
	gtk_widget_show (hbox);

	if(m_bRecycle)
		pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&trash_xpm);
	else
		pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&shred_xpm);

	image = gtk_image_new_from_pixbuf (pixbuf);
	gtk_widget_show (image);
	gtk_box_pack_start (GTK_BOX (hbox), image, TRUE, TRUE, 5);

	label9 = gtk_label_new (_("Are you sure to delete following item(s)?"));
	gtk_box_pack_start (GTK_BOX (hbox), label9, FALSE, FALSE, 5);
	gtk_widget_set_usize (GTK_WIDGET (label9), 250, 1);
	gtk_misc_set_alignment (GTK_MISC (label9), 0.0, 0.5);
	gtk_widget_show (label9);

	vbox9 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox9);
	gtk_box_pack_start (GTK_BOX (dialog_vbox3), vbox9, TRUE, TRUE, 5); 
	
	scrolledwindow8 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow8);
	gtk_box_pack_start (GTK_BOX (vbox9), scrolledwindow8, TRUE, TRUE, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow8), GTK_SHADOW_IN);
	
	textview3 = gtk_text_view_new ();
	gtk_widget_show (textview3);
	gtk_container_add (GTK_CONTAINER (scrolledwindow8), textview3);
	gtk_text_view_set_editable      (GTK_TEXT_VIEW(textview3), FALSE);
	gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW(textview3), FALSE);
	
	dialog_action_area3 = GTK_DIALOG (delete_start_dialog)->action_area;
	gtk_widget_show (dialog_action_area3);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area3), GTK_BUTTONBOX_END);
	
	cancelbutton3 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton3);
	gtk_dialog_add_action_widget (GTK_DIALOG (delete_start_dialog), cancelbutton3, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton3, GTK_CAN_DEFAULT);
	
	okbutton3 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton3);
	gtk_dialog_add_action_widget (GTK_DIALOG (delete_start_dialog), okbutton3, GTK_RESPONSE_OK);
	GTK_WIDGET_SET_FLAGS (okbutton3, GTK_CAN_DEFAULT);
	
	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (delete_start_dialog, delete_start_dialog, "delete_start_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (delete_start_dialog, dialog_vbox3, "dialog_vbox3");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, vbox9, "vbox9");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, label9, "label9");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, scrolledwindow8, "scrolledwindow8");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, textview3, "textview3");
	GLADE_HOOKUP_OBJECT_NO_REF (delete_start_dialog, dialog_action_area3, "dialog_action_area3");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, cancelbutton3, "cancelbutton3");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, okbutton3, "okbutton3");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, hbox, "hbox");
	GLADE_HOOKUP_OBJECT (delete_start_dialog, image, "image");

	gtk_dialog_set_default_response (GTK_DIALOG (delete_start_dialog), GTK_RESPONSE_OK);

	return delete_start_dialog;
}

