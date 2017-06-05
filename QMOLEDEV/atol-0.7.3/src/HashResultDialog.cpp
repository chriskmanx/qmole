////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to display results of hash calculations for one or more files
//////////////////////////////////////////////////////////////////////////// 

#include "HashResultDialog.h"
#include "support.h"
#include "core/OpHash.h"

extern int g_nHashType;
extern std::vector<tFileHash> g_lstHashResults;
extern GtkWidget *atol_main;

HashResultDialog::HashResultDialog()
{
	Create();
}

HashResultDialog::~HashResultDialog()
{
	Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void HashResultDialog::Create()
{
	m_pDialog = create_hash_result_dialog ();

	//fill in the results
	GtkTreeIter iter;
	for(unsigned int i=0; i<g_lstHashResults.size(); i++)
	{
		//add item into the GUI result list 
		gtk_list_store_append(m_store, &iter);
		gtk_list_store_set (m_store, &iter,
							0, g_lstHashResults[i].strHash.c_str(),
							1, g_lstHashResults[i].strFile.c_str(),
						  -1);
	}

	//fix window title based on used algorithm
	const gchar *title = gtk_window_get_title(GTK_WINDOW(m_pDialog));
	String strTitle(title);
	switch(g_nHashType)
	{
	case 1:
		strTitle += " (CRC32)";
		break;
	case 2:
		strTitle += " (MD5)";
		break;
	case 3:
		strTitle += " (SHA1)";
		break;
	}
	gtk_window_set_title(GTK_WINDOW(m_pDialog), strTitle);
}

GtkWidget* HashResultDialog::create_hash_result_dialog ()
{
	GtkWidget *hash_result_dialog;
	GtkWidget *dialog_vbox8;
	GtkWidget *scrolledwindow14;
	GtkWidget *treeview4;
	GtkWidget *dialog_action_area8;
	GtkWidget *okbutton7;
	
	hash_result_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (hash_result_dialog), _("File hash results"));
	gtk_window_set_type_hint (GTK_WINDOW (hash_result_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (hash_result_dialog), GTK_WINDOW(atol_main));   //set parent

	dialog_vbox8 = GTK_DIALOG (hash_result_dialog)->vbox;
	gtk_widget_show (dialog_vbox8);
	
	scrolledwindow14 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow14);
	gtk_box_pack_start (GTK_BOX (dialog_vbox8), scrolledwindow14, TRUE, TRUE, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow14), GTK_SHADOW_IN);
	
	//define result list
	m_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
	treeview4 = gtk_tree_view_new_with_model(GTK_TREE_MODEL(m_store));
	gtk_widget_set_size_request(treeview4, 300, 120);
	gtk_widget_show (treeview4);
	gtk_container_add (GTK_CONTAINER (scrolledwindow14), treeview4);
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview4), TRUE);
	gtk_tree_view_set_headers_clickable (GTK_TREE_VIEW(treeview4), TRUE);
	gtk_tree_view_set_enable_search(GTK_TREE_VIEW(treeview4),FALSE);

	//add multiple text columns
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();

	GtkTreeViewColumn *col = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(col, _("Hash"));
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview4), col);
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 0, NULL);
	gtk_tree_view_column_set_resizable(col, TRUE);

	col = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(col, _("File"));
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview4), col);
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 1, NULL);
	gtk_tree_view_column_set_resizable(col, TRUE);

	dialog_action_area8 = GTK_DIALOG (hash_result_dialog)->action_area;
	gtk_widget_show (dialog_action_area8);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area8), GTK_BUTTONBOX_END);
	
	okbutton7 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton7);
	gtk_dialog_add_action_widget (GTK_DIALOG (hash_result_dialog), okbutton7, GTK_RESPONSE_OK);
	GTK_WIDGET_SET_FLAGS (okbutton7, GTK_CAN_DEFAULT);
	
	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (hash_result_dialog, hash_result_dialog, "hash_result_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (hash_result_dialog, dialog_vbox8, "dialog_vbox8");
	GLADE_HOOKUP_OBJECT (hash_result_dialog, scrolledwindow14, "scrolledwindow14");
	GLADE_HOOKUP_OBJECT (hash_result_dialog, treeview4, "treeview4");
	GLADE_HOOKUP_OBJECT_NO_REF (hash_result_dialog, dialog_action_area8, "dialog_action_area8");
	GLADE_HOOKUP_OBJECT (hash_result_dialog, okbutton7, "okbutton7");

	gtk_widget_grab_focus (okbutton7);	
	gtk_dialog_set_default_response (GTK_DIALOG (hash_result_dialog), GTK_RESPONSE_OK);

	return hash_result_dialog;
}

