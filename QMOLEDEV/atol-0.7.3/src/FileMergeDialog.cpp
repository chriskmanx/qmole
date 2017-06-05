////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define options for a file merge operation
//////////////////////////////////////////////////////////////////////////// 

#include "FileMergeDialog.h"
#include "support.h"
#include <stdio.h> //sprintf

extern GtkWidget *atol_main;
static void on_move_up (GtkMenuItem *menuitem, gpointer user_data);
static void on_move_down (GtkMenuItem *menuitem, gpointer user_data);
static void on_remove (GtkMenuItem *menuitem, gpointer user_data);

FileMergeDialog::FileMergeDialog()
{
	Create();
}

FileMergeDialog::~FileMergeDialog()
{
	Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void FileMergeDialog::Create()
{
	m_pDialog = create_file_merge_dialog ();

	//initial name	
	GtkWidget *entry10 = lookup_widget(m_pDialog, "entry10");
	gtk_entry_set_text(GTK_ENTRY(entry10), "untitled");
}

void FileMergeDialog::AddFile(const char *szName)
{
	//add item into the GUI result list 
	GtkTreeIter iter;
	gtk_list_store_append (m_store, &iter);
	gtk_list_store_set (m_store, &iter, 0, szName, -1);
}

int FileMergeDialog::GetFileCount()
{
	//TOFIX is there any faster way!!!
	gint count = 0;
	GtkTreeIter iter;
	
	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL(m_store), &iter))
	{
		count++;
		while (gtk_tree_model_iter_next (GTK_TREE_MODEL(m_store), &iter))
			count++;
	}

	return count;
}

const char *FileMergeDialog::GetDestFile()
{
	GtkWidget *entry10 = lookup_widget(m_pDialog, "entry10");
	return gtk_entry_get_text(GTK_ENTRY(entry10));
}

const char *FileMergeDialog::GetFileName(int nIdx)
{
	GtkTreePath *path1 = NULL;

	char szPath[20];
	sprintf(szPath, "%d", nIdx);
	path1 = gtk_tree_path_new_from_string (szPath);

	gchar *value = NULL;

	GtkTreeIter  iter;
	if(gtk_tree_model_get_iter(GTK_TREE_MODEL(m_store), &iter, path1))
	{
		gtk_tree_model_get (GTK_TREE_MODEL(m_store), &iter, 0, &value, -1);
	}
	gtk_tree_path_free(path1);

	return value;
}

GtkWidget* FileMergeDialog::create_file_merge_dialog ()
{
	GtkWidget *file_merge_dialog;
	GtkWidget *dialog_vbox10;
	GtkWidget *table13;
	GtkWidget *scrolledwindow15;
	GtkWidget *treeview5;
	GtkWidget *label23;
	GtkWidget *entry10;
	GtkWidget *vbox12;
	GtkWidget *button37;
	GtkWidget *button38;
	GtkWidget *button39;
	GtkWidget *dialog_action_area10;
	GtkWidget *cancelbutton9;
	GtkWidget *okbutton9;
	
	file_merge_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (file_merge_dialog), _("Merge files"));
	gtk_window_set_modal (GTK_WINDOW (file_merge_dialog), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (file_merge_dialog), TRUE);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (file_merge_dialog), TRUE);
#endif
	gtk_window_set_type_hint (GTK_WINDOW (file_merge_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (file_merge_dialog), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_position(GTK_WINDOW (file_merge_dialog), GTK_WIN_POS_CENTER_ON_PARENT);
	gtk_window_set_resizable (GTK_WINDOW (file_merge_dialog), FALSE);
	gtk_widget_realize(file_merge_dialog);
	gdk_window_set_decorations(file_merge_dialog->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE)); 

	dialog_vbox10 = GTK_DIALOG (file_merge_dialog)->vbox;
	gtk_widget_show (dialog_vbox10);
	
	table13 = gtk_table_new (3, 2, FALSE);
	gtk_widget_show (table13);
	gtk_box_pack_start (GTK_BOX (dialog_vbox10), table13, TRUE, TRUE, 0);
	
	scrolledwindow15 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow15);
	gtk_table_attach (GTK_TABLE (table13), scrolledwindow15, 0, 1, 0, 1,
		(GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		(GtkAttachOptions) (GTK_FILL), 0, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow15), GTK_SHADOW_IN);
	
	//define result list
	m_store = gtk_list_store_new (1, G_TYPE_STRING);
	treeview5 = gtk_tree_view_new_with_model(GTK_TREE_MODEL(m_store));
	gtk_widget_set_size_request(treeview5, 150, 150);
	gtk_widget_show (treeview5);
	gtk_container_add (GTK_CONTAINER (scrolledwindow15), treeview5);
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (treeview5), FALSE);
	gtk_tree_view_set_enable_search (GTK_TREE_VIEW (treeview5), FALSE);
	
	//add single text column
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();

	GtkTreeViewColumn *col = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(col, _("File"));
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview5), col);
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 0, NULL);
	gtk_tree_view_column_set_resizable(col, TRUE);

	label23 = gtk_label_new (_("Merge file name:"));
	gtk_widget_show (label23);
	gtk_table_attach (GTK_TABLE (table13), label23, 0, 1, 1, 2,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label23), 0, 0.5);
	
	entry10 = gtk_entry_new ();
	gtk_widget_show (entry10);
	gtk_table_attach (GTK_TABLE (table13), entry10, 0, 1, 2, 3,
		(GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	
	vbox12 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox12);
	gtk_table_attach (GTK_TABLE (table13), vbox12, 1, 2, 0, 1,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (GTK_FILL), 0, 1);
	
	button37 = gtk_button_new_with_mnemonic (_("Move Up"));
	gtk_widget_show (button37);
	gtk_box_pack_start (GTK_BOX (vbox12), button37, FALSE, FALSE, 1);
	//gtk_container_set_border_width (GTK_CONTAINER (button37), 3);
	
	button38 = gtk_button_new_with_mnemonic (_("Move Down"));
	gtk_widget_show (button38);
	gtk_box_pack_start (GTK_BOX (vbox12), button38, FALSE, FALSE, 1);
	//gtk_container_set_border_width (GTK_CONTAINER (button38), 3);
	
	button39 = gtk_button_new_with_mnemonic (_("Remove"));
	gtk_widget_show (button39);
	gtk_box_pack_start (GTK_BOX (vbox12), button39, FALSE, FALSE, 1);
	//gtk_container_set_border_width (GTK_CONTAINER (button39), 3);
	
	dialog_action_area10 = GTK_DIALOG (file_merge_dialog)->action_area;
	gtk_widget_show (dialog_action_area10);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area10), GTK_BUTTONBOX_END);
	
	cancelbutton9 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton9);
	gtk_dialog_add_action_widget (GTK_DIALOG (file_merge_dialog), cancelbutton9, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton9, GTK_CAN_DEFAULT);
	
	okbutton9 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton9);
	gtk_dialog_add_action_widget (GTK_DIALOG (file_merge_dialog), okbutton9, GTK_RESPONSE_OK);
	GTK_WIDGET_SET_FLAGS (okbutton9, GTK_CAN_DEFAULT);
	
	g_signal_connect (GTK_OBJECT (button37), "clicked", G_CALLBACK (on_move_up),	this);
	g_signal_connect (GTK_OBJECT (button38), "clicked", G_CALLBACK (on_move_down), this);
	g_signal_connect (GTK_OBJECT (button39), "clicked", G_CALLBACK (on_remove),	this);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (file_merge_dialog, file_merge_dialog, "file_merge_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (file_merge_dialog, dialog_vbox10, "dialog_vbox10");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, table13, "table13");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, scrolledwindow15, "scrolledwindow15");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, treeview5, "treeview5");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, label23, "label23");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, entry10, "entry10");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, vbox12, "vbox12");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, button37, "button37");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, button38, "button38");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, button39, "button39");
	GLADE_HOOKUP_OBJECT_NO_REF (file_merge_dialog, dialog_action_area10, "dialog_action_area10");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, cancelbutton9, "cancelbutton9");
	GLADE_HOOKUP_OBJECT (file_merge_dialog, okbutton9, "okbutton9");
	
	return file_merge_dialog;
}

void on_move_up (GtkMenuItem *menuitem, gpointer user_data)
{
	FileMergeDialog *pDlg = (FileMergeDialog *)user_data;
	GtkWidget *treeview5 = lookup_widget(pDlg->m_pDialog, "treeview5");

	//get iterator to selected node
	GtkTreeModel *model = gtk_tree_view_get_model((GtkTreeView *)treeview5);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview5));
	GtkTreeIter  iter;
	if(!gtk_tree_selection_get_selected(treesel, &model, &iter))
		return;

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(model, &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	if(nIdx > 0)
	{
		//calculate previous item
		nIdx --;

		char szPath[20];
		sprintf(szPath, "%d", nIdx);
		path1 = gtk_tree_path_new_from_string (szPath);

		GtkTreeIter  iter2;
		if(gtk_tree_model_get_iter(model, &iter2, path1))
		{
			//swap with previous item
			gtk_list_store_swap(GTK_LIST_STORE(model), &iter, &iter2);
		}
		gtk_tree_path_free(path1);
	}
}

void on_move_down (GtkMenuItem *menuitem, gpointer user_data)
{
	FileMergeDialog *pDlg = (FileMergeDialog *)user_data;
	GtkWidget *treeview5 = lookup_widget(pDlg->m_pDialog, "treeview5");

	//get iterator to selected node
	GtkTreeModel *model = gtk_tree_view_get_model((GtkTreeView *)treeview5);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview5));
	GtkTreeIter  iter;
	if(!gtk_tree_selection_get_selected(treesel, &model, &iter))
		return;

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(model, &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	//calculate next item
	nIdx ++;

	char szPath[20];
	sprintf(szPath, "%d", nIdx);
	path1 = gtk_tree_path_new_from_string (szPath);

	GtkTreeIter  iter2;
	if(gtk_tree_model_get_iter(model, &iter2, path1))
	{
		//swap with next item
		gtk_list_store_swap(GTK_LIST_STORE(model), &iter, &iter2);
	}
	gtk_tree_path_free(path1);
}

void on_remove (GtkMenuItem *menuitem, gpointer user_data)
{
	FileMergeDialog *pDlg = (FileMergeDialog *)user_data;
	GtkWidget *treeview5 = lookup_widget(pDlg->m_pDialog, "treeview5");

	//get iterator to selected node
	GtkTreeModel *model = gtk_tree_view_get_model((GtkTreeView *)treeview5);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview5));
	GtkTreeIter  iter;
	if(!gtk_tree_selection_get_selected(treesel, &model, &iter))
		return;

	//delete selected node from the treeview
	gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
}
