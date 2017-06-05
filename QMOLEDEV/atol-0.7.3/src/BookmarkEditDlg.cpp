////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Window to show/edit file system bookmrks
//////////////////////////////////////////////////////////////////////////// 

#include "support.h"
#include "BookmarkEditDlg.h"

extern GtkWidget *atol_main;
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
static void on_ok_clicked (GtkMenuItem *menuitem, gpointer user_data);
static void on_add_clicked (GtkMenuItem *menuitem, gpointer user_data);
static void on_remove_clicked (GtkMenuItem *menuitem, gpointer user_data);
static void on_save_clicked (GtkMenuItem *menuitem, gpointer user_data);
static void on_item_clicked (GtkTreeView *treeview, gpointer user_data);

BookmarkEditDlg::BookmarkEditDlg()
{
	Create();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

BookmarkEditDlg::~BookmarkEditDlg()
{
	Destroy();
}

void BookmarkEditDlg::Create()
{
	m_pDialog = create_bookmark_manager_dialog ();

	//display data
	m_lstBooks.Load();
	RebuildList();
}

void BookmarkEditDlg::RebuildList()
{
	gtk_list_store_clear(m_store);

	for(int i=0; i<m_lstBooks.GetCount(); i++)
	{
		//add item into the GUI result list 
		GtkTreeIter iter;
		gtk_list_store_append (m_store, &iter);
		gtk_list_store_set (m_store, &iter, 0, m_lstBooks.GetBookTitle(i).c_str(), -1);
	}
}

int BookmarkEditDlg::GetSelectedIdx()
{
	GtkWidget *treeview6 = lookup_widget(m_pDialog, "treeview6");
	
	//get iterator to selected node
	GtkTreeModel *model = gtk_tree_view_get_model((GtkTreeView *)treeview6);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview6));
	GtkTreeIter  iter;
	if(!gtk_tree_selection_get_selected(treesel, &model, &iter))
		return -1;

	//calculate row number from iterator
	GtkTreePath *path = gtk_tree_model_get_path(model, &iter);
	int nIdx = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	return nIdx;
}

GtkWidget* BookmarkEditDlg::create_bookmark_manager_dialog ()
{
	GtkWidget *bookmark_manager_dialog;
	GtkWidget *dialog_vbox12;
	GtkWidget *table14;
	GtkWidget *label25;
	GtkWidget *label26;
	GtkWidget *entry11;
	GtkWidget *entry12;
	GtkWidget *scrolledwindow16;
	GtkWidget *treeview6;
	GtkWidget *vbox14;
	GtkWidget *button41;
	GtkWidget *button42;
	GtkWidget *button43;
	GtkWidget *dialog_action_area12;
	GtkWidget *cancelbutton11;
	GtkWidget *okbutton11;
	
	bookmark_manager_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (bookmark_manager_dialog), _("Bookmark manager"));
	gtk_window_set_type_hint (GTK_WINDOW (bookmark_manager_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	
	dialog_vbox12 = GTK_DIALOG (bookmark_manager_dialog)->vbox;
	gtk_widget_show (dialog_vbox12);
	
	table14 = gtk_table_new (4, 3, FALSE);
	gtk_widget_show (table14);
	gtk_box_pack_start (GTK_BOX (dialog_vbox12), table14, TRUE, TRUE, 0);
	
	label25 = gtk_label_new (_("Path:"));
	gtk_widget_show (label25);
	gtk_table_attach (GTK_TABLE (table14), label25, 0, 1, 3, 4,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label25), 0, 0.5);
	
	label26 = gtk_label_new (_("Title:"));
	gtk_widget_show (label26);
	gtk_table_attach (GTK_TABLE (table14), label26, 0, 1, 2, 3,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label26), 0, 0.5);
	
	entry11 = gtk_entry_new ();
	gtk_widget_show (entry11);
	gtk_table_attach (GTK_TABLE (table14), entry11, 1, 2, 2, 3,
		(GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	
	entry12 = gtk_entry_new ();
	gtk_widget_show (entry12);
	gtk_table_attach (GTK_TABLE (table14), entry12, 1, 2, 3, 4,
		(GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	
	scrolledwindow16 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow16);
	gtk_table_attach (GTK_TABLE (table14), scrolledwindow16, 0, 2, 0, 2,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow16), GTK_SHADOW_IN);

	m_store = gtk_list_store_new (1, G_TYPE_STRING);
	treeview6 = gtk_tree_view_new_with_model(GTK_TREE_MODEL(m_store));
	gtk_widget_set_size_request(treeview6, 150, 150);
	gtk_widget_show (treeview6);
	gtk_container_add (GTK_CONTAINER (scrolledwindow16), treeview6);
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (treeview6), FALSE);
	
	//add single text column
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();

	GtkTreeViewColumn *col = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(col, _("File"));
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview6), col);
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 0, NULL);
	gtk_tree_view_column_set_resizable(col, TRUE);

	vbox14 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox14);
	gtk_table_attach (GTK_TABLE (table14), vbox14, 2, 3, 0, 1,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (GTK_FILL), 0, 0);
	
	button41 = gtk_button_new_with_mnemonic (_("Add"));
	gtk_widget_show (button41);
	gtk_box_pack_start (GTK_BOX (vbox14), button41, FALSE, FALSE, 0);
	
	button42 = gtk_button_new_with_mnemonic (_("Remove"));
	gtk_widget_show (button42);
	gtk_box_pack_start (GTK_BOX (vbox14), button42, FALSE, FALSE, 0);
	
	button43 = gtk_button_new_with_mnemonic (_("Save"));
	gtk_widget_show (button43);
	gtk_box_pack_start (GTK_BOX (vbox14), button43, FALSE, FALSE, 0);
	
	dialog_action_area12 = GTK_DIALOG (bookmark_manager_dialog)->action_area;
	gtk_widget_show (dialog_action_area12);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area12), GTK_BUTTONBOX_END);
	
	cancelbutton11 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton11);
	gtk_dialog_add_action_widget (GTK_DIALOG (bookmark_manager_dialog), cancelbutton11, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton11, GTK_CAN_DEFAULT);
	
	okbutton11 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton11);
	gtk_container_add (GTK_CONTAINER(dialog_action_area12), okbutton11);
	GTK_WIDGET_SET_FLAGS (okbutton11, GTK_CAN_DEFAULT);

	g_signal_connect(okbutton11, "clicked",	G_CALLBACK (on_ok_clicked), this);
	g_signal_connect(button41, "clicked",	G_CALLBACK (on_add_clicked), this);
	g_signal_connect(button42, "clicked",	G_CALLBACK (on_remove_clicked), this);
	g_signal_connect(button43, "clicked",	G_CALLBACK (on_save_clicked), this);
	g_signal_connect (treeview6, "cursor-changed", G_CALLBACK (on_item_clicked), this);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (bookmark_manager_dialog, bookmark_manager_dialog, "bookmark_manager_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (bookmark_manager_dialog, dialog_vbox12, "dialog_vbox12");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, table14, "table14");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, label25, "label25");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, label26, "label26");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, entry11, "entry11");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, entry12, "entry12");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, scrolledwindow16, "scrolledwindow16");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, treeview6, "treeview6");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, vbox14, "vbox14");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, button41, "button41");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, button42, "button42");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, button43, "button43");
	GLADE_HOOKUP_OBJECT_NO_REF (bookmark_manager_dialog, dialog_action_area12, "dialog_action_area12");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, cancelbutton11, "cancelbutton11");
	GLADE_HOOKUP_OBJECT (bookmark_manager_dialog, okbutton11, "okbutton11");

	gtk_widget_grab_focus   (okbutton11);
	gtk_widget_grab_default (okbutton11);
	
	return bookmark_manager_dialog;
}

void on_add_clicked (GtkMenuItem *menuitem, gpointer user_data)
{
	BookmarkEditDlg *pDlg = (BookmarkEditDlg *)user_data;

	GtkWidget *entry11 = lookup_widget(pDlg->m_pDialog, "entry11");
	GtkWidget *entry12 = lookup_widget(pDlg->m_pDialog, "entry12");

	String strTitle = gtk_entry_get_text(GTK_ENTRY(entry11));
	String strPath  = gtk_entry_get_text(GTK_ENTRY(entry12));

	if(strTitle.IsEmpty() || strPath.IsEmpty())
	{
		gtkMessageBox(_("Both title and path must be filled!"));
		return;
	}

	int nPos = pDlg->m_lstBooks.FindBookByTitle(strTitle);
	if(nPos >= 0)
	{
		int nRes = gtkMessageBox(_("This will overwrite existing bookmark with same title!\nProceed?"), GTK_BUTTONS_YES_NO);
		if(GTK_RESPONSE_YES != nRes)
			return;

		pDlg->m_lstBooks.Remove(nPos);
	}

	pDlg->m_lstBooks.Insert(strTitle, strPath);
	pDlg->RebuildList();
}

void on_remove_clicked (GtkMenuItem *menuitem, gpointer user_data)
{
	BookmarkEditDlg *pDlg = (BookmarkEditDlg *)user_data;

	int nPos = pDlg->GetSelectedIdx();
	if(nPos >= 0)
	{
		pDlg->m_lstBooks.Remove(nPos);
		pDlg->RebuildList();
	}
}

void on_save_clicked (GtkMenuItem *menuitem, gpointer user_data)
{
	BookmarkEditDlg *pDlg = (BookmarkEditDlg *)user_data;

	GtkWidget *entry11 = lookup_widget(pDlg->m_pDialog, "entry11");
	GtkWidget *entry12 = lookup_widget(pDlg->m_pDialog, "entry12");

	String strTitle = gtk_entry_get_text(GTK_ENTRY(entry11));
	String strPath  = gtk_entry_get_text(GTK_ENTRY(entry12));

	if(strTitle.IsEmpty() || strPath.IsEmpty())
	{
		gtkMessageBox(_("Both title and path must be filled!"));
		return;
	}

	int nPos = pDlg->GetSelectedIdx();
	if(nPos >= 0)
	{
		pDlg->m_lstBooks.SetBookTitle(nPos, strTitle);
		pDlg->m_lstBooks.SetBookPath (nPos, strPath);
		pDlg->RebuildList();
	}
	else
	{
		gtkMessageBox(_("No entry selected!"));
		return;
	}
}

void on_item_clicked (GtkTreeView *treeview, gpointer user_data)
{
	BookmarkEditDlg *pDlg = (BookmarkEditDlg *)user_data;

	int nPos = pDlg->GetSelectedIdx();
	if(nPos >= 0)
	{
		//fill edit fields 
		GtkWidget *entry11 = lookup_widget(pDlg->m_pDialog, "entry11");
		GtkWidget *entry12 = lookup_widget(pDlg->m_pDialog, "entry12");
	
		gtk_entry_set_text(GTK_ENTRY(entry11), pDlg->m_lstBooks.GetBookTitle(nPos).c_str());
		gtk_entry_set_text(GTK_ENTRY(entry12), pDlg->m_lstBooks.GetBookPath(nPos).c_str());
	}
}

void on_ok_clicked (GtkMenuItem *menuitem, gpointer user_data)
{
	BookmarkEditDlg *pDlg = (BookmarkEditDlg *)user_data;
	pDlg->m_lstBooks.Save();
	pDlg->Destroy();
}

