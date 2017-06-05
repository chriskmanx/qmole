////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define options for a file search operation
//////////////////////////////////////////////////////////////////////////// 

#include "support.h"
#include "FileSearchDlg.h"
#include "core/PathName.h"
#include "DualPanel.h"
#include "core/IniFile.h"
#include "core/debug.h"

extern GtkWidget *atol_main;
extern DualPanel g_dp;

int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
static void on_search_clicked(GtkButton *button, gpointer user_data);
static void on_find_text_clicked(GtkButton *button, gpointer user_data);
static void on_find_size_clicked(GtkButton *button, gpointer user_data);
static void on_result_item_clicked (GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col, gpointer userdata);
int monitor_timer(gpointer data);
const char *GetIniFile();

FileSearchDlg::FileSearchDlg()
{
	m_pDialog = NULL;
	m_pObjThread = NULL;
	m_store = NULL;
	m_nTimer = 0;
	m_nResultListSize = 0;
	m_bSearching = false;
}

FileSearchDlg::~FileSearchDlg()
{
	Stop();	//ensure search is stopped
	Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void FileSearchDlg::Destroy()
{
	if(NULL == m_pDialog)
		return;	//already done
	
	GtkWidget *entry3 = lookup_widget(m_pDialog, "entry3");
	GtkWidget *recurse_dir  = lookup_widget(m_pDialog, "checkbutton1");
	GtkWidget *chk_content  = lookup_widget(m_pDialog, "checkbutton2");
	GtkWidget *content_text = lookup_widget(m_pDialog, "entry5");
	GtkWidget *content_case = lookup_widget(m_pDialog, "checkbutton3");

	//save some some data to the INI
	IniFile file;
	file.Load(GetIniFile());

	//save name pattern
	std::string strNamePtrn;
	strNamePtrn = gtk_entry_get_text(GTK_ENTRY(entry3));
	file.SetValue("Search", "NamePattern", strNamePtrn.c_str());

	//store recursive search flag
	bool bSearchSubdirs = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(recurse_dir))>0;
	file.SetValue("Search", "SearchSubDirs", bSearchSubdirs);

	//store search by content state
	bool bSearchByContent = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(chk_content))>0;
	file.SetValue("Search", "SearchByContent", bSearchByContent);

	//store content case
	bool bContentCaseSensitive = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(content_case))>0;
	file.SetValue("Search", "ContentCase", bContentCaseSensitive);
		
	//store content text
	std::string strContentText;
	strContentText = gtk_entry_get_text(GTK_ENTRY(content_text));
	file.SetValue("Search", "ContentText", strContentText.c_str());
		
	file.Save();

	if(m_pDialog)
		gtk_widget_destroy(m_pDialog);
	m_pDialog = NULL;
}

void FileSearchDlg::Create()
{
	//init some data from INI
	IniFile file;
	file.Load(GetIniFile());

	//restore name pattern
	std::string strNamePtrn;
	file.GetValue("Search", "NamePattern", strNamePtrn, "");
	if(strNamePtrn.empty())
	{
		//set default name pattern
	#ifdef _WIN32
		m_strInitialName = "*.*";
	#else
		m_strInitialName = "*";
	#endif
	}
	else
		m_strInitialName = strNamePtrn.c_str();

	//create the dialog
	m_pDialog = create_file_search();

	GtkWidget *recurse_dir  = lookup_widget(m_pDialog, "checkbutton1");
	GtkWidget *chk_content  = lookup_widget(m_pDialog, "checkbutton2");
	GtkWidget *content_text = lookup_widget(m_pDialog, "entry5");
	GtkWidget *content_case = lookup_widget(m_pDialog, "checkbutton3");

	//restore recursive search flag
	bool bSearchSubdirs;
	file.GetValue("Search", "SearchSubDirs", bSearchSubdirs, 0);
	if(bSearchSubdirs)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(recurse_dir), TRUE);

	//restore search by content state
	bool bSearchByContent;
	file.GetValue("Search", "SearchByContent", bSearchByContent, 0);
	if(bSearchByContent)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(chk_content), TRUE);

	//restore content case
	bool bContentCaseSensitive;
	file.GetValue("Search", "ContentCase", bContentCaseSensitive, 0);
	if(bContentCaseSensitive)
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(content_case), TRUE);

	//restore content text
	std::string strContentText;
	file.GetValue("Search", "ContentText", strContentText, "");
	if(!strContentText.empty())
		gtk_entry_set_text(GTK_ENTRY(content_text), strContentText.c_str());
}

void FileSearchDlg::KillTimer()
{
	//destroy timer
	if(m_nTimer > 0)
		g_source_remove (m_nTimer);
	m_nTimer = 0;
}

GtkWidget* FileSearchDlg::create_file_search ()
{
	GtkWidget *file_search;
	GtkWidget *dialog_vbox5;
	GtkWidget *vbox10;
	GtkWidget *notebook1;
	GtkWidget *table6;
	GtkWidget *label13;
	GtkWidget *label15;
	GtkWidget *entry4;
	GtkWidget *button30;
	GtkWidget *entry3;
	GtkWidget *checkbutton1;
	GtkWidget *label10;
	GtkWidget *table7;
	GtkWidget *checkbutton2;
	GtkWidget *entry5;
	GtkWidget *checkbutton3;
	GtkWidget *label11;
	GtkWidget *table10;
	GtkWidget *checkbutton10;
	GtkWidget *checkbutton11;
	GtkWidget *entry7;
	GtkWidget *combobox3;
	GtkWidget *combobox4;
	GtkWidget *label18;
	GtkWidget *scrolledwindow13;
	GtkWidget *treeview3;
	GtkWidget *dialog_action_area5;
	GtkWidget *cancelbutton4;
	GtkWidget *okbutton4;
	GtkWidget *label19;
	
	file_search = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (file_search), _("File Search"));
	gtk_window_set_modal (GTK_WINDOW (file_search), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (file_search), TRUE);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (file_search), TRUE);
#endif
	gtk_window_set_type_hint (GTK_WINDOW (file_search), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (file_search), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_position(GTK_WINDOW (file_search), GTK_WIN_POS_CENTER_ON_PARENT);
	gtk_window_set_resizable (GTK_WINDOW (file_search), FALSE);
	gtk_widget_realize(file_search);
	gdk_window_set_decorations(file_search->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE)); 

	dialog_vbox5 = GTK_DIALOG (file_search)->vbox;
	gtk_widget_show (dialog_vbox5);
	
	vbox10 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox10);
	gtk_box_pack_start (GTK_BOX (dialog_vbox5), vbox10, TRUE, TRUE, 0);
	
	notebook1 = gtk_notebook_new ();
	gtk_widget_show (notebook1);
	gtk_box_pack_start (GTK_BOX (vbox10), notebook1, FALSE, TRUE, 0);
	
	table6 = gtk_table_new (3, 3, FALSE);
	gtk_widget_show (table6);
	gtk_container_add (GTK_CONTAINER (notebook1), table6);
	
	label13 = gtk_label_new (_("Name:"));
	gtk_widget_show (label13);
	gtk_table_attach (GTK_TABLE (table6), label13, 0, 1, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label13), 0, 0.5);
	
	label15 = gtk_label_new (_("Path:"));
	gtk_widget_show (label15);
	gtk_table_attach (GTK_TABLE (table6), label15, 0, 1, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label15), 0, 0.5);
	
	entry4 = gtk_entry_new ();
	gtk_widget_show (entry4);
	gtk_entry_set_activates_default (GTK_ENTRY (entry4), TRUE);
	gtk_table_attach (GTK_TABLE (table6), entry4, 1, 2, 1, 2, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	button30 = gtk_button_new_with_mnemonic (_("Browse"));
	//gtk_widget_show (button30); //TOFIX temporary hidden
	gtk_table_attach (GTK_TABLE (table6), button30, 2, 3, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	entry3 = gtk_entry_new ();
	gtk_widget_show (entry3);
	gtk_entry_set_activates_default (GTK_ENTRY (entry3), TRUE);
	gtk_table_attach (GTK_TABLE (table6), entry3, 1, 3, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	checkbutton1 = gtk_check_button_new_with_mnemonic (_("Search subdirectories"));
	gtk_widget_show (checkbutton1);
	gtk_table_attach (GTK_TABLE (table6), checkbutton1, 0, 3, 2, 3,	(GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	label10 = gtk_label_new (_("General"));
	gtk_widget_show (label10);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 0), label10);
	
	table7 = gtk_table_new (2, 3, FALSE);
	gtk_widget_show (table7);
	gtk_container_add (GTK_CONTAINER (notebook1), table7);
	
	checkbutton2 = gtk_check_button_new_with_mnemonic (_("Find text:"));
	gtk_widget_show (checkbutton2);
	gtk_table_attach (GTK_TABLE (table7), checkbutton2, 0, 1, 0, 1,	(GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	entry5 = gtk_entry_new ();
	gtk_widget_show (entry5);
	gtk_entry_set_activates_default (GTK_ENTRY (entry5), TRUE);
	gtk_table_attach (GTK_TABLE (table7), entry5, 1, 3, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_widget_set_sensitive (entry5, FALSE);
	
	checkbutton3 = gtk_check_button_new_with_mnemonic (_("Case sensitive"));
	gtk_widget_show (checkbutton3);
	gtk_table_attach (GTK_TABLE (table7), checkbutton3, 1, 3, 1, 2,	(GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_widget_set_sensitive (checkbutton3, FALSE);
	
	label11 = gtk_label_new (_("Contents"));
	gtk_widget_show (label11);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 1), label11);
	
	table10 = gtk_table_new (3, 4, FALSE);
	gtk_widget_show (table10);
	gtk_container_add (GTK_CONTAINER (notebook1), table10);
	
	checkbutton10 = gtk_check_button_new_with_mnemonic (_("Size"));
	gtk_widget_show (checkbutton10);
	gtk_table_attach (GTK_TABLE (table10), checkbutton10, 0, 1, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
		
	checkbutton11 = gtk_check_button_new_with_mnemonic (_("Attributes"));
	//gtk_widget_show (checkbutton11);  //TOFIX temporary
	gtk_table_attach (GTK_TABLE (table10), checkbutton11, 0, 1, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	entry7 = gtk_entry_new ();
	gtk_widget_show (entry7);
	gtk_entry_set_activates_default (GTK_ENTRY (entry7), TRUE);
	gtk_table_attach (GTK_TABLE (table10), entry7, 2, 3, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_widget_set_sensitive (entry7, FALSE);

	combobox3 = gtk_combo_box_new_text ();
	gtk_widget_show (combobox3);
	gtk_table_attach (GTK_TABLE (table10), combobox3, 1, 2, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (GTK_FILL), 0, 0);
	gtk_combo_box_append_text (GTK_COMBO_BOX (combobox3), "<");
	gtk_combo_box_append_text (GTK_COMBO_BOX (combobox3), "=");
	gtk_combo_box_append_text (GTK_COMBO_BOX (combobox3), ">");
	gtk_combo_box_set_active (GTK_COMBO_BOX (combobox3), 0);
	gtk_widget_set_sensitive (combobox3, FALSE);

	combobox4 = gtk_combo_box_new_text ();
	gtk_widget_show (combobox4);
	gtk_table_attach (GTK_TABLE (table10), combobox4, 3, 4, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (GTK_FILL), 0, 0);
	gtk_combo_box_append_text (GTK_COMBO_BOX (combobox4), _("byte(s)"));
	gtk_combo_box_append_text (GTK_COMBO_BOX (combobox4), _("kB"));
	gtk_combo_box_append_text (GTK_COMBO_BOX (combobox4), _("MB"));
	gtk_combo_box_append_text (GTK_COMBO_BOX (combobox4), _("GB"));
	gtk_combo_box_set_active (GTK_COMBO_BOX (combobox4), 0);
	gtk_widget_set_sensitive (combobox4, FALSE);

	label18 = gtk_label_new (_("Size/attributes"));
	gtk_widget_show (label18);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 2), label18);
	
	//result list is hidden by default
	scrolledwindow13 = gtk_scrolled_window_new (NULL, NULL);
	gtk_box_pack_start (GTK_BOX (vbox10), scrolledwindow13, TRUE, TRUE, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow13), GTK_SHADOW_IN);
	
	//define result list
	m_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  	treeview3 = gtk_tree_view_new_with_model(GTK_TREE_MODEL(m_store));
	gtk_widget_set_size_request(treeview3, -1, 120);

	//g_object_unref (G_OBJECT (store));  //tree now holds reference
	gtk_widget_show (treeview3);
	gtk_container_add (GTK_CONTAINER (scrolledwindow13), treeview3);
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview3), TRUE);
	gtk_tree_view_set_headers_clickable (GTK_TREE_VIEW(treeview3), TRUE);
	gtk_tree_view_set_enable_search(GTK_TREE_VIEW(treeview3),FALSE);

	//add label to display result count
	label19 = gtk_label_new ("");
	gtk_misc_set_alignment(GTK_MISC(label19), 0.0f, 0.5f);  // horizontal: left aligned, vertical: centered
	gtk_widget_show (label19);
	gtk_box_pack_start (GTK_BOX (vbox10), label19, TRUE, TRUE, 0);
	//gtk_container_add (GTK_CONTAINER (scrolledwindow13), label19);

	//add multiple text coumnts
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();

	GtkTreeViewColumn *col = gtk_tree_view_column_new();
	/*gtk_tree_view_column_set_title(col, _("Name"));
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview3), col);
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 0, NULL);

	col = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(col, _("Ext"));
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview3), col);
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 1, NULL);

	col = gtk_tree_view_column_new();*/
	gtk_tree_view_column_set_title(col, _("Path"));
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview3), col);
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 0, NULL);

	dialog_action_area5 = GTK_DIALOG (file_search)->action_area;
	gtk_widget_show (dialog_action_area5);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area5), GTK_BUTTONBOX_END);
	
	cancelbutton4 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton4);
	gtk_dialog_add_action_widget (GTK_DIALOG (file_search), cancelbutton4, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton4, GTK_CAN_DEFAULT);
	
	okbutton4 = gtk_button_new_with_label(_("Search"));
	gtk_widget_show (okbutton4);
	gtk_container_add(GTK_CONTAINER(dialog_action_area5), okbutton4);
	GTK_WIDGET_SET_FLAGS (okbutton4, GTK_CAN_DEFAULT);

	g_signal_connect(okbutton4,     "clicked",	G_CALLBACK (on_search_clicked), this);
	g_signal_connect(checkbutton2,  "clicked",	G_CALLBACK (on_find_text_clicked), this);
	g_signal_connect(checkbutton10, "clicked",	G_CALLBACK (on_find_size_clicked), this);
	g_signal_connect(treeview3,     "row-activated",G_CALLBACK (on_result_item_clicked), this);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (file_search, file_search, "file_search");
	GLADE_HOOKUP_OBJECT_NO_REF (file_search, dialog_vbox5, "dialog_vbox5");
	GLADE_HOOKUP_OBJECT (file_search, vbox10, "vbox10");
	GLADE_HOOKUP_OBJECT (file_search, notebook1, "notebook1");
	GLADE_HOOKUP_OBJECT (file_search, table6, "table6");
	GLADE_HOOKUP_OBJECT (file_search, label13, "label13");
	GLADE_HOOKUP_OBJECT (file_search, label15, "label15");
	GLADE_HOOKUP_OBJECT (file_search, entry4, "entry4");
	GLADE_HOOKUP_OBJECT (file_search, button30, "button30");
	GLADE_HOOKUP_OBJECT (file_search, entry3, "entry3");
	GLADE_HOOKUP_OBJECT (file_search, checkbutton1, "checkbutton1");
	GLADE_HOOKUP_OBJECT (file_search, label10, "label10");
	GLADE_HOOKUP_OBJECT (file_search, table7, "table7");
	GLADE_HOOKUP_OBJECT (file_search, checkbutton2, "checkbutton2");
	GLADE_HOOKUP_OBJECT (file_search, entry5, "entry5");
	GLADE_HOOKUP_OBJECT (file_search, checkbutton3, "checkbutton3");
	GLADE_HOOKUP_OBJECT (file_search, label11, "label11");
	GLADE_HOOKUP_OBJECT (file_search, table10, "table10");
	GLADE_HOOKUP_OBJECT (file_search, checkbutton10, "checkbutton10");
	GLADE_HOOKUP_OBJECT (file_search, checkbutton11, "checkbutton11");
	GLADE_HOOKUP_OBJECT (file_search, entry7, "entry7");
	GLADE_HOOKUP_OBJECT (file_search, combobox3, "combobox3");
	GLADE_HOOKUP_OBJECT (file_search, combobox4, "combobox4");
	GLADE_HOOKUP_OBJECT (file_search, label18, "label18");
	GLADE_HOOKUP_OBJECT (file_search, label19, "label19");
	GLADE_HOOKUP_OBJECT (file_search, scrolledwindow13, "scrolledwindow13");
	GLADE_HOOKUP_OBJECT (file_search, treeview3, "treeview3");
	GLADE_HOOKUP_OBJECT_NO_REF (file_search, dialog_action_area5, "dialog_action_area5");
	GLADE_HOOKUP_OBJECT (file_search, cancelbutton4, "cancelbutton4");
	GLADE_HOOKUP_OBJECT (file_search, okbutton4, "okbutton4");
	
	//set initial directory, ...
	gtk_entry_set_text(GTK_ENTRY(entry4), m_strInitialDir);
	gtk_entry_set_text(GTK_ENTRY(entry3), m_strInitialName);

	gtk_widget_grab_focus (entry3);
	gtk_widget_grab_default (okbutton4);

	return file_search;
}

void FileSearchDlg::Start()
{
	TRACE("FileSearchDlg::Start\n");

	//TOFIX rename also original widgets to have better names
	GtkWidget *name_pattern = lookup_widget(m_pDialog, "entry3");
	GtkWidget *search_dir   = lookup_widget(m_pDialog, "entry4");
	GtkWidget *recurse_dir  = lookup_widget(m_pDialog, "checkbutton1");
	GtkWidget *chk_content  = lookup_widget(m_pDialog, "checkbutton2");
	GtkWidget *content_text = lookup_widget(m_pDialog, "entry5");
	GtkWidget *content_case = lookup_widget(m_pDialog, "checkbutton3");
	GtkWidget *chk_size     = lookup_widget(m_pDialog, "checkbutton10");
	GtkWidget *size_amount  = lookup_widget(m_pDialog, "entry7");
	GtkWidget *size_relation= lookup_widget(m_pDialog, "combobox3");
	GtkWidget *size_unit    = lookup_widget(m_pDialog, "combobox4");
	GtkWidget *result_list  = lookup_widget(m_pDialog, "scrolledwindow13");
	GtkWidget *search_btn   = lookup_widget(m_pDialog, "okbutton4");

	//TOFIX check if search is correctly defined (dir/name not empty) ?? 
	Stop();	//ensure previous search was stopped
	
	m_bSearching = true;

	//prepare GUI
	gtk_widget_show (result_list);
	gtk_button_set_label(GTK_BUTTON(search_btn), _("Stop"));
	gtk_list_store_clear(m_store);	//clear previous results
	m_nResultListSize = 0;
	m_lstResults.Clear();

	//fill search data and start the thread
	m_pObjThread = new FileSearchThread;

	m_pObjThread->m_pCaller      = this;
	m_pObjThread->m_strDirectory = gtk_entry_get_text(GTK_ENTRY(search_dir));
	m_pObjThread->m_bRecursive   = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(recurse_dir))>0;
	
	String strShowName, strHideName;
	strShowName = gtk_entry_get_text(GTK_ENTRY(name_pattern));

	TRACE("FileSearchDlg::Start search subdirs:%d\n", m_pObjThread->m_bRecursive);
	TRACE("FileSearchDlg::Start set name patterns:%s,%s\n", strShowName.c_str(), strHideName.c_str());

	m_pObjThread->m_objInfo.SetNameGroup(strShowName, strHideName);

	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(chk_content)))
	{
		String strText = gtk_entry_get_text(GTK_ENTRY(content_text));
		bool bCase = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(content_case))>0;
		m_pObjThread->m_objInfo.SetContentsGroup(strText, bCase);
	}

	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(chk_size)))
	{
		String strAmount = gtk_entry_get_text(GTK_ENTRY(size_amount));
		INT64 nAmount = atoi(strAmount);
		int nRelation = gtk_combo_box_get_active(GTK_COMBO_BOX(size_relation));
		int nUnit     = gtk_combo_box_get_active(GTK_COMBO_BOX(size_unit));

		m_pObjThread->m_objInfo.SetSizeGroup(nAmount, nRelation, nUnit);
	}

	m_evOpDone.Reset();
	m_evRefreshList.Reset();

	TRACE("FileSearchDlg::Start run search thread\n");
	m_pObjThread->Run();

	//start timer to handle thread events for GUI
	m_nTimer = g_timeout_add (100, monitor_timer, this); 
}

void FileSearchDlg::Stop()
{
	TRACE("FileSearchDlg::Stop\n");
	m_bSearching = false;

	if(NULL != m_pObjThread)
	{
		GtkWidget *search_btn  = lookup_widget(m_pDialog, "okbutton4");
		//GtkWidget *result_list = lookup_widget(m_pDialog, "scrolledwindow13");

		//TOFIX prepare GUI
		//shrink dialog size (hide result list)
		//int nHeight, nWidth, nResultHeight;
		//nHeight = nWidth = nResultHeight = 0;
		//gtk_window_get_size(GTK_WINDOW(m_pDialog), &nWidth, &nHeight);
		//nResultHeight = GTK_WIDGET(result_list)->allocation.height;
		//gtk_widget_hide (result_list);
		//gtk_window_resize(GTK_WINDOW(m_pDialog), nWidth, nHeight-nResultHeight);
		
		gtk_button_set_label(GTK_BUTTON(search_btn), _("Search"));

		TRACE("FileSearchDlg::Stop - aborting thread ... \n");
		m_pObjThread->Abort();
		TRACE("FileSearchDlg::Stop - waiting for thread ... \n");
		m_pObjThread->Wait();
		delete m_pObjThread;
		m_pObjThread = NULL;
		TRACE(" done!\n");
	}

	TRACE("FileSearchDlg::Stop end\n");
}

void FileSearchDlg::OnSearchDone()
{
	TRACE("FileSearchDlg::OnSearchDone");
	KillTimer();
	Stop();		// cleanup thread data

	GtkWidget *search_btn = lookup_widget(m_pDialog, "okbutton4");
	gtk_button_set_label(GTK_BUTTON(search_btn), _("Search"));
	//TOFIX reenable some items ?

	//refresh result count label
	String strMsg;
	strMsg.Printf(_("Search finished: %d items found"), m_nResultListSize);
	GtkWidget *label19 = lookup_widget(m_pDialog, "label19");
	gtk_label_set_text(GTK_LABEL(label19), strMsg.c_str());
}

void FileSearchDlg::AddItem(VfsItem &item)
{
	TRACE("FileSearchDlg::AddItem %s\n", item.GetName().c_str());

	//insert into result list
	m_objListAccess.Lock();

	m_lstResults.Insert(item);

	m_objListAccess.Unlock();

	//m_nFilesFound ++;
	m_evRefreshList.Set();	// set signal
}

void on_search_clicked(GtkButton *button, gpointer user_data)
{
	TRACE("on_search_clicked\n");

	FileSearchDlg *pDlg = (FileSearchDlg *)user_data;
	if(NULL != pDlg)
	{
		if(pDlg->m_bSearching)
			pDlg->Stop();	//stop search
		else
			pDlg->Start();	//start search
	}
}

void on_find_text_clicked(GtkButton *button, gpointer user_data)
{
	FileSearchDlg *pDlg = (FileSearchDlg *)user_data;
	GtkWidget *checkbutton2 = lookup_widget(pDlg->m_pDialog, "checkbutton2");
	GtkWidget *entry5       = lookup_widget(pDlg->m_pDialog, "entry5");
	GtkWidget *checkbutton3 = lookup_widget(pDlg->m_pDialog, "checkbutton3");

	//enable/disable some widgets based on checkbox state
	int bEnable = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton2));
	gtk_widget_set_sensitive (entry5, bEnable);
	gtk_widget_set_sensitive (checkbutton3, bEnable);
}

void on_find_size_clicked(GtkButton *button, gpointer user_data)
{
	FileSearchDlg *pDlg = (FileSearchDlg *)user_data;
	GtkWidget *checkbutton10 = lookup_widget(pDlg->m_pDialog, "checkbutton10");
	GtkWidget *entry7        = lookup_widget(pDlg->m_pDialog, "entry7");
	GtkWidget *combobox3     = lookup_widget(pDlg->m_pDialog, "combobox3");
	GtkWidget *combobox4     = lookup_widget(pDlg->m_pDialog, "combobox4");

	//enable/disable some widgets based on checkbox state
	int bEnable = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton10));
	gtk_widget_set_sensitive (entry7, bEnable);
	gtk_widget_set_sensitive (combobox3, bEnable);
	gtk_widget_set_sensitive (combobox4, bEnable);
}

void on_result_item_clicked (GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col, gpointer userdata)
{
	FileSearchDlg *pDlg = (FileSearchDlg *)userdata;

	TRACE("on_result_item_clicked \n");
	pDlg->Stop();	//ensure search is stopped

	//TOFIX go to the selected item in active panel
	//get iterator to selected node
	GtkTreeModel *model = gtk_tree_view_get_model(treeview);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (treeview);
	GtkTreeIter  iter;
	
	//TOFIX works in single selection mode
	//if(!gtk_tree_selection_get_selected(treesel, &model, &iter))
	//	return;

	//TOFIX use GetSelection!!!
	//multiple selection list
	GList* list = gtk_tree_selection_get_selected_rows(treesel, &model);

	//execute first one
	if(g_list_length(list)<=0)
	{
		//cleanup list
		g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
		g_list_free (list);
		return;
	}
	GtkTreePath *path2 = (GtkTreePath *)g_list_first(list)->data;
	gtk_tree_model_get_iter(model, &iter, path2); 

	//cleanup list
	g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
	g_list_free (list);

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(model, &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	//
	// handle the double click (go to the path and select clicked item)
	//
	//TOFIX drop the Vfs stack!
	String strPath = pDlg->m_lstResults.GetAt(nIdx).GetPath();
	g_dp.GetActiveFileList().SetDirectory(strPath);

	//select the item in the list
	String strName = pDlg->m_lstResults.GetAt(nIdx).GetName();
	int nNewIdx = g_dp.GetActiveFileList().m_ctrl.m_lstItems.FindItem(strName);
	if(nNewIdx >= 0)
	{
		g_dp.GetActiveFileList().SetItemFocus(nNewIdx);
		g_dp.GetActiveFileList().SelectItem(nNewIdx);
	}

	//close the dialog
	pDlg->Destroy();
}

void FileSearchDlg::AddToList(VfsItem &item)
{
	TRACE("FileSearchDlg::AddToList %s\n", item.GetName().c_str());

	//add item into the GUI result list 
	GtkTreeIter iter;
	gtk_list_store_append(m_store, &iter);

	String strFull = item.GetPath();
#ifdef _WIN32
	PathName::EnsureTerminated(strFull, '\\');
#else
	PathName::EnsureTerminated(strFull);
#endif
	strFull += item.GetName();

	gtk_list_store_set (m_store, &iter,
				0, strFull.c_str(),
		//0, item.GetTitle().c_str(),	//TOFIX multiple cols
		//1, item.GetExtTitle().c_str(),
		//2, item.GetPath().c_str(),
		//3, item.GetSize().c_str(),
		//4, item.GetDate().c_str(),
		//5, item.GetAttr().c_str(),
                      -1);

	m_nResultListSize ++;

	//refresh result count label
	String strMsg;
	strMsg.Printf(_("%d items found"), m_nResultListSize);
	GtkWidget *label19 = lookup_widget(m_pDialog, "label19");
	gtk_label_set_text(GTK_LABEL(label19), strMsg.c_str());
}

gboolean monitor_timer(gpointer data)
{
	FileSearchDlg *pDlg = (FileSearchDlg *)data;

	// first refresh the list, then process the stop signal
	if(pDlg->m_evRefreshList.IsSignaled())
	{
		TRACE("FileSearch GUI: new items detected!\n");
		pDlg->m_evRefreshList.Reset();

		// resync GUI - add latest additions
		pDlg->m_objListAccess.Lock();

		int nSizeItems = pDlg->m_lstResults.GetCount();
		int nSizeList  = pDlg->m_nResultListSize;
		int nDiff      = nSizeItems - nSizeList;
		for(int i=0; i<nDiff; i++)
			pDlg->AddToList(pDlg->m_lstResults.GetAtRaw(nSizeList+i));

		pDlg->m_objListAccess.Unlock();
	}

	// search done?
	if(pDlg->m_evOpDone.IsSignaled())
	{
		TRACE("FileSearch GUI: operation done!\n");
		pDlg->OnSearchDone();
		return FALSE;	// kill timer
	}
	
	return TRUE;
}
