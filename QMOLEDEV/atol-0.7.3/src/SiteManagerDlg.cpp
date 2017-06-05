////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define and start remote connections (FTP, SFTP)
//////////////////////////////////////////////////////////////////////////// 

#include "SiteManagerDlg.h"
#include "support.h"
#include "core/opcodes.h"
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include "GuiInputDlg.h"

#ifdef _WIN32
  #include <io.h>
  #define access _access
#else
 #include<unistd.h>
#endif

extern GtkWidget *atol_main;
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
static void on_site_new_popup (GtkMenuItem *menuitem, gpointer user_data);
static void on_site_remove_popup (GtkMenuItem *menuitem, gpointer user_data);
static void on_site_move_up_popup (GtkMenuItem *menuitem, gpointer user_data);
static void on_site_move_down_popup (GtkMenuItem *menuitem, gpointer user_data);
static void on_site_executed (GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col, gpointer userdata);
static void on_site_selection_changed (GtkTreeSelection *treeselection, gpointer user_data);
static void on_site_connect_button (GtkMenuItem *menuitem, gpointer user_data);
static void on_site_close_button (GtkMenuItem *menuitem, gpointer user_data);
static void on_site_duplicate_popup (GtkMenuItem *menuitem, gpointer user_data);
static void on_protocol_type_combo (GtkComboBox *widget, gpointer user_data);
static void on_site_anonymous_button (GtkButton *menuitem, gpointer user_data);
static void create_menu (GtkWidget *parent, GtkWidget *menubar1, GtkAccelGroup *accel_group, gpointer user_data);
static gint dlg_keyboard_handler (GtkWidget *widget, GdkEventKey *event, gpointer data);
static void on_proxy_type_combo (GtkComboBox *widget, gpointer user_data);
static void on_use_encryption (GtkComboBox *widget, gpointer user_data);

std::string g_strRemoteListPass;

/// PROXY DATA ////
int g_nProxyType = 0;
int g_nProxyPort = 0;
int g_bPassiveMode = false;
String g_strProxyHost;
String g_strProxyUser;
String g_strProxyPass;
/// PROXY DATA ////

 
SiteManagerDlg::SiteManagerDlg()
{
	m_nSelection = -1;
	m_bIniLoaded = false;
	m_bSkipSave = false;
	Create();
	BuildTree();
}

SiteManagerDlg::~SiteManagerDlg()
{
	//save changes
	String strPath;
	m_lstSites.CalcIniFile(strPath);
	
	bool bSave = true;
	if(m_bSkipSave)
		bSave = false;

	if(bSave && !m_bIniLoaded && 0 == access(strPath.c_str(), 0))
	{
		if(GTK_RESPONSE_YES != gtkMessageBox(_("Saving site list will overwrite previous data! Save anyway?"), GTK_BUTTONS_YES_NO))
			bSave = false;
	}

	if(bSave)
	{
		IniFile ini;
		if(g_strRemoteListPass.size()>0)
			ini.SetEncrypted(g_strRemoteListPass.c_str());

		ini.Load(strPath.c_str());	//do not check result, maybe it didn't exist before
	
		m_lstSites.SaveToIni(ini);

		//additionally save proxy data into the same encrypted Ini
		ini.SetValue("Proxy", "Type", g_nProxyType);
		ini.SetValue("Proxy", "Port", g_nProxyPort);
		ini.SetValue("Proxy", "Passive", g_bPassiveMode);
		ini.SetValue("Proxy", "Host", g_strProxyHost.c_str());
		ini.SetValue("Proxy", "User", g_strProxyUser.c_str());
		ini.SetValue("Proxy", "Password", g_strProxyPass.c_str());

		ini.Save();	//TOFIX report error?
	}

	//destroy dialog
	if(m_pDialog){
		gtk_widget_destroy(m_pDialog);
	}
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void SiteManagerDlg::Create()
{
	m_pDialog = create_dialog (atol_main);

	//fill protocol combo
	GtkWidget *combobox8 = lookup_widget(m_pDialog, "combobox8");
	gtk_combo_box_append_text(GTK_COMBO_BOX(combobox8), "FTP");
#ifdef _WIN32	// supported on Win32 only so far
	gtk_combo_box_append_text(GTK_COMBO_BOX(combobox8), "SFTP");
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox8), 0);

	//fill proxy type list
	GtkWidget *comboboxentry2 = lookup_widget(m_pDialog, "comboboxentry2");
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("0 - None"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("1 - SOCKS v5"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("2 - SOCKS v4A"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("3 - SOCKS v4"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("4 - HTTP proxy"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("5 - SITE ftp-host"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("6 - \"USER after logon\""));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("7 - OPEN ftp-host"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("8 - Transparent"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("9 - USER ftp-user@ftp-host:ftp-port"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("10 - USER proxy-user@ftp-host"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("11 - USER ftp-user@ftp-host proxy-user"));	
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry2), _("12 - USER ftp-user@proxy-user@ftp-host"));	

	//enable control state (disable all)
	EnableCtrls(false);
	on_use_encryption(NULL, this);
}

void SiteManagerDlg::ClearData()
{
	//show empty object
	CNodeInfo info;
	UpdateData(info, true);
}

GtkWidget* SiteManagerDlg::create_dialog (GtkWidget* parent)
{
	GtkWidget *site_manager_dialog;
	GtkWidget *dialog_vbox13;
	GtkWidget *hbox1;
	GtkWidget *label5;
	GtkWidget *scrolledwindow18;
	GtkWidget *treeview7;
	GtkWidget *notebook1;
	GtkWidget *notebook2;
	GtkWidget *table15;
	GtkWidget *label37;
	GtkWidget *label38;
	GtkWidget *hbox2;
	GtkWidget *host_entry;
	GtkWidget *port_entry;
	GtkWidget *label39;
	GtkWidget *hbox3;
	GtkWidget *combobox8;
	GtkWidget *checkbutton15;
	GtkWidget *label40;
	GtkWidget *label41;
	GtkWidget *label42;
	GtkWidget *label43;
	GtkWidget *scrolledwindow19;
	GtkWidget *desc_textview;
	GtkWidget *title_entry;
	GtkWidget *user_entry;
	GtkWidget *pass_entry;
	GtkWidget *remotedir_entry;
	GtkWidget *label35;
	GtkWidget *vbox16;
	GtkWidget *hbox4;
	GtkWidget *label44;
	GtkWidget *entry19;
	GtkWidget *checkbutton16;
	GtkWidget *checkbutton17;
	GtkWidget *checkbutton18;
	GtkWidget *label45;
	GtkWidget *checkbutton19;
	GtkWidget *checkbutton20;
	GtkWidget *checkbutton21;
	GtkWidget *checkbutton22;
	GtkWidget *label36;
	GtkWidget *dialog_action_area13;
	GtkWidget *cancelbutton12;
	GtkWidget *okbutton12;
	GtkWidget *menubar1;
	GtkListStore* store;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkAccelGroup *accel_group;

	GtkWidget *table3;
	GtkWidget *label11;
	GtkWidget *label12;
	GtkWidget *comboboxentry2;
	GtkWidget *label13;
	GtkWidget *label14;
	GtkWidget *entry3;
	GtkWidget *entry4;
	GtkWidget *checkbutton9;
	GtkWidget *hbox5;
	GtkWidget *label10;
	GtkWidget *entry1;
	GtkWidget *entry2;

	accel_group = gtk_accel_group_new ();

	site_manager_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (site_manager_dialog), _("Site Manager"));
	gtk_window_set_modal (GTK_WINDOW (site_manager_dialog), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (site_manager_dialog), TRUE);
	gtk_window_set_type_hint (GTK_WINDOW (site_manager_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (site_manager_dialog), TRUE);
#endif
	if(parent)
		gtk_window_set_transient_for(GTK_WINDOW (site_manager_dialog), GTK_WINDOW(parent));   //set parent

	gtk_window_set_destroy_with_parent (GTK_WINDOW (site_manager_dialog), TRUE);
//	gtk_window_set_resizable (GTK_WINDOW (site_manager_dialog), FALSE);
	gtk_widget_realize (site_manager_dialog);
	gdk_window_set_decorations(site_manager_dialog->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE)); 

	dialog_vbox13 = GTK_DIALOG (site_manager_dialog)->vbox;
	gtk_widget_show (dialog_vbox13);
	
	menubar1 = gtk_menu_bar_new ();
	gtk_widget_show (menubar1);
	gtk_box_pack_start (GTK_BOX (dialog_vbox13), menubar1, FALSE, FALSE, 0);

	create_menu (site_manager_dialog, menubar1, accel_group, this);

	notebook1 = gtk_notebook_new ();
	gtk_widget_show (notebook1);
	gtk_box_pack_start (GTK_BOX (dialog_vbox13), notebook1, TRUE, TRUE, 0);

	//
	// "Site list" page
	//

	hbox1 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox1);
	gtk_container_add (GTK_CONTAINER (notebook1), hbox1);
	
	scrolledwindow18 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow18);
	gtk_box_pack_start (GTK_BOX (hbox1), scrolledwindow18, FALSE, TRUE, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow18), GTK_SHADOW_IN);
	gtk_scrolled_window_set_policy  (GTK_SCROLLED_WINDOW (scrolledwindow18), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
                                             
	//
	// tree initialization
	//
	store = gtk_list_store_new(2, G_TYPE_STRING, GDK_TYPE_PIXBUF);
	treeview7 = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));
	g_object_unref (G_OBJECT (store));  //tree now holds reference
	
	//treeview7 = gtk_tree_view_new ();
	gtk_widget_show (treeview7);
	gtk_container_add (GTK_CONTAINER (scrolledwindow18), treeview7);
	gtk_widget_set_size_request (treeview7, 150, -1);
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (treeview7), FALSE);
	
	//pack two renderers in the single column
	column = gtk_tree_view_column_new();
	
	renderer = gtk_cell_renderer_pixbuf_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	gtk_tree_view_column_set_attributes(column, renderer, "pixbuf", 1, NULL);
	
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", 0, NULL);
	
	// add the column to the view
	gtk_tree_view_append_column (GTK_TREE_VIEW (treeview7), column);

	notebook2 = gtk_notebook_new ();
	gtk_widget_show (notebook2);
	gtk_box_pack_start (GTK_BOX (hbox1), notebook2, TRUE, TRUE, 0);
	
	table15 = gtk_table_new (7, 2, FALSE);
	gtk_widget_show (table15);
	gtk_container_add (GTK_CONTAINER (notebook2), table15);
	
	label37 = gtk_label_new (_("Site name:"));
	gtk_widget_show (label37);
	gtk_table_attach (GTK_TABLE (table15), label37, 0, 1, 0, 1,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label37), 0, 0.5);
	
	label38 = gtk_label_new (_("Server/port:"));
	gtk_widget_show (label38);
	gtk_table_attach (GTK_TABLE (table15), label38, 0, 1, 1, 2,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label38), 0, 0.5);
	
	hbox2 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox2);
	gtk_table_attach (GTK_TABLE (table15), hbox2, 1, 2, 1, 2,
		(GtkAttachOptions) (GTK_FILL|GTK_SHRINK),
		(GtkAttachOptions) (0), 0, 0);
	
	host_entry = gtk_entry_new ();
	gtk_widget_show (host_entry);
	gtk_box_pack_start (GTK_BOX (hbox2), host_entry, FALSE, FALSE, 0);
	
	port_entry = gtk_entry_new ();
	gtk_widget_show (port_entry);
	gtk_widget_set_size_request(port_entry, 37, -1);
	gtk_box_pack_start (GTK_BOX (hbox2), port_entry, FALSE, FALSE, 0);

	label39 = gtk_label_new (_("Protocol:"));
	gtk_widget_show (label39);
	gtk_table_attach (GTK_TABLE (table15), label39, 0, 1, 2, 3,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (GTK_FILL), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label39), 0, 0.5);

	hbox3 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox3);
	gtk_table_attach (GTK_TABLE (table15), hbox3, 1, 2, 2, 3,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (GTK_SHRINK), 0, 0);

	combobox8 = gtk_combo_box_new_text ();
	gtk_widget_show (combobox8);
	gtk_box_pack_start (GTK_BOX (hbox3), combobox8, FALSE, TRUE, 0);

	checkbutton15 = gtk_check_button_new_with_mnemonic (_("Anonymous"));
	gtk_widget_show (checkbutton15);
	gtk_box_pack_start (GTK_BOX (hbox3), checkbutton15, TRUE, TRUE, 0);
	
	label40 = gtk_label_new (_("User name:"));
	gtk_widget_show (label40);
	gtk_table_attach (GTK_TABLE (table15), label40, 0, 1, 3, 4,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label40), 0, 0.5);
	
	label41 = gtk_label_new (_("Password:"));
	gtk_widget_show (label41);
	gtk_table_attach (GTK_TABLE (table15), label41, 0, 1, 4, 5,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label41), 0, 0.5);
	
	label42 = gtk_label_new (_("Remote dir:"));
	gtk_widget_show (label42);
	gtk_table_attach (GTK_TABLE (table15), label42, 0, 1, 5, 6,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label42), 0, 0.5);
	
	label43 = gtk_label_new (_("Description:"));
	gtk_widget_show (label43);
	gtk_table_attach (GTK_TABLE (table15), label43, 0, 1, 6, 7,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label43), 0, 0);
	
	scrolledwindow19 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow19);
	gtk_table_attach (GTK_TABLE (table15), scrolledwindow19, 1, 2, 6, 7,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL), 0, 0);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow19), GTK_SHADOW_IN);
	
	desc_textview = gtk_text_view_new ();
	gtk_widget_show (desc_textview);
	gtk_container_add (GTK_CONTAINER (scrolledwindow19), desc_textview);
	
	title_entry = gtk_entry_new ();
	gtk_widget_show (title_entry);
	gtk_table_attach (GTK_TABLE (table15), title_entry, 1, 2, 0, 1,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	
	user_entry = gtk_entry_new ();
	gtk_widget_show (user_entry);
	gtk_table_attach (GTK_TABLE (table15), user_entry, 1, 2, 3, 4,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	
	pass_entry = gtk_entry_new ();
	gtk_widget_show (pass_entry);
	gtk_table_attach (GTK_TABLE (table15), pass_entry, 1, 2, 4, 5,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_entry_set_visibility (GTK_ENTRY (pass_entry), FALSE);
	
	remotedir_entry = gtk_entry_new ();
	gtk_widget_show (remotedir_entry);
	gtk_table_attach (GTK_TABLE (table15), remotedir_entry, 1, 2, 5, 6,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	
	label35 = gtk_label_new (_("General"));
	gtk_widget_show (label35);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 0), label35);
	
	vbox16 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox16);
	gtk_container_add (GTK_CONTAINER (notebook2), vbox16);
	
	hbox4 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox4);
	gtk_box_pack_start (GTK_BOX (vbox16), hbox4, FALSE, TRUE, 0);
	
	label44 = gtk_label_new (_("Account:"));
	gtk_widget_show (label44);
	gtk_box_pack_start (GTK_BOX (hbox4), label44, FALSE, FALSE, 0);
	
	entry19 = gtk_entry_new ();
	gtk_widget_show (entry19);
	gtk_box_pack_start (GTK_BOX (hbox4), entry19, FALSE, TRUE, 0);
	
	checkbutton16 = gtk_check_button_new_with_mnemonic (_("Bypass proxy"));
	gtk_widget_show (checkbutton16);
	gtk_box_pack_start (GTK_BOX (vbox16), checkbutton16, FALSE, TRUE, 0);
	
	checkbutton17 = gtk_check_button_new_with_mnemonic (_("Use passive mode"));
	gtk_widget_show (checkbutton17);
	gtk_box_pack_start (GTK_BOX (vbox16), checkbutton17, FALSE, TRUE, 0);
	
	checkbutton18 = gtk_check_button_new_with_mnemonic (_("Use encryption"));
	gtk_widget_show (checkbutton18);
	gtk_box_pack_start (GTK_BOX (vbox16), checkbutton18, FALSE, TRUE, 0);

	GtkWidget *table4 = gtk_table_new (2, 5, FALSE);
	gtk_widget_show (table4);
	gtk_table_set_col_spacing(GTK_TABLE(table4), 0, 7);
	gtk_container_add (GTK_CONTAINER (vbox16), table4);

	GtkWidget *radio1 =  gtk_radio_button_new_with_label (NULL, _("TLSv1 (recommended)"));
	gtk_widget_show (radio1);
	gtk_table_attach (GTK_TABLE (table4), radio1, 1, 2, 0, 1,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio1), true);  /// Set recommended defaults for not advanced users
 
	GtkWidget *radio2 =  gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON(radio1), _("SSLv2"));
	gtk_widget_show (radio2);
	gtk_table_attach (GTK_TABLE (table4), radio2, 1, 2, 1, 2,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);

	GtkWidget *radio3 =  gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON(radio1), _("SSLv3"));
	gtk_widget_show (radio3);
	gtk_table_attach (GTK_TABLE (table4), radio3, 1, 2, 2, 3,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);

	GtkWidget *checkbutton23 = gtk_check_button_new_with_mnemonic (_("encrypted data connections"));
	gtk_widget_show (checkbutton23);
	gtk_table_attach (GTK_TABLE (table4), checkbutton23, 1, 2, 3, 4,
		(GtkAttachOptions) (GTK_SHRINK|GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton23), true);  /// Set recommended defaults for not advanced users
 
	label45 = gtk_label_new (_("List options:"));
	gtk_widget_show (label45);
	gtk_box_pack_start (GTK_BOX (vbox16), label45, FALSE, TRUE, 0);
	gtk_misc_set_alignment (GTK_MISC (label45), 0, 0.5);
	
	checkbutton19 = gtk_check_button_new_with_mnemonic (_("-l (force long format)"));
	gtk_widget_show (checkbutton19);
	gtk_box_pack_start (GTK_BOX (vbox16), checkbutton19, FALSE, TRUE, 0);
	
	checkbutton20 = gtk_check_button_new_with_mnemonic (_("-L (resolve links)"));
	gtk_widget_show (checkbutton20);
	gtk_box_pack_start (GTK_BOX (vbox16), checkbutton20, FALSE, TRUE, 0);
	
	checkbutton21 = gtk_check_button_new_with_mnemonic (_("-a (show hidden files)"));
	gtk_widget_show (checkbutton21);
	gtk_box_pack_start (GTK_BOX (vbox16), checkbutton21, FALSE, TRUE, 0);
	
	checkbutton22 = gtk_check_button_new_with_mnemonic (_("-T (complete time)"));
	gtk_widget_show (checkbutton22);
	gtk_box_pack_start (GTK_BOX (vbox16), checkbutton22, FALSE, TRUE, 0);
	
	label36 = gtk_label_new (_("Advanced"));
	gtk_widget_show (label36);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook2), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook2), 1), label36);

	label5 = gtk_label_new (_("Site list"));
	gtk_widget_show (label5);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 0), label5);

	//
	// "Proxy" tab
	//

	table3 = gtk_table_new (2, 5, FALSE);
	gtk_widget_show (table3);
	gtk_container_add (GTK_CONTAINER (notebook1), table3);

	label10	= gtk_label_new (_("Proxy type:"));
	gtk_widget_show (label10);
	gtk_table_attach (GTK_TABLE (table3), label10, 0, 1, 0, 1,
		                (GtkAttachOptions) (GTK_FILL),
			            (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label10), 0, 0.5);


	comboboxentry2 = gtk_combo_box_new_text ();
	gtk_widget_show (comboboxentry2);
	gtk_table_attach (GTK_TABLE (table3), comboboxentry2, 1, 2, 0, 1,
				        (GtkAttachOptions) (GTK_FILL),
					    (GtkAttachOptions) (GTK_FILL), 0, 0);

	label11	= gtk_label_new (_("Host/port:"));
	gtk_widget_show (label11);
	gtk_table_attach (GTK_TABLE (table3), label11, 0, 1, 1, 2,
		                (GtkAttachOptions) (GTK_FILL),
			            (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label11), 0, 0.5);

	hbox5 = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox5);
	gtk_table_attach (GTK_TABLE (table3), hbox5, 1, 2, 1, 2,
		(GtkAttachOptions) (GTK_FILL|GTK_SHRINK),
		(GtkAttachOptions) (0), 0, 0);

	entry1 =  gtk_entry_new();
	gtk_widget_show (entry1);
	gtk_box_pack_start (GTK_BOX (hbox5), entry1, FALSE, FALSE, 0);

	entry2 =  gtk_entry_new();
	gtk_widget_show (entry2);
	gtk_widget_set_size_request(entry2, 37, -1);
	gtk_box_pack_start (GTK_BOX (hbox5), entry2, FALSE, FALSE, 0);

	label13	= gtk_label_new (_("User name:"));
	gtk_widget_show (label13);
	gtk_table_attach (GTK_TABLE (table3), label13, 0, 1, 2, 3,
		                (GtkAttachOptions) (GTK_FILL),
			            (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label13), 0, 0.5);

	entry3 =  gtk_entry_new();
	gtk_widget_show (entry3);
	gtk_table_attach (GTK_TABLE (table3), entry3, 1, 2, 2, 3,
		                (GtkAttachOptions) (GTK_FILL),
			            (GtkAttachOptions) (0), 0, 0);

	label14	= gtk_label_new (_("Password:"));
	gtk_widget_show (label14);
	gtk_table_attach (GTK_TABLE (table3), label14, 0, 1, 3, 4,
		                (GtkAttachOptions) (GTK_FILL),
			            (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label14), 0, 0.5);

	entry4 =  gtk_entry_new();
	gtk_widget_show (entry4);
	gtk_entry_set_visibility (GTK_ENTRY (entry4), FALSE);	//password mode
	gtk_table_attach (GTK_TABLE (table3), entry4, 1, 2, 3, 4,
		                (GtkAttachOptions) (GTK_FILL),
			            (GtkAttachOptions) (0), 0, 0);

	checkbutton9 = gtk_check_button_new_with_mnemonic (_("Passive mode"));
	gtk_widget_show (checkbutton9);
	gtk_table_attach (GTK_TABLE (table3), checkbutton9, 0, 1, 4, 5,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);

	label12 = gtk_label_new (_("Proxy server"));
	gtk_widget_show (label12);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 1), label12);

	/////////

	dialog_action_area13 = GTK_DIALOG (site_manager_dialog)->action_area;
	gtk_widget_show (dialog_action_area13);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area13), GTK_BUTTONBOX_END);
	
	cancelbutton12 = gtk_button_new_with_label (_("Close"));
	gtk_widget_show (cancelbutton12);
	gtk_container_add(GTK_CONTAINER(dialog_action_area13), cancelbutton12);
	GTK_WIDGET_SET_FLAGS (cancelbutton12, GTK_CAN_DEFAULT);
	
	okbutton12 = gtk_button_new_with_label (_("Connect"));
	gtk_widget_show (okbutton12);
	gtk_container_add(GTK_CONTAINER(dialog_action_area13), okbutton12);
	GTK_WIDGET_SET_FLAGS (okbutton12, GTK_CAN_DEFAULT);

	g_signal_connect (site_manager_dialog, "key_press_event", G_CALLBACK (dlg_keyboard_handler), this);
	g_signal_connect (combobox8, "changed",	G_CALLBACK (on_protocol_type_combo), this);	
	g_signal_connect (treeview7, "row-activated", G_CALLBACK (on_site_executed), this);
	g_signal_connect (cancelbutton12, "clicked", G_CALLBACK (on_site_close_button), this);
	g_signal_connect (okbutton12, "clicked", G_CALLBACK (on_site_connect_button), this);
	g_signal_connect (checkbutton15, "clicked", G_CALLBACK (on_site_anonymous_button), this);
	g_signal_connect (comboboxentry2, "changed", G_CALLBACK (on_proxy_type_combo), this);	
	g_signal_connect (checkbutton18, "clicked", G_CALLBACK (on_use_encryption), this);
	

	//register selection change signal
	GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview7));
	g_signal_connect (selection, "changed", G_CALLBACK (on_site_selection_changed), this);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (site_manager_dialog, site_manager_dialog, "site_manager_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (site_manager_dialog, dialog_vbox13, "dialog_vbox13");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, hbox1, "hbox1");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, scrolledwindow18, "scrolledwindow18");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, treeview7, "treeview7");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, notebook2, "notebook2");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, table15, "table15");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label37, "label37");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label38, "label38");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, hbox2, "hbox2");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, host_entry, "host_entry");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, port_entry, "port_entry");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label39, "label39");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, hbox3, "hbox3");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, combobox8, "combobox8");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton15, "checkbutton15");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label40, "label40");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label41, "label41");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label42, "label42");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label43, "label43");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, scrolledwindow19, "scrolledwindow19");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, desc_textview, "desc_textview");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, title_entry, "title_entry");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, user_entry, "user_entry");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, pass_entry, "pass_entry");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, remotedir_entry, "remotedir_entry");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label35, "label35");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, vbox16, "vbox16");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, hbox4, "hbox4");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label44, "label44");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, entry19, "entry19");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton16, "checkbutton16");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton17, "checkbutton17");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton18, "checkbutton18");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label45, "label45");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton19, "checkbutton19");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton20, "checkbutton20");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton21, "checkbutton21");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton22, "checkbutton22");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, label36, "label36");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, comboboxentry2, "comboboxentry2");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, entry1, "entry1");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, entry2, "entry2");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, entry3, "entry3");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, entry4, "entry4");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton9, "passive_mode_btn");
	GLADE_HOOKUP_OBJECT_NO_REF (site_manager_dialog, dialog_action_area13, "dialog_action_area13");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, cancelbutton12, "cancelbutton12");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, okbutton12, "okbutton12");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, radio1, "radio1");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, radio2, "radio2");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, radio3, "radio3");
	GLADE_HOOKUP_OBJECT (site_manager_dialog, checkbutton23, "checkbutton23");

	gtk_widget_grab_focus   (okbutton12);
	gtk_widget_grab_default (okbutton12);
	
	return site_manager_dialog;
}

void SiteManagerDlg::BuildTree()
{
	EnableCtrls(false);

	//get password to decrypt INI file
	GuiInputDlg dlg(true);
	if(g_strRemoteListPass.size()>0)
		dlg.SetInput(g_strRemoteListPass.c_str());
	dlg.SetLabel(_("Remote site list password"));
	if(GTK_RESPONSE_OK == dlg.ShowModal())
	{
		g_strRemoteListPass = dlg.GetInput();
		m_lstSites.SetPassword(g_strRemoteListPass.c_str());
	}
	else{
		m_bSkipSave = true;
		gtk_dialog_response(GTK_DIALOG(m_pDialog), GTK_RESPONSE_CANCEL);
		Destroy();
		//g_strRemoteListPass = ""; //TOFIX handle cancel?
		return;
	}

	//load site list
	//TOFIX handle wrong password, ask for overwrite on save!!!
	String strPath;
	m_lstSites.CalcIniFile(strPath);
	
	if(0 == access(strPath.c_str(), 0))
	{
		m_bIniLoaded = false;

		IniFile ini;
		if(g_strRemoteListPass.size()>0)
			ini.SetEncrypted(g_strRemoteListPass.c_str());
		if(ini.Load(strPath.c_str()))
		{
			if(m_lstSites.LoadFromIni(ini))
			{
				std::string strHost, strUser, strPass;

				//additionally load proxy data from the same encrypted Ini
				ini.GetValue("Proxy", "Type", g_nProxyType, 0);
				ini.GetValue("Proxy", "Port", g_nProxyPort);
				ini.GetValue("Proxy", "Passive", g_bPassiveMode);
				ini.GetValue("Proxy", "Host", strHost,"");
				g_strProxyHost = strHost.c_str();
				ini.GetValue("Proxy", "User", strUser,"");
				g_strProxyUser = strUser.c_str();
				ini.GetValue("Proxy", "Password", strPass,"");
				g_strProxyPass = strPass.c_str();

				//TOFIX write reason for failure!!!!
				m_bIniLoaded = true;

			}
		}

		//report error
		if(!m_bIniLoaded){
			gtkMessageBox(_("Failed to load site list!"));
			return;
		}
	}
	else
	{
		//ini does not exists
		m_bIniLoaded = false;
	}

	m_lstSites.Sort();

	//TOFIX support for tree?
	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);

	//fill the list
	for(unsigned int i=0; i<m_lstSites.size(); i++)
	{
		//add item
		GtkTreeIter iter;
		gtk_list_store_append(model, &iter);
		gtk_list_store_set (model, &iter, 0, m_lstSites[i].m_strTitle.c_str(), -1);
	}

	//TOFIX store/restore last selected item
	if(m_lstSites.size() > 0)
	{
		//select item
		GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));
		GtkTreePath *path = gtk_tree_path_new_from_string ("0");
		gtk_tree_selection_select_path (treesel, path);
		gtk_tree_path_free (path);

		//remember selection
		m_nSelection = 0;
	}
}

void SiteManagerDlg::OnCtxMenu_New()
{
	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);

	//insert into GUI
	GtkTreeIter iter;
	gtk_list_store_append(model, &iter);

	String strTitle(_("New site"));
	gtk_list_store_set (model, &iter, 0, strTitle.c_str(), -1);
		
	//update internal list
	CNodeInfo info;
	info.m_strTitle = strTitle;
	info.m_dwProtocol = PROT_FTP;
	m_lstSites.push_back(info);

	//select the new item in the list
	GtkTreeSelection *treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));
	gtk_tree_selection_select_iter(treesel, &iter);
}

void SiteManagerDlg::OnCtxMenu_Delete()
{
	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));

	//get selection
	GtkTreeIter iter;
	if(!gtk_tree_selection_get_selected(treesel, (GtkTreeModel **)&model, &iter))
		return;

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	//delete internal data
	m_lstSites.Remove(nIdx); //refresh internal list
	m_nSelection = -1;	//no selection

	//delete from GUI
	gtk_list_store_remove (model, &iter);
	
	ClearData();
	EnableCtrls(false);
}

//duplicate entry
void SiteManagerDlg::OnCtxMenu_Duplicate()
{
	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);

	//get currently selected site
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));

	//get selection
	GtkTreeIter iter;
	if(!gtk_tree_selection_get_selected(treesel, (GtkTreeModel **)&model, &iter))
		return;

	//update selection data
	if(m_nSelection >= 0){
		UpdateData(m_objCurSite, false);
		m_lstSites[m_nSelection] = m_objCurSite;
	}

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	if(nIdx >= 0)
	{
		//insert copy into the GUI
		GtkTreeIter iter;
		gtk_list_store_append(model, &iter);

		String strTitle = m_lstSites[nIdx].m_strTitle;
		gtk_list_store_set (model, &iter, 0, strTitle.c_str(), -1);
			
		//update internal list
		CNodeInfo info = m_lstSites[nIdx];
		m_lstSites.push_back(info);

		//select the new item in the list
		GtkTreeSelection *treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));
		gtk_tree_selection_select_iter(treesel, &iter);
	}
}

void SiteManagerDlg::OnCtxMenu_MoveUp()
{
	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));

	//get selection
	GtkTreeIter iter;
	if(!gtk_tree_selection_get_selected(treesel, (GtkTreeModel **)&model, &iter))
		return;

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	if(nIdx > 0)
	{
		//swap the items
		m_lstSites.Swap(nIdx-1, nIdx);

		//swap the GUI list items
		GtkTreeIter iterPrev;
		gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(model), &iterPrev, NULL, nIdx-1);
		gtk_list_store_swap (model, &iterPrev, &iter);

		//select item
		char szPath[20];
		sprintf(szPath, "%d", nIdx-1);
		GtkTreePath *path = gtk_tree_path_new_from_string (szPath);
		gtk_tree_selection_select_path (treesel, path);
		gtk_tree_path_free (path);

		//remember selection
		m_nSelection = nIdx-1;
	}
}

void SiteManagerDlg::OnCtxMenu_MoveDown()
{
	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));

	//get selection
	GtkTreeIter iter;
	if(!gtk_tree_selection_get_selected(treesel, (GtkTreeModel **)&model, &iter))
		return;

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	if(nIdx >= 0)
	{
		int nCount = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(model), NULL);
		if(nIdx < nCount-1)
		{
			//swap the items
			m_lstSites.Swap(nIdx, nIdx+1);

			//swap the GUI list items
			GtkTreeIter iterNext;
			gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(model), &iterNext, NULL, nIdx+1);
			gtk_list_store_swap (model, &iterNext, &iter);

			//select item
			char szPath[20];
			sprintf(szPath, "%d", nIdx+1);
			GtkTreePath *path = gtk_tree_path_new_from_string (szPath);
			gtk_tree_selection_select_path (treesel, path);
			gtk_tree_path_free (path);

			//remember selection
			m_nSelection = nIdx+1;
		}
	}
}

void SiteManagerDlg::OnSiteTree_SelChange()
{
	//update previous selection
	if(m_nSelection >= 0)
	{
		UpdateData(m_objCurSite, false);
		m_lstSites[m_nSelection] = m_objCurSite;
	}

	int nNewSel = GetSelectedSite();
	if(nNewSel >= 0)
	{
		m_objCurSite = m_lstSites[nNewSel];
		UpdateData(m_objCurSite, true);
		m_nSelection = nNewSel;
	}

	EnableCtrls(true);
}

void SiteManagerDlg::OnSiteTree_DblClick()
{
	OnBtn_OK();
}

void SiteManagerDlg::OnBtn_OK()
{
	//update the data
	if(m_nSelection >= 0)
	{
		UpdateData(m_objCurSite, false);
		m_lstSites[m_nSelection] = m_objCurSite;

		if(!m_objCurSite.IsValid()){
			gtkMessageBox(_("Site description is incomplete!"));
			return;
		}

		//end dialog to start connecting
		gtk_dialog_response(GTK_DIALOG(m_pDialog), GTK_RESPONSE_OK);
	}
	else
		gtkMessageBox(_("No site selected!"));
}

void SiteManagerDlg::OnBtn_Close()
{
	//update the data
	if(m_nSelection >= 0){
		UpdateData(m_objCurSite, false);
		m_lstSites[m_nSelection] = m_objCurSite;
	}

	//end dialog
	gtk_dialog_response(GTK_DIALOG(m_pDialog), GTK_RESPONSE_CANCEL);
}

void SiteManagerDlg::OnChk_Anonymous()
{
	//enable/disable user name and password fields
	GtkWidget *checkbutton15 = lookup_widget(m_pDialog, "checkbutton15");
	GtkWidget *user_entry = lookup_widget(m_pDialog, "user_entry");
	GtkWidget *pass_entry = lookup_widget(m_pDialog, "pass_entry");

	gboolean bChecked = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton15));
	gtk_widget_set_sensitive(user_entry, !bChecked);
	gtk_widget_set_sensitive(pass_entry, !bChecked);
}

int SiteManagerDlg::GetSelectedSite()
{
	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));

	//get selection
	GtkTreeIter iter;
	if(!gtk_tree_selection_get_selected(treesel, (GtkTreeModel **)&model, &iter))
		return -1;

	//get index from iterator
	GtkTreePath *path1 = gtk_tree_model_get_path(GTK_TREE_MODEL(model), &iter);
	int nIdx = gtk_tree_path_get_indices(path1)[0];
	gtk_tree_path_free(path1);

	return nIdx;
}

void SiteManagerDlg::UpdateData(CNodeInfo &info, bool bToScreen)
{
	GtkWidget *host_entry = lookup_widget(m_pDialog, "host_entry");
	GtkWidget *port_entry = lookup_widget(m_pDialog, "port_entry");
	GtkWidget *combobox8  = lookup_widget(m_pDialog, "combobox8");
	GtkWidget *checkbutton15 = lookup_widget(m_pDialog, "checkbutton15");
	GtkWidget *desc_textview = lookup_widget(m_pDialog, "desc_textview");
	GtkWidget *title_entry = lookup_widget(m_pDialog, "title_entry");
	GtkWidget *user_entry = lookup_widget(m_pDialog, "user_entry");
	GtkWidget *pass_entry = lookup_widget(m_pDialog, "pass_entry");
	GtkWidget *remotedir_entry = lookup_widget(m_pDialog, "remotedir_entry");
	GtkWidget *entry19 = lookup_widget(m_pDialog, "entry19");
	GtkWidget *checkbutton16 = lookup_widget(m_pDialog, "checkbutton16");
	GtkWidget *checkbutton17 = lookup_widget(m_pDialog, "checkbutton17");
	GtkWidget *checkbutton18 = lookup_widget(m_pDialog, "checkbutton18");
 	GtkWidget *checkbutton19 = lookup_widget(m_pDialog, "checkbutton19");
	GtkWidget *checkbutton20 = lookup_widget(m_pDialog, "checkbutton20");
	GtkWidget *checkbutton21 = lookup_widget(m_pDialog, "checkbutton21");
	GtkWidget *checkbutton22 = lookup_widget(m_pDialog, "checkbutton22");
	GtkWidget *checkbutton23 = lookup_widget(m_pDialog, "checkbutton23");
	GtkWidget *radio1 = lookup_widget(m_pDialog, "radio1");
	GtkWidget *radio2 = lookup_widget(m_pDialog, "radio2");
	GtkWidget *radio3 = lookup_widget(m_pDialog, "radio3");

	GtkTextBuffer* buffer1 = gtk_text_view_get_buffer(GTK_TEXT_VIEW(desc_textview));

	GtkWidget *treeview7 = lookup_widget(m_pDialog, "treeview7");
	GtkListStore *model = (GtkListStore *)gtk_tree_view_get_model((GtkTreeView *)treeview7);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview7));

	//global proxy data
	//TOFIX refresh other data
	//GtkWidget *comboboxentry2 = lookup_widget(m_pDialog, "comboboxentry2");	//proxy type
	//GtkWidget *entry3 = lookup_widget(m_pDialog, "entry3");
	//GtkWidget *entry4 = lookup_widget(m_pDialog, "entry4");
	GtkWidget *checkbutton9 = lookup_widget(m_pDialog, "passive_mode_btn");
	//GtkWidget *entry1 = lookup_widget(m_pDialog, "entry1");
	//GtkWidget *entry2 = lookup_widget(m_pDialog, "entry2");


	if(bToScreen)
	{
		//refresh tree widget text to make sure it is same as in title entry box
		GtkTreeIter iter;
		if(gtk_tree_selection_get_selected(treesel, (GtkTreeModel **)&model, &iter))
			gtk_list_store_set (model, &iter, 0, info.m_strTitle.c_str(), -1);

		//refresh data entires
		gtk_entry_set_text(GTK_ENTRY(title_entry), info.m_strTitle);
		gtk_entry_set_text(GTK_ENTRY(host_entry), info.m_strHost);

		String strTmp;
		strTmp.Printf("%d", info.m_uPort);
		gtk_entry_set_text(GTK_ENTRY(port_entry), strTmp);

		if(PROT_FTP == info.m_dwProtocol)
			gtk_combo_box_set_active(GTK_COMBO_BOX(combobox8), 0);
		else
			gtk_combo_box_set_active(GTK_COMBO_BOX(combobox8), 1);

		gtk_entry_set_text(GTK_ENTRY(user_entry), info.m_strUser);
		gtk_entry_set_text(GTK_ENTRY(pass_entry), info.m_strPassword);
		gtk_entry_set_text(GTK_ENTRY(remotedir_entry), info.m_strRemoteDir);

		//set description
		GtkTextIter itStart, itEnd;
		gtk_text_buffer_get_iter_at_offset(buffer1, &itStart, 0);
		gtk_text_buffer_get_iter_at_offset(buffer1, &itEnd, gtk_text_buffer_get_char_count(buffer1));
		gtk_text_buffer_delete(buffer1, &itStart, &itEnd);
		gtk_text_buffer_insert_at_cursor(buffer1, info.m_strDesc.c_str(), -1);

		gtk_entry_set_text(GTK_ENTRY(entry19), info.m_strAccount);

		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton15), info.m_bAnonymous);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton16), !info.m_bUseFirewall);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton17), info.m_bPassiveMode);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton18), info.m_nEncryptType != CFtpInfo::ENCRYPT_NONE);
		if(info.m_nEncryptType)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton23), info.m_nEncryptData);

		switch(info.m_nEncryptType)
		{
			case CFtpInfo::ENCRYPT_SSLv2:
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio2), info.m_nEncryptType);
				break;
			case CFtpInfo::ENCRYPT_SSLv3:
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio3), info.m_nEncryptType);
				break;
			case CFtpInfo::ENCRYPT_TLSv1:
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio1), info.m_nEncryptType);
				break;
			default:
				break;
		}
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton19), info.m_nListLongFormat);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton20), info.m_nListResolveLinks);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton21), info.m_nListShowHidden);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton22), info.m_nListCompleteTime);

		//global proxy data
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton9), g_bPassiveMode);

		OnChk_Anonymous();
	}
	else
	{
		info.m_strTitle = gtk_entry_get_text(GTK_ENTRY(title_entry));

		//refresh tree widget text to make sure it is same as in title entry box
		GtkTreeIter iter;
		if(gtk_tree_selection_get_selected(treesel, (GtkTreeModel **)&model, &iter))
			gtk_list_store_set (model, &iter, 0, info.m_strTitle.c_str(), -1);
		
		info.m_strHost  = gtk_entry_get_text(GTK_ENTRY(host_entry));

		String strTmp;
		strTmp = gtk_entry_get_text(GTK_ENTRY(port_entry));
		unsigned long lData;
		lData = atoi(strTmp);
		info.m_uPort = lData;

		if(0 == gtk_combo_box_get_active(GTK_COMBO_BOX(combobox8)))
			info.m_dwProtocol = PROT_FTP;
		else
			info.m_dwProtocol = PROT_SFTP;

		info.m_strUser      = gtk_entry_get_text(GTK_ENTRY(user_entry));
		info.m_strPassword  = gtk_entry_get_text(GTK_ENTRY(pass_entry));
		info.m_strRemoteDir = gtk_entry_get_text(GTK_ENTRY(remotedir_entry));

		//get description
		GtkTextIter itStart, itEnd;
		gtk_text_buffer_get_iter_at_offset(buffer1, &itStart, 0);
		gtk_text_buffer_get_iter_at_offset(buffer1, &itEnd, gtk_text_buffer_get_char_count(buffer1));
		info.m_strDesc = gtk_text_buffer_get_text(buffer1, &itStart, &itEnd, FALSE);

		info.m_strAccount = gtk_entry_get_text(GTK_ENTRY(entry19));

		info.m_bAnonymous   = (0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton15)));
		info.m_bUseFirewall = !gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton16));
		info.m_bPassiveMode = (0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton17)));
		if(0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton18))) {
			if(0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(radio1))) {
				info.m_nEncryptType = CFtpInfo::ENCRYPT_TLSv1;
			} else if(0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(radio2))) {
				info.m_nEncryptType = CFtpInfo::ENCRYPT_SSLv2;
			} else if(0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(radio3))) {
				info.m_nEncryptType = CFtpInfo::ENCRYPT_SSLv3;
			}
		} else {
			info.m_nEncryptType = CFtpInfo::ENCRYPT_NONE;
		}
		info.m_nEncryptData = info.m_nEncryptType && (0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton23)));
		info.m_nListLongFormat = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton19));
		info.m_nListResolveLinks = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton20));
		info.m_nListShowHidden = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton21));
		info.m_nListCompleteTime = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton22));

		//global proxy data
		g_bPassiveMode = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton9));
	}
}

void SiteManagerDlg::EnableCtrls(bool bEnable)
{
	GtkWidget *host_entry = lookup_widget(m_pDialog, "host_entry");
	GtkWidget *port_entry = lookup_widget(m_pDialog, "port_entry");
	GtkWidget *combobox8 = lookup_widget(m_pDialog, "combobox8");
	GtkWidget *checkbutton15 = lookup_widget(m_pDialog, "checkbutton15");
	GtkWidget *desc_textview = lookup_widget(m_pDialog, "desc_textview");
	GtkWidget *title_entry = lookup_widget(m_pDialog, "title_entry");
	GtkWidget *user_entry = lookup_widget(m_pDialog, "user_entry");
	GtkWidget *pass_entry = lookup_widget(m_pDialog, "pass_entry");
	GtkWidget *remotedir_entry = lookup_widget(m_pDialog, "remotedir_entry");
	GtkWidget *entry19 = lookup_widget(m_pDialog, "entry19");
	GtkWidget *checkbutton16 = lookup_widget(m_pDialog, "checkbutton16");
	GtkWidget *checkbutton17 = lookup_widget(m_pDialog, "checkbutton17");
	GtkWidget *checkbutton18 = lookup_widget(m_pDialog, "checkbutton18");
 	GtkWidget *checkbutton19 = lookup_widget(m_pDialog, "checkbutton19");
	GtkWidget *checkbutton20 = lookup_widget(m_pDialog, "checkbutton20");
	GtkWidget *checkbutton21 = lookup_widget(m_pDialog, "checkbutton21");
	GtkWidget *checkbutton22 = lookup_widget(m_pDialog, "checkbutton22");

	gtk_widget_set_sensitive(title_entry, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(host_entry, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(port_entry, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(combobox8, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(checkbutton15, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(user_entry, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(pass_entry, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(remotedir_entry, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(desc_textview, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(entry19, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(checkbutton16, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(checkbutton17, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(checkbutton18, bEnable?TRUE:FALSE);
 	gtk_widget_set_sensitive(checkbutton19, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(checkbutton20, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(checkbutton21, bEnable?TRUE:FALSE);
	gtk_widget_set_sensitive(checkbutton22, bEnable?TRUE:FALSE);

	if(bEnable)
		OnChk_Anonymous();	//some ctrls must be always disabled, based on state
}

void on_site_new_popup (GtkMenuItem *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnCtxMenu_New();
}

void on_site_remove_popup (GtkMenuItem *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnCtxMenu_Delete();
}

void on_site_duplicate_popup (GtkMenuItem *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnCtxMenu_Duplicate();
}

void on_site_move_up_popup (GtkMenuItem *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnCtxMenu_MoveUp();
}

void on_site_move_down_popup (GtkMenuItem *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnCtxMenu_MoveDown();
}

void on_site_executed (GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col, gpointer userdata)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)userdata;
	pDlg->OnSiteTree_DblClick();
}

void on_site_selection_changed (GtkTreeSelection *treeselection, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnSiteTree_SelChange();
}

void on_site_connect_button (GtkMenuItem *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnBtn_OK();
}

void on_site_close_button (GtkMenuItem *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnBtn_Close();
}

void on_protocol_type_combo (GtkComboBox *widget, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	GtkWidget *combobox8 = lookup_widget(pDlg->m_pDialog, "combobox8");
	GtkWidget *port_entry = lookup_widget(pDlg->m_pDialog, "port_entry");

	//refresh default port for each protocol
	if(0 == gtk_combo_box_get_active(GTK_COMBO_BOX(combobox8)))
		gtk_entry_set_text(GTK_ENTRY(port_entry), "21");
	else
		gtk_entry_set_text(GTK_ENTRY(port_entry), "22");
}

void on_site_anonymous_button (GtkButton *menuitem, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	pDlg->OnChk_Anonymous();
}

void create_menu (GtkWidget *parent, GtkWidget *menubar1, GtkAccelGroup *accel_group, gpointer user_data)
{
	GtkWidget *menuitem4;
	GtkWidget *menu_item;
	GtkWidget *menu4;
	GtkWidget *separatormenuitem1;
	//GtkWidget *icon;

	menuitem4 = gtk_menu_item_new_with_mnemonic (_("_Edit"));
	gtk_widget_show (menuitem4);
	gtk_container_add (GTK_CONTAINER (menubar1), menuitem4);
	
	menu4 = gtk_menu_new ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem4), menu4);
	gtk_menu_set_accel_group (GTK_MENU (menu4), accel_group);
	
	// ... add menu items with accelerators ... 
	menu_item = gtk_menu_item_new_with_label(_("New site"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_site_new_popup), user_data);
	gtk_menu_append(menu4, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 
	//gtk_widget_add_accelerator (menu_item, "activate", accel_group, GDK_Insert, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));

	menu_item = gtk_menu_item_new_with_label(_("Remove site"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_site_remove_popup), user_data);
	gtk_menu_append(menu4, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 
	//gtk_widget_add_accelerator (menu_item, "activate", accel_group, GDK_Delete, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));

	menu_item = gtk_menu_item_new_with_label(_("Duplicate"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_site_duplicate_popup), user_data);
	gtk_menu_append(menu4, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 
	//gtk_widget_add_accelerator (menu_item, "activate", accel_group, GDK_Insert, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));

	separatormenuitem1 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem1);
	gtk_container_add (GTK_CONTAINER (menu4), separatormenuitem1);
	gtk_widget_set_sensitive (separatormenuitem1, FALSE);

	menu_item = gtk_menu_item_new_with_label(_("Move up"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_site_move_up_popup), user_data);
	gtk_menu_append(menu4, menu_item);
	gtk_widget_show (menu_item);  // Show the widget
	//gtk_widget_add_accelerator (menu_item, "activate", accel_group, GDK_Delete, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	
	menu_item = gtk_menu_item_new_with_label(_("Move down"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_site_move_down_popup), user_data);
	gtk_menu_append(menu4, menu_item);
	gtk_widget_show (menu_item);  // Show the widget
	//gtk_widget_add_accelerator (menu_item, "activate", accel_group, GDK_Delete, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
}

gint dlg_keyboard_handler(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	//Esc closes the dialog with cancel response
	if( event->keyval == GDK_Escape )  
	{
		SiteManagerDlg *pDlg = (SiteManagerDlg *)data;
		pDlg->OnBtn_Close();
		return TRUE;    //eat event (handled here)
	}
	return FALSE;
}

void on_proxy_type_combo (GtkComboBox *widget, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	GtkWidget *comboboxentry2 = lookup_widget(pDlg->m_pDialog, "comboboxentry2");

	//enable/disable these entries based on proxy type picked	
	GtkWidget *entry3 = lookup_widget(pDlg->m_pDialog, "entry3");
	GtkWidget *entry4 = lookup_widget(pDlg->m_pDialog, "entry4");
	GtkWidget *checkbutton9 = lookup_widget(pDlg->m_pDialog, "passive_mode_btn");
	GtkWidget *entry1 = lookup_widget(pDlg->m_pDialog, "entry1");
	GtkWidget *entry2 = lookup_widget(pDlg->m_pDialog, "entry2");

	//refresh default port for each protocol
	bool bEnable = (0 != gtk_combo_box_get_active(GTK_COMBO_BOX(comboboxentry2)));

	gtk_widget_set_sensitive(entry3, bEnable);
	gtk_widget_set_sensitive(entry4, bEnable);
	gtk_widget_set_sensitive(checkbutton9, bEnable);
	gtk_widget_set_sensitive(entry1, bEnable);
	gtk_widget_set_sensitive(entry2, bEnable);
}

void on_use_encryption (GtkComboBox *widget, gpointer user_data)
{
	SiteManagerDlg *pDlg = (SiteManagerDlg *)user_data;
	GtkWidget *checkbutton18 = lookup_widget(pDlg->m_pDialog, "checkbutton18");

	//enable/disable these entries based on check box state
	GtkWidget *radio1 = lookup_widget(pDlg->m_pDialog, "radio1");
	GtkWidget *radio2 = lookup_widget(pDlg->m_pDialog, "radio2");
	GtkWidget *radio3 = lookup_widget(pDlg->m_pDialog, "radio3");
	GtkWidget *checkbutton23 = lookup_widget(pDlg->m_pDialog, "checkbutton23");

	//refresh default port for each protocol
	bool bEnable = (0 != gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton18)));

	gtk_widget_set_sensitive(radio1, bEnable);
	gtk_widget_set_sensitive(radio2, bEnable);
	gtk_widget_set_sensitive(radio3, bEnable);
	gtk_widget_set_sensitive(checkbutton23, bEnable);
	gtk_widget_set_sensitive(checkbutton23, bEnable);
}
