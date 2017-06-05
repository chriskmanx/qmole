////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Window class for Options dialog
////////////////////////////////////////////////////////////////////////////

#include "OptionsDialog.h"
#include "core/IniFile.h"
#include "core/GuiLanguage.h"
#include "core/System.h"
#include "support.h"
#include "DualPanel.h"

#if _MSC_VER > 1000
 #pragma warning(disable:4786)
#endif

#ifdef _WIN32
  #include "core/_win/bootstart.h" //register program loading at boot time
#endif

extern DualPanel g_dp;
extern GtkWidget *atol_main;
extern GuiLanguage g_lang;
extern String g_strEditor;
extern String g_strTerminal;
extern bool g_bMinimizeToTray;
extern bool g_bNCSelection;
extern bool g_bRefreshPanelsOnFocus;
extern GdkColor g_labelTextColor;
extern GdkColor g_labelBackgroundColor;
extern bool g_bShowExtColumn;

const char *GetIniFile();
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
static void on_ok_clicked (GtkMenuItem *menuitem, gpointer user_data);

OptionsDialog::OptionsDialog()
{
	Create();
}

OptionsDialog::~OptionsDialog()
{
	Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void OptionsDialog::Create()
{
	m_pDialog = create_options_dialog (atol_main);
	OnDialogInit();
}

void OptionsDialog::OnDialogInit()
{
	GtkWidget *bootstart_btn  = lookup_widget(m_pDialog, "bootstart_btn");
	GtkWidget *maximize_btn   = lookup_widget(m_pDialog, "maximize_btn");
	GtkWidget *singleinst_btn = lookup_widget(m_pDialog, "singleinst_btn");
	GtkWidget *comboboxentry1 = lookup_widget(m_pDialog, "comboboxentry1");
	GtkWidget *min2tray_btn   = lookup_widget(m_pDialog, "min2tray_btn");
	GtkWidget *show_ext_btn   = lookup_widget(m_pDialog, "show_ext_btn");
	GtkWidget *restorepath_btn = lookup_widget(m_pDialog, "restorepath_btn");
	GtkWidget *nc_selection_btn = lookup_widget(m_pDialog, "nc_selection_btn");
	GtkWidget *refresh_panels_btn = lookup_widget(m_pDialog, "refresh_panels_btn");
	GtkWidget *editor_entry = lookup_widget(m_pDialog, "editor_entry");
#ifndef _WIN32
	GtkWidget *terminal_entry = lookup_widget(m_pDialog, "terminal_entry");
#endif
	GtkWidget *colorbtn1      = lookup_widget(m_pDialog, "colorbtn1");
	GtkWidget *colorbtn2      = lookup_widget(m_pDialog, "colorbtn2");
	GtkWidget *colorbtn3      = lookup_widget(m_pDialog, "colorbtn3");
	GtkWidget *colorbtn4      = lookup_widget(m_pDialog, "colorbtn4");

	//read options from the INI
	IniFile file;
	file.Load(GetIniFile());

	bool bBootstart;
	if(file.GetValue("Startup", "BootStart", bBootstart))
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(bootstart_btn), bBootstart);
	
	bool bMaximize;
	if(file.GetValue("Startup", "Maximize", bMaximize))
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(maximize_btn), bMaximize);

	bool bSingleInst;
	if(file.GetValue("Startup", "AllowSingleInstance", bSingleInst))
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(singleinst_btn), bSingleInst);

	bool bRestorePaths;
	if(file.GetValue("Startup", "RestorePaths", bRestorePaths))
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(restorepath_btn), bRestorePaths);

	bool bMin2Tray;
	if(file.GetValue("Display", "MinimizeToTray", bMin2Tray))
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(min2tray_btn), bMin2Tray);

	if(g_bShowExtColumn) //use global setting here
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(show_ext_btn), TRUE);
		
	if(g_bNCSelection) //use global setting here
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(nc_selection_btn), TRUE);

	if(g_bRefreshPanelsOnFocus) //use global setting here
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(refresh_panels_btn), TRUE);

	//fill combo box with available languages
	std::string strLang;
	gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry1), "English (default)");
	for(int i=0; i<g_lang.GetCatalogCount(); i++)
	{
		//map back from locale to Language name
		strLang = g_lang.GetLangName(g_lang.GetCatalogAt(i).c_str());
		gtk_combo_box_append_text(GTK_COMBO_BOX(comboboxentry1), strLang.c_str()); 
	}

	//select current saved INI language in the combo (not necessarily current language)
	file.GetValue("Display", "Language", m_strLocale, "");
	int nPos = g_lang.GetLocaleIdx(m_strLocale.c_str());
	gtk_combo_box_set_active(GTK_COMBO_BOX(comboboxentry1), nPos+1);

	gtk_entry_set_text(GTK_ENTRY(editor_entry), g_strEditor.c_str());
#ifndef _WIN32
	gtk_entry_set_text(GTK_ENTRY(terminal_entry), g_strTerminal.c_str());
#endif

	//init text/background colors reading widget's style
	GtkWidget *widget = GTK_WIDGET(g_dp.GetRightFileList().m_pWidget);
	gtk_color_button_set_color(GTK_COLOR_BUTTON(colorbtn1), &widget->style->text[GTK_STATE_NORMAL]);
	gtk_color_button_set_color(GTK_COLOR_BUTTON(colorbtn2), &widget->style->base[GTK_STATE_NORMAL]);
	gtk_color_button_set_color(GTK_COLOR_BUTTON(colorbtn3), &widget->style->text[GTK_STATE_SELECTED]);
	gtk_color_button_set_color(GTK_COLOR_BUTTON(colorbtn4), &widget->style->base[GTK_STATE_SELECTED]);
}

void OptionsDialog::OnDialogOK()
{
	GtkWidget *bootstart_btn  = lookup_widget(m_pDialog, "bootstart_btn");
	GtkWidget *maximize_btn   = lookup_widget(m_pDialog, "maximize_btn");
	GtkWidget *singleinst_btn = lookup_widget(m_pDialog, "singleinst_btn");
	GtkWidget *comboboxentry1 = lookup_widget(m_pDialog, "comboboxentry1");
	GtkWidget *min2tray_btn   = lookup_widget(m_pDialog, "min2tray_btn");
	GtkWidget *show_ext_btn   = lookup_widget(m_pDialog, "show_ext_btn");
	GtkWidget *restorepath_btn = lookup_widget(m_pDialog, "restorepath_btn");
	GtkWidget *nc_selection_btn = lookup_widget(m_pDialog, "nc_selection_btn");
	GtkWidget *refresh_panels_btn = lookup_widget(m_pDialog, "refresh_panels_btn");
	GtkWidget *editor_entry = lookup_widget(m_pDialog, "editor_entry");
#ifndef _WIN32
	GtkWidget *terminal_entry = lookup_widget(m_pDialog, "terminal_entry");
#endif
	GtkWidget *colorbtn1      = lookup_widget(m_pDialog, "colorbtn1");
	GtkWidget *colorbtn2      = lookup_widget(m_pDialog, "colorbtn2");
	GtkWidget *colorbtn3      = lookup_widget(m_pDialog, "colorbtn3");
	GtkWidget *colorbtn4      = lookup_widget(m_pDialog, "colorbtn4");

	//reload INI data because it might have changed in dialog
	IniFile file;
	file.Load(GetIniFile());

	//
	// write options back to the INI (and activate them if necessary)
	//
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(bootstart_btn)))
	{
		file.SetValue("Startup", "BootStart", 1);
		#ifdef _WIN32
			//register program loading at boot time
			if(!IsBootKeySet("atol"))
				RunProgramAtBoot("atol", System::GetAppPath().c_str(), TRUE);	//set
		#endif
	}
	else
	{
		file.SetValue("Startup", "BootStart", 0);
		#ifdef _WIN32
			//unregister program loading at boot time
			if(IsBootKeySet("atol"))
				RunProgramAtBoot("atol", System::GetAppPath().c_str(), FALSE);	//remove
		#endif
	}

	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(maximize_btn)))
		file.SetValue("Startup", "Maximize", 1);
	else
		file.SetValue("Startup", "Maximize", 0);

	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(singleinst_btn)))
		file.SetValue("Startup", "AllowSingleInstance", 1);
	else
		file.SetValue("Startup", "AllowSingleInstance", 0);

	//save selected language locale
	std::string strNewLocale;
	int nRes = gtk_combo_box_get_active(GTK_COMBO_BOX(comboboxentry1));
	if(nRes > 0) //first item in the list is "Default"
		strNewLocale = g_lang.GetCatalogAt(nRes-1).c_str();
	file.SetValue("Display", "Language", strNewLocale.c_str());
	
	//"minimize to tray" setting
	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(min2tray_btn)))
		g_bMinimizeToTray = true;
	else
		g_bMinimizeToTray = false;
	file.SetValue("Display", "MinimizeToTray", g_bMinimizeToTray);

	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(show_ext_btn)))
		g_bShowExtColumn = true;
	else
		g_bShowExtColumn = false;
	file.SetValue("Display", "ShowExtColumn", g_bShowExtColumn);

	//restore path settings
	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(restorepath_btn)))
		file.SetValue("Startup", "RestorePaths", 1);
	else
		file.SetValue("Startup", "RestorePaths", 0);

	//"NC-like selection" setting
	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(nc_selection_btn)))
		g_bNCSelection = true;
	else
		g_bNCSelection = false;
	file.SetValue("Operation", "NC_Selection", g_bNCSelection);

	//refresh Vfs::LOCAL panels on windowfocus setting
	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(refresh_panels_btn)))
		g_bRefreshPanelsOnFocus = true;
	else
		g_bRefreshPanelsOnFocus = false;
	file.SetValue("Operation", "RefreshPanels", g_bRefreshPanelsOnFocus);

	g_strEditor = gtk_entry_get_text(GTK_ENTRY(editor_entry));
	file.SetValue("Operation", "FileEditor", g_strEditor);

#ifndef _WIN32
	g_strTerminal = gtk_entry_get_text(GTK_ENTRY(terminal_entry));
	file.SetValue("Operation", "Terminal", g_strTerminal);
#endif

	//save text/background colors
	GdkColor color;
	gtk_color_button_get_color(GTK_COLOR_BUTTON(colorbtn1), &color);
	gchar *szColor = g_strdup_printf ("#%04x%04x%04x", color.red, color.green, color.blue);
	file.SetValue("Display", "TextColor", szColor);
	g_free(szColor);

	GdkColor color1;
	gtk_color_button_get_color(GTK_COLOR_BUTTON(colorbtn2), &color1);
	szColor = g_strdup_printf ("#%04x%04x%04x", color1.red, color1.green, color1.blue);
	file.SetValue("Display", "BackgroundColor", szColor);
	g_free(szColor);

	gtk_color_button_get_color(GTK_COLOR_BUTTON(colorbtn3), &g_labelTextColor);
	szColor = g_strdup_printf ("#%04x%04x%04x", g_labelTextColor.red, g_labelTextColor.green, g_labelTextColor.blue);
	file.SetValue("Display", "SelectionTextColor", szColor);
	g_free(szColor);

	gtk_color_button_get_color(GTK_COLOR_BUTTON(colorbtn4), &g_labelBackgroundColor);
	szColor = g_strdup_printf ("#%04x%04x%04x", g_labelBackgroundColor.red, g_labelBackgroundColor.green, g_labelBackgroundColor.blue);
	file.SetValue("Display", "SelectionBackgroundColor", szColor);
	g_free(szColor);
	
	//update widgets with new color
	gtk_widget_modify_text(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_NORMAL, &color);
	gtk_widget_modify_text(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_NORMAL, &color);
	gtk_widget_modify_base(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_NORMAL, &color1);
	gtk_widget_modify_base(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_NORMAL, &color1);
	gtk_widget_modify_text(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelTextColor);
	gtk_widget_modify_text(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelTextColor);
	gtk_widget_modify_base(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelBackgroundColor);
	gtk_widget_modify_base(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelBackgroundColor);

	//save new settings to INI
	file.Save();

	//if language selection changed, issue the warning
	if(0 != strcmp(m_strLocale.c_str(), strNewLocale.c_str()))
	{
		GtkWidget* msgbox;
		msgbox = gtk_message_dialog_new ( (GtkWindow*)m_pDialog,
			GTK_DIALOG_DESTROY_WITH_PARENT,
			(GtkMessageType)GTK_MESSAGE_INFO,
			(GtkButtonsType)GTK_BUTTONS_OK,
			_("You must restart Atol for this change to take effect!"));
		gtk_dialog_run (GTK_DIALOG (msgbox));
		gtk_widget_destroy (msgbox);	
	}
}

GtkWidget* OptionsDialog::create_options_dialog (GtkWidget* parent)
{
	GtkWidget *Options;
	GtkWidget *dialog_vbox3;
	GtkWidget *notebook1;
	GtkWidget *vbox4;
	GtkWidget *checkbutton0;
	GtkWidget *checkbutton1;
	GtkWidget *checkbutton2;
	GtkWidget *checkbutton3;
	GtkWidget *label5;
	GtkWidget *label3;
	GtkWidget *dialog_action_area3;
	GtkWidget *cancelbutton3;
	GtkWidget *okbutton3;
	GtkWidget *table1;
	GtkWidget *comboboxentry1; 
	GtkWidget *label8;
	GtkWidget *checkbutton6;
	GtkWidget *table2;
	GtkWidget *checkbutton7;
	GtkWidget *checkbutton8;
	GtkWidget *checkbutton9;
	GtkWidget *label9;
	GtkWidget *label10;
	GtkWidget *editor_entry;
#ifndef _WIN32
	GtkWidget *label11;
	GtkWidget *terminal_entry;
#endif
	GtkWidget *colorbtn1;
	GtkWidget *colorbtn2;
	GtkWidget *colorbtn3;
	GtkWidget *colorbtn4;
	GtkWidget *label21;
	GtkWidget *label22;
	GtkWidget *label23;
	GtkWidget *label24;

	Options = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (Options), _("Options"));
	gtk_window_set_modal (GTK_WINDOW (Options), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (Options), TRUE);
	gtk_window_set_type_hint (GTK_WINDOW (Options), GDK_WINDOW_TYPE_HINT_DIALOG);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (Options), TRUE);
#endif
	if(parent)
		gtk_window_set_transient_for(GTK_WINDOW (Options), GTK_WINDOW(parent));   //set parent
		
#if GTK_CHECK_VERSION(2,4,0) //new API TOFIX set proper version
	#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
		gtk_window_set_keep_above(GTK_WINDOW (Options), TRUE);
	#endif
#endif
	gtk_window_set_destroy_with_parent (GTK_WINDOW (Options), TRUE);
	gtk_window_set_resizable (GTK_WINDOW (Options), FALSE);
	gtk_widget_realize(Options);
	gdk_window_set_decorations(Options->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE)); 

	dialog_vbox3 = GTK_DIALOG (Options)->vbox;
	gtk_widget_show (dialog_vbox3);
	
	notebook1 = gtk_notebook_new ();
	gtk_widget_show (notebook1);
	gtk_box_pack_start (GTK_BOX (dialog_vbox3), notebook1, TRUE, TRUE, 0);
	
	//
	// "Startup" tab
	//

	vbox4 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox4);
	gtk_container_add (GTK_CONTAINER (notebook1), vbox4);

	checkbutton0 = gtk_check_button_new_with_mnemonic (_("Start at logon"));
	gtk_widget_show (checkbutton0);
	gtk_box_pack_start (GTK_BOX (vbox4), checkbutton0, FALSE, FALSE, 0);
#ifndef _WIN32
	gtk_widget_set_sensitive (checkbutton0, FALSE); //TOFIX temp disable until implemented
#endif

	checkbutton1 = gtk_check_button_new_with_mnemonic (_("Maximize on startup"));
	gtk_widget_show (checkbutton1);
	gtk_box_pack_start (GTK_BOX (vbox4), checkbutton1, FALSE, FALSE, 0);
	
	checkbutton2 = gtk_check_button_new_with_mnemonic (_("Allow single instance only"));
	gtk_widget_show (checkbutton2);
	gtk_box_pack_start (GTK_BOX (vbox4), checkbutton2, FALSE, FALSE, 0);
#ifndef _WIN32
	gtk_widget_set_sensitive (checkbutton2, FALSE); //TOFIX temp disable until implemented
#endif
	
	checkbutton3 = gtk_check_button_new_with_mnemonic (_("Remember/restore panel paths"));
	gtk_widget_show (checkbutton3);
	gtk_box_pack_start (GTK_BOX (vbox4), checkbutton3, FALSE, FALSE, 0);

	label5 = gtk_label_new (_("Startup"));
	gtk_widget_show (label5);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 0), label5);
	
	//
	// "Display" tab
	//

	table1 = gtk_table_new (2, 5, FALSE);
	gtk_widget_show (table1);
	gtk_container_add (GTK_CONTAINER (notebook1), table1);

	comboboxentry1 = gtk_combo_box_new_text (); //ex gtk_combo_box_entry_new_text()
	gtk_widget_show (comboboxentry1);
	gtk_table_attach (GTK_TABLE (table1), comboboxentry1, 1, 2, 2, 3,
				        (GtkAttachOptions) (GTK_FILL),
					    (GtkAttachOptions) (GTK_FILL), 0, 0);
 
	label8	= gtk_label_new (_("Language:"));
	gtk_widget_show (label8);
	gtk_table_attach (GTK_TABLE (table1), label8, 0, 1, 2, 3,
		                (GtkAttachOptions) (GTK_FILL),
			            (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label8), 0, 0.5);

	checkbutton6 = gtk_check_button_new_with_mnemonic (_("Minimize to tray"));
	gtk_widget_show (checkbutton6);
	gtk_table_attach (GTK_TABLE (table1), checkbutton6, 0, 2, 4, 5,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);

	checkbutton8 = gtk_check_button_new_with_mnemonic (_("Show file extension in a separate column"));
	gtk_widget_show (checkbutton8);
	gtk_table_attach (GTK_TABLE (table1), checkbutton8, 0, 2, 5, 6,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);

	colorbtn1 = gtk_color_button_new();
	gtk_widget_show (colorbtn1);
	gtk_table_attach (GTK_TABLE (table1), colorbtn1, 1, 2, 6, 7, (GtkAttachOptions) (0), (GtkAttachOptions) (0), 0, 0);

	label21	= gtk_label_new (_("Text color:"));
	gtk_widget_show (label21);
	gtk_table_attach (GTK_TABLE (table1), label21, 0, 1, 6, 7, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label21), 0, 0.5);
	
	colorbtn2 = gtk_color_button_new();
	gtk_widget_show (colorbtn2);
	gtk_table_attach (GTK_TABLE (table1), colorbtn2, 1, 2, 7, 8, (GtkAttachOptions) (0), (GtkAttachOptions) (0), 0, 0);

	label22	= gtk_label_new (_("Background color:"));
	gtk_widget_show (label22);
	gtk_table_attach (GTK_TABLE (table1), label22, 0, 1, 7, 8, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label22), 0, 0.5);

	colorbtn3 = gtk_color_button_new();
	gtk_widget_show (colorbtn3);
	gtk_table_attach (GTK_TABLE (table1), colorbtn3, 1, 2, 8, 9, (GtkAttachOptions) (0), (GtkAttachOptions) (0), 0, 0);

	label23	= gtk_label_new (_("Selected Text color:"));
	gtk_widget_show (label23);
	gtk_table_attach (GTK_TABLE (table1), label23, 0, 1, 8, 9, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label23), 0, 0.5);
	
	colorbtn4 = gtk_color_button_new();
	gtk_widget_show (colorbtn4);
	gtk_table_attach (GTK_TABLE (table1), colorbtn4, 1, 2, 9, 10, (GtkAttachOptions) (0), (GtkAttachOptions) (0), 0, 0);

	label24	= gtk_label_new (_("Selected Background color:"));
	gtk_widget_show (label24);
	gtk_table_attach (GTK_TABLE (table1), label24, 0, 1, 9, 10, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label24), 0, 0.5);

	label3 = gtk_label_new (_("Display"));
	gtk_widget_show (label3);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 1), label3);

	//
	// "Operation" tab
	//

	table2 = gtk_table_new (2, 5, FALSE);
	gtk_widget_show (table2);
	gtk_container_add (GTK_CONTAINER (notebook1), table2);

	checkbutton7 = gtk_check_button_new_with_mnemonic (_("NC-like selection"));
	gtk_widget_show (checkbutton7);
	gtk_table_attach (GTK_TABLE (table2), checkbutton7, 0, 2, 0, 1,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);

	checkbutton9 = gtk_check_button_new_with_mnemonic (_("Refresh panels on window focus"));
	gtk_widget_show (checkbutton9);
	gtk_table_attach (GTK_TABLE (table2), checkbutton9, 0, 2, 1, 2,
					(GtkAttachOptions) (GTK_FILL),
 					(GtkAttachOptions) (0), 0, 0);

	label10 = gtk_label_new (_("Editor"));
	gtk_widget_show (label10);
	gtk_table_attach (GTK_TABLE (table2), label10, 0, 1, 2, 3,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);

	editor_entry = gtk_entry_new();
	gtk_widget_show (editor_entry);
	gtk_table_attach (GTK_TABLE (table2), editor_entry, 1, 2, 2, 3,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);

#ifndef _WIN32
	label11 = gtk_label_new (_("Terminal"));
	gtk_widget_show (label11);
	gtk_table_attach (GTK_TABLE (table2), label11, 0, 1, 3, 4,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);

	terminal_entry = gtk_entry_new();
	gtk_widget_show (terminal_entry);
	gtk_table_attach (GTK_TABLE (table2), terminal_entry, 1, 2, 3, 4,
					(GtkAttachOptions) (GTK_FILL),
					(GtkAttachOptions) (0), 0, 0);
#endif

	label9 = gtk_label_new (_("Operation"));
	gtk_widget_show (label9);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook1), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook1), 2), label9);

	///////

	dialog_action_area3 = GTK_DIALOG (Options)->action_area;
	gtk_widget_show (dialog_action_area3);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area3), GTK_BUTTONBOX_END);

	cancelbutton3 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton3);
	gtk_dialog_add_action_widget (GTK_DIALOG (Options), cancelbutton3, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton3, GTK_CAN_DEFAULT);
	
	okbutton3 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton3);
	gtk_container_add (GTK_CONTAINER(dialog_action_area3), okbutton3);
	GTK_WIDGET_SET_FLAGS (okbutton3, GTK_CAN_DEFAULT);

	g_signal_connect (okbutton3, "clicked",	G_CALLBACK (on_ok_clicked), this);

	// Store pointers to all widgets, for use by lookup_widget()
	GLADE_HOOKUP_OBJECT_NO_REF (Options, Options, "Options");
	GLADE_HOOKUP_OBJECT_NO_REF (Options, dialog_vbox3, "dialog_vbox3");
	GLADE_HOOKUP_OBJECT (Options, notebook1, "notebook1");
	GLADE_HOOKUP_OBJECT (Options, vbox4, "vbox4");
	GLADE_HOOKUP_OBJECT (Options, checkbutton0, "bootstart_btn");
	GLADE_HOOKUP_OBJECT (Options, checkbutton1, "maximize_btn");
	GLADE_HOOKUP_OBJECT (Options, checkbutton2, "singleinst_btn");
	GLADE_HOOKUP_OBJECT (Options, checkbutton3, "restorepath_btn");
	GLADE_HOOKUP_OBJECT (Options, checkbutton6, "min2tray_btn");
	GLADE_HOOKUP_OBJECT (Options, checkbutton8, "show_ext_btn");
	GLADE_HOOKUP_OBJECT (Options, label5, "label5");
	GLADE_HOOKUP_OBJECT (Options, label3, "label3");
	GLADE_HOOKUP_OBJECT (Options, table1, "table1");
	GLADE_HOOKUP_OBJECT (Options, label8, "label8");
	GLADE_HOOKUP_OBJECT (Options, checkbutton7, "nc_selection_btn");
	GLADE_HOOKUP_OBJECT (Options, checkbutton9, "refresh_panels_btn");
	GLADE_HOOKUP_OBJECT (Options, comboboxentry1, "comboboxentry1");
	GLADE_HOOKUP_OBJECT_NO_REF (Options, dialog_action_area3, "dialog_action_area3");
	GLADE_HOOKUP_OBJECT (Options, cancelbutton3, "cancelbutton3");
	GLADE_HOOKUP_OBJECT (Options, okbutton3, "okbutton3");
	GLADE_HOOKUP_OBJECT (Options, editor_entry, "editor_entry");
#ifndef _WIN32
	GLADE_HOOKUP_OBJECT (Options, terminal_entry, "terminal_entry");
#endif
	GLADE_HOOKUP_OBJECT (Options, colorbtn1, "colorbtn1");
	GLADE_HOOKUP_OBJECT (Options, colorbtn2, "colorbtn2");
	GLADE_HOOKUP_OBJECT (Options, colorbtn3, "colorbtn3");
	GLADE_HOOKUP_OBJECT (Options, colorbtn4, "colorbtn4");

	return Options;
}

void on_ok_clicked (GtkMenuItem *menuitem, gpointer user_data)
{
	OptionsDialog *pDlg = (OptionsDialog *)user_data;
	pDlg->OnDialogOK();
	pDlg->Destroy();
}

